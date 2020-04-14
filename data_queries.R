require(httr)
require(rlist)
require(jsonlite)
require(readr)
require(dplyr)
library(googledrive)
library(readxl)

#' helper function to download daily RKI data for "Landkreise" and format them as a tibble
#'
#' @param n_entries check n_entries here https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6&view=list#data for the current moment
#' @param batch_size check batch size here https://services7.arcgis.com/mOBPykOjAyBO2ZKk/ArcGIS/rest/services/RKI_COVID19/FeatureServer/0
#' @param dir path to save data
#'
#' @return tibble
#' @export
#'
#' @examples dat <- query_arcgis_all()
query_arcgis_all <- function(n_entries = 36520,
                             batch_size = 5000,
                             force_download = FALSE,
                             write2file = T,
                             dir = './data/data_cases') {
  n_batch <- seq(0, plyr::round_any(n_entries, 1e3), by = batch_size)
  # check if data already queried
  time <- Sys.time() %>% 
    as.Date()
  time <- time# - 1
  file <- file.path(dir,paste0('data_landkreise','_',time,'.csv'))
  
  if(file.exists(file) & !force_download) {
    print('read from file')
    dat <- readr::read_csv(file, 
                           col_types = list(
                             X1 = col_double(),
                             id = col_double(),
                             IdBundesland = col_double(),
                             Bundesland = col_character(),
                             Landkreis = col_character(),
                             Altersgruppe = col_character(),
                             Geschlecht = col_character(),
                             AnzahlFall = col_double(),
                             AnzahlTodesfall = col_double(),
                             ObjectId = col_double(),
                             Meldedatum = col_datetime(format = ""),
                             IdLandkreis = col_character()
                           )
                           ) %>% 
      as_tibble()
  } else {
    print('query database')
    arcgis_url <- 'https://services7.arcgis.com/mOBPykOjAyBO2ZKk/ArcGIS/rest/services/RKI_COVID19/FeatureServer/0/'
    dat <- lapply(n_batch, function(n_b){
      qloc <- paste0(arcgis_url,
                     'query?where=ObjectId+>+0&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=',
                     as.character(n_b),'&resultRecordCount=&sqlFormat=standard&f=pgeojson&token='
                     )
      resp <- GET(qloc)
      if (http_error(resp)) stop('http error')
      #resp <- content(resp, as = 'parsed', type = 'application/json')
      resp <- content(resp, as = 'text', type = 'application/json', encoding = 'UTF-8') %>% 
        fromJSON(.)
      resp <- resp$features %>% 
        as_tibble()
      resp <- bind_cols(tibble(id = resp$id), resp$properties)
    }) %>% 
      bind_rows()
    dat$Meldedatum <- as.POSIXct(dat$Meldedatum/1000, origin = "1970-01-01")
    date <- max(dat$Meldedatum) %>% 
      as.Date()
    if (write2file) {
        if (!dir.exists(dir)) dir.create(dir)
        file <- file.path(dir,paste0('data_landkreise','_',date,'.csv'))
        readr::write_csv(x = dat, path = file)   
    }
  }
  return(list(dat = dat, time = time))
}

#' Helper funtion to query the unprocessed mitigation measures from GDrive
#'
#' @return
#' @export
#'
#' @examples
query_gform_measures <- function() {
    if(!drive_has_token()){
        stop("No authentification with googledrive set up so far, please use drive_auth() and sheets_auth(token = drive_token())")
    } else {
        if (!dir.exists(gpaths$measuresr)) dir.create(gpaths$measuresr)
        drive_download(
            file = as_id(gfiletokens$measuresr), 
            path = gfiles$measuresr,
            type = gtypes$measuresr, 
            overwrite = TRUE
        )
        response <- read_xlsx(path = gfiles$measuresr, sheet = 'Form Responses 2') %>% 
          select(matches("Wann wurden die Maßnahmen|Postleitzahl|Stadt|Um welche Maßnahme|Erzähle uns mehr|wieder aufgehoben|Wann wurde die Maßnahme aufgehoben")) %>% rename(wann=1, plz=2, stadt=3, was=4, info=5, aufgehoben=6, wann_aufgehoben=7)
        
        gverzeichnis.lookup <- read_delim(paste0(getwd(), "/data/data_landkreise/gemeindeverzeichnis.csv"), ";", escape_double = FALSE, trim_ws = TRUE)  %>% 
          select(matches("Amtl.Gemeindeschlüssel|PLZ Ort")) %>% rename(schlüssel=1, plz_ort=2)
        
        gverzeichnis <- tibble(
            plz = as.numeric(unlist(regmatches(gverzeichnis.lookup$plz_ort, gregexpr("[[:digit:]]+", gverzeichnis.lookup$plz_ort)))),
            stadt = sub("^\\s+", "", gsub('[[:digit:]]+', '', gverzeichnis.lookup$plz_ort)),
            IdLandkreis = gverzeichnis.lookup$schlüssel %>% substr(1, 5)
        )
        
        response <- response %>% as_tibble() %>% left_join(measures_short, by = 'was')
        if (nrow(filter(response, is.na(measure_short) & was != 'sonstiges')) > 0) {
            warning(
                paste0('Unconfigured short measure for measure(s) ',
                       paste0(filter(response, is.na(measure_short))$was, collapse = ' '),
                       'using defaults.')
            )
        }
        response <- response %>% 
            mutate(measure_short = if_else(is.na(measure_short), was, measure_short))
        
        #join by plz  
        joined.plz <- response %>% select(-stadt) %>% inner_join(gverzeichnis)
        
        #join by stadt
        joined.stadt <- response %>% select(-plz) %>% inner_join(gverzeichnis)
        
        #merge: filtered result
        result <- bind_rows(
            joined.stadt, joined.plz
        ) %>% #full_join(joined.plz, joined.stadt) %>% 
            distinct() %>% 
            filter(was != "sonstiges") %>% filter(!is.na(IdLandkreis))
        #failed matches, here post-processing is required
        joined.fail <- anti_join(
          response,
          result %>% select(-plz, -stadt))
        
        l <- list(
            result = result,
            joined.fail = joined.fail
        )
        
        return(l)
    }
}
