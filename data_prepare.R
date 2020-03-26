source('MitigationHubs-shiny/config.R')
source('data_queries.R')
library(stringr)
library(tidyr)
library(tibble)

# plot and fit cummulative growth of Covid19 infections
# for Bundesländer & Landkreise
LK_dat_list <- query_arcgis_all()
LK_time <- as.character(LK_dat_list$time)
LK_dat <- LK_dat_list$dat

LK_dat_csum <- LK_dat %>%
    group_by(Landkreis, Meldedatum, IdLandkreis) %>% 
    summarise(sum_LK = sum(AnzahlFall)) %>% 
    group_by(Landkreis) %>% 
    arrange(Meldedatum, .by_group = T) %>% 
    mutate(csum = cumsum(sum_LK)) %>% 
    mutate_at(vars(IdLandkreis), as.numeric) %>%
    mutate_at(vars(Meldedatum), as.Date) %>% 
    ungroup()

LK_pred <- read_delim('data/data_predictors/predictors_landkreise_mit_mobilitaet.csv', delim = ",") %>% 
    as_tibble() %>% 
    select(Schluessel, !!sym('Bevölkerung(2018)')) %>% 
    rename(IdLandkreis = Schluessel) %>% 
    mutate(!!sym('Bevölkerung(2018)') := str_replace_all(!!sym('Bevölkerung(2018)'),' ','')) %>% 
    mutate_at(vars(IdLandkreis, !!sym('Bevölkerung(2018)')), as.numeric)
    #mutate(!!sym('männlich') := str_replace_all(!!sym('männlich'),' ','')) %>% 
    #mutate_at(vars(IdLandkreis, !!sym('männlich')), as.numeric) %>% 
    #mutate(!!sym('weiblich') := str_replace_all(!!sym('weiblich'),' ','')) %>% 
    #mutate_at(vars(IdLandkreis, !!sym('weiblich')), as.numeric)

LK_dat_csum_proc <- LK_dat_csum %>% 
    group_by(IdLandkreis, Meldedatum) %>% 
    inner_join(LK_pred, by = 'IdLandkreis') %>% 
    mutate(csum_LK_100kEinwohner = 1e5*csum/!!sym('Bevölkerung(2018)')) %>% 
    ungroup()

# growth rates based on 3-day-rolling mean and excluding first and last 3 days
LK_dat_con <- LK_dat_csum %>% 
    select(Landkreis, Meldedatum) %>% 
    group_by(Landkreis) %>% 
    summarise(datmn = min(Meldedatum), datmx = max(Meldedatum)) %>% 
    mutate(datsq = purrr::map2(.$datmn, .$datmx, function(x,y) seq.Date(as.Date(x), as.Date(y), 1))) %>% 
    mutate(datmn = datmn + 3, datmx = datmx - 3) %>% 
    unnest() %>% 
    mutate(use = if_else(datsq >= datmn & datsq <= datsq, T, F)) %>% 
    rename(Meldedatum = datsq)

LK_dat_csum_ma <- LK_dat_csum %>% 
    right_join(., LK_dat_con) %>% 
    group_by(IdLandkreis) %>% 
    mutate(csum_ma = zoo::rollapply(csum,3,mean,fill = NA,na.rm=T)) %>% 
    filter(!is.na(csum_ma)) %>% 
    group_by(IdLandkreis, Meldedatum) %>% 
    inner_join(LK_pred, by = 'IdLandkreis') %>% 
    mutate(csum_ma_LK_100kEinwohner = 1e5*csum_ma/!!sym('Bevölkerung(2018)')) %>% 
    ungroup()

# growth rates over all (using 3-day ma)
LK_dat_csum_gr_oa <- LK_dat_csum_ma %>%
    group_by(IdLandkreis) %>%
    nest() %>%
    mutate(lm = purrr::map(.$data, function(d) lm(y ~ x, rename(d, x = Meldedatum, y = csum_ma) %>% mutate(y = log(y))))) %>% 
    mutate(gr = purrr::map(.$lm, function(l) coef(l)[2])) %>% 
    select(-data,-lm) %>% 
    unnest(gr) %>% #print()
    #filter(csum_ma_LK_100kEinwohner > 0.05) %>% 
    filter(gr > 0)

# growth rates on 3-day ma
LK_dat_csum_gr <- LK_dat_csum_ma %>%
    group_by(IdLandkreis) %>% 
    nest() %>% 
    mutate(gr_ma = purrr::map(.$data, function(d) {
        if (nrow(d) >= 3) {
            z <- d %>% select(Meldedatum, csum_ma) %>% as.ts() %>% zoo::as.zoo()
            zoo::time(z) <- d$Meldedatum#z[,1]
            z <- z %>% zoo::rollapply(., width = 3, FUN = function(z) coef(lm(csum_ma ~ Meldedatum, data = as.data.frame(z))), by.column = F) %>% as.data.frame() %>%
                rename(gr_ma = Meldedatum) %>% tibble::rownames_to_column(.,'Meldedatum') %>% as_tibble() %>% select(Meldedatum, gr_ma)
        } else {
            z <- tibble(Meldedatum = NA_real_, gr_ma = NA_real_)
        }
        return(z)
    })
    ) %>% 
    unnest(gr_ma) %>% 
    inner_join(LK_dat_csum_ma %>% distinct(Landkreis,IdLandkreis)) %>% 
    mutate_at(vars(Meldedatum), as.Date)

LK_set <- LK_dat_csum_gr %>% 
    distinct(Landkreis, IdLandkreis)

if (!dir.exists(file.path('MitigationHubs-shiny', paths$data_prepared))) dir.create(file.path('MitigationHubs-shiny', paths$data_prepared))
save(LK_dat_csum_proc, LK_dat_csum_ma, LK_dat_csum_gr, LK_dat_csum_gr_oa, LK_time, LK_set, file = file.path('MitigationHubs-shiny', files$LK_dat))
