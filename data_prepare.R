source('MitigationHubs-shiny/config.R', local = TRUE, encoding = 'UTF-8')
source('data_queries.R', local = TRUE, encoding = 'UTF-8')
library(stringr)
library(tidyr)
library(tibble)
library(googledrive)
library(writexl)
library(readr)

# GDrive authentication (via Browser; Don't add any credentials here, the script is on a public GitHub!!)
drive_auth()

# query and process case numbers
## check N_ENTRIES here: https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6&view=list#data
N_ENTRIES <- 93078
## query, prepare and write data locally
source('data_prepare_cases.R', local = TRUE, encoding = 'UTF-8')

dat <- list(
    LK_dat_csum_proc = LK_dat_csum_proc,
    LK_dat_csum_ma = LK_dat_csum_ma,
    LK_dat_csum_gr3d = LK_dat_csum_gr3d,
    LK_dat_csum_gr7d = LK_dat_csum_gr7d,
    LK_dat_csum_gr_oa = LK_dat_csum_gr_oa,
    LK_time = tibble(time = LK_time),
    LK_set = LK_set
)

if (!dir.exists(gpaths$cases)) dir.create(gpaths$cases)
write_xlsx(
    dat, 
    path = gfiles$cases
)
## upload to GDrive 
if(!drive_has_token()){
    print("No authentification with googledrive set up so far, please use drive_auth() and sheets_auth(token = drive_token())")
} else {
    drive_update(
        media = gfiles$cases, 
        file = as_id(gfiletokens$cases),
        name = 'MitigationHubs_cases'
    )
}

# query and process mitigation measures
source('data_prepare_measures.R', local = TRUE, encoding = 'UTF-8')

LK_meas <- LK_meas %>% 
    inner_join(LK_set, by = 'IdLandkreis')

dat <- list(
    LK_meas = LK_meas,
    LK_meas_set = LK_meas_set
)

if (!dir.exists(gpaths$measuresf)) dir.create(gpaths$measuresf)
write_xlsx(
    dat, 
    path = gfiles$measuresf
)
## upload to GDrive
if(!drive_has_token()){
    print("No authentification with googledrive set up so far, please use drive_auth() and sheets_auth(token = drive_token())")
} else {
    drive_update(
        media = gfiles$measuresf, 
        file = as_id(gfiletokens$measuresf),
        name = 'MitigationHubs_measures_filtered'
    )
}

# update last update time and upload to GDrive
time_update <- Sys.time()
dat <- tibble(
        name = c('cases', 'measuresf'),
        last_update = c(time_update, time_update)
)
if (!dir.exists(gpaths$update)) dir.create(gpaths$update)
write_csv(dat, path = gfiles$update)
if(!drive_has_token()){
    print("No authentification with googledrive set up so far, please use drive_auth() and sheets_auth(token = drive_token())")
} else {
    drive_update(
        media = gfiles$update, 
        file = as_id(gfiletokens$update),
        name = 'MitigationHubs_update'
    )
}
