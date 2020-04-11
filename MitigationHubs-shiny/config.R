# paths and files related to app
apaths <- list(
    data_prepared = 'data_prepared',
    logs = 'logs'
)
afiles <- list(
    cases = file.path(apaths$data_prepared, 'data_landkreise.RData'),
    measuresf = file.path(apaths$data_prepared, 'data_landkreise_measures.RData'),
    log_update = file.path(apaths$logs, 'log_update.RData')
)

# paths and files related to Google Drive
gfiletokens <- list(
    cases = "1VfFHK-GefntAYQvF-nW7k29PzXVQ7LyuOvwsU-FKy-o",
    measuresf = "1bQ2PQJE9PW_-0Or7xHg0UxH10LEN8r8L1h5Y-hNZQwE",
    measuresr = "1J8KhzH5UwtVdXEViRASh1SVCpJSbicCpLb6Bedvf4JA",
    update = "18RqQ7fz5CN1XLA8SfTK4ZVuvjxxNvp-rvROVFISFDlo"
)
gtypes <- list(
    cases = "xlsx",
    measuresf = 'xlsx',
    measuresr = 'csv',
    update = 'csv'
)
gpaths <- list(
    cases = "data/data_cases",
    measuresf = "data/data_measures_filtered",
    measuresr = "data/data_measures_raw",
    update = "data/data_update"
)
gfiles <- list(
    cases = file.path(gpaths$cases, paste("data_cases", gtypes$cases, sep = '.')),
    measuresf = file.path(gpaths$measuresf, paste("data_measures_filtered", gtypes$measuresf, sep = '.')),
    measuresr = file.path(gpaths$measuresr, paste("data_measures_raw", gtypes$measuresr, sep = '.')),
    update = file.path(gpaths$update, paste("data_update", gtypes$update, sep = '.'))
)

lapply(apaths, function(p) if(!dir.exists(p)) dir.create(p, recursive = T))
lapply(gpaths, function(p) if(!dir.exists(p)) dir.create(p, recursive = T))

# configuration for Landkreise selection
nlk_max <- 6
clks_selected <- c('SK Heidelberg')

# plot configuration
gr_logax_breaks <- c(0.1,0.2,0.3,0.4,0.5)
csc <- list(
    dorange = '#fc910d',
    lorange = '#fcb13e',
    red = '#ed6d50',
    lblue = '#239cd3',
    dblue = '#1674b1'
)
