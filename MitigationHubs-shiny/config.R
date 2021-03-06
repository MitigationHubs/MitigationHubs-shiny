library(tibble)
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
    measuresr = 'xlsx',
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

# type configuration for data
actypes = list(
    cases = list(
        LK_dat_csum_proc = c('text', 'date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'),
        LK_dat_csum_ma = c('text', 'date', 'numeric', 'numeric', 'numeric', 'date', 'date', 'logical', 'numeric', 'numeric', 'numeric'),
        LK_dat_csum_gr3d = c('numeric', 'date', 'numeric', 'text'),
        LK_dat_csum_gr7d = c('numeric', 'date', 'numeric', 'text'),
        LK_dat_csum_gr_oa = c('numeric', 'numeric'),
        LK_time = c('text'),
        LK_set = c('text', 'numeric') 
    ),
    measuresf = list(
        LK_meas = c('date', 'text', 'text', 'text', 'text', 'date', 'text', 'text', 'numeric', 'numeric', 'text'), 
        LK_meas_set = c('numeric', 'text', 'text')
    )
)

# configuration for Landkreise selection
nlk_max <- 4
clks_selected <- c('SK Heidelberg')

# configuration for measures
measures_short <- tibble(
    was = c(
        "Schul-, Kita- &/oder Kindergartenschließung",
        "Änderung Öffnungszeiten von Supermärkten für Risikogruppen",
        "Einrichtung von Einkaufshilfen für Risikogruppen",
        "Schließung von Gastronomiebetrieben",
        "Einschränkung des Universitäts-Betriebes",
        "Verbot von Veranstaltungen über 1000 Personen",
        "Verbot von Veranstaltungen über 100 Personen",
        "allgemeines Veranstaltungsverbot",
        "HomeOffice-Option für Arbeitnehmer eines großen Betriebs in der Region",
        "Schutzvorrichtungen zur Kontaktvermeidung in Läden wie Supermärkten oder Apotheken",
        "Betretungsregeln für öffentliche Geschäfte",
        "weitgehende Ladenschließungen (Supermärkte, Drogerien, Apotheken ausgenommen)",
        "1. Maßnahmenpaket der Bundesregierung",
        "2. Maßnahmenpaket der Bundesregierung",
        "bundesweite Schließung von Kindergärten & Schulen"
    ),
    measure_short = c(
        "Schul-, Kita- &/oder Kigaschließung",
        "Öffnungszeiten für Riskiogruppen",
        "Einkaufshilfen für Risikogruppen",
        "Schließung Gastronomie",
        "Einschränkung Universität",
        "Veranstaltungsverbot > 1000",
        "Veranstaltungsverbot > 100",
        "Veranstaltungsverbot",
        "Home Office für viele Beschäftigte",
        "Schutzmaßnahmen in Geschäften",
        "Betretungsregeln für Geschäfte",
        "Ladenschließungen",
        "1. Maßnahmenpaket",
        "2. Maßnahmenpaket",
        "bundesweite Schul-& KiGa-Schließungen"
    )
)

# plot configuration
gr_ax_breaks <- c(0.1,0.2,0.3,0.4,0.5)
csc <- list(
    dorange = '#fc910d',
    lorange = '#fcb13e',
    red = '#ed6d50',
    lblue = '#239cd3',
    dblue = '#1674b1'
)
