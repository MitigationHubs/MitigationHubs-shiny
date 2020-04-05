paths <- list(
    data_prepared = 'data_prepared'
)
files <- list(
    LK_dat = file.path(paths$data_prepared, 'data_landkreise.RData')
)

# configuration for Landkreise selection
nlk_max <- 6
clks_selected <- c('SK Heidelberg')

# plot configuration
gr_logax_breaks <- c(0.1,0.2,0.3,0.4,0.5)
