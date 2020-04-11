l <- query_gform_measures()
LK_meas <- l$result %>%
    rename(date = wann, measure = was) %>% 
    mutate_at(vars(date), as.Date, format = '%m/%d/%y') %>% 
    mutate_at(vars(IdLandkreis), as.numeric)
LK_meas_set <- LK_meas %>% 
    distinct(measure)
