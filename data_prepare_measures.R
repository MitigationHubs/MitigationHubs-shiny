LK_meas <- query_gform_measures() %>%
    rename(date = wann, measure = was) %>% 
    mutate_at(vars(date), as.Date, format = '%m/%d/%y') %>% 
    mutate_at(vars(IdLandkreis), as.numeric)
LK_meas_set <- LK_meas %>% 
    distinct(measure)
