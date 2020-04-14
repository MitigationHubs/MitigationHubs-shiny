l <- query_gform_measures()
LK_meas <- full_join(l$result, l$result.global) %>%
    rename(date = wann, measure = was) %>% 
    mutate_at(vars(date), as.Date, format = '%m/%d/%y') %>% 
    mutate_at(vars(IdLandkreis), as.numeric) %>% 
    mutate(measure_short = sapply( strwrap(measure_short, 15, simplify=FALSE), paste, collapse="\n" ))
LK_meas_set <- LK_meas %>% 
    distinct(measure, measure_short) %>% 
    rowid_to_column(var = 'measureId')
LK_meas <- LK_meas %>% 
    inner_join(LK_meas_set)
