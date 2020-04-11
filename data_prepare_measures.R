l <- query_gform_measures()
LK_meas <- l$result %>%
    rename(date = wann, measure = was) %>% 
    mutate_at(vars(date), as.Date, format = '%m/%d/%y') %>% 
    mutate_at(vars(IdLandkreis), as.numeric) %>% 
    mutate(measure = sapply( strwrap(measure, 20, simplify=FALSE), paste, collapse="\n" ))
LK_meas_set <- LK_meas %>% 
    distinct(measure) %>% 
    rowid_to_column(var = 'measureId')
LK_meas <- LK_meas %>% 
    inner_join(LK_meas_set)
