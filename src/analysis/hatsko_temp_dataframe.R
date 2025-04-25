ds0 %>% 
  mutate(prep_sum = rowSums(select(., starts_with('prep'), ends_with('feb')), na.rm = T)) %>% 
  select(prep_sum)


d1 <- ds0 %>% 
  select(hromada_code, prep_winter_count, starts_with('prep') & ends_with('feb')) %>% 
  select(-c(prep_score_feb,prep_partly_backup_feb)) %>% 
  mutate(prep_sum = rowSums(select(., starts_with('prep'), ends_with('feb')), na.rm = T)) %>% 
  select(prep_sum) %>% mutate(prep_sum = as.factor(prep_sum))

d1$prep_sum

d1 %>% group_by(prep_sum) %>% count() 

table(d1) %>% prop.table() %>% round(digits = 2) 
                                            
ds0$prep_winter_count %>% table() %>% prop.table() %>% round(digits = 2)

d2  <-  ds_general0 %>% left_join(d1, by = 'hromada_code') %>% 
  left_join(ds_survey %>% select(hromada_code, admin_services_stopped, garbage_interruptions)
            , by = "hromada_code")
d3 <- d2 %>% select(hromada_code, hromada_name, hromada_full_name, raion_code, raion_name,
              oblast_code, oblast_name, oblast_name_en, starts_with('prep'), 
              prep_winter_count, admin_services_stopped, garbage_interruptions) 

write.csv(d3, 'hromada_scores.csv')
ds0$prep_winter_count