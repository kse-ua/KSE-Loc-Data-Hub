
view_a_person <- function(
  d                     #
  ,vars="all"                 # vector of variable names to be displayed
  ,show_random_id = TRUE     # replace with specific ID, otherwise random every time
  ,idvar = "person_oid" # name of the variable to be used as person ID
){
  # browser()
  # show_random_id = TRUE
  # show_random_id = 1402158
  # d <- ds1is
  if(show_random_id==TRUE){
    selected_person <- d %>%
      get_sample_uniques(.,  sample_size = 1,var_name =  idvar)
  }
  else{
    selected_person <- show_random_id
  }
  # select the rows
  d_person <- d %>%
    # filter(person_oid %in% selected_person ) %>%
    filter( !!rlang::sym(idvar) %in% selected_person )
  # select the columns
  if(!vars[1] == "all"){
    d_person <- d_person  %>%
      select(all_of(unique(c(idvar,"person_oid",vars))))
  }
  return(d_person)
}
# ds_is %>% view_a_person("person_oid",names(.),show_random_id = 1402158)
# ds0is %>% view_a_person(vars = c("person_oid","period_start"))
# ds0is %>% view_a_person(vars = c("period_start"))
# ds0is %>% view_a_person(vars = c("person_oid","period_start"),show_random_id = 3784794 )

# THis function would have to wait until IS and EA are joined
view_a_person_graph <- function(d,pid){

  # pid <- 1581109
  d1 <- ds0 %>%
    filter(person_oid %in% pid) %>%
    select(period_start, period_end, is_duration,
           obs_assessment_date, from_is_to_ea, is_stint_number, is_stint_count, has_ea_ever)

  g1 <- d1 %>%
    mutate(
      ea_event = factor(obs_assessment_date)
      ,is_stint_number = factor(is_stint_number)

    ) %>%
    ggplot(aes(y = is_stint_number))+
    geom_point(aes(x=obs_assessment_date, fill = ea_event), shape = 21, size = 5, alpha = .8)+
    geom_segment(
      aes(x=period_start, xend=period_end, y = is_stint_number, yend=is_stint_number)
      ,size = 1, alpha = .5, color = "black"
    )+
    geom_text(aes(label = is_duration, x = period_end, vjust = -.5 , hjust = 0))+
    # scale_y_continuous(breaks = c(1:9), expand = expansion(mult = c(.1,.5)))
    scale_y_discrete(expand = expansion(mult = c(.1,.2)))+
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 month",
                 date_labels = "%Y")+
    scale_fill_brewer(type = "qual", palette = 2)+
    labs(fill = "Employability\nAssessment", x = NULL, y = "Income Support Stint"
         ,title = "Employability Assessment events relative to Income Support history"
         ,subtitle = paste0("View for a single person (",pid,")")
         ,caption = "Left truncated data (2015-04 - 2021-03) | Stint duration indicated in months")
  return(g1)
}
# view_a_person_graph(ds0)
