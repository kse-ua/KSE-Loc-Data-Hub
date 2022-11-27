make_bi_freq_table <- function(
  d
  , var1                # 1st categorical variable
  , var2=var1           # 2nd categorical variable
  ,idvar = "edb_service_id"
){
  # browser()
  # d <- ds1 # load the data set before testing the function
  # var1 = "gender"
  # var2 = "category_group"
  # to ensure only one variable is passed if univariate
  ls_var_names <- list(var1, var2)
  if( ls_var_names[1][[1]] == ls_var_names[2][[1]] ){
    ls_var_names <- ls_var_names[-2]
  }
  
  d1 <- d %>%
    # dplyr::group_by(.dots = unique(c(var1, var2)))%>% # simpler but depricated
    dplyr::group_by(!!!rlang::syms(ls_var_names)) %>%
    dplyr::summarize(
      row_count = n()
      ,id_count = n_distinct(!!rlang::sym(idvar)) # counts UNIUQE persons
      ,.groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      id_count_total = sum(id_count, na.rm =T)
    ) %>%
    dplyr::group_by(.dots = var1) %>%
    dplyr::mutate(
      var1_count = sum(id_count, na.rm = T)
      
      ,var1_prop = (var1_count/id_count_total)
      ,var12_prop = (id_count/var1_count)
      #
      ,var1_pct = scales::label_percent(accuracy = .1)(var1_prop)
      ,var12_pct = scales::label_percent(accuracy=.01)(var12_prop)
    ) %>%
    ungroup()
  
  # ,pct_1 = scales::label_percent()(total_1/total)
  # ,pct_12 = scales::label_percent()(n_people/total_1)
  #
  n_total <-  d1 %>% pull(id_count_total) %>% unique()
  return(d1)
}
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state")
# ds2 %>% make_bi_freq_table(var1="gender", var2="employment_state")
# ds2 %>% make_bi_freq_table("sex","employment_state")
# View relationship between two categorical variables as a set of bar graphs
make_bi_freq_graph <- function(
  d
  ,var1                # 1st categorical variable
  ,var2     = var1     # 2nd categorical variable
  ,idvar = "survey_oid"
  ,voption  ="plasma"  # a palette from {viridis} package
  ,n_breaks = 10       # number of labeled values on horizontal axis
  ,rev      = FALSE    # reverse the color gradient
){
  # var1 = "gender"
  # var1 = "is_episode_count"
  # var2 = var1
  # voption="plasma"
  # n_breaks = 10
  # d <- ds2
  # browser()
  d1 <- d  %>%
    make_bi_freq_table(var1, var2,idvar = idvar) %>%
    mutate_at(
      .vars = all_of(var1)
      ,.funs = ~factor(.data[[var1]])
    ) %>%
    mutate_at(
      .vars = all_of(var2)
      ,.funs = ~factor(.data[[var2]])
    )
  if(rev){
    d1 <- d1 %>%
      mutate_at(
        .vars = all_of(var1)
        ,.funs = ~fct_rev(.data[[var1]])
        
      )
  }
  
  d2 <- d1 %>% filter(!is.na(.data[[var1]])) # drop empty factors
  
  n_total <-  d2 %>% pull(id_count) %>% unique()
  
  g1 <- d2 %>%
    ggplot2::ggplot(ggplot2::aes_string(x =var1, y = "id_count", fill = var2 ))+
    ggplot2::geom_col(position = ggplot2::position_dodge(), alpha = .7, color = "black")+
    # ggplot2::geom_text(ggplot2::aes(label = n_people),position = ggplot2::position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
    ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = voption
                                  ,guide= guide_legend(reverse=T)
    )+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(
      expand=ggplot2::expansion(mult = c(0,.3))
      , labels = comma
      # ,breaks =pretty_breaks()
      ,breaks = breaks_pretty(n=n_breaks)
      
    )
  g1
  if(var1 == var2){
    g1 <- g1 +
      ggplot2::geom_text(ggplot2::aes_string(label = "var1_pct"),position = ggplot2::position_dodge(.9), hjust = -0.1, color = "black", size = 4)+
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0("  ",scales::comma(id_count) )
        )
        ,position = ggplot2::position_dodge(.9)
        # , vjust = -.5
        ,hjust = -1.1
        , color = "red"
        , size = 4
      )
    ggplot2::labs(y = "Count", title = paste0("Frequency distribution of (",var1,")"))
  }else{
    g1 <- g1 +
      ggplot2::geom_text(ggplot2::aes_string(label = "var12_pct"),position = ggplot2::position_dodge(.9)
                         # , vjust = -.5
                         ,hjust = -0.1
                         , color = "black", size = 4)+
      
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0("  ",scales::comma(id_count) )
        )
        ,position = ggplot2::position_dodge(.9)
        # , vjust = -.5
        ,hjust = -1.1
        , color = "red"
        , size = 4
      )
    g1 <- g1 + ggplot2::labs(y = "Number of unique respondents", title = paste0("Association between (",var1,") and (",var2,")"))
  }
  
  
  return(g1)
}
# How to use
# ds2 %>% make_bi_freq_graph(var1="sex", var2="employment_state")
# ds2 %>% make_bi_freq_graph(var1="employment_state", var2="sex")