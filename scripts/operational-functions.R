# Functions loaded by SOME scripts in the project

# Define bottom and right limits when import Excel sheet
trim_sheet_input <- function(
  d
  , skip_n_rows      = 0
  , total_n_rows     = nrow(d)
  , column_positions = 1:ncol(d)

){
  # browser()
  # d <- dto$EmpButLost
  # skip_n_rows  <- 4
  # total_n_rows <- 35
  # column_positions <- c(1,2,3)
  # nrow(d)
  d_out <- d %>%
    slice( (skip_n_rows+1):total_n_rows ) %>%
    select(all_of(column_positions))
  return(d_out)
}


draw_random_id <- function(d, idvar="person_oid", n = 1){
  all_unique_ids <- d %>%
    pull(!!rlang::sym(idvar) ) %>%
    unique()
  a_random_id <- sample(all_unique_ids, size = n)
  return(a_random_id)
}
# ds_is %>% draw_random_id()
# ds_ea %>% draw_random_id()

# see https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
# for details on using `.data[[]]` pronoun
get_sample <- function(
  d
  ,sample_size=10
  , idvar = "PERSON_OID"
  , uniques = TRUE # if TRUE, only unique values are sampled
  ,seed = 42
){
  # browser()
  sampling_universe <- d %>% pull(.data[[idvar]])
  if(uniques){
    sampling_universe <- sampling_universe %>% unique()
  }
  x_out <- sample(
    x        = sampling_universe
    ,size    = sample_size
    ,replace = FALSE
  )
  return(x_out)
}
# How to use
# a_sample <- ds2 %>% get_sample_uniques(sample_size =10, "person_oid")
# a_sample <- ds2 %>% get_sample_uniques(10)

# Graph a history of composition of hromada
# IMPORTANT: need to have admin and time file in data-private
graph_hromada_history <- function(target_hromada_ua_code){
  
  path_admin    <- "./data-private/derived/ua-admin-map.csv"
  path_time <- "./data-private/derived/time_rada.csv"
  ds_admin <- readr::read_csv(path_admin)
  ds_time <- readr::read_csv(path_time)
  
  d <- 
    ds_time %>% 
    filter(hromada_code == target_hromada_ua_code) %>% 
    group_by(hromada_code, date) %>% 
    summarize(
      rada_count = n_distinct(rada_code, na.rm = T)
    )
  
  hromada_label <-
    ds_admin %>% 
    filter(hromada_code == target_hromada_ua_code) %>%
    distinct(hromada_name, raion_name, oblast_name) %>% 
    mutate(
      unit_label = paste0("Громада: ", hromada_name, " | Район: ",raion_name, " | Область: ", oblast_name)
    ) %>% 
    pull(unit_label)
  
  g <- 
    d %>% 
    ggplot(aes(x = date, y = rada_count))+
    geom_line() +
    geom_point() +
    geom_text(aes(label = date), size = 3.2, vjust = 2) +
    scale_y_continuous(breaks = seq(from = 0, to = max(d$rada_count), by = 1),
                       limits = c(0, max(d$rada_count)+1))+
    labs(title = hromada_label, x = 'Дата зміни складу громади', y = 'Кількість рад')
  g
  
}
# How to use
# graph_hromada_history('UA56040270000014394')
