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
    pull(idvar) %>%
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
