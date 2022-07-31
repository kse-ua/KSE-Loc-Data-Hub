# For a given data set `ds` defined as
ds_in <- 
  tibble::tribble(
  ~id, ~codes_1, ~date_1, ~codes_2, ~date_2,
  1, "22"   , "2015-01-01", "22,33"   , "2021-01-01",
  2, "44,55", "2015-01-01", "44,55,66", "2021-01-01",
  3, "77,88", "2015-01-01", "77,99"   , "2021-01-01",
  4, "100"  , "2015-01-01", "100"     , "2021-01-01",
 )
# where `id` identifies hromadas (ATC) and `codes_*` contain the list of
# radas that comprise the hromada at that time (`date_*`)

ds_1 <- ds_in %>%
  dplyr::select(id, codes_1, date_1) %>%
  tidyr::separate(codes_1, into = c('code_1', 'code_2'), sep = ',', extra = 'merge') %>%
  # problem with separate() - need to know number of columns in advance
  tidyr::pivot_longer(!c(id, date_1), values_to = 'code', values_drop_na = TRUE) %>%
  select(!name) %>%
  rename(date = date_1)

ds_2 <- ds_in %>%
  dplyr::select(id, codes_2, date_2) %>%
  tidyr::separate(codes_2, into = c('code_1', 'code_2', 'code_3'), sep = ',', extra = 'merge') %>%
  tidyr::pivot_longer(!c(id, date_2), values_to = 'code', values_drop_na = TRUE) %>%
  select(!name) %>%
  rename(date = date_2)

ds_out <- rbind(ds_1, ds_2) %>% arrange(id, date)

# please develop a solution that would transform the data into the following form:

ds_out <- tibble::tribble(
  ~id ,~date        ,~code
  ,1  ,"2015-01-01" , 22
  ,1  ,"2021-01-01" , 22
  ,1  ,"2021-01-01" , 33
  
  ,2  ,"2015-01-01" , 44
  ,2  ,"2015-01-01" , 55
  ,2  ,"2021-01-01" , 44
  ,2  ,"2021-01-01" , 55
  ,2  ,"2021-01-01" , 66
  
  ,3  ,"2015-01-01" , 77
  ,3  ,"2015-01-01" , 88
  ,3  ,"2021-01-01" , 77
  ,3  ,"2021-01-01" , 99

  ,4  ,"2015-01-01" , 100
  ,4  ,"2021-01-01" , 100
)

# Notes & Thoughts
# 1. Let's try to stay in the tidyverse, if possible.
# 2. I think `purrr` package might be useful here, but not sure
# 3. Use `pivot_longer` for wide-to-long transformation
