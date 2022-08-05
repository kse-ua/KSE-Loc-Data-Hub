#' ---
#' title: "01 - Compositon Codes"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F ----------------------------------------------------------------------
# rmarkdown::render(input = "./analysis/tasks/01-composition-codes.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# Task Definition")

#+ load-date -------------------------------------------------------------------
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

# please develop a solution that would transform the data into the following form:

ds_out_target <- tibble::tribble(
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
) %>% 
  mutate(
    id = id %>% as.integer()
    ,date = date %>% as.Date()
    ,code = code %>% as.integer()
  )

# Notes & Thoughts
# 1. Let's try to stay in the tidyverse, if possible.
# 2. I think `purrr` package might be useful here, but not sure
# 3. Use `pivot_longer` for wide-to-long transformation

#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# Valentyn")

#+ solution-1 ------------------------------------------------------------------
# there will be a lot of repeating code chunks that should be vectorized or wrapped in a function

# put all code_* and date_* into vectors to vectorize code in the future
# codes <- names(dplyr::select(ds_in,contains("codes_")))
# dates <- names(dplyr::select(ds_in,contains("date_")))

# count how many different codes in a codes column
(ncol <- max(stringr::str_count(ds_in$codes_1, ","))+1)
(ncol_1 <- max(stringr::str_count(ds_in$codes_2, ","))+1)

# split dataset and transform each one separately

ds_1 <- ds_in %>%
  dplyr::select(id, codes_1, date_1) %>%
  tidyr::separate(codes_1, into = c(paste0('code_', c(1:ncol))), sep = ',') %>%
  tidyr::pivot_longer(!c(id, date_1), values_to = 'code', values_drop_na = TRUE) %>%
  dplyr::select(!name) %>%
  dplyr::rename(date = date_1)
ds_1 

ds_2 <- ds_in %>%
  dplyr::select(id, codes_2, date_2) %>%
  tidyr::separate(codes_2, into = c(paste0('code_', c(1:ncol_1))), sep = ',') %>%
  tidyr::pivot_longer(!c(id, date_2), values_to = 'code', values_drop_na = TRUE) %>%
  dplyr::select(!name) %>%
  dplyr::rename(date = date_2)
ds_2

ds_out <- rbind(ds_1, ds_2) %>%
  mutate(
    id = id %>% as.integer()
    ,date = date %>% as.Date()
    ,code = code %>% as.integer()
  )%>% 
  dplyr::arrange(id, date)

identical(ds_out, ds_out_target) # if TRUE, then QED

#+ solution-2 ------------------------------------------------------------------

#+ solution-3 ------------------------------------------------------------------