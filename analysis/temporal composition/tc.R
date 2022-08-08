#' ---
#' title: "01 - Temporal Composition"
#' author: "Valentyn"
#' date: "Last updated: `r Sys.Date()`"
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

#+ load-data -------------------------------------------------------------------
source("C:/Users/Dell/Documents/GitHub/ua-de-center/manipulation/0-import.R")

#+ inspect-data ----------------------------------------------------------------
ds1_event %>% glimpse()

ds1_event %>%
  explore::describe_all()

#+ transform-data ----------------------------------------------------------------
ds0_ts <- 
  ds1_event %>% 
  mutate(
    across(
      starts_with('rada_codes')
      , as.character
    ),
    across(
      starts_with('decision_date')
      , as.character
    )
    ) %>%
  tidyr::pivot_longer(
    cols = !starts_with("hromada_code")
  ) %>% 
  mutate(
    wave  = str_extract(name, "\\d$" )
    , name = str_extract(name, "(?<=\\_)(.*?)(?=\\_)")
    ) %>% 
  pivot_wider(
    names_from = "name", values_from = "value"
  ) %>% 
  mutate(
    code = str_split(codes, ',')
  ) %>% 
  unnest(cols = c("code")) %>% 
  select(-codes, -wave) %>% 
  arrange(hromada_code, date, code) %>%
  mutate(
    # hromada_code = hromada_code %>% as.character()
    date = date %>% as.Date()
    # ,code = code %>% as.integer()
  ) %>%
  filter(
    !is.na(date)
  )

ds0_ts %>%
  explore::describe_all()

##
