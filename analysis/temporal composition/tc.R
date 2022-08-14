#' ---
#' title: "01 - Temporal Composition"
#' author: "Valentyn"
#' date: "Last updated: `r Sys.Date()`"
#' ---
#+ echo=F ----------------------------------------------------------------------
rmarkdown::render(input = "./analysis/temporal composition/tc.R") # run to knit, don't uncomment
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

#+ declare-globals -------------------------------------------------------------
path_rada    <- "./data-private/derived/rada_hromada.csv"
path_hromada <- "./data-private/derived/hromada.csv"
path_time   <- "./data-private/derived/time_rada.csv"    

#+ declare-functions -----------------------------------------------------------


#+ load-data -------------------------------------------------------------------
ds0_rada <- readr::read_csv(path_rada)
ds0_hromada <- readr::read_csv(path_hromada)
ds0_time <- readr::read_csv(path_time)

#+ inspect-data ----------------------------------------------------------------

ds0_rada %>%
  explore::describe_all()

ds0_hromada %>%
  explore::describe_all()

ds0_time %>%
  explore::describe_all()
#+ transform-data ----------------------------------------------------------------

# separating date in separate columns
ds1_time  <- 
  ds0_time %>%
  mutate(year = lubridate::year(date)
         , month = lubridate::month(date)
         , day = lubridate::day(date)
         , .after = date
  )

# making NA for years when no events (from 2015 to 2020), then filling with 
# previous number of distinct radas; when no hromada before - 0 radas

ds1 <- 
  ds1_time %>% 
  group_by(hromada_code, year) %>%
  summarise(rada_count = n_distinct(rada_code), .groups = 'drop') %>%
  group_by(hromada_code) %>%
  complete(year = c(2015:2020)) %>%
  fill(rada_count, .direction = 'down') %>%
  mutate(rada_count = replace_na(rada_count, 0))

#+ plot-data ----------------------------------------------------------------

# Plot for number of amalgamated radas by year
ds1 %>%
  group_by(year) %>%
  summarise(n_radas = sum(rada_count)) %>%
  ggplot(aes(x=year, y=n_radas)) +
  geom_bar(stat='identity', position = 'identity') +
  geom_text(aes(label=n_radas), vjust = -0.2) +
  theme_classic()

# Then we can count radas in hromada by adding facet_wrap by hromadas to previous plot
# problem: a lot of hromadas, look for interactive plotting


