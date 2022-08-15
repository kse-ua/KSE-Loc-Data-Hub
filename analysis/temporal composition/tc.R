#' ---
#' title: "01 - Temporal Composition"
#' author: "Valentyn"
#' date: "Last updated: `r Sys.Date()`"
#' ---
#+ echo=F ----------------------------------------------------------------------
# rmarkdown::render(input = "./analysis/temporal composition/tc.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
requireNamespace("trelliscopejs"   )# Interactive faceting

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
path_admin    <- "./data-private/derived/ua-admin-map.rds"
path_plots <- './analysis/temporal composition/plots'


Sys.setlocale("LC_CTYPE", "russian")
#+ declare-functions -----------------------------------------------------------

plot_hromada <- function(x, path = getwd()) {
  tmp <- ds2 %>%
    filter(oblast_hromada == x)
  p <- ggplot(tmp, aes(x=year, y=rada_count)) +
    geom_bar(stat='identity', position = 'identity') +
    geom_text(aes(label=rada_count), vjust = -0.2) +
    theme_classic()
  ggsave(p, file=paste0(x, ".png"), path = path)
}

#+ load-data -------------------------------------------------------------------
ds0_time <- readr::read_csv(path_time)
ds_admin <- readr::read_rds(path_admin)

#+ inspect-data ----------------------------------------------------------------

ds0_time %>%
  explore::describe_all()

ds_admin %>%
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
  mutate(rada_count = replace_na(rada_count, 0)) %>%
  ungroup()

# adding administrative names
hromada_names <- ds_admin %>%
  select(hromada_code, hromada_name, raion_name, oblast_name) %>%
  distinct()

# important - hromada names have 133 duplicates
hromada_names  %>%
  group_by(hromada_name) %>%
  summarise(n=n()) %>%
  filter(n > 1) %>%
  arrange(desc(n))

ds2 <- 
  ds1 %>%
  left_join(
    hromada_names
    ,by = "hromada_code"
  ) %>%
  mutate(oblast_hromada = paste(oblast_name, hromada_name, sep = '_'))

ds2 %>%
  explore::describe_all()
# number of distinct codes not equal to number of unique hromada names -
# probaply some names are the same (1470 codes, 1323 names)


#+ plot-data ----------------------------------------------------------------

# Plot for number of amalgamated radas by year
ds1 %>%
  group_by(year) %>%
  summarise(n_radas = sum(rada_count)) %>%
  ggplot(aes(x=year, y=n_radas)) +
  geom_bar(stat='identity', position = 'identity') +
  geom_text(aes(label=n_radas), vjust = -0.2) +
  theme_classic()

## Alternative 1 - making separate plots for each hromada
## 1470 images to save!

# sample of hromadas for plotting
l <- sample(ds2$oblast_hromada, 10)
# l <- unique(ds2$oblast_hromada)
# gets an error

# separate plots for each hromada 
for (k in l) {
  plot_hromada(k, path = path_plots)
}

## Alternative 2 - visualisation with interactive filtering (prob plotly)

