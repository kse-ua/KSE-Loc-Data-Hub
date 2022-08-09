#' ---
#' title: "02 - Compare Rada Source"
#' author: "Andriy Koval"
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
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "russian")

#+ declare-globals -------------------------------------------------------------
# Constant values that won't change throughout the report

# printed figures will go here:
prints_folder <- paste0("./manipulation/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }


names_hromada <- c(
  "category"
  ,"oblast"
  ,"raion"
  ,"hromada_name"
  ,"hromada_code"
  ,"main_rada_code"
  ,"rada_codes_v1"
  ,"decision_date_v1"
  ,"rada_codes_v2"
  ,"decision_date_v2"
  ,"rada_codes_v3"
  ,"decision_date_v3"
  ,"rada_codes_v4"
  ,"decision_date_v4"
  ,"rada_codes_v5"
  ,"decision_date_v5"
  ,"rada_codes_final"
  ,"voluntary_amalgamation"
)

names_rada <- c(
  "oblast"
  ,"rai_center"
  ,"rada_name"
  ,"rada_code"
  ,"hromada_name"
  ,"hromada_code"
  ,"note"
)

col_types_hromada_raw <- readr::cols_only(
  `Hromada_category`          = readr::col_character(),
  `Region`                    = readr::col_character(),
  `Raion`                     = readr::col_character(),
  `Hromada`                   = readr::col_character(),
  `Hromada_code`              = readr::col_character(),
  `Main_council_code`         = readr::col_character(),
  `Council_codes_v1`          = readr::col_character(),
  `Decision_date_v1`          = readr::col_character(),
  `Council_codes_v2`          = readr::col_character(),
  `Decision_date_v2`          = readr::col_character(),
  `Council_codes_v3`          = readr::col_character(),
  `Decision_date_v3`          = readr::col_character(),
  `Council_codes_v4`          = readr::col_character(),
  `Decision_date_v4`          = readr::col_character(),
  `Council_codes_v5`          = readr::col_character(),
  `Decision_date_v5`          = readr::col_character(),
  `Council_codes_final`       = readr::col_character(),
  `voluntary_amalgamation`    = readr::col_character()
)

path_rada    <- "./data-private/raw/rada.csv"
path_hromada <- "./data-private/raw/hromada.csv"
path_admin   <- "./data-private/derived/ua-admin-map.rds"    
#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------

# source: local. Центр суспільних даних. Місцеві ради 2014
# Цhttps://docs.google.com/spreadsheets/d/1iEbUsZSDGbJUzl_6wC3vgoVJ7GzOlc9f/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=trueентр суспільних даних. Місцеві ради 2014
ds0_rada<- readr::read_csv(path_rada, col_names = names_rada, skip = 1)

# source: united. Центр суспільних даних. Обєдання громад
# https://docs.google.com/spreadsheets/d/1xAFUDx8nf2oaIezWSBLaqitdxwEiQaOw/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true
ds0_hromada <- readr::read_csv(
  path_hromada
  ,col_types = col_types_hromada_raw # because had some trouble with parsing issues
  ,skip = 0
)

# Kодифікатор. tab "raw"
# https://docs.google.com/spreadsheets/d/1_M-MOSIOkpiBHrP0ieiK0iFmm1_gnP_7/edit#gid=1382135566
ds_map   <- readr::read_rds(path_admin)


#+ inspect-data ----------------------------------------------------------------
ds_map      %>% glimpse()
ds0_rada    %>% glimpse()
ds0_hromada %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------
# List of radas and hromadas they belong to from the file "Місцеві ради 2014"
rada_local <- 
  ds0_rada %>%
  # filter(!is.na(hromada_code)) %>%
  distinct(rada_code, hromada_code) %>% 
  arrange(rada_code, hromada_code)

# List of rads and hromadas they belong to from the file "Обь'єднання громад"
names(ds0_hromada) <- names_hromada
rada_united <- 
  ds0_hromada %>% 
  filter(!hromada_code == "#N/A") %>% 
  select(hromada_code, rada_codes_final) %>% 
  mutate(
    rada_code = str_split(rada_codes_final, ',')
  ) %>% 
  select(-rada_codes_final) %>% 
  unnest(cols = c("rada_code")) %>% 
  arrange(hromada_code, rada_code) 


#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# Problem")
# We have two data files mapping radas to hromadas
# The first dataset we called  `rada_local` comes from the file
# "Центр суспільних даних. Місцеві ради 2014"  https://docs.google.com/spreadsheets/d/1iEbUsZSDGbJUzl_6wC3vgoVJ7GzOlc9f/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=trueентр суспільних даних. Місцеві ради 2014
rada_local
#  this data set stores information on N = _______ radas
rada_local %>% pull(rada_code) %>% unique() %>% length() %>% scales::comma()

#  the SECOND dataset we called `rada_united` comes from the file
# Центр суспільних даних. Обєдання громад - https://docs.google.com/spreadsheets/d/1xAFUDx8nf2oaIezWSBLaqitdxwEiQaOw/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true
rada_united
# the list of radas in this file counts N = ________ radas
rada_united %>% pull(rada_code) %>% unique() %>% length() %>% scales::comma()
# this file records what radas makes up hromadas at the end of the amalgamation (2021)
# TODO:
# Explore the discrepancy between these two files
# Using the labels in the dataset `ds_admin`,  describe what radas/hromadas are
# missing from each file and speculate/expolain why. 

# Answer the following question:
# If we disregard the "Local" source and use only rada_united, will we miss anything relevant to our project? 
# In other words, if we need to rely on the mapping between radas and hromadas,
# are we safe to use the mapping derived from the "United" source? ( I think yes,
# but we need the proof)

# Notes:
# 1. Occupied territories is the most likely culprit, but there might be something else 
# 2. The report should compile into an html document
# 3. Please use the "main" branch, but create a separate script with your solution
# and call it "./analysis/tasks/02-compare-rada-sources-yourname.R"
# #



#+ results="asis", echo=F ------------------------------------------------------
cat("\n# Solution ")

#+ 1 ---------------------------------------------------------------------------

#+ 2 ---------------------------------------------------------------------------

#+ 3 ---------------------------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# A. Session Information{#session-info}")
#+ results="show", echo=F ------------------------------------------------------
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)

