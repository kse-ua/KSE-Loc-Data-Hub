#' ---
#' title: "Ellis Lane"
#' author: 
#'   - "Andriy Koval"
#'   - "other author"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-rada-hromada.R") # run to knit, don't uncomment

#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console
#+ echo=FALSE, results="asis" --------------------------------------------------
cat("Report's native working directory: `", getwd(),"`") # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "ukr")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
# Prefer to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(tidyverse)
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("readr"    )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("forcats"  )# factors
requireNamespace("stringr"  )# strings
requireNamespace("lubridate")# dates

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

names_event <- c(
  "hromada_code"
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
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
# source: "LOCAL" Центр суспільних даних. Місцеві ради 2014. tab 'all'
# https://docs.google.com/spreadsheets/d/1iEbUsZSDGbJUzl_6wC3vgoVJ7GzOlc9f/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=trueентр суспільних даних. Місцеві ради 2014
ds0_rada<- readr::read_csv(path_rada, col_names = names_rada, skip = 1)

# source: "UNITED" Центр суспільних даних. Обєдання громад. tab 'all'
# https://docs.google.com/spreadsheets/d/1xAFUDx8nf2oaIezWSBLaqitdxwEiQaOw/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true
ds0_hromada <- readr::read_csv(
  path_hromada
  ,col_types = col_types_hromada_raw # because had some trouble with parsing issues
  ,skip = 0
)

# Kодифікатор. tab "raw"
# Initial source: https://docs.google.com/spreadsheets/d/1_M-MOSIOkpiBHrP0ieiK0iFmm1_gnP_7/edit#gid=1382135566
# Here, we import he product of ./manipulation/ellis-ua-admin.R
ds_admin   <- readr::read_rds(path_admin)

#+ inspect-data ----------------------------------------------------------------
ds0_rada    %>% glimpse()
ds0_hromada %>% glimpse()
ds_admin    %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------

# Create a mapping of radas to hromadas
ds1_rada <- 
  ds0_rada %>% 
  select(
    rada_code, rada_name, hromada_code, hromada_name, rai_center, oblast, note
  ) %>% 
  arrange(desc(hromada_code))
ds1_rada %>% glimpse(80)
# ds1_rada %>% filter(rada_code == "8000000000")
# ds1_rada %>% filter(hromada_code == "UA80")

names(ds0_hromada) <- names_hromada
# ds0_hromada <- ds0_hromada %>% filter(!hromada_code == "#N/A") # Kyiv
#  We can derive the rada-hromada mapping from "United" file:
ds1a_rada <-
  ds0_hromada %>% # notice that it's not from ds0_rada!
  select(hromada_code, rada_codes_final) %>% 
  mutate(
    rada_code = str_split(rada_codes_final, ',')
  ) %>% 
  select(-rada_codes_final) %>% 
  unnest(cols = c("rada_code")) %>% 
  select(rada_code, hromada_code) %>% 
  mutate(rada_code = as.numeric(rada_code)) %>% 
  arrange( rada_code, hromada_code) 
ds1a_rada %>% glimpse()
# However, it will be missing the radas which did not morph into a hromada (e.g. Crimea, Kyiv)
# Specifically:
missing_radas <-
  ds1_rada %>% 
  dplyr::anti_join(ds1a_rada, by = "rada_code") %>% 
  select(rada_code, rada_name, rai_center, oblast, note)
  
missing_radas %>% neat_DT()

# manualy adjust for Kyiv
# Maybe: change it in the source data? 

ds0_hromada %>% glimpse()

ds0_hromada %>% 
  filter(main_rada_code %in% c("8000000000","8500000000")) %>% t()

# ds0_hromada <- 
#   ds0_hromada %>% 
#   mutate(
#     hromada_code = case_when(
#       main_rada_code == "8000000000" ~ "UA8"
#       ,TRUE ~ hromada_code
#     )
#   ) 

# create a ds listing the final list of hromadas along with the founding radas
ds1_hromada <-
  ds0_hromada %>% 
  select(!starts_with("rada_codes_v")) %>% 
  select(!starts_with("decision_date")) %>% 
  select(
    hromada_code
    # ,hromada_name
    ,main_rada_code
    ,rada_codes_final
  ) %>% 
  arrange(desc(hromada_code)) 
ds1_hromada 

ds0_hromada %>% glimpse(60)
ds1_hromada %>% glimpse(60)

# create a ds listing the dates on which hromadas changed their composition 
ds0_time <- 
  ds0_hromada %>% 
  select(names_event) %>% 
  print()

ds1_time <- 
  ds0_time %>% 
  tidyr::pivot_longer(
    cols = !starts_with("hromada_code")
  ) %>% 
  mutate(
    wave  = str_extract(name, "\\d$" )
    ,name = str_remove( name, "^rada_|decision_")
    ,name = str_remove( name, "_v\\d{1}$")
  ) %>% 
  pivot_wider(
    names_from = "name", values_from = "value"
  ) %>% 
  mutate(
    rada_code = str_split(codes, ',')
  ) %>% 
  unnest(cols = c("rada_code")) %>% 
  select(-codes, -wave) %>% 
  filter(!is.na(date)) %>%
  mutate(
    date = as.Date(date)
    # ,rada_code = as.integer(rada_code)
  ) %>% 
  filter(!is.na(rada_code)) %>% 
  arrange(desc(hromada_code), date, rada_code) %>% 
  select(date,rada_code, hromada_code) %>% 
  print()

# ds1_time  %>% filter(hromada_code=="UA8")
#+ inspect-data-2 ----------------------------------------------------------------
ds1_time    %>% glimpse(80) # date when rada joins a hromada / when hromada alters its composition
ds1_rada    %>% glimpse(80) # mapping of radas to hromadas at the end of amalgamation
ds1_hromada %>% glimpse(80) # the final list of hromadas at the end of amalgamation proces
ds_admin    %>% glimpse(80) # supporing meta-data file with admin level mapping



#+ table-1 ---------------------------------------------------------------------
ds1_time %>%
  left_join(ds_admin %>% distinct(hromada_code, hromada_name)) %>% 
  arrange(desc(hromada_code))

#+ graph-1 ---------------------------------------------------------------------
#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------

ds1_time %>% readr::write_csv("./data-private/derived/time_rada.csv")
ds1_rada %>% readr::write_csv("./data-private/derived/rada_hromada.csv")
ds1_hromada %>% readr::write_csv("./data-private/derived/hromada.csv")

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

