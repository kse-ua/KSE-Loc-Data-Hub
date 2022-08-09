#' ---
#' title: "Ellis Lane"
#' author: 
#'   - "Andriy Koval"
#'   - "other author"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/1-ellis.R") # run to knit, don't uncomment

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
Sys.setlocale("LC_CTYPE", "russian")
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
ds_map   <- readr::read_rds(path_admin)
ds0_hromada <- readr::read_csv(
  path_hromada
  ,col_types = col_types_hromada_raw # because had some trouble with parsing issues
  ,skip = 0
) 

ds0_rada<- readr::read_csv(path_rada, col_names = names_rada, skip = 1)
ds0_rada <-
  ds0_rada %>%
  filter(!is.na(hromada_code)) %>%
  distinct(rada_code, hromada_code)

#+ inspect-data ----------------------------------------------------------------
ds_map      %>% glimpse()
ds0_rada    %>% glimpse()
ds0_hromada %>% glimpse()



#+ tweak-data, eval=eval_chunks ------------------------------------------------
names(ds0_hromada) <- names_hromada
ds0_hromada <- ds0_hromada %>% filter(!hromada_code == "#N/A")
# Create a ds mapping radas to hromadas
ds1_rada <-
  ds0_hromada %>% # notice that it's not from ds0_rada!
  select(hromada_code, rada_codes_final) %>% 
  mutate(
    rada_code = str_split(rada_codes_final, ',')
  ) %>% 
  select(-rada_codes_final) %>% 
  unnest(cols = c("rada_code")) %>% 
  select(rada_code, hromada_code) %>% 
  arrange( rada_code, hromada_code) 
ds1_rada

# create a ds listing the final list of hromadas along with the founding radad
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
  arrange(hromada_code)
ds1_hromada 

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
  arrange(hromada_code, date, rada_code) %>% 
  select(date,rada_code, hromada_code) %>% 
  print()

#+ inspect-data-2 ----------------------------------------------------------------
ds1_time    %>% glimpse() # date when rada joins a hromada / when hromada alters its composition
ds1_rada    %>% glimpse() # mapping of radas to hromadas at the end of amalgamation
ds1_hromada %>% glimpse() # the final list of hromadas at the end of amalgamation proces




#+ table-1 ---------------------------------------------------------------------


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

