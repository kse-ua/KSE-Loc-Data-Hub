#' ---
#' title: "0-import"
#' author: "First Last"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/0-import.R") # run to knit, don't uncomment
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
path_rada <- "./data-private/raw/local-councils.csv"
path_hromada <- "./data-private/raw/amalgomated-territorial-communities.csv"

names_rada <- c(
  "oblast"
  ,"rai_center"
  ,"rada_name"
  ,"rada_code"
  ,"hromada_name"
  ,"hromada_code"
  ,"note"
)

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

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
ds0_rada <- readr::read_csv(path_rada, col_names = names_rada, skip = 1)
ds0_hromada <- readr::read_csv(path_hromada, col_names = names_hromada, skip=1)

#+ inspect-data ----------------------------------------------------------------
ds0_rada %>% glimpse()
ds0_hromada %>% glimpse()



#+ tweak-data, eval=eval_chunks ------------------------------------------------
ds1_rada <- 
  ds0_rada 

ds0_hromada %>% glimpse()

ds1_hromada <-
  ds0_hromada %>% 
  select(!starts_with("rada_codes_v")) %>% 
  select(!starts_with("decision_date"))
ds1_hromada %>% glimpse()  


ds1_event <- 
  ds0_hromada %>% 
  select(names_event)


ds1_rada %>% glimpse()
ds1_hromada %>% glimpse()
ds1_event %>% glimpse()




#+ table-1 ---------------------------------------------------------------------
#+ graph-1 ---------------------------------------------------------------------
#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------

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


