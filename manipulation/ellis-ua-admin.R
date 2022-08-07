#' ---
#' title: "Ellis UA Admin"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-ua-admin.R") # run to knit, don't uncomment
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
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
path_file <- "./data-private/raw/ua-admin-codes.csv"

names_admin_ua <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"level_extra"
  ,"object_category"
  ,"object_name"
  
)
#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_file, col_names = names_admin_ua, skip=1)
#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()

ds0 %>% count(object_category)

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds1 <- 
  ds0 %>% 
  mutate(
    category_label = case_when(
      object_category =="O"  ~ "область"
    , object_category =="K"  ~ "місто (спец статус)"
    , object_category =="P"  ~ "район"
    , object_category =="H"  ~ "громада"
    , object_category =="M"  ~ "місто"
    , object_category =="T"  ~ "селище міського типу"
    , object_category =="C"  ~ "село"
    , object_category =="X"  ~ "селище"
    , object_category =="B"  ~ "район міста"
    , TRUE ~ NA_character_
    )
  )
ds1 %>% group_by(object_category, category_label) %>% tally()

ds1 %>% glimpse()

#+ table-1 ---------------------------------------------------------------------

ds_oblast <- 
  ds1 %>% 
  filter(object_category == "O") %>% 
  distinct(oblast_code = level_1, oblast_name = object_name)

ds_oblast 

ds_raion <-
  ds1 %>% 
  filter(object_category == "P") %>% 
  distinct(raion_code = level_2, raion_name = object_name)
ds_raion

ds_hromada <-
  ds1 %>% 
  filter(object_category == "H") %>% 
  distinct(hromada_code = level_3, hromada_name = object_name)
ds_hromada

ds_settlement <-
  ds1 %>% 
  filter(object_category %in% c("X","C","T","M") ) %>% 
  distinct(settlement_code = level_4, settlement_name = object_name)
ds_settlement



  

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

