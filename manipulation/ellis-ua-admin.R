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
names_admin_ua <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"level_extra"
  ,"object_category"
  ,"object_name"
  
)

path_raw    <- "./data-private/raw/ua-admin-codes.csv"
path_oblasti <- "./data-private/raw/oblast.csv"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
# Kодифікатор. tab "raw"
# https://docs.google.com/spreadsheets/d/1_M-MOSIOkpiBHrP0ieiK0iFmm1_gnP_7/edit#gid=1382135566
ds0 <- readr::read_csv(path_raw, col_names = names_admin_ua, skip=1)

# Kодифікатор. tab "області"
# https://docs.google.com/spreadsheets/d/1_M-MOSIOkpiBHrP0ieiK0iFmm1_gnP_7/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true 
ds0_oblast <- readr::read_csv(path_oblasti, skip=0)

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
  distinct(settlement_code = level_4, settlement_name = object_name, 
           settlement_type = category_label)
ds_settlement


#+ combine ---------------------------------------------------------------------
ds_map_hromada <- 
  #1
  ds_hromada %>% 
  left_join(
     ds1 %>% distinct(raion_code = level_2, hromada_code = level_3)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_raion
     ,by = "raion_code"
  ) %>% 
  # 2
  left_join(
    ds1 %>% distinct(oblast_code = level_1, hromada_code = level_3)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_oblast
    ,by = "oblast_code"
  )
ds_map_hromada


ds_map_settlement <- 
  #1
  ds_settlement %>% 
  inner_join(
    ds1 %>% distinct(settlement_code = level_4, hromada_code = level_3)
    ,by = "settlement_code"
  ) %>% 
  inner_join(
    ds_map_hromada
    ,by = "hromada_code"
  ) 
ds_map_settlement


# demonstrate that ds_map_hromada can be devided from ds_map_settlement
identical(
  ds_map_hromada
  ,ds_map_settlement %>% 
    select(!starts_with("settlement_")) %>% 
    distinct()
)
# Therefore we will use ds_map_settlement as the primary file
# last touch: adding oblast-level helpers

ds_admin <- 
  ds_map_settlement %>% 
  left_join(ds0_oblast, by = c("oblast_code", "oblast_name")) %>% 
  mutate(
    oblast_name_display = paste0(region_ua," - ",oblast_name)
    ,oblast_name_display = fct_reorder(oblast_name_display, map_position)
  ) 
ds_admin %>% glimpse(90)
#+ graph-1 ---------------------------------------------------------------------
#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------

ds_admin %>% 
  # readr::write_csv("./data-private/ua-admin-map.csv") # causes fatal error for RSTUdio, investigate later
  readr::write_rds("./data-private/derived/ua-admin-map.rds")


#+ sanity-check, eval=F, echo=F -------------------------------
# rm(list = ls(all.names = TRUE))
# cat("\014") # Clear the console
# library(tidyverse)
# 
# ds_map <- readr::read_rds("./data-private/derived/ua-admin-map.rds")
# 
# ds_map_hromada <- 
#   ds_map %>% 
#   select(!starts_with("settlement")) %>% 
#   filter(!is.na(hromada_name)) 
# ds_map_hromada
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

