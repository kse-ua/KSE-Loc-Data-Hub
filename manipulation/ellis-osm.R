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
Sys.setlocale("LC_CTYPE", "ukr")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
# names_pop22 <- c(
#   "object_name_ua"
#   ,"Persons"
#   ,"object_name_en"
# )

path_osm   <- "./data-private/raw/OSM_data_Ukraine.csv"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_osm) %>% janitor::clean_names()

#main admin dataset
ds_admin <- readr::read_csv(path_admin)
ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv")

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds1 <- 
  ds0 %>% 
  rename(raion = region) %>% 
  mutate(
    type = str_extract(tags, "shop|amenity|tourism")
    ,categorry = str_extract(tags, "(?<=shop=|amenity=|tourism=)[^,]*")
    ,raion = str_remove(raion, " район")
    ,hromada = str_remove(hromada, " громада")
    ,key = paste(oblast, raion, hromada)
  ) %>% 
  filter(!raion %in% c("ОРЛО", "ОРДО"))
  
ds0 %>% slice_sample(n=100) %>% pull(tags)  
ds1 %>%  filter(key %in% c("Дніпропетровська Нікопольський Покровська", "Одеська Одеський Чорноморська",
                           "Сумська Сумський Миколаївська"))
  
ds_hromada %>% 
  filter(oblast_name != "Автономна Республіка Крим") %>% 
  mutate(key = paste(oblast_name, raion_name, hromada_name)) %>% 
  count(key)  %>% arrange(desc(n))

