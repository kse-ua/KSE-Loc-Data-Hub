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
library(tmap)
library(tmaptools)
library(sf)


#+ declare-globals -------------------------------------------------------------
#Data of changes in governmental transfers as a share of hromada income in 2021-2022
path_budget_data <- "./data-private/derived/budget-change-for-map.csv"

#Shapefile of hromada polygons
#Source: https://drive.google.com/file/d/1X3Ym-7s1RgQgsi_p4uJ3EGEWYGTd-UMz/view?usp=sharing
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

ds_budget_data <- readr::read_csv(path_budget_data)
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names()

#+ inspect-data ----------------------------------------------------------------
ds_budget_data %>% glimpse()
ds_polygons %>% glimpse()


#+ tweek-data, eval=eval_chunks ------------------------------------------------
ds_1 <- st_sf(
  left_join(
    ds_budget_data %>% select(hromada_code, hromada_name, own_income_change, own_prop_change)
    ,ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
)

#+ graph, eval=eval_chunks ------------------------------------------------

g1 <- 
  ds_1 %>%
  tm_shape() + 
  tm_fill("own_prop_change",
          palette = "RdBu",
          id="hromada_code",
          popup.vars=c("hromada_name")
  ) + 
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g1

g2 <- 
  ds_1 %>%
  tm_shape() + 
  tm_fill("own_income_change",
          palette = "RdBu",
          id="hromada_code",
          popup.vars=c("hromada_name")
  ) + 
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g2


