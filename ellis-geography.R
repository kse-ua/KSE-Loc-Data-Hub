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
library(tmap)
library(tmaptools)
library(osrm)

#+ declare-globals -------------------------------------------------------------
path_file <- "./data-private/raw/ua-admin-codes.csv"

path_oblast <- "./data-private/raw/oblast.csv"

#Data on squares and number of settlements parsed from decenrtalization.org.ua
#Source: https://docs.google.com/spreadsheets/d/19Xxsq9O7fuHdNB4_GDyEMVSWBx3ENu3x9gNnnPrkqyo/edit?usp=sharing, sheet "parsed"
path_squares <- "./data-private/raw/hromada_squares.csv"

names_admin_ua <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"level_extra"
  ,"object_category"
  ,"object_name"
)

names_squares <- c(
  "oblast"
  ,"raion"
  ,"hromada_name"
  ,"hromada_type"
  ,"date_formed"
  ,"n_settlements"
  ,"square"
  ,"pop_21"
)


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_file, col_names = names_admin_ua, skip=1)
ds_hromada <- readr::read_csv("./data-public/derived/ua-pop-2022.csv")
ds0_oblast <- readr::read_csv(path_oblast, skip=0)
ds0_squares <- readr::read_csv(path_squares, col_names = names_squares, skip=1)

#+ inspect-data ----------------------------------------------------------------

#+ tweak-data, eval=eval_chunks ------------------------------------------------
data = tibble()
i=0
for (i in 1:length(ds0$level_1)) {
  if (ds0$object_category[i] == "H"){
    data <- rbind(data, ds0[i,], ds0[i+1,])
  }
}

ds1 <- 
  data %>% 
  mutate(
    object_category = case_when(
      object_category ==  "H" ~ "hromada"
      ,object_category %in% c("C", "M", "T", "X") ~ "hromada_center"
    )
  ) %>%
  pivot_wider(
    names_from = object_category
    ,values_from = object_name) %>%
  fill(hromada) %>%
  filter(is.na(hromada_center) == F)

ds2 <- 
  ds1 %>%
  left_join(
    ds_hromada %>% select(hromada_code, oblast, raion, hromada_type)
    ,by = c("level_3"="hromada_code")
  ) %>% 
  select(-c(level_1, level_2, level_extra)) %>%
  rename(hromada_code = level_3, hromada_center_code = level_4) %>% 
  filter(is.na(oblast) == F) %>%
  mutate(
    key1=paste(oblast,raion,hromada,hromada_center)
    ,key2=paste(oblast,hromada_center)
  )

ds_squares <- 
  ds0_squares %>% 
  mutate(
    hromada_name = str_extract(hromada_name, ".+(?=\\sтериторіальна громада)")
    ,oblast = str_extract(oblast, "(.+(?=\\sобласть))|(м\\.\\sКиїв)")
    ,raion = str_extract(raion, "(.+(?=\\sрайон))|(м\\.\\sКиїв)")
    ,key = paste(oblast,raion,hromada_name,hromada_type)
  )


#+ add-geocodes, eval=eval_chunks ------------------------------------------------
ds2_geo1 <- 
  ds2 %>% 
  pull(key1) %>% 
  geocode_OSM(keep.unfound = T)

ds2_geo2<- 
  ds2 %>% 
  pull(key2) %>% 
  geocode_OSM(keep.unfound = T)

ds_oblast_geo <- 
  ds0_oblast %>% 
  mutate(key = paste(oblast_name, oblast_center)) %>% 
  pull(key) %>% 
  geocode_OSM(keep.unfound = T) 


#+ add-squares, eval=eval_chunks ------------------------------------------------




#+ combine ---------------------------------------------------------------------
ds_oblast <- 
  ds0_oblast %>% 
  cbind(
    ds_oblast_geo %>% 
      select(lat, lon) %>% 
      rename(oblast_center_lat = lat,oblast_center_lon=lon)
  ) %>%
  filter(!oblast_name %in% c("Автономна Республіка Крим", "Севастополь"))

ds_geo <- 
  ds2 %>% 
  cbind(
    ds2_geo1 %>% select(lat, lon) %>% rename(lat1 = lat, lon1=lon)
    ,ds2_geo2 %>% select(lat, lon) %>% rename(lat2 = lat, lon2=lon)
  ) %>%
  mutate(
    lat_center = case_when(
      is.na(lat1) == F ~ lat1
      ,is.na(lat1) == T ~ lat2
    )
    ,lon_center = case_when(
      is.na(lon1) == F ~ lon1
      ,is.na(lon1) == T ~ lon2
    )
  ) %>% 
  select(-c(lat1, lon1, lat2, lon2, key1, key2))


times <- tibble()
for (i in 1:length(ds_oblast$oblast_name)) {
  obl <- ds_oblast$oblast_name[i]
  a <- ds_oblast %>% 
    filter(oblast_name == obl) %>% 
    select(oblast_name, oblast_center_lat, oblast_center_lon)
  b <- ds_geo %>% 
    filter(oblast == obl) %>% 
    select(hromada_code, lat_center, lon_center)
  times <- rbind(times, 
    as_tibble(
      osrmTable(src = b, dst = a)$durations
      , rownames = "hromada_code") %>% 
      rename(ttime = ds_oblast$oblast_name[i])
  )
}

ds_geo_full <- 
  ds_geo %>% 
  left_join(
    times
    , by = "hromada_code") %>% 
  mutate(key = paste(oblast,raion,hromada,hromada_type)) %>% 
  left_join(
    ds_squares %>% select(key, n_settlements, square)
    ,by = "key"
  ) 



#+ graph-1 ---------------------------------------------------------------------
#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(ds_pop_22, "./data-public/derived/ua-pop-2022.csv")



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

