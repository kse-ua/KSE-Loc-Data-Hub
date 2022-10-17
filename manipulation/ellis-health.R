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
path_declarations<- "./data-private/raw/active_declarations_by_age_gender.csv"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_declarations)
ds_admin <- readr::read_csv(path_admin)


#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds1 <- ds0 %>% 
  left_join(
    ds_admin %>% select(settlement_code_old, settlement_code, hromada_code, hromada_name)
    ,by = c("settlement_koatuu" = "settlement_code_old")
  ) %>% 
  mutate(
    settlement = str_to_title(settlement)
    ,settlement = str_replace_all(settlement, c("'"="’"))
    ,area = str_to_title(area)
    ,key = paste(area, settlement)
  )

#create vectors of unique combination oblast-settlement for further filtering
v_admin_unique_names <-  ds_admin %>% 
  select(hromada_code, settlement_code, settlement_name, oblast_name) %>% 
  mutate(key = paste(oblast_name, settlement_name)) %>% 
  count(key) %>% 
  filter(n == 1) %>% 
  pull(key)
  

#select settlements with old koatuu which don't have unique combination oblast-settlement for manual coding
ds_coding <- ds1 %>% 
  left_join(
    ds_admin %>% 
      mutate(key = paste(oblast_name, settlement_name)) %>% 
      filter(key %in% v_admin_unique_names) %>% 
      select(key, hromada_code, hromada_name, settlement_code, settlement_name)
    ,by = "key"
  ) %>% 
  filter(is.na(settlement_code.x) & is.na(settlement_code.y)) %>% 
  # filter(settlement_code.x != "UA26040390080042180") %>% #village Tsenzhiv which was created in 2021
  distinct(area, settlement,settlement_koatuu) 

# openxlsx::write.xlsx(ds_coding, "./data-private/raw/old-koatuu-coding-raw.xlsx") - DO NOT UNCOMMENT

#+ load-coded-data, eval=eval_chunks -------------------------------------------------
ds_koatuu <- 
  readxl::read_excel("./data-private/raw/old-koatuu-coding.xlsx") %>% 
  left_join(
    ds_admin %>% select(hromada_code, hromada_name, settlement_code)
    ,by = "settlement_code"
  ) %>% 
  mutate(
    hromada_code = ifelse(settlement_koatuu == "8000000000","UA80000000000093317", hromada_code)
    ,hromada_name = ifelse(settlement_koatuu == "8000000000","Київ", hromada_name)
  )

ds2 <- ds1 %>% 
  left_join(
    ds_admin %>% 
      mutate(key = paste(oblast_name, settlement_name)) %>% 
      filter(key %in% v_admin_unique_names) %>% 
      select(key, hromada_code, hromada_name, settlement_code, settlement_name)
    ,by = "key"
  ) %>%
  left_join(
    ds_koatuu %>% select(settlement_koatuu, hromada_code, hromada_name, settlement_code)
    ,by = "settlement_koatuu"
  ) %>% 
  mutate(
    settlement_code = case_when(
      is.na(settlement_code.x) == T & is.na(settlement_code) == T ~ settlement_code.y
      ,is.na(settlement_code.y) == T & is.na(settlement_code) == T ~ settlement_code.x
      ,is.na(settlement_code.x) == F & is.na(settlement_code.y) == F ~ settlement_code.x
      ,is.na(settlement_code.x) == T & is.na(settlement_code.y) == T ~ settlement_code
    )
    ,hromada_code = case_when(
      is.na(hromada_code.x) == T & is.na(hromada_code) == T ~ hromada_code.y
      ,is.na(hromada_code.y) == T & is.na(hromada_code) == T ~ hromada_code.x
      ,is.na(hromada_code.x) == F & is.na(hromada_code.y) == F ~ hromada_code.x
      ,is.na(hromada_code.x) == T & is.na(hromada_code.y) == T ~ hromada_code
    )
    ,hromada_name = case_when(
      is.na(hromada_name.x) == T & is.na(hromada_name) == T ~ hromada_name.y
      ,is.na(hromada_name.y) == T & is.na(hromada_name) == T ~ hromada_name.x
      ,is.na(hromada_name.x) == F & is.na(hromada_name.y) == F ~ hromada_name.x
      ,is.na(hromada_name.x) == T & is.na(hromada_name.y) == T ~ hromada_name
    )
  ) %>% 
  select(-c(settlement_code.x:settlement_code.y, settlement_name))

#check hromadas which do not have declarations
v_hromadas <- ds2 %>% distinct(area, hromada_code, hromada_name) %>% pull(hromada_code)

ds_admin %>% 
  filter(oblast_name != "Автономна Республіка Крим", !(hromada_code %in% v_hromadas)) %>% 
  distinct(oblast_name, hromada_name) %>% View()


#+ aggregate-hromada-level, eval=eval_chunks -------------------------------------------------
ds3 <- 
  ds2 %>% 
  mutate(
    person_age = as.numeric(case_when(
      person_age == "100+" ~ "100" #consider 100 years+ as 100 years old
      ,TRUE ~ person_age
    ))
   ,age_group = case_when(
     person_age < 18 ~ "0-17"
     ,person_age < 30 ~ "18-29"
     ,person_age < 45 ~ "30-44"
     ,person_age < 60 ~ "45-59"
     ,person_age >=60 ~ "60+"
   )
   ,working_age = case_when(
     person_age > 14 & person_age < 71 ~ 1
     ,TRUE ~ 0
   )
   ,settlement_type = case_when(
     settlement_type == "місто" ~ "urban"
     ,settlement_type == "селище" ~ "rural"
     ,settlement_type == "село" ~ "rural"
     ,settlement_type == "смт" ~ "urban"
   )
  ) %>% 
  group_by(
    area, hromada_code, hromada_name
  ) %>% 
  summarise(
    total = sum(count_declarations)
    ,female = sum(count_declarations[person_gender == "жіноча"])
    ,male = sum(count_declarations[person_gender == "чоловіча"])
    ,female_pct = round(female/total, 2)
    ,male_pct = round(male/total,2)
    ,urban = sum(count_declarations[settlement_type == "urban"])
    ,rural = sum(count_declarations[settlement_type == "rural"])
    ,urban_pct = round(urban/total,2)
    ,rural_pct = round(rural/total,2)
    ,youth =  sum(count_declarations[person_age < 30])
    ,youth_pct = round(youth/total, 2)
    ,working_age_total = sum(count_declarations[working_age == "1"])
    ,working_age_pct = round(working_age_total/total,2)
  )

#+ save-data, eval=eval_chunks -------------------------------------------------
readr::write_csv(ds2, "./data-public/derived/declarations-all.csv") #long format
readr::write_csv(ds3, "./data-public/derived/declarations-hromada.csv") #aggregated on hromada level


#+ maps, eval=eval_chunks -------------------------------------------------
library(sf)
library(tmap)

path_polygons <-  "./data-private/raw/terhromad_fin.geojson"

ds_polygons <- st_read(path_polygons) %>% janitor::clean_names() %>% 
  mutate(
    admin_3 = str_replace_all(admin_3,c("a" = "а", "o" = "о", "p"="р", "e"="е", "'" = "’"))
  )

ds_map <- st_sf(
    ds3 %>% 
    left_join(
      ds_polygons %>% select(cod_3, geometry)
      ,by = c("hromada_code"="cod_3")
    )
  )

tmap_mode("view")
g1 <- 
  ds_map %>%
  tm_shape() + 
  tm_fill("youth_pct",
          palette = "RdBu",
          id="hromada_code",
          popup.vars=c("hromada_name")
  ) + 
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g1



