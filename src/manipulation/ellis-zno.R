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
library(archive)


#+ declare-globals -------------------------------------------------------------
path_zno21 <- "./data-public/raw/OpenDataZNO2021.7z"
# path_zno20 <- "./data-private/raw/OpenDataZNO2020.7z"
# path_zno19
# path_zno18
# path_zno17

path_admin <- "./data-public/derived/ua-admin-map.csv"
path_admin_old <- "./data-public/derived/ua-admin-old.csv"

path_schools <- "./data-public/raw/schools-register.xlsx"

# names_school_fin <- c(
#   "oblast_n"
#   ,"oblast"
#   ,"school_type"
#   ,"ID"
#   ,"hromada_name"
#   ,"school_name"
#   ,"level"
#   ,"squere"
#   ,"n_ped"
#   ,"n_non_ped"
#   ,"n_ped_old"
#   ,"n_students"
#   ,"n_classes"
#   ,"expenditures1"
#   ,"expenditures2"
#   ,"oporna"
#   ,"budget_code"
#   ,"ownership"
#   ,"diso_code"
#   ,"edrpou"
#   ,"note"
# )


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
zno21 <- readr::read_delim(
  archive_read(path_zno21)
  , delim = ";"
  , escape_double = FALSE
  , na = "null"
  , trim_ws = TRUE
  ,locale = locale(decimal_mark = ",")
  )

#CHANGE ENCODING
# zno20 <- readr::read_delim(
#   archive_read(path_zno20)
#   , delim = ";"
#   , escape_double = FALSE
#   , na = "null"
#   , trim_ws = TRUE
#   ,locale = locale(decimal_mark = ",")
# )

schools <- readxl::read_excel(path_schools)

ds_admin <- readr::read_csv(path_admin)

ds_admin_old <- readr::read_csv(path_admin_old) %>% 
  left_join(
    ds_admin %>% 
      select(settlement_code_old, hromada_code, hromada_name)
    ,by = c("settlement_code"="settlement_code_old")
  ) %>% 
  select(oblast_name, raion_name, category_label, settlement_name, hromada_code, hromada_name) %>% 
  mutate(
    key = paste(oblast_name, raion_name, category_label, settlement_name)
  )

#+ inspect-data ----------------------------------------------------------------
zno21 %>% glimpse()


#+ tweak-data-1, eval=eval_chunks ------------------------------------------------
ds_1 <- 
  zno21 %>% 
  mutate(
    settlement_student = case_when(
      str_detect(TERNAME, "район міста") == T ~ str_remove(AREANAME, "м\\.|с\\.|смт ")
      ,str_detect(TERNAME, "район міста") == F ~ str_remove(TERNAME, "м\\.|с\\.|смт ")
    )
    ,oblast_student = str_extract(RegName, "(.+(?=\\sобласть))|(м\\.Київ)")
    ,raion_student = str_extract(AREANAME, ".+(?= район)")
    ,settlement_type_student = replace(
      TerTypeName
      , TerTypeName == "селище, село"
      , "село")
    ,settlement_school = case_when(
      str_detect(EOAreaName, "район міста") ~ str_extract(EOAreaName, "м\\..+(?=\\. )")
      ,str_detect(EOAreaName, "м\\.[\\p{Cyrillic}]++(?!\\.)") ~ EOAreaName
      ,str_detect(EOAreaName, "район(?! міста)") ~ EOTerName
    )
    ,settlement_type_school = case_when(
      str_detect(settlement_school, "м\\.") ~ "місто"
      ,str_detect(settlement_school, "с\\.") ~ "село"
      ,str_detect(settlement_school, "смт ") ~ "селище міського типу"
    )
    ,settlement_school = str_remove(settlement_school, "м\\.|с\\.|смт ")
    ,oblast_school = str_extract(EORegName, "(.+(?=\\sобласть))|(м\\.Київ)")
    ,raion_school = str_extract(EOAreaName, ".+район(?! міста)")
    ,year = "2021"
    ,raion_school = str_replace(raion_school, "'", "’")
    ,settlement_school = str_replace(settlement_school, "'", "’")
    ,raion_student = str_replace(raion_student, "'", "’")
    ,settlement_student = str_replace(settlement_student, "'", "’")
  ) %>%
  filter(RegTypeName == "Випускник загальноосвітнього навчального закладу 2021 року") %>% 
  rename(school_name = EONAME) %>% 
  select(OUTID,year,oblast_student, raion_student, settlement_student, settlement_type_student # general variables - student
         ,school_name, oblast_school, raion_school, settlement_school, settlement_type_school # general variables - school
         ,UMLTestStatus, UMLBall100, UkrTestStatus, UkrBall100 #Ukrainian
         ,HistTestStatus, HistBall100 #history
         ,MathTestStatus, MathBall100 #math
         ,EngTestStatus, EngBall100 #English
  )

#add hromada code and name for cities
cities <- 
  ds_1 %>% 
  filter(is.na(raion_school) == T) %>% 
  # select(oblast_school, settlement_school) %>% distinct() #no need to specify oblast/raion - 
  #all city names are unique
  select(settlement_school) %>% 
  mutate(settlement_school = str_replace(settlement_school, "Володимир-Волинський", "Володимир")) %>% 
  distinct() %>% 
  left_join(
    ds_admin %>% 
      filter(settlement_type == "місто")
    ,by = c("settlement_school" = "settlement_name")
  ) %>% 
  filter(!(settlement_school == "Миколаїв" & oblast_name == "Львівська") #remove duplicated set
          ,!(settlement_school == "Первомайськ" & oblast_name == "Луганська") #remove duplicated set 
  ) %>% 
  mutate(
    key = paste(oblast_name, settlement_school)
    ,hromada_code = ifelse(key == "NA Київ", "UA80000000000093317", hromada_code)
    ,hromada_name = ifelse(key == "NA Київ", "Київ", hromada_name)
    ,key = ifelse(key == "NA Київ", "м.Київ Київ", key)
  ) %>% 
  select(key, hromada_code, hromada_name)


#add hromada code and name for small cities, settlements and villages
settlements_villages <- 
  ds_1 %>% 
  filter(is.na(raion_school) == F) %>% 
  select(oblast_school, raion_school, settlement_school,settlement_type_school) %>% 
  mutate(
    raion_school = str_remove(raion_school, " район")
    ,key = paste(oblast_school, raion_school,settlement_type_school,settlement_school)
    ,key = str_replace(key,  "NA с-ще", "селище")
  ) %>% 
  #key as a combination of oblast-raion-settlement type-settlement name - not ideal, because can
  #merge different settlements with the same name in one raion as one settlements - FIND SOLUTION
  distinct(key) %>% 
  left_join(
    ds_admin_old %>% 
      select(key, hromada_code, hromada_name) %>% 
      filter(!(key == "Дніпропетровська Дніпровський село Миколаївка" & is.na(hromada_code) == T))
    ,by = "key"
  ) 

#combine all settlement types for joining
to_merge <- rbind(cities, settlements_villages)

ds_2 <- 
  ds_1 %>% 
  mutate(
    raion_school = str_remove(raion_school, " район")
    ,settlement_school = str_replace(settlement_school, "Володимир-Волинський", "Володимир")
    ,key = case_when(
      is.na(raion_school) == T ~ paste(oblast_school, settlement_school)
      ,is.na(raion_school) == F ~ paste(oblast_school, raion_school, settlement_type_school, settlement_school)
    )
    ,key = str_replace(key,  "NA с-ще", "селище")
  ) %>% 
  left_join(
    to_merge
    ,by = "key"
  )

#TO-DO - identify and remove duplicates
dublicates <- ds_2 %>% count(OUTID) %>% filter(n > 1) %>% pull(OUTID)
ds_2 %>% filter(OUTID %in% dublicates) %>% arrange(OUTID) %>% View()


#+ tweak-data-2, eval=eval_chunks ------------------------------------------------
ds_zno21_hromada <- 
  ds_2 %>% 
  mutate_at(vars(contains("Ball100")), ~as.character(.)) %>% 
  pivot_longer(
    cols = UMLTestStatus:EngBall100
  ) %>%
  mutate(
    subject = str_remove(name, "Ball100|TestStatus")
    ,name = str_extract(name, "Ball|TestStatus")
  ) %>%
  pivot_wider(
    names_from = "name", values_from = "value"
  ) 
  # mutate(Ball = as.numeric(Ball))
#TO-DO - identify and remove duplicates

ds_zno21_hromada_means <- 
  ds_zno21_hromada %>% 
  filter(!str_detect(Ball, "c")) %>% 
  mutate(
    Ball = as.numeric(Ball)
    ,didnt_pass = ifelse(TestStatus == "Не подолав поріг", 1, 0)
    ,Ball = ifelse(Ball == 0, NA, Ball)
  ) %>% 
  group_by(hromada_code, hromada_name, subject) %>% 
  summarise(mean_score = mean(Ball, na.rm = T), n_didnt_pass = sum(didnt_pass), n = n()) %>% 
  mutate(pct_didnt_pass = n_didnt_pass/n)
#TO-DO - Check mean scores for hromadas AND add Kyiv
#TO-DO - Check mean scores and numbers of test takers based on the official analysis: https://zno.testportal.com.ua/stat/2021

#+ graph-1 ---------------------------------------------------------------------
# library(sf)
# library(tmap)
# 
# path_polygons <-  "./data-private/raw/terhromad_fin.geojson"
# 
# ds_polygons <- st_read(path_polygons) %>% janitor::clean_names() %>% 
#   mutate(
#     admin_3 = str_replace_all(admin_3,c("a" = "а", "o" = "о", "p"="р", "e"="е", "'" = "’"))
#   )
# 
# ds_map <- st_sf(
#   ds_zno21_hromada_means %>% 
#     left_join(
#       ds_polygons %>% select(cod_3, geometry)
#       ,by = c("hromada_code"="cod_3")
#     )
# )
# 
# tmap_mode("view")
# g1 <- 
#   ds_map %>%
#   filter(subject == "UML") %>% 
#   tm_shape() + 
#   tm_fill("pct_didnt_pass",
#           palette = "Reds",
#           id="hromada_code",
#           popup.vars=c("hromada_name")
#   ) + 
#   tm_legend(outside=TRUE) +
#   tm_layout(frame = FALSE) +
#   tmap_options(check.and.fix = TRUE)
# g1
# 


#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(ds_zno21_hromada_means, "./data-public/derived/zno-2022-aggragated.csv")



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

