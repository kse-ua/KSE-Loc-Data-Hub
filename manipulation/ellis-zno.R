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
path_zno21 <- "./data-private/raw/OpenDataZNO2021.7z"
path_zno20 <- "./data-private/raw/OpenDataZNO2020.7z"
# path_zno19
# path_zno18
# path_zno17

path_admin <- "./data-private/derived/ua-admin-map.csv"

path_schools20 <- "./data-private/raw/schools-2020.xlsx"

names_school20 <- c(
  "oblast_n"
  ,"oblast"
  ,"school_type"
  ,"ID"
  ,"hromada_name"
  ,"school_name"
  ,"level"
  ,"squere"
  ,"n_ped"
  ,"n_non_ped"
  ,"n_ped_old"
  ,"n_students"
  ,"n_classes"
  ,"expenditures1"
  ,"expenditures2"
  ,"oporna"
  ,"budget_code"
  ,"ownership"
  ,"diso_code"
  ,"edrpou"
  ,"note"
)



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

schools20 <- readxl::read_excel(path_schools20, col_names = names_school20, skip =2)

ds_admin <- readr::read_csv(path_admin)

#+ inspect-data ----------------------------------------------------------------
zno21 %>% glimpse()


#+ tweak-data, eval=eval_chunks ------------------------------------------------
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


ds_2 <- 
  ds_1 %>% 
  mutate(
    key_school  = paste(oblast_school, raion_school, settlement_type_school, settlement_school)
    ,key_student = paste(oblast_student, raion_student, settlement_type_student, settlement_student)
  ) %>% 
  distinct(key_student)

cities <- 
  ds_1 %>% 
  filter(is.na(raion_school) == T) %>% 
  # select(oblast_school, settlement_school) %>% distinct() #no need to specify oblast/raion - 
  #all city names are unique
  select(settlement_school) %>% 
  mutate(settlement_school = str_replace_all(
    settlement_school, c("'"="’", "Володимир-Волинський"="Володимир"))
    ) %>% 
  distinct() %>% 
  left_join(
    ds_admin %>% 
      filter(settlement_type == "місто" & settlement_name.x %in% cities$settlement_school)
    ,by = c("settlement_school" = "settlement_name.x")
  ) %>% 
  filter(!(settlement_school == "Миколаїв" & oblast_name == "Львівська") #remove duplicated set
          ,!(settlement_school == "Первомайськ" & oblast_name == "Луганська")) #remove duplicated set




# ds_zno21 %>% 
#   distinct(school_name) %>%
#   mutate(
#     school_name = tolower(school_name)
#     ,school_name = str_remove(school_name, "[:punct:]")
#   ) %>% 
#   left_join(
#     schools20 %>% select(school_name, level) %>% 
#       mutate(
#         school_name = tolower(school_name)
#         ,school_name = str_remove(school_name, "[[:punct:]]")
#       )
#     ,by = "school_name"
#   ) %>% count(level)


ds_zno21_hromada <- 
  ds_zno21 %>% 
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


colnames(ds_zno21)

#+ combine ---------------------------------------------------------------------



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

