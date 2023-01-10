#' ---
#' title: "Ellis Hromada Survey Preparation"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-survey-hatsko.R") # run to knit, don't uncomment
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
cat("\n# 1.Environment")#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "ukr")
rm(list = ls())

#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(survey)
library(fastDummies)
library(lubridate)
#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-survey-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "n/a", "NOt available", '<NA>')
#+ declare-functions -----------------------------------------------------------
'%ni%' <- Negate(`%in%`)

#+ load-data, eval=eval_chunks -------------------------------------------------

# Xls_form
survey_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
choices_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "choices")

# Survey data
d0 <- readxl::read_excel("./data-private/raw/Resilience_survey_2022_12_07_eng_clean.xlsx")

ds_population <- readr::read_csv("./data-private/derived/ua-pop-2022.csv")

ds_hromada <- readr::read_delim("./data-private/derived/hromada.csv", delim = ';') %>% 
  mutate(
    key = paste(hromada_name, type, "громада")
  )

ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

oblasts <- readr::read_csv("./data-private/raw/oblast.csv") %>% 
  mutate(
    oblast_name_en = case_when(
      oblast_name_en == "Dripropetrovska" ~ "Dnipropetrovska"
      ,oblast_name_en == "Ivano-Frankivsk" ~ "Ivano_Frankivsk"
      ,oblast_name_en == "Kyiv-oblast" ~ "Kyivska"
      ,oblast_name_en == "Vonyn" ~ "Volyn"
      ,TRUE ~ oblast_name_en
    )
  )

#+ storing contact data --------------------------------------------------------
#select and store contact data
contact_data <- d0 %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  select(oblast, raion_text, hromada_text, telegram_link, facebook_link, contact_text, '_id')

#write contact data
openxlsx::write.xlsx(contact_data, "./data-private/derived/survey-contact-data.xlsx")

#---- clean data ---------------------------------------------------------------

##GENERAL CLEANING
d1 <- 
  d0 %>% 
  # remove empty and unnecessary variables
  select(!c(start, end, deviceid, contains("note"), contains("group_"), 
            contains("evacuation_actions"), contains('_labels')
            # contact_text:`_tags`
  )
  ) %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  rename(index = `_index`) %>% 
  left_join(
    oblasts %>% select(oblast_name_en, region_en)
    ,by = c("oblast"="oblast_name_en")
  )

#+ storing text data for coding ------------------------------------------------

#save text variables for coding
text <- survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(grepl("text",type))%>%
  dplyr::select(name)%>%
  pull()

text_data <- d1 %>% 
  select(index, contains(text)) 

#write text data for further coding
openxlsx::write.xlsx(text_data, "./data-private/derived/survey-text-data.xlsx")

#+ creating new variables ------------------------------------------------------

#create counters for mcq
preparation <- d1 %>% select(starts_with("prep_")) %>% colnames()
comm_channels <- d1 %>% select(telegram:hotline) %>% colnames()
idp_help <- d1 %>% select(starts_with('idp_help/')) %>% colnames()
military_help <- d1 %>% select(starts_with('help_for_military/')) %>% colnames()
# only for occupied hromadas - few cases
hromada_cooperation <- d1 %>% select(starts_with('hromada_cooperation/')) %>% 
  colnames() %>% select(-c('help_for_military/other', 'help_for_military/none'))
prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')

# recoding
d2 <- d1 %>% 
  mutate_at(
    vars(all_of(preparation)), ~case_when(
      . == "before_24" ~ 2
      ,. == "after_24" ~ 1
      ,. == "not_executed" ~ 0
      ,. == "doesnt_apply" ~ NA_real_
    ) 
  ) %>% 
  mutate_at(   
    vars(all_of(comm_channels)), ~case_when(
      . == "before_24" ~ 2
      ,. == "after_24" ~ 1
      ,. == "none" ~ 0
    )
  ) %>% 
  mutate_at(   
    vars(all_of(prep_for_winter)), ~case_when(
    . == "yes" ~ 1
    ,. == "no" ~ 0
    ,. == "not_apply" ~ NA_real_
  )) %>%
  mutate(
    idp_registration_number = as.numeric(idp_registration_number)
    ,idp_real_number = as.numeric(idp_real_number)
    ,idp_child_education = as.numeric(idp_child_education)
    ,across(starts_with('idp_help/'), ~ . * idp_registration_number, .names = '{.col}_number')
    ,idp_help_count = rowSums(across(all_of(idp_help), na.rm = T))
    ,prep_count= rowSums(across(all_of(preparation)), na.rm = T)
    ,comm_channels_count = rowSums(across(all_of(comm_channels)), na.rm = T)
    ,help_military_count = rowSums(across(all_of(military_help)), na.rm = T)
    ,hromada_cooperation_count = rowSums(across(all_of(hromada_cooperation)), na.rm = T)
    ,dftg_creation_time = floor(difftime(dftg_creation_date, "2022-02-24", unit = "day")) #negative values - choose another date
    ,idp_registration_time = floor(difftime(idp_registration_date, "2022-02-24", unit = "day"))
    ,commun_between_hromadas = case_when(commun_between_hromadas == '__' ~ 'Daily',
                                         commun_between_hromadas == '______' ~ 'Several times a week',
                                         commun_between_hromadas == '_______1' ~ 'Several times a month',
                                         commun_between_hromadas == '________' ~ 'Once a month and less',
                                         commun_between_hromadas == '____' ~ 'No meetings/calls')
    ,prep_winter_count= rowSums(across(all_of(prep_for_winter)), na.rm = T)
  )

# upload coded hromada names dataset
coded_hromada_names <- readxl::read_excel("./data-private/raw/survey-contact-data-coded.xlsx")

#+ make final dataset ----------------------------------------------------------
d3 <- 
  d2 %>% 
  left_join(
    coded_hromada_names %>% select("_id", oblast_name, hromada_name_right, raion_name)
    , by = "_id"
  ) %>% 
  left_join(
    ds_hromada %>% select(key, hromada_code, hromada_name, raion_name, 
                          oblast_name, type)
    ,by = c('oblast_name', 'raion_name'
            ,"hromada_name_right"="key")
  ) %>%
  select(-c('hromada_name_right', 'oblast', 'raion_text', 'hromada_text', 
            'raion_name', 'hromada_name', 'type', 'oblast_name')) %>%
  relocate(hromada_code, .after = '_id')

d4 <- d3 %>% 
  left_join(
    ds_general
    ,by = "hromada_code"
  ) %>%
  relocate(c('hromada_name', 'hromada_full_name', 'raion_code', 'raion_name', 'oblast_code', 'oblast_name',
             'type'), .after = 'hromada_code')

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
openxlsx::write.xlsx(d4, './data-private/derived/survey_hromadas_clean.xlsx')

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

