rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
library(tidyr)     # data wrangling
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(survey)
library(fastDummies)

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-hromada-analysis/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
# ---- declare-functions -------------------------------------------------------
'%ni%' <- Negate(`%in%`)


# ---- load-data ---------------------------------------------------------------
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")
# Xls_form
survey_xls  <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
choices_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "choices")

# ---- inspect-data ------------------------------------------------------------
ds_survey %>% glimpse()
# ---- variable-groups -----------------------------------------------------------
# create supporting objects for convenient reference of variable groups

# multiple choice questions
mcq <-
  survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(str_detect(type, "select_multiple"))%>%
  dplyr::select(name)%>%
  pull() %>%  
  print()

#vectors of mcq names
preparation <- 
  ds_survey %>% 
  select(starts_with("prep_"), -prep_winter_count, -prep_count) %>% 
  colnames() %>% 
  print()

comm_channels <- 
  ds_survey %>% 
  select(telegram:hotline) %>% 
  colnames() %>% 
  print()

idp_help <- 
  ds_survey %>%
  select(starts_with('idp_help/'), -ends_with('number')) %>% 
  colnames() %>% 
  print()

military_help <- 
  ds_survey %>% 
  select(starts_with('help_for_military/')) %>% 
  colnames() %>% 
  print()

# only for occupied hromadas - few cases
hromada_cooperation <- 
  ds_survey %>% 
  select(starts_with('hromada_cooperation/')) %>% 
  colnames() %>% 
  print()

prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')
# vector of income variables 
income <- 
  ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()

# ---- meta-data-1 -------------------------------------------------------------
d_meta_prep <- 
  tibble::tribble(
    ~item_name, ~item_number, ~label_ua,  ~label_en, ~label_ua_full,
    "prep_first_aid_water"            ,1L  , "Воду запасли - (1)"             , "Water stored (1)"             ,"Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби)",
    "prep_first_aid_fuel"             ,2L  , "Паливо запасли - (2)"           , "Fuel stored (2)"               ,"Сформовані запаси товарів першої необхідності (паливо)",
    "prep_reaction_plan"              ,3L  , "План реагування є - (3)"        , "Plan of response (3)"         ,"Оновлено чи затверджено план реагування на надзвичайні ситуації",
    "prep_evacuation_plan"            ,4L  , "План евакуації є - (4)"         , "Plan of evacuation (4)"             ,"Складено спеціальний план евакуації населення при загрозі збройного конфлікту",
    "prep_reaction_plan_oth_hromadas" ,5L  , "План узгоджен з інш.грм. - (5)" , "Plan coord w/ oth. Hs (5)"         ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад",
    "prep_reaction_plan_oda"          ,6L  , "План узгоджен з ОДА - (6)"      , "Plan coord w/ Oblast (6)"         ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА",
    "prep_dftg_creation"              ,7L  , "Створили ТРО - (7)"             , "Territorial Defense (7)"         ,"Розпочато створення (добровольчого) формування територіальної громади",
    "prep_national_resistance"        ,8L  , "План спротиву є - (8)"          , "Plan of resistance (8)"           ,"Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади",
    "prep_starosta_meeting"           ,9L  , "Зустрілись зі старостами - (9)" , "Meeting with heads (9)"              ,"Проведена зустріч зі старостами з приводу дій у випадку вторгнення",
    "prep_communal_meetiing"          ,10L , "Зустрілись із ЖЕКами - (10)"    , "Meeting with utilities (10)"    ,"Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення",
    "prep_online_map"                 ,11L , "Мапа укриттів онлайн є - (11)"  , "Shelter map online (11)"          ,"Опублікована онлайн-мапа укриттів в громаді",
    "prep_shelter_list"               ,12L , "Список укриттів онлан є - (12)" , "Shelter list online (12)"        ,"Опублікований перелік адрес укриттів в соцмережах або на сайті громади",
    "prep_notification_check"         ,13L , "Оповіщення працює! - (13)"      , "Communication tested (13)"           ,"Перевірено засоби оповіщення населення",
    "prep_backup"                     ,14L , "Дані зберегли повністю - (14)"  , "Data backup fully (14)"        ,"Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)",
    "prep_partly_backup"              ,15L , "Дані зберегли частково - (15)"  , "Data backed up partially (15)"  ,"Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)"
)

# ---- tweak-data-0 ----------------------
ds0 <- 
  ds_survey %>% 
  mutate(
    income_own_per_capita       = income_own_2021         / total_population_2022,
    income_total_per_capita     = income_total_2021       / total_population_2022,
    income_tranfert_per_capita  = income_transfert_2021   / total_population_2022,
    idp_registration_share      = idp_registration_number / total_population_2022,
    idp_real_share              = idp_real_number         / total_population_2022,
    idp_child_share             = idp_child_education     / idp_registration_number
  ) %>% 
  mutate(
    prep_score = rowSums(across(preparation),na.rm = T) # composite score of preparedness
    ,prep_score_binary = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          . ==  0 ~ 0L #"No"
          ,. == 1 ~ 1L #"After Feb 24"
          ,. == 2 ~ 1L #"Before Feb 24"
          ,TRUE   ~ NA_integer_
        ) 
      )
      ,na.rm = T
    )
  ) 

# ---- inspect-data-0 ------------------------------------------------------------

# ---- tweak-data-1 ------------------------------------------------------------
# compute total binary score (preparations are made at all, regardless of timing)

# Raw scale (0,1,2)
ds1_ordinal_integers <- 
  ds0 %>% 
  select(hromada_code, preparation, prep_score, prep_score_binary)


# Raw scale (0,1,2) with factors
ds1_ordinal_factors <- 
  ds0 %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "After Feb 24"
        ,. == 2 ~ "Before Feb 24"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Before Feb 24","After Feb 24",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code, preparation, prep_score, prep_score_binary)

# Binary scales (0,1)
ds1_binary_integers <- 
  ds0 %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . ==  0 ~ FALSE #"No"
        ,. == 1 ~ TRUE #"After Feb 24"
        ,. == 2 ~ TRUE #"Before Feb 24"
        ,TRUE   ~ NA
      ) 
    ) 
  ) %>% 
  select(hromada_code, preparation, prep_score, prep_score_binary)
# Binary scale (0,1) with factors
ds1_binary_factors <- 
  ds0 %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0  ~ "No"
        ,. == 1 ~ "Yes"
        ,. == 2 ~ "Yes"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Yes","Not Applicable"))
    )
  ) %>% 
  select(hromada_code, preparation, prep_score, prep_score_binary)

# ----- inspect-data-1 -----------------------

ds1_ordinal_integers %>% glimpse()
ds1_ordinal_factors %>% glimpse()
ds1_binary_integers %>% glimpse()
ds1_binary_factors %>% glimpse()

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/survey-hromada-analysis/survey-1-preparations.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)