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
library(gt)

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-overview/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
# ---- declare-functions -------------------------------------------------------
'%ni%' <- Negate(`%in%`)


make_corr_matrix <- function(d,na_action="remove", method="pearson"){
  
  item_names <- names(d)
  # browser()
  d1 <- 
    d %>% 
    select(all_of(item_names)) %>% 
    mutate(
      across(
        .cols = everything()
        ,.fns = ~as.numeric(.)
      )
    ) 
  
  if(na_action == "remove"){
    d2 <- d1 %>% drop_na()
  }
  
  if(na_action == "replace"){
    d2 <-
      d1 %>%
      mutate(
        across(
          .cols = everything()
          ,.fns = ~replace_na(.,0L)
        )
      )
  }
  cormat <- cor(d2,method = method)
  # row.names(cormat) <- item_names
  return(cormat)
}


make_corr_plot <- function (
    corr,
    lower="number",
    upper="number",
    bg="white",
    addgrid.col="gray"
    ,title 
    , ...
){
  corrplot::corrplot(
    corr
    , add=F
    , type   = "lower"
    , method = lower
    , diag   = TRUE
    , tl.pos = "lt"
    , cl.pos = "n"
    # ,order = "hclust"
    # ,addrect = 3
    ,...
  )
  corrplot::corrplot(
    corr
    ,add=T
    , type="upper"
    , method=upper
    , diag=TRUE
    , tl.pos="n"
    # ,order = "hclust"
    # ,addrect = 3
    ,title = title  
    , ...
  )
  
}

# ---- load-data ---------------------------------------------------------------
# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")
# 
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean_new.xlsx")

ds_deoccup <- readxl::read_excel("./data-private/raw/deoccupied.xlsx")
# meta_oblast <- googlesheets4::read_sheet(sheet_name,"choices",skip = 0)

# Originally, we pulled the meta data object from Kobo front end and stored to 
# survey_xls  <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
# we put this on google drive now, to control manually
googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# https://googlesheets4.tidyverse.org/ 
# https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)



# ---- inspect-data ------------------------------------------------------------
ds_survey %>% glimpse()
meta_survey %>% filter(group == 'preamble')

ds_survey %>% pull(hromada_code) %>% unique()

# ---- variable-groups -----------------------------------------------------------
# create supporting objects for convenient reference of variable groups

# multiple choice questions
mcq <-
  meta_survey%>%
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

military_help_short <- military_help[1:4]

# only for occupied hromadas - few cases
hromada_cooperation <- 
  ds_survey %>% 
  select(starts_with('hromada_cooperation/')) %>% 
  colnames() %>% 
  print()

prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')

skills <- 
  ds_survey %>% 
  select(starts_with('skills_needed/')) %>% 
  colnames() %>% 
  print()

# vector of income variables 
income <- 
  ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()

# ---- meta-data-1 -------------------------------------------------------------
meta_survey %>% glimpse()

meta_survey %>% 
  filter(type %in% c("begin_group","end_group")) %>% 
  select(1:5) %>% 
  print_all()


meta_survey %>% glimpse()

# ---- tweak-data-0 ----------------------

ds_general0 <- 
  ds_general %>% 
  mutate(
    survey_response = case_when(
      hromada_code %in% (ds_survey %>% pull(hromada_code) %>% unique()) ~ TRUE
      ,TRUE ~ FALSE
    )
  )
# ds_general0 %>% group_by(survey_response) %>% count()

ds_general1 <- ds_general %>% 
  mutate(oblast_name_en = case_when(oblast_name_en == 'Vonyn' ~ "Volyn",
                                    oblast_name_en == 'Driproptrovska' ~ "Dnipropetrovska",
                                    TRUE ~ oblast_name_en)) %>%
           left_join(ds_deoccup %>% select(hromada_code, deoccupied_at_feb_2023),
                     by = c('hromada_code'))

ds0 <- 
  ds_survey %>% 
  left_join(ds_general1,
            by = 'hromada_code') %>% 
  select(-ends_with('.y')) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = list(~ sub("[.]x$", "", .))) %>% 
  mutate(
    income_own_per_capita       = income_own_2021         / total_population_2022,
    income_total_per_capita     = income_total_2021       / total_population_2022,
    income_tranfert_per_capita  = income_transfert_2021   / total_population_2022,
    idp_registration_share      = idp_registration_number / total_population_2022,
    idp_real_share              = idp_real_number         / total_population_2022 * 100,
    idp_child_share             = idp_child_education     / idp_registration_number * 100,
    type                        = case_when(type == 'сільська' ~ 'village',
                                            type == 'селищна' ~ 'urban village',
                                            type == 'міська' ~ 'urban'),
    type = factor(type, levels  = c("village", "urban village", "urban")),
    help_military_count         = rowSums(across(all_of(military_help_short))),
    idp_help_count              = rowSums(across(all_of(idp_help))),
    occupation_and_combat       = case_when(military_action == 'no_combat' & occupation == 'not_occupied' ~ 0,
                                            TRUE ~ 1),
    occupation_and_combat_fct   = factor(occupation_and_combat, 
                                         labels = c('Rear communities', 
                                                    'Communities exposed to war (n = 22)')),
    occupation_fct              = factor(deoccupied_at_feb_2023,
                                         labels = c('Rear communities', 
                                                    'Deoccupied communities (n = 16)')),
    voluntary_fct               = factor(voluntary,
                                         labels = c('Top-down amalgamated', 'Voluntary amalgamated')),
    oblast_name_en              = case_when(oblast_name_en == 'Vonyn' ~ "Volyn",
                                            oblast_name_en == 'Driproptrovska' ~ "Dnipropetrovska",
                                            TRUE ~ oblast_name_en)
     )

ds1_winter_prep <- ds0 %>% 
  mutate(
    winter_prep_count = rowSums(across(info_campaign:solid_fuel_boiler), na.rm = T)
    ,winter_prep_count = case_when(
      occupation =='not_occupied' | occupation == 'occupied_april' ~ winter_prep_count
      ,TRUE ~ NA_real_
    )
  )

ds1_problem <- ds0 %>% 
  mutate(
    hromada_exp = ifelse(hromada_exp == "yes", 1, 0)
    ,problem_info_index         =           ifelse(`hromada_problem_info/nobody`==1, 0, 
                                           rowSums(across(contains("hromada_problem_info/"))))
    ,problem_consultation_index =   ifelse(`hromada_problem_consultation/nobody`==1, 0, 
                                           rowSums(across(contains("hromada_problem_consultation/"))))
    ,problem_proposition_index  =   ifelse(`hromada_problem_proposition/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_proposition/"))))
    ,problem_system_index       =   ifelse(`hromada_problem_system/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_system/"))))
    ,problem_feedback_index     =   ifelse(`hromada_problem_feedback/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_feedback/"))))
    ,problem_execution_index    =   ifelse(`hromada_problem_execution/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_execution/"))))
    ,problem_additive_index     =   .4*problem_info_index + .6*problem_consultation_index + 
      .6*problem_proposition_index + .8*problem_system_index + .8*problem_feedback_index +
      problem_execution_index
  )

# ---- inspect-data-0 ------------------------------------------------------------

# --- -----
meta_survey %>% filter(group== "information") %>% select(name,label_en,item_number)


# ---- tweak-data-1-prep ------------------------------------------------------------
# compute total binary score (preparations are made at all, regardless of timing)
d_meta_prep <- 
  meta_survey %>% 
  filter(group=="preparation") %>% 
  select(item_name = name,label_en,label)

ds1_prep <-
  ds0 %>% 
  mutate(
    # sum of 0|1|2 where larger numbers indicate more preparedness
    prep_score_combo = rowSums(across(all_of(preparation)),na.rm = T) 
    ,prep_score_feb = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 0 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
    ,prep_score_oct = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 1 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
  )  %>% 
  select(hromada_code, starts_with("prep_score"),preparation)  
ds1_prep %>% select(1:4)

## Prep Score Weighted

ds_prep_new <- ds0 %>%
mutate(
  across(
    .cols = preparation
    ,.fns = ~case_when(
      .  == 0 ~ 0 #"No"
      ,. == 1 ~ 0 #"After Feb 24"
      ,. == 2 ~ 1 #"Before Feb 24"
      ,.default = 0
    ),
    .names = "{col}_feb"),
  across(
    .cols = preparation
    ,.fns = ~case_when(
      .  == 0 ~ 0 #"No"
      ,. == 1 ~ 1 #"After Feb 24"
      ,. == 2 ~ 1 #"Before Feb 24"
      ,.default = 0
    ),
    .names = "{col}_oct"
  )
) %>% 
  select(hromada_code,
         paste0(preparation, "_feb"),
         paste0(preparation, "_oct")) %>%
  mutate(prep_score_feb = prep_first_aid_water_feb*1.19 + prep_first_aid_fuel_feb*1.18 +
           prep_reaction_plan_feb*1.16 + prep_evacuation_plan_feb*1.08 + 
           prep_reaction_plan_oth_hromadas_feb*.98 + prep_reaction_plan_oda_feb*1.02 + 
           prep_dftg_creation_feb*1.04 + prep_national_resistance_feb*.94 + 
           prep_starosta_meeting_feb*1.12 + prep_communal_meetiing_feb*1.13 + 
           prep_online_map_feb*.91 + prep_shelter_list_feb*.97 + 
           prep_notification_check_feb*1.19 + prep_backup_feb*1.08,
         prep_score_oct = prep_first_aid_water_oct*1.19 + prep_first_aid_fuel_oct*1.18 +
           prep_reaction_plan_oct*1.16 + prep_evacuation_plan_oct*1.08 + 
           prep_reaction_plan_oth_hromadas_oct*.98 + prep_reaction_plan_oda_oct*1.02 + 
           prep_dftg_creation_oct*1.04 + prep_national_resistance_oct*.94 + 
           prep_starosta_meeting_oct*1.12 + prep_communal_meetiing_oct*1.13 + 
           prep_online_map_oct*.91 + prep_shelter_list_oct*.97 + 
           prep_notification_check_oct*1.19 + prep_backup_oct*1.08) %>%
  select(hromada_code, prep_score_feb, prep_score_oct, starts_with('prep'))


## Some handy datasets for quick visualization
# Raw scale (0,1,2) with factors
ds1_prep_ordinal_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "As of Oct"
        ,. == 2 ~ "As of Feb"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","As of Oct","As of Feb",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)

# Binary scale (0,1) with factors
ds1_prep_binary_factors <- 
  ds1_prep %>% 
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
  select(hromada_code, starts_with("prep_score"),preparation)

# Binary scale (0,1) with factors
ds1_prep_binary_factors_feb <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0  ~ "No"
        ,. == 1 ~ "No"
        ,. == 2 ~ "Yes"
        ,TRUE   ~ "No"
      ) %>% factor(levels=c("No","Yes"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)

# ----- inspect-data-1-prep -----------------------


ds1_prep_ordinal_integers %>% glimpse()
ds1_prep_ordinal_factors %>% glimpse()
ds1_prep_binary_integers %>% glimpse()
ds1_prep_binary_factors %>% glimpse()
# ---- tweak-data-1-info --------------------

d_meta_info <- 
  meta_survey %>% 
  filter(group== "information") %>%
  select(item_name = name,label_en,item_number)
meta_choices %>% filter(list_name=="commun_prep")
item_information <- d_meta_info %>% pull(item_name)

ds1_info <- 
  ds0 %>% 
  mutate(
    across(
      .cols = item_information
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "After Feb 24"
        ,. == 2 ~ "Before Feb 24"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Before Feb 24","After Feb 24",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code,item_information)

# ---- testing chunks ----------------------------------------------------------

library(rpivotTable)
rpivotTable(ds_general1)

ds0 <- ds_survey %>% 
  left_join(ds_general %>% 
              filter(occipied_before_2022 == 0) %>% 
              select(hromada_code, war_zone_20_06_2022, hromadas_30km_russia_belarus)) %>% 
  summarize(n = n(), .by = war_zone_20_06_2022) %>% 
  summarize(prop = n / sum(n))


ds_general %>% 
  filter(occipied_before_2022 == 0) %>% 
  summarise(n = n(), .by = type) %>% 
  mutate(prop = n / sum(n))
# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/survey-overview/survey-overview-hatsko.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)