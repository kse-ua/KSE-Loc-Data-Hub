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
source("./analysis/survey-prep-model/custom-model-functions.R")

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-prep-model/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
# ---- declare-functions -------------------------------------------------------
'%ni%' <- Negate(`%in%`)

# ---- load-data ---------------------------------------------------------------
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean_new.xlsx")

# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

# https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)
ds_weights <- readr::read_csv("./data-private/derived/index_preparedness_weights.csv")

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
  select(-c('help_for_military/other')) %>%
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
  ds_general %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()

geographic_vars <- c("distance_to_russia_belarus",'distance_to_russia', 'distance_to_eu',
                     "mountain_hromada", "near_seas", "bordering_hromadas", 
                     "hromadas_30km_from_border", "hromadas_30km_russia_belarus",
                     "buffer_nat_15km", "buffer_int_15km")

# ---- tweak-data-0 ----------------------
ds_general0 <- 
  ds_general %>% 
  mutate(
    survey_response = case_when(
      hromada_code %in% (ds_survey %>% pull(hromada_code) %>% unique()) ~ TRUE
      ,TRUE ~ FALSE
    )
  )

ds0 <- 
  ds_survey %>% 
  left_join(ds_general,
            by = 'hromada_code') %>% 
  select(-ends_with('.y')) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = list(~ sub("[.]x$", "", .))) %>%
  mutate(
    across(
      .cols = all_of(geographic_vars),
      .fns = ~replace_na(., 0L)
    )
  ) %>% 
  mutate(
    across(starts_with('income') & ends_with("2021"), ~ . / total_population_2022, .names = "{col}_per_capita")) %>% 
  mutate(
    occupation_and_combat = case_when(
      military_action == 'no_combat' & occupation == 'not_occupied' ~ 0,
      .default = 1)
    ) %>% 
  mutate(
    across(
      .cols = all_of(preparation)
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 0 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
        ,.default = 0
      ),
      .names = "{col}_feb"), 
    across(
        .cols = all_of(preparation)
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 1 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
          ,.default = 0
        ),
        .names = "{col}_oct"
      )
  ) %>%
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
           prep_notification_check_oct*1.19 + prep_backup_oct*1.08) 
  

vars1 <- ds0 %>% select(contains("income_total_"), contains("income_own_"),
                       contains("income_transfert_"), own_income_prop_2021,
                       transfert_prop_2021) %>% 
  select(-ends_with('2022')) %>% colnames()
         
vars2 <- ds0 %>% 
  select(contains('pct'), contains('prep_score')) %>% colnames()

vars3 <- ds0 %>% select(4:15, 191, 196:208, 210:212, 243:244, 248, 250:251, 255, 258:279,
                        295, 304) %>% colnames()

var_select <- c(vars1, vars2, vars3)

ds1 <- ds0 %>% select(all_of(var_select))




#+ - tweak-data-2 --------------------------------------------------------------

ds2 <-
  ds1 %>%
  # scaling
  mutate(
    income_own_per_capita_k = income_own_2021_per_capita/1000
    ,income_total_per_capita_k = income_total_2021_per_capita/1000
    ,income_tranfert_per_capita_k = income_transfert_2021_per_capita/1000
    ,time_before_24th_years = time_before_24th/365
    ,dfrr_executed_k = dfrr_executed/1000
    ,urban_perc_100 = urban_pct * 100
    ,passengers_2021_per_capita = passangers_2021 / total_popultaion_2022
    ,osbb_per_capita_k_2020 = sum_osbb_2020 / total_popultaion_2022 * 1000
  )  %>%
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k, 0)
    ,passengers_2021_per_capita_zeros = replace_na(passengers_2021_per_capita, 0)
    ,osbb_per_capita_2020_zeros = replace_na(osbb_per_capita_k_2020, 0)
  ) %>%
  # making binary vars where not enough variation
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
    ,youth_centers_b = ifelse(youth_centers == 0, 0, 1)
    ,youth_councils_b =ifelse(youth_councils == 0, 0, 1)
    ,city = factor(ifelse(type == 'міська', 1, 0))
  )

##+ Preparation Score February -------------------------------------------------

# check distribution
x <- ds2 %>% select(prep_score_feb) %>% filter(!is.na(prep_score_feb)) %>% pull()
pois_dist <- fitdistrplus::fitdist(x, distr = "norm")
plot(pois_dist)
# too left-skewed for poisson 
nbin_dist <- fitdistrplus::fitdist(x, distr = "poison", densfun = "poison")
plot(nbin_dist)
# seems like negative binomial

cor_ds <- ds2 %>% 
  select(is.numeric) %>% 
  select(-c(income_total_2021, income_own_2021, income_transfert_2021, 
            transfert_prop_2021, contains('declarations'), contains('war_zone'),
            dfrr_executed, dfrr_executed_k, sum_osbb_2020, osbb_per_capita_k_2020,
            business_support_centers, youth_councils, youth_centers, 
            income_total_2021_per_capita, income_own_2021_per_capita, 
            income_transfert_2021_per_capita, time_before_24th, passangers_2021,
            urban_pct, passengers_2021_per_capita)) %>% 
  na.omit()

cor <- cor(cor_ds, method = 'pearson')
cor[cor == 1 | cor < 0.3 | is.na(cor)] <- ''

g1 <- corrplot::corrplot(cor, method="circle", type = 'upper', tl.cex = .2)

fit1_gaussian <- 
  glm(
    formula = prep_score_feb ~ urban_perc_100
    ,data = ds2
    ,family = "gaussian"
  )

fit1_poisson <- 
  glm(
    formula = prep_score_feb ~ urban_perc_100
    ,data = ds2
    ,family = "poisson"
  )

summary(fit1_gaussian)
summary(fit1_poisson)

# for zero-inflated model

# fit1_zi_pois <- pscl::zeroinfl(formula = prep_score_feb ~ occupation_and_combat | 1
#                                ,data = ds2_prep
#                                ,dist = "pois")
# 
# summary(fit1_zi_pois)

fit1_zi_nbinom <- pscl::zeroinfl(formula = prep_score_feb ~ train_station | 1
                                 ,data = ds2_prep
                                 ,dist = "negbin")

summary(fit1_zi_nbinom)

##+ Preparation Score October --------------------------------------------------

# check distribution
x <- ds2_prep %>% select(prep_score_oct) %>% filter(!is.na(prep_score_oct)) %>% pull()
norm_dist <- fitdistrplus::fitdist(x, distr = "norm")
plot(norm_dist)
# seems like normal

fit1_norm <- 
  glm(
    formula = prep_score_oct ~ buffer_int_15km
    ,data = ds_prep_new
    ,family = "gaussian"
  )

summary(fit1_norm)

fit1_norm <- 
  glm(
    formula = prep_score_oct ~ buffer_int_15km + occupation_and_combat
    ,data = ds_prep_new
    ,family = "gaussian"
  )

summary(fit1_norm)

occupation_and_combat


# plot
d <-
  ds_prep_new %>%
  run_complex_scan(
    dependent = 'prep_score_oct'
    ,depdist = "gaussian"
    ,explantory_continous = geographic_vars_cont
    , explanatory_categorical = geographic_vars_cat
  )
d %>% plot_complex_scan()

geographic_vars <- c("distance_to_russia_belarus",'distance_to_russia', 
                     'distance_to_eu', "mountain_hromada", "near_seas", 
                     "bordering_hromadas", "hromadas_30km_from_border", 
                     "hromadas_30km_russia_belarus", "buffer_nat_15km", 
                     "buffer_int_15km")

geographic_vars_cont <- c("distance_to_russia_belarus",'distance_to_russia', 
                          'distance_to_eu')
geographic_vars_cat <- c("mountain_hromada", "near_seas", 
                         "bordering_hromadas", "hromadas_30km_from_border", 
                         "hromadas_30km_russia_belarus", "buffer_nat_15km", 
                         "buffer_int_15km")

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/survey-prep-model/survey-prep-model.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
