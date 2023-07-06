#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
# Sys.setlocale("LC_CTYPE", "ukr")
Sys.setlocale("LC_CTYPE", 'en_US.UTF-8')
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


##--- Read data ---

# Survey data
colnames <- readxl::read_excel("./data-private/raw/agro-survey.xlsx", sheet = "variables") %>% 
  pull(eng_label)

d0 <- readxl::read_excel("./data-private/raw/agro-survey.xlsx", sheet = "answers", col_names = colnames, skip=1)  %>% 
  mutate(
    oblast = str_remove(oblast, " область")
    ,raion = str_remove(raion, " район")
    ,raion = str_replace_all(raion, c("'"="ʼ", "’"="ʼ"))
    ,hromada_name = str_replace_all(hromada_name, c("a"="а", "o"="о", "e"="е", "O"="О", "p"="р", "'"="ʼ", "ʼ"="ʼ"))
    ,key = paste(oblast, raion, hromada_name)
  )

#general data
ds_population <- readr::read_csv("./data-public/derived/ua-pop-2022.csv")


ds_hromada <- readr::read_csv("./data-public/derived/ua-admin-hromada.csv") %>% 
  mutate(
    hromada_name = ifelse(hromada_code == "UA05120030000061384", "Війтівецька", hromada_name)
    ,key = paste(oblast_name, raion_name, hromada_name, type, "громада")
    ,key = str_replace_all(key, c("'"="ʼ", "’"="ʼ"))
  )

ds_budget <- readxl::read_xlsx("./data-public/derived/hromada_budget_2020_2022.xlsx") 

ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

#transform budget data as predictors (2021)
ds1_budget <- 
  ds_budget %>% 
  group_by(hromada_name, hromada_code, year) %>% 
  summarise(total = sum(income_total), own = sum(income_own), .groups = "drop") %>% 
  filter(year == "2021") %>% 
  left_join(
    ds_population %>% select(hromada_code, total_population_2022)
    ,by = "hromada_code"
  ) %>% 
  mutate(
    income_tot_per_capita = total/total_population_2022
    ,income_own_per_capita = own/total_population_2022
  ) %>% 
  ungroup() 


oblasts <- readr::read_csv("./data-private/raw/oblast.csv") %>% 
  mutate(
    oblast_name_en = case_when(
      oblast_name_en == "Driproptrovska" ~ "Dnipropetrovska"
      ,oblast_name_en == "Ivano-Frankivsk" ~ "Ivano_Frankivsk"
      ,oblast_name_en == "Kyiv-oblast" ~ "Kyivska"
      ,oblast_name_en == "Vonyn" ~ "Volyn"
      ,TRUE ~ oblast_name_en
    )
  )


#---- tweak data -------------------------------------------------------------------

d1 <- 
  d0 %>% 
  select(!c(start:note1)) %>% 
  rename(index = `_index`) %>% 
  left_join(
    ds_hromada %>% select(key, hromada_code, type) 
    ,by = "key"
  ) %>%
  left_join(
    ds_general %>% select(!c(hromada_name:hromada_full_name))
    ,by = "hromada_code"
  ) %>% 
  distinct(hromada_code, .keep_all = T)


#add counters
d2 <- d1 %>% 
  mutate(
    budget_cut_sphere_count = case_when(
      budget_cut_sphere.no_cuts == 1 ~ 0
      ,budget_cut_sphere.no_cuts == 0 ~ rowSums(across(budget_cut_sphere.state_functions:budget_cut_sphere.other))
    )
    ,budget_cut_type_count = case_when(
      budget_cut_type.no_cuts == 1 ~ 0
      ,budget_cut_type.no_cuts == 0 ~ rowSums(across(budget_cut_type.wages:budget_cut_type.other))
    )
    ,ingo_count = rowSums(across(ingo_ac:ingo_other))
    ,countries_count = rowSums(across(countries_australia:countries_other))
  )

#---- compare survey data with population-------------------------------------------------------------------
# library(tableone)
# 
# predictors_all <- 
#   ds_general %>% 
#   select(-c("hromada_code", "hromada_name","raion_code", "raion_name","oblast_code",
#             "oblast_name","hromada_full_name","hromada_center_code","hromada_center",           
#             "lat_center","lon_center", "party")) %>% 
#   colnames()
# 
# ds_survey_codes <- 
#   d2 %>% 
#   pull(hromada_code)
# 
# ds_surveyed <- 
#   ds_general %>% 
#   filter(hromada_code %in% ds_survey_codes) %>% 
#   mutate(survey_participant = "surveyed" )
# 
# ds_comparison <- 
#   ds_general  %>% 
#   mutate(survey_participant = "all")
# 
# #add to all gromadas (including surveyed) surveyed hromadas one more time to make a comparison
# ds_comparison <- rbind(ds_surveyed, ds_comparison)
# 
# table_pre <- CreateTableOne(vars=predictors_all
#                             ,factorVars = c("sex_head", "education_head", "incumbent", "rda")
#                             , strata = "survey_participant"
#                             , data=ds_comparison)
# print(table_pre, smd=T)

#---- weighting -------------------------------------------------------------------
library(pewmethods)

summary(ds_general$total_popultaion_2022)
summary(ds_general$age_head)
summary(ds_general$income_total_2021/1000000)
summary(ds_general$turnout_2020)
summary(ds_general$travel_time)

ds_weighting <- ds_general %>% 
  filter(occipied_before_2022 == 0) %>% 
  mutate(
    population_categories = cut(
      total_population_2022
      ,breaks = c(0, 10000, 25000, 50000, 100000, 1500000)
      ,labels = c("0-10k", "10-25k", "25-50k", "50-100k", "100k+")
    )
    ,age_categories = cut(
      age_head
      ,breaks = c(0, 44, 59, 100)
      ,labels = c("26-44", "45-59", "60+")
    )
    ,income_categories = cut(
      income_total_2021/1000 #convert form thousands to millions
      ,breaks = c(0, 25, 50, 100, 250, 10000)
      ,labels = c("0-25 mln", "25-50 mln", "50-100 mln", "100-250 mln", "250+ mln")
    )
    ,turnout_categories = cut(
      turnout_2020
      ,breaks = c(0, 0.35, 0.50, 1)
      ,labels = c("<35%", "35-50%", ">50%")
    )
    ,travel_time_categories = cut(
      travel_time
      ,breaks = c(-1, 60, 120, 350)
      ,labels = c("0-60 min", "60-120 min", "120+ min")
    )
    # ,age_categories  = as.character(age_categories )
    # ,age_categories = case_when(
    #   is.na(age_categories) == T ~ "no elections"
    #   ,is.na(age_categories) == F ~ age_categories
    # )
  ) 



d3 <- d2 %>% 
  filter(occipied_before_2022 == 0) %>% 
  mutate(
    population_categories = cut(
      total_population_2022
      ,breaks = c(0, 10000, 25000, 50000, 100000, 1500000)
      ,labels = c("0-10k", "10-25k", "25-50k", "50-100k", "100k+")
    )
    ,age_categories = cut(
      age_head
      ,breaks = c(0, 44, 59, 100)
      ,labels = c("26-44", "45-59", "60+")
    )
    ,income_categories = cut(
      income_total_2021/1000 #convert form thousands to millions
      ,breaks = c(0, 25, 50, 100, 250, 10000)
      ,labels = c("0-25 mln", "25-50 mln", "50-100 mln", "100-250 mln", "250+ mln")
    )
    ,turnout_categories = cut(
      turnout_2020
      ,breaks = c(0, 0.35, 0.50, 1)
      ,labels = c("<35%", "35-50%", ">50%")
    )
    ,travel_time_categories = cut(
      travel_time
      ,breaks = c(-1, 60, 120, 350)
      ,labels = c("0-60 min", "60-120 min", "120+ min")
    )
  ) 


categories_weights <- ds_weighting %>% 
  count(type, population_categories, war_zone_10_10_2022, voluntary,
        income_categories, high_educ, sex_head) %>% 
  mutate(weight = n/sum(n))

targets <- create_raking_targets(
  categories_weights
  ,vars = c("type", "population_categories", "war_zone_10_10_2022", "voluntary",
            "income_categories")
  ,wt = "weight"
)


d3 %>% count(income_categories)

d4 <- 
  d3 %>% 
  mutate(
    rk_type = as.factor(type)
    ,rk_population_categories = as.factor(population_categories)
    ,rk_war_zone_10_10_2022 = as.factor(war_zone_10_10_2022)
    ,rk_voluntary = as.factor(voluntary)
    ,rk_income_categories = as.factor(income_categories)
    # ,rk_high_educ = as.factor(high_educ)
    # ,rk_sex_head = as.factor(sex_head)
  ) 

d4$weight <- rake_survey(d4, pop_margins = targets)

calculate_deff(d4$weight)

#ALTERNATIVE APPROACH WITH SURVEY PACKAGE
# svy.unweighted <- svydesign(ids=~1, data=d2)

# weighting_varibales <- c("type", "region_en", "population_categories", "war_zone_10_10_2022", "voluntary",
#                          "income_categories", "turnout_categories", "sex_head", "age_categories", "incumbent", 
#                          "party_national_winner", "high_educ", "travel_time_categories")
# 
# weighting_targets <- list() 
# for (i in seq_along(weighting_varibales)){
#   weighting_targets[[i]]<-data.frame()
#   l<-weighting_varibales[i]
#   weighting_targets[[i]] <- 
#     ds_weighting %>% 
#     count(.data[[l]]) %>% 
#     rename(Freq = n)
# }
# 
# sample_margins <- list() 
# for (i in seq_along(weighting_varibales)){
#   sample_margins[[i]]<-data.frame()
#   l<-weighting_varibales[i]
#   sample_margins[[i]] <- 
#     d3 %>% 
#     count(.data[[l]]) %>% 
#     rename(Freq = n)
# }
# 
# data.svy.rake <- rake(design = svy.unweighted,
#                       sample.margins = ~sample_margins,
#                       population.margins = weighting_targets)


#---- save data -------------------------------------------------------------------
readr::write_csv(d4, "data-private/derived/agro-survey-full.csv")



