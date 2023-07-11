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
ds_general <- readr::read_csv("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/full_dataset.csv")

# https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)
ds_weights <- readr::read_csv("./data-private/derived/index_preparedness_weights.csv")
ds_partnerships <- readr::read_csv("./data-private/derived/partnerships-hromadas.csv")
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
  select(ends_with('prop'), starts_with('income')) %>%
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
  ) %>% 
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
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
  mutate(prep_score_feb = prep_first_aid_water_feb*1.11 + prep_first_aid_fuel_feb*1.11 +
           prep_reaction_plan_feb*1.08 + prep_evacuation_plan_feb*1.01 + 
           prep_reaction_plan_oth_hromadas_feb*.91 + prep_reaction_plan_oda_feb*.95 + 
           prep_dftg_creation_feb*.97 + prep_national_resistance_feb*.88 + 
           prep_starosta_meeting_feb*1.04 + prep_communal_meetiing_feb*1.05 + 
           prep_online_map_feb*.85 + prep_shelter_list_feb*.91 + 
           prep_notification_check_feb*1.11 + prep_backup_feb*1.01,
         prep_score_oct = prep_first_aid_water_oct*1.19 + prep_first_aid_fuel_oct*1.18 +
           prep_reaction_plan_oct*1.16 + prep_evacuation_plan_oct*1.08 + 
           prep_reaction_plan_oth_hromadas_oct*.98 + prep_reaction_plan_oda_oct*1.02 + 
           prep_dftg_creation_oct*1.04 + prep_national_resistance_oct*.94 + 
           prep_starosta_meeting_oct*1.12 + prep_communal_meetiing_oct*1.13 + 
           prep_online_map_oct*.91 + prep_shelter_list_oct*.97 + 
           prep_notification_check_oct*1.19 + prep_backup_oct*1.08) 
  

vars_income <- ds0 %>% select(starts_with('income'), ends_with('prop')) %>% 
  colnames()

vars_expenses <- ds0 %>% select(starts_with('expenses'), association) %>% colnames()

         
vars2 <- ds0 %>% 
  select(contains('pct'), contains('prep_score'), prep_winter_count) %>% colnames()

var_select <- c(vars_expenses, vars2)

ds1 <- ds0 %>% select(all_of(var_select), Status_war_sept_ext) %>% select(-prep_score_oct)

ds2 <- ds0 %>% 
  select(contains('expenses'), oblast_significance, type, prep_score_feb, 
         prep_winter_count, Status_war_sept_ext) %>% 
  mutate(city = factor(ifelse(type == 'міська', 1, 0)),
         expenses_local_government_2021_perc = expenses_local_government_2021*100,
         expenses_state_functions_2021_perc = expenses_state_functions_2021*100) %>% 
  filter(Status_war_sept_ext!="occupied")

ds1 %>% summarise(across(everything(), ~sum(is.na(.)))) %>% t()
#+ - tweak-data-2 --------------------------------------------------------------

ds2 <-
  ds1 %>%
  # scaling
  mutate(
    # income_total_per_capita_k = income_total_2021_per_capita/1000
    time_before_24th_years = time_before_24th/365
    ,dfrr_executed_k_per_capita = dfrr_executed/ total_population_2022
    ,urban_perc_100 = urban_pct * 100
    ,passengers_2021_per_capita = passangers_2021 / total_population_2022
    ,osbb_per_capita_k_2020 = sum_osbb_2020 / total_population_2022 * 1000
    ,own_income_prop_2021 = own_income_prop_2021 * 100
    ,turnout_2020 = turnout_2020 * 100
    ,travel_time_60 = travel_time / 60
    ,distance_to_russia_belarus_100 = distance_to_russia_belarus / 100
    ,distance_to_eu_100 = distance_to_eu / 100
    ,dfrr_executed_20_21_k = dfrr_executed_20_21 / total_population_2022
  )  %>%
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k_per_capita, 0)
    ,passengers_2021_per_capita_zeros = replace_na(passengers_2021_per_capita, 0)
    ,osbb_per_capita_2020_zeros = replace_na(osbb_per_capita_k_2020, 0)
  ) %>%
  # making binary vars where not enough variation
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
    ,youth_centers_b = ifelse(youth_centers == 0, 0, 1)
    ,youth_councils_b =ifelse(youth_councils == 0, 0, 1)
    ,city = factor(ifelse(type == 'міська', 1, 0))
    ,dfrr_bin = ifelse(dfrr_executed_20_21 > 0, 1, 0)
    ,pioneer = ifelse(creation_year %in% c(2015, 2016), 1, 0)
  ) %>% fastDummies::dummy_cols(select_columns = c('type', 'region_en')) %>% 
  select(-c(hromada_name, hromada_full_name, raion_code, raion_name, oblast_code,
            oblast_name, occupation, military_action, hromada_code, 
            contains("declarations"), occipied_before_2022, contains('war_zone'),
            dfrr_executed, sum_osbb_2020, 
            osbb_per_capita_k_2020, business_support_centers, youth_councils, 
            youth_centers, time_before_24th, 
            passangers_2021, urban_pct, passengers_2021_per_capita, region_en, 
            type, city)) %>% 
  select(-c(mountain_hromada, near_seas, bordering_hromadas,
            hromadas_30km_from_border, buffer_nat_15km,
            buffer_int_15km, oblast_center, 
            passengers_2021_per_capita_zeros, creation_date,
            creation_year, edem_consultations, edem_petitions, edem_participatory_budget,
            edem_open_hromada))


descr <- ds2 %>% 
  summarise(across(everything(), list(mean = mean, sd = sd, na = ~sum(is.na(.x))))) %>% 
  pivot_longer(everything(),
               names_to = c("my_names", ".value"),
               names_pattern = "(.+)_(.+$)") 

descr %>% write.table('clipboard')


ds2 %>% select(turnout_2020) %>% 
  na.omit() %>% 
  summarise(mean = mean(turnout_2020), sd = sd(turnout_2020))
##+ Preparation Score February -------------------------------------------------

ds2$prep_score_feb_round <- round(ds2$prep_score_feb)

model_gaussian <- lm(prep_score_feb ~ oblast_significance + city,
                     data = ds2)
summary(model_gaussian)

model_gaussian_re <- lmtest::coeftest(model_gaussian, 
                                      vcov = sandwich::vcovHC(model_gaussian, type = 'HC3'))
model_gaussian_re

plot(model_gaussian)

model_poisson <- glm(prep_score_feb ~ expenses_state_functions_2021_perc, 
                     data = ds2, family = "poisson")

summary(model_poisson)

model_negbin <- MASS::glm.nb(prep_score_feb_round ~ expenses_local_government_2021_perc, 
                             data = ds2)

lrtest <- lmtest::lrtest(model_poisson, model_negbin)

if (lrtest$Pr[2] < 0.05) {
  print("Use negative binomial regression")
} else {
  print("Use Poisson regression")
}

# Fit the zero-inflated binomial model
zib_model <- pscl::zeroinfl(prep_score_feb_round ~  expenses_local_government_2021_perc | expenses_local_government_2021_perc, 
                            dist = "negbin", data = ds2)

# Compare the AIC of the models
if (AIC(zib_model) < AIC(model_negbin)) {
  print("Zero-inflated binomial model fits better than negative binomial model")
} else {
  print("Negative binomial model fits better than zero-inflated binomial model")
}

summary(zib_model)
summary(model_negbin)

##+ Preparation Winter -------------------------------------------------

model_negbin <- MASS::glm.nb(
  prep_winter_count ~ expenses_local_government_2021_perc, 
                             data = ds2)

summary(model_negbin)

model_negbin_re <- lmtest::coeftest(model_negbin, 
                                      vcov = sandwich::vcovHC(model_negbin, type = 'HC0'))
model_negbin_re

model_negbin_re <- lmtest::coeftest(model_negbin, 
                                    vcov = sandwich::vcovHC(model_negbin, type = 'HC1'))
model_negbin_re

model_negbin_re <- lmtest::coeftest(model_negbin, 
                                    vcov = sandwich::vcovHC(model_negbin, type = 'HC2'))
model_negbin_re

variables <- colnames(ds2)[-c(4, 5 , 24, 28)]

# Create an empty dataframe to store the results
results_df <- data.frame(variable = character(),
                         estimate = numeric(),
                         std_error = numeric(),
                         z_value = numeric(),
                         p_value = numeric(),
                         stringsAsFactors = FALSE)

# Loop over the variables and fit a poisson regression model for each variable
for (var in variables) {
  # Create a formula for the poisson regression model with the current variable
  formula <- as.formula(paste("prep_winter_count ~", var, '+', 'occupation_and_combat'))
  
  # Fit the poisson regression model
  model <- glm(formula, data = ds2, family = "poisson")
  
  # Extract the coefficient estimates, standard errors, z-values, and p-values
  coef_estimates <- coef(summary(model))[2,1]
  std_errors <- coef(summary(model))[2,2]
  z_values <- coef(summary(model))[2,3]
  p_values <- coef(summary(model))[2,4]
  
  # Add the results to the dataframe
  results_df <- rbind(results_df, data.frame(variable = var,
                                             estimate = round(coef_estimates,3),
                                             std_error = round(std_errors,3),
                                             z_value = z_values,
                                             p_value = p_values,
                                             stringsAsFactors = FALSE))
}

# Print the results dataframe
print(results_df)

write.table(results_df, 'clipboard')

filter_result <- results_df %>%
  filter(p_value < 0.05)

#+ Best Subset ----------------------

regfit.full <- leaps::regsubsets(prep_score_feb ~ ., data = ds2, really.big=TRUE, 
                                 method = "forward", nvmax = 15)
reg.summary <- summary(regfit.full)

reg.summary

reg.summary$cp

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

which.max(reg.summary$adjr2)
points(7, reg.summary$adjr2[7], col = "red", cex = 2, 
       pch = 21)

plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(4, reg.summary$cp[4], col = "red", cex = 2,
       pch = 21)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(2, reg.summary$bic[2], col = "red", cex = 2,
       pch = 21)

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_model_formula(2, regfit.full, "prep_score_feb")
get_model_formula(4, regfit.full, "prep_score_feb")
get_model_formula(6, regfit.full, "prep_score_feb")
get_model_formula(9, regfit.full, "prep_score_feb")

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

# Compute cross-validation error
model.ids <- 1:15
cv.errors <-  map(model.ids, get_model_formula, regfit.full, "prep_score_feb") %>%
  map(get_cv_error, data = ds2) %>%
  unlist()
cv.errors

# Select the model that minimize the CV error
which.min(cv.errors)


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
