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
prints_folder <- paste0("./analysis/survey-prep-model/prints")
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
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")

# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

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
ds_general %>% glimpse(80)
ds_survey %>% glimpse(80)

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
meta_survey %>% glimpse()

meta_survey %>% 
  filter(type %in% c("begin_group","end_group")) %>% 
  select(type, name) %>% 
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
  left_join(ds_general %>% select(hromada_code, train_station, passangers_2021),
            by = 'hromada_code')

ds0 %>% glimpse(80)
# ---- inspect-data-0 ------------------------------------------------------------

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



# ----- prep-modeling sex---------------
# Support

ds_general %>% explore::describe(passangers_2021) 
ds_general %>% select(passangers_2021, hromada_name) %>% arrange(passangers_2021) %>% View()


support_vars <- c(
  "hromada_name"
)

# Outcomes

outcomes_vars <- c(
  "prep_score_feb"
  ,"prep_score_oct"
  ,"prep_score_combo"
)


# Continuous - good for spreading out # Valentyn, please add relevant predictors here
predictor_vars_continuous <- c(
  "income_own_per_capita"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita" # що надходить від держави, 
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_share" # corrected index above 
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'dfrr_executed' # сума всіх проектів (скільки дали на розвиток громади - спец прокти), виграні інвестиційні проекти, на скільки г. залучила інвест кошти в рамках програми
  ,'passangers_2021'
  ,'business_support_centers' # кількість центрів
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ 
  ,"turnout_2020" # явка
  ,'square' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th" # коли сформувалась громада
)

predictor_vars_continuous_scaled <- c(
  "income_own_per_capita_k"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita_k"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita_k" # що надходить від держави, 
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_share" # corrected index above 
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'dfrr_executed' # сума всіх проектів (скільки дали на розвиток громади - спец прокти), виграні інвестиційні проекти, на скільки г. залучила інвест кошти в рамках програми
  ,'passangers_2021'
  ,'business_support_centers' # кількість центрів
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ 
  ,"turnout_2020" # явка
  ,'square' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th_years" # коли сформувалась громада
)
# Categorical - for color # Valentyn, please add relevant predictors here
predictor_vars_categorical <- c(
  "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
  ,'incumbent'
  ,'rda'
  ,'not_from_here'
  ,'enterpreuner'
  ,'unemployed'
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_27_04_2022'
  ,'train_station'
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  ,'youth_centers'
  ,'youth_councils'
)
predictor_vars <- c(
  predictor_vars_continuous
  ,predictor_vars_categorical
)



ds2_prep <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score")) %>% 
  left_join(
    ds0 %>% 
      select(hromada_code,all_of(predictor_vars), oblast_name_en, hromada_name)
  ) %>% 
  mutate(
    across(
      .cols = all_of(predictor_vars_categorical)
      ,.fns = ~factor(.)
    )
  ) %>% 
  # scaling 
  mutate(
    income_own_per_capita_k = income_own_per_capita/1000
    ,income_total_per_capita_k = income_total_per_capita/1000
    ,income_tranfert_per_capita_k = income_tranfert_per_capita/1000
    ,time_before_24th_years = time_before_24th/365
  )  %>% 
  mutate(country = "Ukraine") 

ds2_prep %>% glimpse(90)
ds2_prep %>% select(predictor_vars_categorical) %>% look_for()


# ----- simple-model-scan -----------------------
# Goal: scan the bivariate relationship bw outcome and a set of predictors
# to understand what has the strongest association

source("./analysis/survey-prep-model/custom-model-functions.R")
predictor_vars_continuous_scaled

ls_temp <- list()
# for(i in predictor_vars_continuous_scaled){
for(i in predictor_vars_categorical){
# source("./analysis/survey-prep-model/custom-model-functions.R")
  model_i <- 
    ds2_prep %>% 
    run_simple_model(
      dependent    = "prep_score_feb"
      ,explanatory = c(i)
      ,depdist     = "poisson"
   )
  
  ls_temp[[model_i$equation$formula %>% deparse()]] <- 
    list(
      "outcome"     = model_i$equation$outcome
      ,"predictor"  = model_i$equation$predictor 
      ,"confounder"  = model_i$equation$confounder %>% paste0(collapse = " + ")
      ,"rsq"       = model_i$model_fit$rsquare # explanatory capacity of the full model
      ,"rsq_change" = model_i$rsq_change # gains in explanatory capacity due to predictor
      ,"nobs"       = model_i$nobs
      ,"distribution" = model_i$depdist
    )
  }
d <- 
  ls_temp %>% bind_rows()
d

d %>% 
  ggplot(aes(y= fct_reorder(predictor,rsq),x = rsq ))+
  geom_segment(aes(x=0,xend=rsq,yend=predictor))+
  geom_point()+
  geom_point(aes(x=rsq_change),color="red")+
  geom_segment(aes(x=0,xend=rsq,yend=predictor))+
  scale_x_continuous(labels = scales::percent_format())+
  labs(
    title = paste0("Outcome: ",d %>% pull(outcome) %>% unique())
    ,subtitle = paste0("Adjusting for: ", d %>% pull(confounder) %>% unique())
    ,x = "Percent of variabilty in the outcome explained by predictor(s)"
  )
# ---- model-graph-testers-------------

# developing function to print a faceted scatter y=criterion
d <- 
  ds2_prep %>% 
  pivot_longer(
    cols = predictor_vars_continuous_scaled
    ,names_to = "item_name"
    ,values_to = "item_value"
  ) %>% glimpse()

make_plot_prepvs <- function(
    d
    ,xvar    # "prep_score"
    ,yvar    # "item_value"
    ,fillvar # "region_en"
)
{
  g <- 
    d %>% 
    mutate(
      item_name = factor(item_name, levels = predictor_vars_continuous_scaled)
    ) %>% 
    ggplot(aes(
      x     = !!rlang::sym(xvar)
      ,y    = !!rlang::sym(yvar)
      ,fill = !!rlang::sym(fillvar)
      ,color = !!rlang::sym(fillvar)
    ))+
    ggplot2::scale_fill_viridis_d(
      begin = 0, end = .8, direction = 1
      ,option = "plasma",guide= guide_legend(reverse=T)
    )+
    ggplot2::scale_color_viridis_d(
      begin = 0, end = .8, direction = 1
      ,option = "plasma",guide= guide_legend(reverse=T)
    )+
    geom_point(
      shape=21,color = "black", size =3, alpha = .1
      ,position=position_jitter(seed=42)
    )+
    geom_smooth(method="lm", se=F,linewidth=.8)+
    scale_y_continuous(
      labels = scales::comma_format()
      ,expand = expansion(mult =c(.1,.4))
      )+
    scale_x_continuous(
      labels = scales::comma_format()
    )+
    facet_wrap(facets = "item_name", scales = "free_x")+
    ggpmisc::stat_poly_eq(
      formula = y ~ + x
      ,aes(
        label = paste(..eq.label.., ..rr.label.., ..n.label..,sep = "~~~")
        ,color = !!rlang::sym(fillvar)
        )
      ,parse = TRUE
      , vstep = .08
      ,size = 3
      ) +
    # coord_flip()+
    labs(
      title = paste0("Relationship between Invasion Preparedness Score (horizontal) and other attributes of hromadas")
    )
  g
}  
# To see how it works
g <- 
  d %>% 
  mutate(country = "Ukraine") %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "prep_score_oct"
    ,fillvar = "country"
  )  
g %>% quick_save("tester",w=16,h=9)


# What hromada is outlier on total population?
outliers_total_population_2022 <- 
  ds2_prep %>%
  filter(total_population_2022 > 200000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_sum_osbb_2020 <- 
  ds2_prep %>%
  filter(sum_osbb_2020 > 190) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_passangers_2021 <- 
  ds2_prep %>%
  filter(passangers_2021 >60000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)

outliers <- 
  c(outliers_total_population_2022
  ,outliers_sum_osbb_2020
  ,outliers_passangers_2021) %>% 
  unique() 
outliers %>% length()

g <- 
  d %>% 
  filter(hromada_code %ni% outliers) %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "prep_score_oct"
    ,fillvar = "country"
  )  
g %>% quick_save("tester2",w=16,h=9)

g <- 
  d %>% 
  filter(hromada_code %in% outliers) %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "prep_score_oct"
    ,fillvar = "country"
  )  
g %>% quick_save("tester3",w=16,h=9)


# ---- one-model -----------------------

fit1_gaussian <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + sex_head
    ,data = ds2_prep
    ,family = "gaussian"
  )
fit1_poisson <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + sex_head
    ,data = ds2_prep
    ,family = "poisson"
  )

fit1_gaussian %>% summary()

fit1_gaussian %>% broom::glance() # model properties
fit1_gaussian %>% broom::tidy() # coefficients
fit1_gaussian %>% broom::augment() # add predicted values

fit1_poisson %>% broom::glance() # model properties
fit1_poisson %>% broom::tidy() # coefficients
fit1_poisson %>% broom::augment() # add predicted values

fit1_gaussian %>% jtools::plot_summs()
fit1_poisson %>% jtools::plot_summs()

jtools::plot_summs(fit1_gaussian, fit1_poisson)


# Resources for handling modeling objects
# jtools vignette - https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# broom vignette - https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# GGally - https://ggobi.github.io/ggally/

fit2_gaussian <- 
  glm(
    formula = prep_score_feb ~ sex_head
    ,data = ds2_prep
    ,family = "gaussian"
  )
fit2_poisson <- 
  glm(
    formula = prep_score_feb ~ sex_head
    ,data = ds2_prep
    ,family = "poisson"
  )

fit2_gaussian %>% broom::tidy() # coefficients
fit2_poisson %>% broom::tidy() # coefficients


jtools::plot_summs(fit2_gaussian, fit2_poisson)


vars_density <- c( 
"prep_score"
, 'square'
,"n_settlements"
,"urban_pct"
# ,"time_before_24th_years"
,"total_population_2022"
# ,"urban_population_2022"
  )
ds2_prep %>% 
  filter(total_population_2022 < 200000) %>% #select(hromada_name, total_population_2022)
  select(vars_density) %>% GGally::ggpairs()


fit3_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type
    ,data = ds2_prep
    ,family = "gaussian"
  )
fit3_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k 
    ,data = ds2_prep
    ,family = "poisson"
  )
anova(fit3_full, fit3_reduced,test = "Chisq")

jtools::plot_summs(model.names = c("Full","Reduced"),fit3_full, fit3_reduced)
fit3_full %>% broom::tidy()

d <- 
  fit3_full %>% 
  broom::augment()

g <- 
  d %>% 
  # ggplot(aes(y = prep_score_feb, x = income_own_per_capita_k, fill=type, color=type))+
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  # geom_smooth(method = "lm", se=F)+
  # facet_wrap("type")+
  geom_point()
g

ds2_prep %>% 
  ggplot(aes(x=type, y = prep_score_feb))+
  geom_boxplot()+
  geom_jitter()



# ----- fit4 ---------------------

fit4_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type +income_own_per_capita_k*type 
    ,data = ds2_prep
    ,family = "poisson"
  )
fit4_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type 
    ,data = ds2_prep
    ,family = "poisson"
  )
anova(fit4_full, fit4_reduced,test = "Chisq")
fit4_full %>% broom::tidy()
# jtools::plot_summs(model.names = c("Full","Reduced"),fit4_full, fit4_reduced)

g <- 
  fit4_full %>% 
  broom::augment() %>% 
  # ggplot(aes(y = prep_score_feb, x = income_own_per_capita_k, fill=type, color=type))+
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  # geom_smooth(method = "lm", se=F)+
  # facet_wrap("type")+
  geom_point()
g
# 
# ds2_prep %>% 
#   ggplot(aes(x=type, y = prep_score_feb))+
#   geom_boxplot()+
#   geom_jitter()


# ---- fit5 ----------

d_model <- 
  ds2_prep %>% 
  mutate(
    urban = case_when(
      type %in% c("селищна","сільска") ~ TRUE
      ,TRUE ~ FALSE
    )
  ) %>% 
  select(prep_score_feb, income_own_per_capita_k, type) %>% glimpse()


fit5_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type + income_own_per_capita_k*type 
    ,data = ds2_prep
    ,family = "poisson"
  )
fit5_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type 
    ,data = ds2_prep
    ,family = "poisson"
  )
anova( fit5_reduced,fit5_full,test = "Chisq")
fit5_full %>% broom::tidy()

g <- 
  fit5_full %>% 
  broom::augment() %>% 
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  geom_point()
g

ds2_prep %>% 
  ggplot(aes(x=type, y = prep_score_feb))+
  geom_boxplot()+
  geom_jitter()


ds2_prep %>% select(income_own_per_capita, income_total_per_capita, income_tranfert_per_capita) %>% 
  GGally::ggpairs()

# ----- fit6 ---------------------------------
d_model <- 
  ds2_prep %>% 
  mutate(
    urban = case_when(
      type %in% c("селищна","сільска") ~ TRUE
      ,TRUE ~ FALSE
    )
  ) %>% 
  # select(prep_score_feb, income_total_per_capita_k, urban)
  select(prep_score_feb, income_own_per_capita_k, urban)

fit5_full <- 
  glm(
    formula = prep_score_feb ~ (.)^2 
    ,data = d_model
    ,family = "poisson"
  )
fit5_reduced <- 
  glm(
    formula = prep_score_feb ~  (.)^2
    ,data = d_model
    ,family = "poisson"
  )
anova(fit5_full, fit5_reduced,test = "Chisq")
fit5_full %>% broom::tidy()

g <- 
  fit5_full %>% 
  broom::augment() %>% 
  ggplot(aes(y = .fitted, x = income_total_per_capita_k, fill=urban, color=urban))+
  geom_point()
g

# ---- --------

vars_density <- c( 
   'square'
  ,"n_settlements"
  ,"total_population_2022"
)


ds2_prep %>% make_bi_freq_graph("type")
ds2_prep %>% make_bi_freq_graph("type", "voluntary")



# ----- print-many-models -------------
# To execution multiple scenarios
for(i in predictor_vars_categorical){
  
  for(ii in outcomes_vars){
    
    g <- 
      d %>% 
      make_plot_prepvs(
        xvar    = "item_value"
        ,yvar     = ii
        ,fillvar = i
      )   
      file_name <- paste0(ii,"/",i)
      g %>% quick_save(paste0("/bivar-cont/",file_name),w=16,h=9)
      
      g <- 
        d %>% 
        filter(hromada_code %ni% outliers) %>% 
        make_plot_prepvs(
          xvar    = "item_value"
          ,yvar     = ii
          ,fillvar = i
        )   
      file_name <- paste0(ii,"/",i)
      g %>% quick_save(paste0("/bivar-cont/",file_name,"-no_out"),w=16,h=9)
  }
}



# ---- simple-model-scan --------------------
# 1 - Distributions of outcomes and predictors (cont + cat)
# 2 - Rsq of a simple model: outcome ~  p1 (+p(2...N)), where p2...N is an optional confounder
# 3 -  Faceted scatter (x = p1...N, y = outcome, color = p1...N)
# 3a - Faceted scatter (x = cont.predictor, y =outcome, color=cat.prdictor)
# 3b - Faceted boxplot (x = cat.predictor, y =outcome, color=con.prdictor)



# ---- modeling-simple-case -----------------------
# Let us review the anatomy of glm models using a simple case to see how we can scale it up
# we want to run many simple  models (only one or two predictors)

d_model <- 
  ds2_prep %>% 
  select(
    prep_score_oct, age_head, sex_head, incumbent, income_own_per_capita, idp_child_share , travel_time
  )

d_model %>% GGally::ggpairs()

fit1  <- 
  glm(
    f
  )

# ---- modeling-demonstration -----------------------

model <- 
  stats::glm(
    
  )

model <- 
  stats::glm(
    
  )


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
