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

# ----- selecting variables ---------------------------------------------------------------

support_vars <- c(
  "hromada_name"
)

# Outcomes

outcomes_vars <- c(
  "prep_score_feb"
  ,"prep_score_oct"
  ,"prep_score_combo"
  ,"idp_registration_number" # внутрішньо переміщені особи - кількість ВПО
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_number" # corrected index above 
  ,"idp_real_share" # corrected index above 
  ,"idp_child_education" # кількість ВПО дітей
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'created_jobs' # ordinal
  ,'relocated_companies_text' # к-сть релокованих компаній
  ,'international_projects' # к-сть проєктів з міжнародними донорами
  ,'finance_school_shelters'
  ,'no_school_days_coded'
  ,'hromada_exp'
)


# Continuous - good for spreading out # Valentyn, please add relevant predictors here
predictor_vars_continuous <- c(
  "income_own_per_capita"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita" # що надходить від держави, 
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
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
  ,"age_head"
  ,"time_before_24th" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers' # к-сть молодіжних центрів
  ,'youth_councils' # к-сть молодіжних рад
)

predictor_vars_continuous_scaled <- c(
  "income_own_per_capita_k"           
  ,"income_total_per_capita_k"         
  ,"income_tranfert_per_capita_k"
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,"idp_registration_share" # more of an outcome
  ,"idp_real_share" # more of an outcome
  ,"idp_child_share" # more of an outcome
  ,'dfrr_executed_k'
  ,'passangers_2021'
  ,'business_support_centers'
  ,"travel_time"
  ,"sum_osbb_2020"
  ,"turnout_2020"
  ,"age_head"
  ,'square'
  ,"n_settlements"
  ,"urban_pct"
  ,"time_before_24th_years"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers' # к-сть молодіжних центрів
  ,'youth_councils' # к-сть молодіжних рад
)

# Continuous variables with filled by 0 NAs
predictor_vars_continuous_scaled_wo_na <- c(
  "income_own_per_capita_k"           
  ,"income_total_per_capita_k"         
  ,"income_tranfert_per_capita_k"
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  # ,"idp_registration_share" # more of an outcome
  # ,"idp_real_share" # more of an outcome
  # ,"idp_child_share" # more of an outcome
  ,'dfrr_executed_k_zeros'
  ,'passengers_2021_zeros'
  ,'business_support_centers'
  ,"travel_time"
  ,"sum_osbb_2020_zeros"
  ,"turnout_2020"
  ,"age_head"
  ,'square'
  ,"n_settlements"
  ,"urban_pct"
  ,"time_before_24th_years"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers' # к-сть молодіжних центрів
  ,'youth_councils' # к-сть молодіжних рад
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
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
)

predictor_vars <- c(
  predictor_vars_continuous_scaled_wo_na
  ,predictor_vars_categorical
)

ds1 <- 
    ds0 %>% 
      select(hromada_code,relocated_companies_text, all_of(predictor_vars), oblast_name_en, hromada_name
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
    ,dfrr_executed_k = dfrr_executed/1000
    ,relocated_companies_number = as.numeric(relocated_companies_text)
  )  %>% 
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k, 0)
    ,passengers_2021_zeros = replace_na(passangers_2021, 0)
    ,sum_osbb_2020_zeros = replace_na(sum_osbb_2020, 0)
  ) %>%
  mutate(country = "Ukraine") 

# --- modeling -----------------------------------------

d <- 
  ds1 %>% 
  pivot_longer(
    cols = predictor_vars_continuous_scaled_wo_na
    ,names_to = "item_name"
    ,values_to = "item_value"
  )

make_plot_prepvs <- function(
    d
    ,xvar    # "outcome"
    ,yvar    # "item_value"
    ,fillvar # "region_en"
)
{
  g <- 
    d %>% 
    mutate(
      item_name = factor(item_name, levels = predictor_vars_continuous_scaled_wo_na)
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
      title = paste0("Relationship between --- and other attributes of hromadas")
    )
  g
}  

# plotting
g <- 
  d %>% 
  mutate(country = "Ukraine") %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "relocated_companies_number"
    ,fillvar = "country"
  )  
g

# What hromada is outlier on total population?
outliers_total_population_2022 <- 
  ds1 %>%
  filter(total_population_2022 > 200000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_sum_osbb_2020 <- 
  ds1 %>%
  filter(sum_osbb_2020 > 190) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_passangers_2021 <- 
  ds1 %>%
  filter(passangers_2021 >60000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)

outliers <- 
  c(outliers_total_population_2022
    ,outliers_sum_osbb_2020
    ,outliers_passangers_2021) %>% 
  unique() 

g <- 
  d %>% 
  filter(hromada_code %ni% outliers) %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "relocated_companies_number"
    ,fillvar = "country"
  )  
g
