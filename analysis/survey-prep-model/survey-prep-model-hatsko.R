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


ds0 <- 
  ds_survey %>% 
  mutate(
    idp_help_count              = rowSums(across(all_of(idp_help), na.rm = T)), # idp_help_count fix
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

table(ds0$bussiness_stimules_none)

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

#+ ----- vectors with predictors and outcomes ----------------------------------

outcomes_vars <- c(
  # "prep_score_feb"
  # ,"prep_score_oct"
  # ,"prep_score_combo"
  "idp_registration_number" # внутрішньо переміщені особи - кількість ВПО
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_number" # corrected index above 
  ,"idp_real_share" # corrected index above 
  ,"idp_child_education" # кількість ВПО дітей
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'relocated_companies_text' # к-сть релокованих компаній
  ,'international_projects' # к-сть проєктів з міжнародними донорами
  ,'finance_school_shelters_coded'
  ,'no_school_days_coded'
  ,'hromada_exp'
  ,'dftg_creation'
  ,'dftg_creation_date'
  ,'head_hromada_communication'
)

outcomes_vars_new <-  c(
  "idp_registration_number" # внутрішньо переміщені особи - кількість ВПО
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_number" # corrected index above 
  ,"idp_real_share" # corrected index above 
  ,"idp_child_education" # кількість ВПО дітей
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'relocated_companies_number' # к-сть релокованих компаній
  ,'international_projects_number' # к-сть проєктів з міжнародними донорами
  ,'finance_school_shelters_coded'
  ,'no_school_days_number'
  ,'hromada_exp_b'
  ,'head_hromada_communication_numeric'
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
  ,"age_head" # вік голови громади
  ,"time_before_24th" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers'
  ,'youth_councils'
  
)

predictor_vars_continuous_scaled <- c(
  "income_own_per_capita_k"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita_k"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita_k" # що надходить від держави, 
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,'passangers_2021'
  # ,'business_support_centers' # кількість центрів
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
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  # ,'youth_centers'
  # ,'youth_councils'
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
  # ,'business_support_centers'
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
  # ,'youth_centers' # к-сть молодіжних центрів
  # ,'youth_councils' # к-сть молодіжних рад
)

# Categorical - for color
predictor_vars_categorical <- c(
  "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
  ,'incumbent'
  # ,'rda' # too small variation
  ,'not_from_here'
  # ,'enterpreuner' # too small variation
  # ,'unemployed' # too small variation
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_27_04_2022'
  ,'train_station'
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  # ,'city'
  # ,'youth_centers_b' # наявність молодіжних центрів
  # ,'youth_councils_b' # наявність молодіжних рад
  # ,'business_support_centers_b' # наявність центру підтримки бізнесу
  )

predictor_vars_categorical_new <- c(
  "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
  ,'incumbent'
  # ,'rda' # too small variation
  ,'not_from_here'
  # ,'enterpreuner' # too small variation
  # ,'unemployed' # too small variation
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_27_04_2022'
  ,'train_station'
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  ,'city'
  ,'youth_centers_b' # наявність молодіжних центрів
  ,'youth_councils_b' # наявність молодіжних рад
  ,'business_support_centers_b' # наявність центру підтримки бізнесу
)

predictor_vars <- c(
  predictor_vars_continuous
  ,predictor_vars_categorical
)


#+ - tweak-data-2 --------------------------------------------------------------

ds2_prep <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score")) %>% 
  left_join(
    ds0 %>% 
      select(hromada_code,all_of(predictor_vars), oblast_name_en, hromada_name)
  ) %>% 
  # scaling 
  mutate(
    income_own_per_capita_k = income_own_per_capita/1000
    ,income_total_per_capita_k = income_total_per_capita/1000
    ,income_tranfert_per_capita_k = income_tranfert_per_capita/1000
    ,time_before_24th_years = time_before_24th/365
    ,dfrr_executed_k = dfrr_executed/1000
    ,urban_perc_100 = urban_pct * 100
  )  %>% 
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k, 0)
    ,passengers_2021_zeros = replace_na(passangers_2021, 0)
    ,sum_osbb_2020_zeros = replace_na(sum_osbb_2020, 0)
  ) %>%
  # making binary vars where not enough variation
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
    ,youth_centers_b = ifelse(youth_centers == 0, 0, 1)
    ,youth_councils_b =ifelse(youth_councils == 0, 0, 1)
    ,city = factor(ifelse(type == 'міська', 1, 0))
    ) %>%
  mutate(
    across(
      .cols = all_of(predictor_vars_categorical_new)
      ,.fns = ~factor(.)
    )
  ) %>% 
  mutate(country = "Ukraine")

ds1 <- 
  ds0 %>% 
  select(hromada_code, all_of(predictor_vars), oblast_name_en, hromada_name, 
         all_of(outcomes_vars)
  ) %>% 
  # scaling 
  mutate(
    income_own_per_capita_k = income_own_per_capita/1000
    ,income_total_per_capita_k = income_total_per_capita/1000
    ,income_tranfert_per_capita_k = income_tranfert_per_capita/1000
    ,time_before_24th_years = time_before_24th/365
    ,dfrr_executed_k = dfrr_executed/1000
    ,relocated_companies_number = as.numeric(relocated_companies_text)
    ,international_projects_number = as.numeric(international_projects)
    ,no_school_days_number = as.numeric(no_school_days_coded)
    ,urban_perc_100 = urban_pct * 100
    ,idp_reg_number_log = log(idp_registration_number)
    ,dftg_creation_time = as.numeric(floor(difftime(dftg_creation_date, "2021-12-29", unit = "day")))
    ,dftg_creation_time_na = ifelse(dftg_creation_time < 0, NA_integer_, dftg_creation_time)
    ,head_hromada_communication_numeric = fct_recode(head_hromada_communication,
                                                       "0"  = "none"   
                                                     , "1"  = "once_a_week"    
                                                     , "2"  = "few_times_a_week"  
                                                     , "3"  = "once_a_day"   
                                                     , "4" = "2_3_times"
                                                     ) %>% as.character() %>% as.integer()
  )  %>% 
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k, 0)
    ,passengers_2021_zeros = replace_na(passangers_2021, 0)
    ,sum_osbb_2020_zeros = replace_na(sum_osbb_2020, 0)
  ) %>%
  # making binary vars where not enough variation
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
    ,youth_centers_b = ifelse(youth_centers == 0, 0, 1)
    ,youth_councils_b =ifelse(youth_councils == 0, 0, 1)
    ,city = factor(ifelse(type == 'міська', 1, 0))
    ,hromada_exp_b = ifelse(hromada_exp == 'yes', 1, 0)
    ,dftg_creation_b = ifelse(dftg_creation == 'yes', 1, 0)
  ) %>%
  mutate(
    across(
      .cols = all_of(predictor_vars_categorical_new)
      ,.fns = ~factor(.)
    )
  ) %>% 
  mutate(country = "Ukraine") 


ds1 %>% select(all_of(outcomes_vars_new)) %>% explore::describe_all() %>%neat_DT()

ds1 %>% select(all_of(outcomes_vars_new)) %>% GGally::ggpairs()
hist(ds1$no_school_days_coded)
ds1 %>% sapply(outcomes_vars_new, plot)

par(mfrow = c(4, 3))

lapply(ds1[outcomes_vars_new], FUN=hist)

ggplot(reshape2::melt(ds1[outcomes_vars_new]),aes(x=value)) + geom_histogram() + facet_wrap(~variable, scales = 'free')
#+ - plot-linear-models-1 ------------------------------------------------------

d <- 
  ds2_prep %>% 
  pivot_longer(
    cols = predictor_vars_continuous_scaled_wo_na
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
      title = paste0("Relationship between Invasion Preparedness Score BEFORE invasion (horizontal) and other attributes of hromadas")
    )
  g
}  

# To see how it works
g <- 
  d %>% 
  mutate(country = "Ukraine") %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "prep_score_feb"
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
    ,yvar    = "prep_score_feb"
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


#+ ---- one-model --------------------------------------------------------------

fit1_norm <- 
  glm(
    formula = head_hromada_communication_numeric ~ no_party
    ,data = ds1
    ,family = "gaussian"
  )

summary(fit1_norm)

fit1_poisson <- 
  glm(
    formula = prep_score_feb ~ war_zone_27_04_2022
    ,data = ds2_prep
    ,family = "poisson"
  )

summary(fit1_poisson)

fit1_nbinom <- 
  MASS::glm.nb(
    formula = prep_score_feb ~ region_en
    ,data = ds2_prep
    )

summary(fit1_nbinom)

##

1 - pchisq(summary(fit1_nbinom)$deviance,
           summary(fit1_nbinom)$df.residual
)

# gaussian

fit1_logn <- 
  glm(
    formula = dftg_creation_b ~ train_station
    ,data = ds1
    ,family = "binomial"
  )

summary(fit1_logn)

# logistic

fit1_logit <- 
  glm(
    formula = idp_reg_number_log ~ no_party
    ,data = ds1
    ,family = "binomial"
  )

summary(fit1_logit)

# for zero-inflated model
fit1_zi_pois <- pscl::zeroinfl(formula = prep_score_feb ~ war_zone_27_04_2022 | 1
               ,data = ds2_prep
               ,dist = "pois")

summary(fit1_zi_pois)

fit1_zi_nbinom <- pscl::zeroinfl(formula = prep_score_feb ~ war_zone_27_04_2022 | 1
                                 ,data = ds2_prep
                                 ,dist = "negbin")

summary(fit1_zi_nbinom)


fit1_poisson %>% broom::glance() # model properties
fit1_nbinom %>% broom::glance() # model properties
fit1_logn %>% broom::glance() # model properties

fit1_poisson %>% broom::tidy() # coefficients

summary(fit1_poisson)
summary(fit1_nbinom)
summary(fit1_zi_pois)
summary(fit1_zi_nbinom)

lmtest::lrtest(fit1_zi_pois, fit1_zi_nbinom)

#
E2 <- resid(fit1_zi_nbinom, type = "pearson")
N  <- nrow(ds2_prep)
p  <- length(coef(fit1_zi_nbinom))  
sum(E2^2) / (N - p)

#

performance::check_overdispersion(fit1_poisson)

hist(ds1$international_projects_number)

x <- ds1 %>% select(head_hromada_communication_numeric) %>% filter(!is.na(head_hromada_communication_numeric)) %>% pull()

fitur::fit_dist_addin()

fitdistrplus::descdist(x, discrete = TRUE)
normal_dist <- fitdistrplus::fitdist(x, distr = "pois")
plot(normal_dist)
normal_dist <- fitdistrplus::fitdist(x, distr = "nbinom")
plot(normal_dist)

###

pois_data <-x
lambda_est <- mean(pois_data)

p0_tilde <- exp(-lambda_est)
p0_tilde
n0 <- sum(1*(!(x >0)))
n <- length(x)

# number of observtions 'expected' to be zero
n*p0_tilde

#now lets perform the JVDB score test 
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)

test_stat <- numerator/denominator

pvalue <- pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
pvalue 

###

fit2_poisson <- 
  glm(
    formula = idp_registration_number ~ urban_pct + sum_osbb_2020_zeros
    ,data = ds1
    ,family = "poisson"
  )

fit3_poisson <- 
  glm(
    formula = idp_registration_number ~ sum_osbb_2020_zeros + region_en + sum_osbb_2020_zeros*region_en
    ,data = ds1
    ,family = "poisson"
  )



fit2_poisson %>% broom::tidy() # coefficients
fit3_poisson %>% broom::tidy() # coefficients

summary(fit2_poisson)

jtools::plot_summs(fit1_poisson, fit2_poisson) # plot coeficients

# Resources for handling modeling objects
# jtools vignette - https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# broom vignette - https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# GGally - https://ggobi.github.io/ggally/


vars_density <- c( 
  "prep_score_feb"
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



# ----- fit4 -------------------------------------------------------------------

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

g <- 
  fit4_full %>% 
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
