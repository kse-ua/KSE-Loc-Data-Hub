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

support_vars <- c(
  "hromada_name"
)

# Outcomes

outcomes_vars <- c(
  "prep_score_feb"
  ,"prep_score_oct"
  ,"prep_score_combo"
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
  ,'edem_petitions'
  ,'edem_consultations'
  ,'edem_participatory_budget'
  ,'edem_open_hromada'
  ,'war_zone_27_04_2022'
  ,'train_station'
  ,'youth_centers'
  ,'youth_councils'
)
predictor_vars <- c(
  predictor_vars_continuous
  ,predictor_vars_categorical
)


# Continuous - good for spreading out # Valentyn, please add relevant predictors here
predictor_vars_continuous <- c(
  "income_own_per_capita"           
  ,"income_total_per_capita"         
  ,"income_tranfert_per_capita"      
  ,"idp_registration_share"
  ,"idp_real_share"
  ,"idp_child_share"
  ,'dfrr_executed'
  ,'edem_total'
  ,'passangers_2021'
  ,'business_support_centers'
  ,"n_settlements"
  ,"travel_time"
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020"
  ,"turnout_2020"
  ,'square'
  ,"age_head"
  ,"time_before_24th"
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
    ""
  )  


ds2_prep %>% glimpse(90)
ds2_prep %>% select(predictor_vars_categorical) %>% look_for()

# ---- -------------

d <- 
  ds2_prep %>% 
  pivot_longer(
    cols = predictor_vars_continuous
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
      item_name = factor(item_name, levels = predictor_vars_continuous)
    ) %>% 
    ggplot(aes(
      x     = !!rlang::sym(xvar)
      ,y    = !!rlang::sym(yvar)
      ,fill = !!rlang::sym(fillvar)
      ,color = !!rlang::sym(fillvar)
    ))+
    ggplot2::scale_fill_viridis_d(
      begin = 0, end = .8, direction = -1
      ,option = "plasma",guide= guide_legend(reverse=T)
    )+
    ggplot2::scale_color_viridis_d(
      begin = 0, end = .8, direction = -1
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
    ,yvar    = "prep_score_feb"
    ,fillvar = "sex_head"
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
    ,fillvar = "voluntary"
  )  
g %>% quick_save("tester2",w=16,h=9)

g <- 
  d %>% 
  filter(hromada_code %in% outliers) %>% 
  make_plot_prepvs(
    xvar     = "item_value"
    ,yvar    = "prep_score_feb"
    ,fillvar = "voluntary"
  )  
g %>% quick_save("tester3",w=16,h=9)


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






# ---- modeling-demonstration -----------------------
d_model <- 
  ds2_prep %>% 
  select(
    prep_score_oct, age_head, sex_head, incumbent, income_own_per_capita, idp_child_share , travel_time
  )

d_model %>% GGally::ggpairs()

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
