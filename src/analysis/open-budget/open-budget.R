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
# base::source("./analysis/open-budget/custom-model-functions.R") # plots
# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/open-budget/prints")
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


ls_fin <- readr::read_rds("./data-private/derived/ellis-budget-alt.rds")
ds_budget <- ls_fin$budget$Data
ds_budget_meta <- ls_fin$budget$Metadata
ds_taxes <- ls_fin$taxes$Data
ds_taxes_meta <- ls_fin$taxes$Metadata

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


path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)
ds_admin_full %>% glimpse(70)



# ---- inspect-data ------------------------------------------------------------
ds_general %>% glimpse(80)
ds_survey %>% glimpse(80)

ds_budget %>% glimpse(80)
ds_taxes %>% glimpse(80)

# ---- tweak-data-0 ----------------------

ds_budget %>% glimpse()

ds0 <- 
  ds_budget %>% 
  mutate(
    survey_response = case_when(
      hromada_code %in% (ds_survey %>% pull(hromada_code) %>% unique()) ~ TRUE
      ,TRUE ~ FALSE
    )
  ) %>% 
  rowwise() %>% 
  mutate(
    income_other_2021 = income_own_2021 - 
      sum(c_across(income_military_2021:income_excise_duty_2021), na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(
    hromada_code
    ,any_of(names(income_vars))
  ) 

ds0 %>% glimpse()


# ---- variable-sets ----------------------------------------------------------

hromada_stem <- ds0 %>% select(budget_code:region_code_en) %>% names()
budget_vars <- ds0 %>% select(income_total:total_income_change) %>% names()
income_raw <- c(
  "income_total"           
  ,"income_transfert"       
  ,"income_military"        
  ,"income_pdfo"            
  ,"income_unified_tax"     
  ,"income_property_tax"    
  ,"income_excise_duty"     
  ,"income_own"    
)

income_prop <- c(
   "own_income_prop"           
  ,"transfert_prop"       
  ,"military_tax_prop"        
  ,"pdfo_prop"            
  ,"unified_tax_prop"     
  ,"property_tax_prop"    
  ,"excise_duty_prop"     
)


# ---- bird-eye-view -------------------------
#   filter(!hromada_code %in% looks_fishy) %>%  d %>% 
# looks_fishy <- 
#   ds0 %>% 
#   filter(own_income_prop > 1 | own_income_prop < 0) %>% # how can they be outside of this window?
#   # select(hromada_stem) %>%
#   pull(hromada_code)
# pivot_vars <- c(income_raw,income_prop,budget_vars) %>% unique()
# 
# 


d <- 
  ds0 %>% 

looks_fishy <- 
  ds0 %>% 
  filter(own_income_prop > 1 | own_income_prop < 0) %>% # how can they be outside of this window?
  # select(hromada_stem) %>%
  pull(hromada_code)
pivot_vars <- c(income_raw,income_prop,budget_vars) %>% unique()




d <- 
  ds0 %>% 

  {
    ggplot(.,aes(x=date, y = value, group = hromada_code))+
      geom_line(alpha = .2)+
      scale_y_continuous(labels = scales::comma_format())+
      facet_wrap(facets = "metric",scales= "free_y")
  }

g %>% quick_save("1-bird-eye-view-without-fishy",w=16, h=9)

# ---- income-covar ---------------

ds_general %>% glimpse()

d1 <- 
  ds_general %>% 
  # filter(!hromada_code %in% looks_fishy) %>% 
  filter(!hromada_code %in% (ds_general %>% filter(income_total_2021>300000) %>% pull(hromada_code))) %>% 
  select(income_total_2021:income_own_2021)
d1_log <-
  d1 %>% 
  mutate(
    across(
      .cols = everything()
      ,.fns = ~log(.)
    )
  ) 

d1 %>% GGally::ggpairs()

# ---- view-one-outcome ----------------------------

d <- 
  ds0 %>% 
  filter(!hromada_code %in% looks_fishy) %>% 
  left_join(
    ds_general %>% select(hromada_code,voluntary)
  ) %>% 
  left_join(
    ds_admin_full %>% distinct(hromada_code,map_position, map_position2)
  ) %>% 
  mutate(
    voluntary = as.logical(voluntary)
    # voluntary = factor(as.logical(voluntary)) %>% fct_rev()
    ,region_en = factor(region_en)
    ,oblast_name_en = factor(oblast_name_en) %>% fct_reorder(map_position)
  ) 
d %>% glimpse()
g <-
  d %>% 
  {
    ggplot(.,aes(x=date, y = own_income_prop, color = region_en))+
 
     geom_line( data = . %>% filter(!voluntary),alpha = .2 ,aes( group = hromada_code))+
      geom_line(data = . %>% filter(voluntary),alpha=.1,aes( group = hromada_code))+
      geom_smooth(linewidth=.6, method="loess", se=F,data = . %>% filter(!voluntary))+
      geom_smooth(linewidth=.8, method="loess", se=F,data = . %>% filter(voluntary), linetype="dashed")+
      # geom_line( data = . %>% filter(!survey_response),alpha = .1 )+
      # geom_line(aes(color = voluntary), data = . %>% filter(survey_response))+
      scale_y_continuous(labels = scales::percent_format(trim=T),
                         breaks = seq(.25,.75,.25),minor_breaks = seq(0,1,.1))+
      # scale_y_continuous(labels = scales::comma_format())+
      # scale_color_viridis_d(
      #     begin = 0, end = .8, direction = 1
      #     ,option = "plasma",guide= guide_legend(reverse=T)
      #   )+
      scale_color_brewer(type="qual",palette = "Dark2")+
      facet_wrap(facets = "oblast_name_en",scales= "free_y")+
      guides(color = guide_legend(override.aes = list(linewidth = 5) ) )+
      labs(
        title = "Share of own income"
        # ,subtitle = "Non-Voluntary Hromadas"
        ,caption = "Loess smoother for VOLUNTARY hromadas shown with dashed line"
        ,color = ""
      )
  }
g %>% quick_save("2-one-outcome",w=16, h=9)






  









