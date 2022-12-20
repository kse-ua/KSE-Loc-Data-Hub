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
# import::from("magrittr", "%>%")
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
# library(gt)
library(stargazer)

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


make_corr_matrix <- function(d,metaData=d_meta,item_names,display_var="label_en", method="pearson"){
  # browser()
  # d <- ds0
  # metaData <- d_meta
  # item_names <- (d_meta %>% pull(item_name) %>% as.character() )[1:3]
  # add_short_label <- TRUE
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select(all_of(item_names))
  d2 <- d1[complete.cases(d1),]
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = !!rlang::sym(display_var))
  # rownames <- rownames[,"display_name"]
  # rownames <- rownames %>% as.list() %>% unlist() %>% as.character()
  rownames <- rownames %>% pull(display_name)
  d3 <- sapply(d2, as.numeric)
  # d3 %>% glimpse()
  cormat <- cor(d3,method = method)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
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

# the product of ./manipulation/ellis-agro-survey.R
ds_survey <- readr::read_csv("./data-private/derived/agro-survey-full.csv")

#original survey dataset with variables and question types
meta_survey <- readxl::read_excel("./data-private/raw/agro-survey.xlsx", sheet = "variables")


# meta_oblast <- googlesheets4::read_sheet(sheet_name,"choices",skip = 0)

# Originally, we pulled the meta data object from Kobo front end and stored to 
# survey_xls  <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
# # we put this on google drive now, to control manually
# googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# # https://googlesheets4.tidyverse.org/ 
# # https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
# survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
# meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
# meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)



# ---- inspect-data ------------------------------------------------------------
ds_survey %>% glimpse()

ds_survey %>% pull(hromada_code) %>% unique()

# ---- variable-groups -----------------------------------------------------------
# create supporting objects for convenient reference of variable groups

# multiple choice questions
# mcq <-
#   meta_survey%>%
#   dplyr::select(type,name)%>%
#   dplyr::filter(str_detect(type, "select_multiple"))%>%
#   dplyr::select(name)%>%
#   pull() %>%  
#   print()

#vectors of mcq names
budget_cut_spheres <- 
  ds_survey %>% 
  select(starts_with("budget_cut_sphere"), -ends_with("text"), -budget_cut_sphere, -budget_cut_sphere_count) %>% 
  colnames() %>% 
  print()

budget_cut_types <- 
  ds_survey %>% 
  select(starts_with("budget_cut_type"), , -ends_with("text"), -budget_cut_type, -budget_cut_type_count) %>% 
  colnames() %>% 
  print()

aid_ingo <- 
  ds_survey %>%
  select(starts_with('ingo_'), -ends_with('text'), -ingo, -ingo_count) %>% 
  colnames() %>% 
  print()

aid_countries <- 
  ds_survey %>% 
  select(starts_with('countries'), -ends_with('text'), -countries, -countries_count) %>% 
  colnames() %>% 
  print()


# vector of income variables 
income <- 
  ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()

# ---- meta-data-1 -------------------------------------------------------------
# meta_survey %>% glimpse()
# 
# meta_survey %>% 
#   filter(type %in% c("begin_group","end_group")) %>% 
#   select(1:5) %>% 
#   print_all()
# 
# 
# meta_survey %>% glimpse()

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
    income_own_per_capita       = income_own_2021         / total_population_2022
    ,income_total_per_capita     = income_total_2021       / total_population_2022
    ,income_tranfert_per_capita  = income_transfert_2021   / total_population_2022
    ,idp_share                   = idp_number / total_population_2022
    ,idp_number = ifelse(idp_number == 0, NA, idp_number)
    ,log_income_total_2021 = log(income_total_2021)
    ,log_income_own_2021= log(income_own_2021)
    ,income_total_per_capita = income_total_2021/total_population_2022
    ,income_own_per_capita= income_own_2021/total_population_2022
    ,income_own_pct = income_own_2021/income_total_2021
    ,military_action = factor(military_action, labels = c("No", "Yes, Feb-March", "Yes, currently"))
    ,occupation = factor(occupation, labels = c("No", "Yes, was occupied", "Yes, currently occupied"))
  ) 


d_volunteers <- ds0 %>% 
  mutate(
    volunteers_per_capita = volunteers_number/total_population_2022 * 100
  ) %>% 
  filter(hromada_name %ni% c("Боярська міська громада", "Городищенська сільська громада")) 


d_garbage <- ds0 %>% 
  mutate(
    garbage_interruptions = case_when(
      garbage_interruptions == "Важко сказати" ~ NA_integer_
      ,garbage_interruptions == "Так" ~ 1L
      ,garbage_interruptions == "Ні"~ 0L)
  )


# ---- inspect-data-0 ------------------------------------------------------------

# ---- tweak-data-1 ------------------------------------------------------------
# compute total binary score (preparations are made at all, regardless of timing)



# ----- inspect-data-1 -----------------------



# ---- save-to-disk ------------------------------------------------------------


# ---- publish ------------------------------------------------------------
path <- "./analysis/survey-overview/agro_survey-overview.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)