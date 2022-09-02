rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# for asserting conditions meet expected patterns.
requireNamespace("scales"   )# formatting

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
Sys.setlocale("LC_CTYPE", "ukr")
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/ua-pop-2022/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_pop22 <- "./data-private/raw/population-2022.xlsx"

#main admin dataset
ds_admin <- readr::read_csv("./data-private/derived/ua-admin-map.csv")

names_pop22 <- c(
  "object_name_ua"
  ,"Persons"
  ,"object_name_en"
)

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds0 <- readxl::read_xlsx(
  path_pop22
  , sheet = "12-47"
  , skip = 12
  , col_names = names_pop22
)
ds0 %>% glimpse()
ds0
# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------
ds_admin_merge <-
  ds_admin %>% 
  mutate(
    key = paste(oblast_name, raion_name, settlement_type, settlement_name.x)
  ) %>%
  select(key, hromada_name,hromada_code)

ds_pop_22_set <- 
  ds0 %>% 
  mutate(
    object_name_ua = str_replace_all(
      object_name_ua, c("'"="’", "\\s\\s"=" ","Короcтишів"="Коростишів", "\\s+\\(.+\\)"=""))
    ,oblast = str_extract(object_name_ua, "(.+(?=\\sобласть))|(м\\.\\sКиїв)")
    ,raion = str_extract(object_name_ua, "(.+(?=\\sрайон))|(м\\.\\sКиїв)")
    ,settlement_type = case_when(
      str_detect(object_name_ua, "м\\.") ~ "місто"
      ,str_detect(object_name_ua, "смт") ~ "селище міського типу"
    )
    ,settlement_name = str_extract(object_name_ua, "(?<=м\\.\\s).+|(?<=смт\\s).+")
  ) %>% 
  fill(oblast, raion) %>% 
  filter(
    !str_detect(object_name_ua, "(.+\\sрайон)|(.+\\sобласть)|(.+\\sнаселення)|Севастополь")
    ,is.na(settlement_type) == F
  ) %>%
  mutate(
    key = paste(oblast, raion, settlement_type, settlement_name)
  ) %>%  
  left_join(
    ds_admin_merge
    ,by = "key"
  ) %>%
  filter(is.na(hromada_name) == F)


#clean dataset is 
ds_pop_22_set %>% glimpse()
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/ua-pop-2022/ua-pop-2022.Rmd" # connect with Rmd for publishing
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
