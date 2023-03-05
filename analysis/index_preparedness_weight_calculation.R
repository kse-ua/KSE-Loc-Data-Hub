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
rescale <- function(x) (1 / 9 * (x - 10) + 1) # rescale from 1-10 to 0-1
# ---- load-data ---------------------------------------------------------------
ds_index <- readr::read_csv("./data-private/raw/index_preparedness_weight.csv")

# ---- calculating-weights ------------------------------------------------------------

vars <- c("time", "name", "water_stored", "fuel_stored", "response_plan", "evacuation_plan", 
          "plan_coord_oth_hs", "plan_coord_oblast", "vdf", "resistance_plan",
          "meet_heads", "meet_utilities", "shelter_map_online", "shelter_list_online",
          "comm_test", "data_backup")

colnames(ds_index) <- vars

ds_index <- ds_index %>% na.omit()

ds_index <- ds_index %>%
  mutate(group = case_when(name %in% c("adarkovich@kse.org.ua", "stytiuk@kse.org.ua", 
                                       "marinarabi93@gmail.com", "valgat29@gmail.com",
                                       "saviskom@gmail.com") ~ "non-experts",
                           .default = 'experts'),
         weight = case_when(group == "non-experts" ~ .2,
                            .default = 1))

index_means <- ds_index %>% 
  summarise(across(.cols = water_stored:data_backup,
                   .fns = ~weighted.mean(., w=weight)
                   )
            ) %>% t() %>% as_tibble()

ds_fin <- index_means %>% 
  mutate(name = vars[3:16]
         ,v1_norm = rescale(V1)
         ,share = V1 / sum(V1)
         ,new_prop = 100 * share) %>%
  arrange(desc(v1_norm))

ds_index %>% 
  summarise(across(.cols = water_stored:data_backup,
                   .fns = ~round(mean(.),2)
                   )
            ,.by = "group"
            ) %>% 
  t() %>% 
  as_tibble() %>%
  janitor::row_to_names(row_number = 1) %>%
  mutate_all(.,as.numeric) %>%
  mutate(name = vars[3:16],
         diff = round(.$'non-experts' - .$"experts", 2)) %>%
  arrange(diff)
# ---- save-data ------------------------------------------------------------
readr::write_csv(ds_fin, "./data-private/derived/index_preparedness_weights.csv")


