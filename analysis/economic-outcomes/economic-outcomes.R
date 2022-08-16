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
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels 
library(dplyr)     # data wrangling 
library(tidyr)     # tidy data
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/economic-outcomes/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_economics    <- "./data-private/derived/economics.csv"    # product of ./manipulation/ellis-economics.R
path_admin        <- "./data-private/derived/ua-admin-map.rds" # product of ./manipulation/ellis-ua-admin.R
path_time         <- "./data-private/derived/time_rada.csv"    # product of ./manipulation/ellis-rada-hromada.R

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds_economics    <- readr::read_csv(path_economics)
ds_admin        <- readr::read_rds(path_admin)
ds_time         <- readr::read_csv(path_time)
# ---- inspect-data ------------------------------------------------------------

ds_economics %>% glimpse()  # available economic indicators
ds_time      %>% glimpse()  # date when a rada joined a hromada
ds_admin      %>% glimpse() # map of codes and labels settlement-hromada-raion-oblast
# ---- tweak-data --------------------------------------------------------------

# ---- table-1 -----------------------------------------------------------------

# ---- graph-1 -----------------------------------------------------------------

# ---- graph-2 -----------------------------------------------------------------

# ---- graph-3 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/economic-outcomes/economic-outcomes.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
