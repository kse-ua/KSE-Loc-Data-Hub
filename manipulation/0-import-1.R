rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. 
#This is not called by knitr, because it's above the first chunk.
cat("/014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default, unless overwritten

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")# functions sourced throughout the project

# ---- load-packages -----------------------------------------------------------
# disable tho
# core packages - turn ON/OFF to help overview the scope of the script
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(readxl)    # data import
library(explore)   # for `describe_all()` 
library(scales)    # formatting
library(labelled)  # labels - https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
library(rlang)     # tidy evaluations -  https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/   
# 
# ---- declare-globals ---------------------------------------------------------
# ensure that the `prints` sub-folder exists (!). One folder per script.
# folder name MUST match the name of the R script
prints_folder <- paste0("./manipulation/prints/0-import-1/") 
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}


# define URLs here, so that they can be recycled
# Note FORWARD SLASHES(!!), copy-pasting from address line WILL need adjustment
path_file1  <- "M:/Folder1/Folder2/Data.xlsx"
path_file2  <- "M:/Folder1/Folder2/Data-group-1.csv"
# path_files should be the last element of the chunk
# ---- declare-functions -------------------------------------------------------
# store script-specific function here

# ---- load-data ---------------------------------------------------------------
# import data from every sheet of a `*.xlsx` file
sheet_names <- readxl::excel_sheets(path_file1) # to cycle through
dto <- list() # empty shell to hold elements
for(sheet_i in sheet_names){
  # i <- sheet_names[1]
  dto[[sheet_i]] <- readxl::read_xlsx(path_file, sheet = sheet_i)
}
# to isolate a given imported sheet in a tibble
ds0 <- dto$`Sheet Name` %>% tibble::as_tibble()

dto <- list() # to store data sets 
dto[["dataset_name1"]] <- readxl::read_xlsx(path_file1, sheet = "Sheet Name 1")
dto[["dataset_name2"]] <- readr::read_csv(path_file2, trim_ws = T)
dto

# minimize (if needed) and store a local copy for quick assess and development
# dto %>% readr::write_rds("./data-unshared/derived/00-import-1-small.rds",compress = "xz")


# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
# naming convention: step_id - step_name - cohort_id
path_data_out <- "./data-unshared/derived/.../0-import-1.rds"
# ---- publish -----------------------------------------------------------------
# naming convention: step_id - data_transfer_object - cohort_id
# one report (.Rmd) per script (.R), unless report series
path_report_out <- "./manipulation/reports/0-import-1/0-import-1.Rmd"
rmarkdown::render(
  input = path_report_out ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
