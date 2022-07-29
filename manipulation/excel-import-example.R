#' ---
#' title: "0-import"
#' author: "First Last"
#' date: "YYYY-MM-DD"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/1-ellis.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("/014") # Clear the console
#+ echo=FALSE, results="show" --------------------------------------------------
cat("Working directory: ", getwd()) # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

#+ load-packages -----------------------------------------------------------
# Prefer to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("readxl"   )# data import/export
requireNamespace("readr"    )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("forcats"  )# factors
requireNamespace("stringr"  )# strings
requireNamespace("lubridate")# dates

#+ declare-globals ---------------------------------------------------------
# printed figures will go here:
# prints_folder <- paste0("./analysis/.../prints/")
# if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}


# define URLs here, so that they can be recycled
# Note FORWARD SLASHES(!!), copy-pasting from address line WILL need adjustment
path_file1  <- "M:/Folder1/Folder2/Data.xlsx"
path_file2  <- "M:/Folder1/Folder2/Data-group-1.csv"
# path_files should be the last element of the chunk
#+ declare-functions -------------------------------------------------------
# store script-specific function here

#+ load-data ---------------------------------------------------------------
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
# dto %>% readr::write_rds("./data-private/derived/00-import-1-small.rds",compress = "xz")


#+ inspect-data ------------------------------------------------------------


#+ tweak-data --------------------------------------------------------------


#+ table-1 -----------------------------------------------------------------


#+ graph-1 -----------------------------------------------------------------


#+ graph-2 -----------------------------------------------------------------

#+ save-to-disk ------------------------------------------------------------
# naming convention: step_id - step_name - cohort_id
dto %>% readr::write_rds("./data-private/derived/0-import.rds",compress = "xz")

#+ results="asis", echo=echo_chunks
cat("\n# A. Session Information{#session-info}")
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)


