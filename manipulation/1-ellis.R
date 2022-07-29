#' ---
#' title: "1-ellis"
#' author: "First Last"
#' date: "last Updated: `r Sys.Date()`"
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
cat("\014") # Clear the console
#+ echo=FALSE, results="asis" --------------------------------------------------
cat("Report's native working directory: `", getwd(),"`") # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
#+ load-sources ----------------------------------------------------------------
source("./scripts/common-functions.R")# functions sourced throughout the project

#+ load-packages ---------------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

#+ declare-globals -------------------------------------------------------------
# figures will be printed into the following folder:
(prints_folder <- paste0("./manipulation/0-ellis-prints/"))
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))} # to make sure folder exists

#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks--------------------------------------------------
ds0 <- readr::read_rds(path_data_input)

#+ tweak-data, eval=eval_chunks-------------------------------------------------

#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 3.Exploration ")


#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 9.Data Export")

#+ save-to-disk, eval=eval_chunks ----------------------------------------------
# naming convention: step_id - lane_name - version_date
version_date <- "YYYY-MM-DD"
path_save_dto       <- paste0("./data-private/derived/1-ellis-",version_date,".rds")
path_save_dto_small <- paste0("./data-private/derived/1-ellis-",version_date,"-small.rds")
# small version is useful during development and testing
# dto       %>% readr::write_rds(path_save_dto,      compress = "xz")
# dto_small %>% readr::write_rds(path_save_dto_small,compress = "xz")

#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# A. Session Information{#session-info}")
#+ echo=F, results="show" ------------------------------------------------------
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)

