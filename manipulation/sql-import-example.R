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
# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- load-packages -----------------------------------------------------------
# Prefer to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("DBI"         ) # database
requireNamespace("odbc"        ) # database
requireNamespace("OuhscMunge"  ) # remotes::install_github("OuhscBbmc/OuhscMunge")
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("readr"    )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("forcats"  )# factors
requireNamespace("stringr"  )# strings
requireNamespace("lubridate")# dates

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.

# ---- declare-functions -------------------------------------------------------

# ---- load-data, eval=eval_chunks ---------------------------------------------------------------
# config      <- config::get()
# See ODBC setup guide developed by BBMC of OUHSC
# https://github.com/OuhscBbmc/BbmcResources/blob/master/instructions/odbc-dsn.md
# Set up the DSN connection using ODBC tool as described in the guide above
dsn <- "CONNECTION_NAME_DSN" # one per database, typically in config file
# SQL that will extract the table(s)
sql <- "
  SELECT TOP 10
    *
  FROM [CAO_PROD].[dbo].[TEAM_TABLE]
"
cnn <- DBI::dbConnect(odbc::odbc(),dsn=dsn) # open the connection
ds0 <- DBI::dbGetQuery(cnn, sql) # read the data
DBI::dbDisconnect(cnn) # hang up the phone
rm(dsn, cnn, sql) # clean up
# ---- inspect-data ------------------------------------------------------------
ds0 %>% glimpse()


# ---- tweak-data, eval=eval_chunks --------------------------------------------------------------
ds1 <- ds0 %>% janitor::clean_names()

# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk, eval=eval_chunks ------------------------------------------------------------
ds1 %>% readr::write_rds("./data-private/derived/0-import.rds",compress = "xz")
# ---- publish ------------------------------------------------------------
# path <- "./analysis/.../report-isolated.Rmd"
# rmarkdown::render(
#   input = path ,
#   output_format=c(
#     "html_document"
#     # "word_document"
#     # "pdf_document"
#   ),
#   clean=TRUE
# )



