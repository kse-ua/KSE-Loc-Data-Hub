# knitr::stitch_rmd(script="manipulation/mlm-scribe.R", output="stitched-output/manipulation/mlm-scribe.md")
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# source("manipulation/osdh/ellis/common-ellis.R")
# base::source(file="dal/osdh/arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
import::from("magrittr", "%>%")
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("tibble")
requireNamespace("readr"                      )  # remotes::install_github("tidyverse/readr")
library("dplyr"                      )
requireNamespace("checkmate"                  )
requireNamespace("testit"                     )
requireNamespace("config"                     )
requireNamespace("babynames"                  )
requireNamespace("TeachingDemos"              )
requireNamespace("OuhscMunge"                 )  # remotes::install_github("OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
# config      <- config::get()
# https://github.com/OuhscBbmc/BbmcResources/blob/master/instructions/odbc-dsn.md
dsn                   <- "EDM1_old_driver" # one per database, typically in config file
# SQL that will extract the table(s)
sql <- "

"
cnn <- DBI::dbConnect(odbc::odbc(),dsn=dsn) # =den --> needs to go into config file
ds <- DBI::dbGetQuery(cnn, sql) # actual extract
DBI::dbDisconnect(cnn) # hang up the phone
rm(dsn, cnn, sql) # clean up
ds %>% readr::write_rds("./data-unshared/derived/00-ferry.rds",compress = "xz")
# ---- declare-functions -------------------------------------------------------


# ---- load-data ---------------------------------------------------------------
ds %>% glimpse()

# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
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



