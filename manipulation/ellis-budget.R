#' ---
#' title: "Ellis Economics"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-economics.R") # run to knit, don't uncomment
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
Sys.setlocale("LC_CTYPE", "russian")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_1 <- "./data-private/raw/revenues-1.csv"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

paths_budget <-  list.files("./data-private/raw/",pattern = "revenues-\\d.csv$",full.names = T)
# names_budget <-  paths_budget %>% str_replace("^.+revenues-(\\d{1}).csv","\\1")

ls_import <- list()
for(i in seq_along(paths_budget)){
  
  ls_import[[i]] <- 
    readr::read_delim(
      file = paths_budget[[i]]
      ,delim = ";"
      , escape_double = FALSE
      , locale = locale(encoding = "Windows-1251",asciify = TRUE)
      , trim_ws = TRUE
  ) %>% 
    janitor::clean_names() %>%
     mutate_all(.funs = as.character)
}
lapply(ls_import, glimpse)
ds0 <- bind_rows(ls_import,.id = "file_number")
ds0 %>% glimpse()
#+ inspect-data ----------------------------------------------------------------

import_custom <- function(p){
  d_out <- 
   readr::read_delim(
      file = p
      ,delim = ";"
      , escape_double = FALSE
      , locale = locale(encoding = "Windows-1251",asciify = TRUE)
      # , locale = locale(encoding = "Windows-1251",asciify = FALSE)
      , trim_ws = TRUE
    ) %>% 
    janitor::clean_names() %>%
    mutate_all(.funs = as.character)
}

d <- paths_budget[2] %>% import_custom()
d %>% count(admin1)
d %>% count(admin2)
d %>% count(admin3)
d %>% count(admin4) %>% arrange(admin4)

d %>% filter(admin1 == "с-ща Озерянівка") %>% View()
d %>% filter(admin2 == "10000000 Податкові надходження") %>% View()

ds0 %>% count(as.factor(admin1))
ds0 %>% count(as.factor(admin2)) %>% print_all()




ds0 %>% 
  filter(admin1 == "с-ща Іванівка") %>% 
  View()
d7 <- ls_import[[7]]
d7 %>% count(admin1)
ds0 %>% 
  filter(admin2 == "10000000 Податкові надходження") %>% 
  View()
#+ tweak-data-1, eval=eval_chunks ------------------------------------------------
ds0 %>% glimpse()
ds1 <- 
  ds0 %>% 
  select(1:7)

ds1 %>% glimpse()  
#+ tweak-data-2 ----------------------------------------------------------------

#+ tweak-data-3 ----------------------------------------------------------------

#+ table-1 ---------------------------------------------------------------------

#+ graph-1 ---------------------------------------------------------------------


#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# A. Session Information{#session-info}")
#+ results="show", echo=F ------------------------------------------------------
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)

