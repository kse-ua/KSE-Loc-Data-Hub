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
#+ load-sources ----------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
nakaz_war_status <- "./data-private/raw/hromada_war_status.xlsx"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"
occupation_status_path <- "./data-private/raw/occupation_status_hromada.xlsx"


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
nakaz_war_status <- readxl::read_excel(nakaz_war_status)
ds_admin <- readr::read_csv(path_admin)
occupation_status <- readxl::read_excel(occupation_status_path)

#hromada dataset
ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv")

# Exlcuding Crimea from the sample
hromadas <- ds_hromada %>% filter(oblast_name!="Автономна Республіка Крим") %>%
  select(oblast_code,
         oblast_name,
         raion_code,
         raion_name,
         hromada_code,
         hromada_name,
         type)

# merging admin data with war status data

merge <- nakaz_war_status %>% 
   right_join(
    hromadas
    ,by = c("hromada_short_name" = "hromada_name",
            "raion_short_name" = "raion_name",
            "oblast_short_name" = "oblast_name",
            'hromada_type' = 'type')
  ) %>%
  left_join(hromadas %>% select(hromada_code, type)
            ,by = c('hromada_code')) %>%
  mutate(across(starts_with('war_zone'), ~replace_na(.,0))) %>%
  left_join(occupation_status,
            by = c('hromada_code'))


#+ save-data, eval=eval_chunks -------------------------------------------------
readr::write_csv(merge, "./data-public/derived/minregion-war-status.csv") #aggregated on hromada level
