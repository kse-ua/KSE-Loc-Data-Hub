#' ---
#' title: "Ellis UA Admin"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-ua-admin.R") # run to knit, don't uncomment
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
Sys.setlocale("LC_CTYPE", "ukr")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)
library(tmap)


#+ load-all-datasets -------------------------------------------------------------
#main datasets
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"
path_hromada <- "./data-private/derived/hromada.csv"
# path_hromada_events <- "./data-private/raw/hromada.csv"
path_hromada_dates <- "./data-private/derived/hromadas-dates.csv"
path_geography <- "./data-public/derived/geography.csv"
path_demography <- "./data-private/derived/ua-pop-2022.csv"
path_osbb <- "./data-private/derived/osbb-hromada.csv"
path_zno <- "./data-private/derived/zno-2022-aggragated.csv"
path_budget_income <- "./data-public/derived/hromada_budget_2020_2022.xlsx"
path_heads <- "./data-public/derived/hromada_heads.xlsx"
path_dfrr <- "./data-private/derived/dfrr_hromadas.csv"
# path_community_competence <- "./data-private/raw/ua-admin-codes.csv"
# path_budget_expences <- 

#additional datasets
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"
path_oblast <- "./data-private/raw/oblast.csv"


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds_admin <- readr::read_csv(path_admin)
ds_hromada <- readr::read_csv(path_hromada)
# ds_hromada_events <- readr::read_csv(path_hromada_events)
hromada_dates <-  readr::read_csv(path_hromada_dates)
ds_geography <- readr::read_csv(path_geography) %>% 
  rename(area = square, travel_time = ttime)
ds_demography <- readr::read_csv(path_demography) 
ds_osbb <- readr::read_csv(path_osbb)
ds_zno<- readr::read_csv(path_zno)
ds_budget_income <- readxl::read_xlsx(path_budget_income)
ds_heads <- readxl::read_xlsx(path_heads)
ds_dfrr <- readr::read_csv(path_dfrr)

# ds_community_competence <- readr::read_csv(path_community_competence)
# ds_budget_expences <- readr::read_csv(path_budget_expences)


#+ inspect-data ----------------------------------------------------------------


#+ tweak-data, eval=eval_chunks ------------------------------------------------



#aggregate income data for 2021 as a predictor
ds1_budget_income <- 
  ds_budget_income %>% 
  group_by(hromada_name, hromada_code, year) %>% 
  summarise_at(vars(income_total:income_own), ~sum(.x, na.rm = TRUE)) %>% 
  filter(year == "2021") %>% 
  ungroup()
  
colnames(ds1_budget_income) <- ifelse(
  str_detect(colnames(ds1_budget_income), "income")
  ,paste(colnames(ds1_budget_income), "2021", sep = "_")
  ,colnames(ds1_budget_income))

#aggregate income data for 2022 as a dependent variable



#aggregate DFRR data for all years
ds1_dfrr <- 
  ds_dfrr %>% 
  group_by(hromada_code) %>% 
  summarise(dfrr_executed = sum(budget_executed, na.rm=T), .groups = "drop")


ds1_heads <-
  ds_heads %>% 
  select(hromada_code, turnout, sex, age, education, incumbent, rda, not_from_here) %>% 
  rename(sex_head = sex, age_head = age, education_head = education, turnout_2020 = turnout) %>% 
  mutate(
    sex_head = factor(sex_head, labels = c("female", "male"))
    ,education_head = case_when(
      education_head == "освіта вища" ~ "higher"
      ,education_head != "освіта вища" ~ "non-higher"
    )
  ) %>% 
  mutate_at(
    vars(incumbent, rda, not_from_here), ~case_when(
      . == "yes" ~ 1
      ,TRUE ~ 0
    )
  )



#+ combine ---------------------------------------------------------------------

d1 <- 
  ds_hromada %>% 
  filter(!oblast_name == "Автономна Республіка Крим") %>% 
  mutate(
    hromada_full_name = paste(hromada_name, type, "громада")
  ) %>% 
  left_join(
    ds_geography %>% select(-c(hromada_type, hromada, oblast_name, raion_name, key))
    ,by = "hromada_code"  
  ) %>% 
  mutate(
    occipied_before_2022 = case_when(
      is.na(n_settlements) ~ 1
      ,TRUE ~ 0
    )
  ) %>%
  left_join(
    ds_demography %>% select(-hromada_name)
    ,by = "hromada_code"  
  ) %>% 
  left_join(
    ds1_budget_income %>% select(-hromada_name, -year)
    ,by = "hromada_code"  
  ) %>% 
  left_join(
    ds1_dfrr
    ,by = "hromada_code"  
  ) %>% 
  left_join(
    ds1_heads 
    ,by = "hromada_code"  
  ) %>% 
  left_join(
    ds_osbb %>% select(hromada_code, sum_osbb_2020)
    ,by = "hromada_code"  
  ) 

#TO-DO: add partnerships
#TO-DO: add big taxpayers
#TO-DO: add community competence + civic activities
#TO-DO: add dates of creation + status based on military actions/occupation


#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(d1, "./data-private/derived/full_dataset.csv")



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

