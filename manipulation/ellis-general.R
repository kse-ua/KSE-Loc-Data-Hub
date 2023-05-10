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

#+ load-all-datasets -------------------------------------------------------------
#main datasets
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"
path_hromada <- "./data-public/derived/ua-admin-hromada.csv"
path_time  <- "./data-public/derived/time_rada.csv" #TO-DO: check the dates
path_geography <- "./data-public/derived/geography.csv"
path_demography <- "./data-public/derived/ua-pop-2022.csv"
path_osbb <- "./data-public/derived/osbb-hromada.csv"
path_zno <- "./data-public/derived/zno-2022-aggragated.csv"
#path_budget_income <- "./data-public/derived/hromada_budget_2020_2022.xlsx"
path_budget_income <- "./data-public/derived/hromada_budget_2020_2022.csv"
path_heads <- "./data-private/raw/hromada_heads.xlsx"
path_dfrr <- "./data-public/derived/dfrr_hromadas.csv"
path_edem <- "./data-public/derived/edem-data.csv"
path_community_competence <- "./data-public/derived/community-competence-hromada.csv"
path_declarations <- "./data-public/derived/declarations-hromada.csv"
path_war <- "./data-public/derived/minregion-war-status.csv"
path_internet <- "./data-public/derived/internet-speed.csv"
path_partnerships <- "./data-public/derived/partnerships-hromadas.csv"


# path_budget_expences <- 

#additional datasets
path_polygons <-  "./data-public/derived/shapefiles/terhromad_fin.geojson"
path_oblast <- "./data-private/raw/oblast.csv"
path_passangers <- "./data-public/derived/passangers.csv"
path_internet_speed <- "./data-public/derived/internet-speed.csv"
path_roads_lengths <- "./data-public/derived/roads-lengths.csv"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds_admin <- readr::read_csv(path_admin)
ds_hromada <- readr::read_delim(path_hromada)
ds_time <-  readr::read_csv(path_time) #TO-DO: check the dates
ds_geography <- readr::read_csv(path_geography) %>% select(-geometry)
ds_demography <- readr::read_csv(path_demography) 
ds_osbb <- readr::read_csv(path_osbb)
ds_zno<- readr::read_csv(path_zno)
#ds_budget_income <- readxl::read_xlsx(path_budget_income)
ds_budget_income <- readr::read_csv(path_budget_income)
ds_heads <- readxl::read_xlsx(path_heads)
ds_dfrr <- readr::read_csv(path_dfrr)
ds_edem <- readr::read_csv(path_edem)
ds_declarations<- readr::read_csv(path_declarations)
ds_community_competence <- readr::read_csv(path_community_competence) %>% 
  janitor::clean_names() #TODO: check NAs
# ds_budget_expences <- readr::read_csv(path_budget_expences)
ds_oblasts <- readr::read_csv(path_oblast)
ds_war <- readr::read_csv(path_war)
ds_passangers <- readr::read_csv(path_passangers)
ds_internet_speed <- readr::read_csv(path_internet_speed)
ds_roads_lengths <- readr::read_csv(path_roads_lengths)
ds_partnerships <- readr::read_csv(path_partnerships)
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names()

#+ inspect-data ----------------------------------------------------------------


#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds1_declarations <- ds_declarations %>% 
  rename_at(vars(total:working_age_pct), ~paste(., "declarations", sep ="_")) %>%
  select(-area, -hromada_name) %>% 
  left_join(
    ds_demography %>% select(hromada_code, total_population_2022, urban_population_2022)
    ,by = "hromada_code"
  ) %>% 
  mutate(
    declarations_pct = total_declarations/total_population_2022
    ,urban_declarations_pct = case_when(
      urban_population_2022 != 0 ~ urban_declarations/urban_population_2022
      ,TRUE ~ NA_real_
    )
  ) %>% 
  filter(declarations_pct >= 0.5)


#comparison population 2022 and declarations


#ADD script on events

#aggregate income data for 2022 as a dependent variable
income_2022 <- 
  ds_budget_income %>% 
  filter(year == "2022") %>% 
  select(hromada_code, own_income_change, own_prop_change, total_income_change, 
         income_own, income_total, income_transfert 
         # own_income_no_mil_change_YoY_jan_feb,
         # own_income_no_mil_change_YoY_jun_aug,
         # own_income_no_mil_change_YoY_mar_may,
         # own_income_no_mil_change_YoY_adapt
  ) %>% 
  rename(income_own_2022 = income_own, income_total_2022 = income_total, income_transfert_2022 = income_transfert)

#aggregate income data for 2021 as a predictor and combine with data for 2022
ds1_budget_income <- 
  ds_budget_income %>% 
  filter(year == "2021") %>%
  select(-c(ends_with('change'), ends_with('net'))) 
# %>%
  # select(-c("own_income_no_mil_change_YoY_jan_feb",
  #           "own_income_no_mil_change_YoY_jun_aug",
  #           "own_income_no_mil_change_YoY_mar_may",
  #           "own_income_no_mil_change_YoY_adapt")) 
  #
colnames(ds1_budget_income) <- ifelse(
  str_detect(colnames(ds1_budget_income), "income") |
    str_detect(colnames(ds1_budget_income), "prop")
  ,paste(colnames(ds1_budget_income), "2021", sep = "_")
  ,colnames(ds1_budget_income)) %>%
  rename(own_income_no_mil_change_YoY_jan_feb = own_income_no_mil_change_YoY_jan_feb_2021,
         own_income_no_mil_change_YoY_mar_apr = own_income_no_mil_change_YoY_mar_apr_2021,
         own_income_no_mil_change_YoY_mar_may = own_income_no_mil_change_YoY_mar_may_2021,
         own_income_no_mil_change_YoY_jun_aug = own_income_no_mil_change_YoY_jun_aug_2021,
         own_income_no_mil_change_YoY_jul_sep = own_income_no_mil_change_YoY_jul_sep_2021,
         own_income_no_mil_change_YoY_adapt = own_income_no_mil_change_YoY_adapt_2021
         )


#aggregate DFRR data for all years
ds1_dfrr <- 
  ds_dfrr %>% 
  group_by(hromada_code) %>% 
  summarise(
    dfrr_executed = sum(budget_executed_2015, na.rm=T)
    ,dfrr_executed_20_21 = sum(budget_executed_2015[year %in% c(2020, 2021)]) 
    , .groups = "drop"
  )

#changes in ds_heads
ds1_heads <-
  ds_heads %>% 
  select(hromada_code, turnout, sex, age, education, incumbent, rda, not_from_here,
         party, enterpreuner, unemployed, priv_work, polit_work, communal_work,
         ngo_work) %>% 
  rename(sex_head = sex, age_head = age, education_head = education, turnout_2020 = turnout) %>% 
  mutate(
    sex_head = factor(sex_head, labels = c("female", "male"))
    ,education_head = case_when(
      education_head == "освіта вища" ~ "higher"
      ,education_head != "освіта вища" ~ "non-higher"
    )
    ,party_national_winner = case_when(
      party == 'Слуга народу' ~ 1,
      TRUE ~ 0
    )
    ,no_party = case_when(
      party == 'Самовисування' ~ 1
      ,TRUE ~ 0
    )
    ,male = case_when(
      sex_head == 'male' ~ 1
      ,TRUE ~ 0
    )
    ,high_educ = case_when(
      education_head == 'higher' ~ 1
      ,TRUE ~ 0
    )
  ) %>% 
  mutate_at(
    vars(incumbent, rda, not_from_here), ~case_when(
      . == "yes" ~ 1
      ,TRUE ~ 0
    )
  )

#create dataset of creation dates
ds_hromada_dates <- 
  ds_time %>% 
  group_by(hromada_code) %>% 
  mutate(
    creation_date = min(date)
  ) %>% 
  ungroup() %>% 
  distinct(hromada_code, creation_date) %>% 
  mutate(
    creation_year = lubridate::year(creation_date)
    ,time_before_24th = difftime("2022-02-24", creation_date, units = "days")
    ,voluntary = ifelse(creation_date != "2020-08-16", 1, 0)
  )



#create dummy variables for hromadas-oblast centers
hromadas_oblast_centers <- 
  ds_admin %>% 
  filter(settlement_code == oblast_center_code) %>% 
  pull(hromada_code)


#+ combine ---------------------------------------------------------------------

d1 <- 
  ds_hromada %>% 
  filter(!oblast_name == "Автономна Республіка Крим") %>% 
  mutate(
    hromada_full_name = paste(hromada_name, type, "громада")
    ,oblast_center = ifelse(hromada_code %in% hromadas_oblast_centers, 1, 0)
  ) %>% 
  left_join(
    ds_geography %>% select(-c(hromada_type, hromada, oblast_name, raion_name, key))
    ,by = "hromada_code"  
  ) %>% 
  mutate_at(
    vars(mountain_hromada, near_seas, bordering_hromadas, hromadas_30km_from_border,
         hromadas_30km_russia_belarus, buffer_nat_15km, buffer_int_15km),
    ~replace(., is.na(.), 0)
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
    ds1_budget_income %>% select(-c(hromada_name, year, raion_code, raion_name,
                                    oblast_code, oblast_name))
    ,by = "hromada_code"  
  ) %>% 
  left_join(
    income_2022
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
  ) %>% 
  left_join(
    ds_edem %>% select(hromada_code, edem_total, edem_petitions, edem_consultations,       
                       edem_participatory_budget, edem_open_hromada)
    ,by = "hromada_code" 
  ) %>% 
  left_join(
    ds_community_competence %>% select(hromada_code, youth_councils, youth_centers, business_support_centers)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_oblasts %>% select(oblast_code)
    ,by = "oblast_code"
  ) %>% 
  #TEMPORARY - CHECK THE DATES IN THIS DATASET
  left_join(
    ds_hromada_dates
    ,by = "hromada_code"
  ) %>% 
  mutate_at(
    vars(edem_total, edem_petitions, edem_consultations,       
         edem_participatory_budget, edem_open_hromada, youth_councils, youth_centers, 
         business_support_centers, enterpreuner, unemployed, priv_work,     
         polit_work, communal_work, ngo_work)
    ,~replace(., is.na(.), 0)
  ) %>%
  left_join(
    ds_war %>% select(hromada_code, starts_with('war_zone'),
                      Status_war_sept,
                      Status_war_sept_ext, 
                      deoccupation_date)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_passangers
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds1_declarations %>% select(-total_population_2022, -urban_population_2022)
    ,by = "hromada_code"
  ) %>% 
  mutate(
    train_station = ifelse(is.na(passangers_2021), 0, 1)
  ) %>% 
  mutate_at(
    vars(starts_with("income_")), ~./1000
  ) %>% 
  rename(area = square) %>% 
  left_join(
    ds_partnerships %>% select(-hromada_name)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
  

#TO-DO: add big taxpayers
#TO-DO: add dates of creation + status based on military actions/occupation (DONE)


#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(d1, "./data-public/derived/full_dataset.csv")



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

