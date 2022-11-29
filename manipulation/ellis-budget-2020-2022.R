#' ---
#' title: "Ellis Hromada Budget 2020-2022"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-budget-2020-2022.R") # run to knit, don't uncomment
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
library(openxlsx)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# vector of hromadas that were occupied before 24th February
toh_before_22 <- c("05561000000","05556000000","12538000000","05555000000","12534000000"
                   ,"05549000000","05557000000","05551000000","12539000000","05547000000","05548000000"
                   ,"05563000000","12537000000","12540000000","05560000000","12533000000","05552000000"
                   ,"05554000000","05564000000","12532000000","12541000000","05562000000","12535000000"
                   ,"05566000000","12531000000","05565000000","05559000000","05558000000","05550000000"
                   ,"12536000000","05553000000") 


#+ declare-functions -----------------------------------------------------------

'%nin%' <- Negate('%in%') 
  
#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

paths_budget <-  list.files("./data-private/raw/", pattern = "^boost_incomes_INCO4.+.xlsx$",full.names = T)

ls_import <- list()
for(i in seq_along(paths_budget)){
  
  ls_import[[i]] <- 
    readxl::read_xlsx(
      path = paths_budget[[i]]
    ) %>% 
    janitor::clean_names() %>%
    mutate_all(.funs = as.character)
}

lapply(ls_import, glimpse)

#+ tweak-data ------------------------------------------------------------------
# to join multiple files (slices) downloaded from the source
ds0 <- 
  bind_rows(ls_import,.id = "file_number") 

path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)
ds_admin_full %>% glimpse(70)


#+ inspect-data ----------------------------------------------------------------

#+ tweak-data-1 ----------------------------------------------------------------

# tidying up admin codes
ds1 <- 
  ds0 %>% 
  filter(admin4 != 'Обласні бюджети та бюджет АР Крим') %>%
  # deleting oblast budgets
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
  ) %>% 
  filter(grepl('^[0-9][0-9]5', admin4_code)) %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("file_number", "admin4")) %>% 
  arrange(admin4_code) %>%
  filter(!is.na(admin4_code)) %>%  # because it's summary for oblast
  arrange(admin4_code)
ds1 %>% glimpse()
ds1

ds1 %>% distinct(admin4_code)

# frame explaining hierarchy of incomes
ds_inco <- 
  ds1 %>% 
  distinct(inco4) %>% 
  arrange(inco4) %>%
  mutate(
    inco4_code = str_extract(inco4, "^(\\d)+") %>% as.integer()
    ,inco4_label = str_remove(inco4, "^(\\d)+") %>% str_trim("both")
  ) %>%  
  select(
    ends_with("_code"), ends_with("_label")
  ) %>% 
  arrange(inco4_code)

# frame with hromada budget codes and names
ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# ---- tweak-data-2 ------------------------------------------------------------
ds1 %>% glimpse()

ds2_long <- 
  ds1 %>%
  mutate(inc_code = str_extract(inco4, '[0-9]+')) %>%
  select(-c(inco4)) %>%
  pivot_longer(
    -c(starts_with('adm'), inc_code)
    , names_to = 'year_month'
    , values_to = 'income'
  ) %>%
  mutate(
    year = str_extract(year_month, "(?<=x)....(?=_)")
    ,month = str_extract(year_month, "(?<=_)[0-9]+(?=_)")
  ) %>%
  select(-c(year_month)) 

ds2_wide <- 
  ds2_long %>% 
  pivot_wider(names_from = inc_code, values_from = income) %>%
  select(admin4_code, admin4_label, year, month, sort( names(.)))

ds1 %>% glimpse()
ds2_long %>% glimpse()
ds2_wide %>% glimpse()

# ---- final dataset with separate incomes ------------------------------------------------------------

ds3 <- ds2_wide %>%
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = paste0(budget_code,"0"),
             ) %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("admin4_code"  = "budget_code")
  ) %>%
  mutate(
    date = paste0(year,"-",ifelse(
      nchar(month)==1L, paste0("0",month), month),  "-01"
    ) %>% as.Date()) %>%
  relocate(hromada_name, hromada_code, admin4_label, admin4_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, 
           region_code_en, year, month, date) %>%
  rename(budget_name = admin4_label,
         budget_code = admin4_code) %>%
  # filtering out non-hromadas and hromadas occupied before 2022
  filter(!is.na(hromada_code) & budget_code %nin% toh_before_22)

ds3_long <- ds3 %>% pivot_longer(
  -c(1:14)
  , names_to = 'tax_code'
  , values_to = 'amount'
)

#+ ---- 

ds4_long <- 
  ds3_long %>% 
  mutate(
    transfert = str_detect(tax_code, "^4.+")
    ,target_segment = month %in% c(3:7) & year %in% c(2021, 2022)
    ,military_tax = tax_code %in% c('11010200')
    ,income_tax = str_detect(tax_code, "^1101.+")
    ,unified_tax = str_detect(tax_code, "^1805.+")
    ,property_tax = str_detect(tax_code, "^1801.+")
    ,excise_duty = str_detect(tax_code, "^140.+")
    )

ds5_long <- ds4_long %>%
  filter(target_segment) %>%
  # group_by(budget_code, budget_name, year, month, date) %>% 
  group_by(budget_code, budget_name, year) %>%
  summarize(
    income_total = sum(amount, na.rm = T)
    ,income_transfert = sum(amount*transfert, na.rm = T)
    ,income_military = sum(amount*military_tax, na.rm = T)
    ,income_pdfo = sum(amount*income_tax, na.rm = T)
    ,income_unified_tax = sum(amount*unified_tax, na.rm = T)
    ,income_property_tax = sum(amount*property_tax, na.rm = T)
    ,income_excise_duty = sum(amount*excise_duty, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(
    income_own = income_total - income_transfert
    ,own_income_prop = round(income_own/income_total,2)
    ,transfert_prop = round(income_transfert/income_total,2)
    ,military_tax_prop = round(income_military/income_total,2)
    ,pdfo_prop = round(income_pdfo/income_total,2)
    ,unified_tax_prop = round(income_unified_tax/income_total,2)
    ,property_tax_prop = round(income_property_tax/income_total,2)
    ,excise_duty_prop = round(income_excise_duty/income_total,2)
  ) %>%
  group_by(budget_code) %>% 
  mutate(
    own_income_change = round((income_own / lag(income_own)) - 1,2)
    ,own_prop_change = own_income_prop - lag(own_income_prop)
    ,own_income_change_net = income_own - lag(income_own)
  ) %>%
  ungroup() %>%
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = paste0(budget_code,"0")) %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("budget_code")
  ) %>%
  relocate(budget_code, budget_name, hromada_name, hromada_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, region_code_en) %>%
  arrange(oblast_name, raion_code, hromada_name, year)


#+ ---- adding metadata ---------------------------------------------------------

# for disaggregated on separate incomes dataset
variables_dis <- c(colnames(ds3)[1:14], '11010100-50110000')
description_dis <- c('Hromada name',
                 'Hromada code from Codifier of administrative-territorial units (CATUTTC)',
                 'Hromada budget name', 'Hromada budget code', 'Raion name', 
                 'Raion code from Codifier of administrative-territorial units (CATUTTC)',
                 'Oblast name', 'Oblast name Eng', 
                 'Oblast code from Codifier of administrative-territorial units (CATUTTC)',
                 'Region name Eng', 'Region short code', 'Year', 'Month', 'Date',
                 'Tax code')

metadata_dis <- data.frame(variables_dis, description_dis)

# for aggregated incomes dataset
variables <- c(colnames(ds5_long))
description <- c('Hromada budget code', 'Hromada budget name', 'Hromada name',
                 'Hromada code from Codifier of administrative-territorial units (CATUTTC)',
                 'Raion name', 'Raion code from Codifier of administrative-territorial units (CATUTTC)',
                 'Oblast name', 'Oblast name Eng', 'Oblast code from Codifier of administrative-territorial units (CATUTTC)',
                 'Region name Eng', 'Region short code', 'Year', 
                 'Total revenue amount', 'Revenue from tranferts', 
                 'Revenue from tax on military personnel income', 'Revenue from income tax',
                 'Revenue from unified tax', 'Revenue from property tax', 'Revenue from excise duty',
                 'Own income (w/o tranferts)', 'Share of own income', 'Share of tranferts',
                 'Share of tax on military personnel income', 'Share of income tax',
                 'Share of unified tax', 'Share of property tax', 'Share of excise duty',
                 'Percent change of own income (from 2021)', 'Change of own income share (from 2021)',
                 'Absolute change of own income (from 2021)')

metadata <- data.frame(variables, description)

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
dataset_names_dis <- list('Data' = ds3, 'Metadata' = metadata_dis)

dataset_names <- list('Data' = ds5_long, 'Metadata' = metadata)

openxlsx::write.xlsx(dataset_names_dis, './data-public/derived/hromada_budget_2020_2022_taxes.xlsx')
openxlsx::write.xlsx(dataset_names, './data-public/derived/hromada_budget_2020_2022.xlsx')

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

