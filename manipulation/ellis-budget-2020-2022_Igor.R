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

paths_budget <-  list.files("./data-public/raw/", pattern = "^boost_incomes_INCO4.+.xlsx$",full.names = T)

ls_import <- list()
for(i in seq_along(paths_budget)){
  
  ls_import[[i]] <- 
    readxl::read_xlsx(
      path = paths_budget[[i]]
    ,guess_max = 1048576) %>% 
    janitor::clean_names() %>%
    mutate_all(.funs = as.character)
}

lapply(ls_import, glimpse)

#+ tweak-data ------------------------------------------------------------------
# to join multiple files (slices) downloaded from the source
ds0 <- 
  bind_rows(ls_import,.id = "file_number") 

path_admin <- "./data-public/derived/ua-admin-map.csv"
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

# ---- final dataset with separate incomes ------------------------------------------------------------

ds3 <- ds2_wide %>%
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = budget_code,
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

#+ tweak-data-5-old ---- 
# `5` because it produces `ds5` form
ds4_long <- 
  ds3_long %>% 
  mutate(
    transfert = str_detect(tax_code, "^4.+")
    ,target_segment = month %in% c(1:12) & year %in% c(2021, 2022)
    ,military_tax = tax_code %in% c('11010200')
    ,income_tax = str_detect(tax_code, "^1101.+")
    ,unified_tax = str_detect(tax_code, "^1805.+")
    ,property_tax = str_detect(tax_code, "^1801.+")
    ,excise_duty = str_detect(tax_code, "^140.+")
    ,corporate_tax = str_detect(tax_code, "^11020200.+")
    ,parking_fee = str_detect(tax_code, "^1802.+")
    ,tourist_fee = str_detect(tax_code, "^1803.+")
    ,eco_tax = str_detect(tax_code, "^1901.+")
    ,non_tax = str_detect(tax_code, "^2.+")
    ,capital_proceedings = str_detect(tax_code, "^3.+")
    ,special_funds = str_detect(tax_code, "^5.+")
    ,rent = str_detect(tax_code, "^130.+")
    
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
    ,income_corporate_tax = sum(amount*corporate_tax, na.rm = T)
    ,income_parking_fee = sum(amount*parking_fee, na.rm = T)
    ,income_tourist_fee = sum(amount*tourist_fee, na.rm = T)
    ,income_eco_tax = sum(amount*eco_tax, na.rm = T)
    ,income_non_tax = sum(amount*non_tax, na.rm = T)
    ,income_capital_proceedings = sum(amount*capital_proceedings, na.rm = T)
    ,income_special_funds = sum(amount*special_funds, na.rm = T)
    ,income_rent = sum(amount*rent, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(
    income_own = income_total - income_transfert
    ,own_income_prop = round(income_own/income_total,4)
    ,transfert_prop = round(income_transfert/income_total,4)
    ,military_tax_prop = round(income_military/income_total,4)
    ,pdfo_prop = round(income_pdfo/income_total,4)
    ,unified_tax_prop = round(income_unified_tax/income_total,4)
    ,property_tax_prop = round(income_property_tax/income_total,4)
    ,excise_duty_prop = round(income_excise_duty/income_total,4)
    ,corporate_tax_own_prop = round(income_corporate_tax/income_own,4)
    ,parking_fee_own_prop = round(income_parking_fee/income_own,4)
    ,tourist_fee_own_prop = round(income_tourist_fee/income_own,4)
    ,eco_tax_own_prop = round(income_eco_tax/income_own,4)
    ,non_tax_own_prop = round(income_non_tax/income_own,4)
    ,capital_proceedings_own_prop = round(income_capital_proceedings/income_own,4)
    ,special_funds_own_prop = round(income_special_funds/income_own,4)
    ,excise_duty_own_prop = round(income_excise_duty/income_own,4)
    ,pdfo_own_prop = round(income_pdfo/income_own,4)
    ,unified_tax_own_prop = round(income_unified_tax/income_own,4)
    ,property_tax_own_prop = round(income_property_tax/income_own,4)
    ,rent_own_prop = round(income_rent/income_own,4)
  ) %>%
  group_by(budget_code) %>% 
  mutate(
    own_income_change = round((income_own / lag(income_own)) - 1,2)
    ,own_prop_change = own_income_prop - lag(own_income_prop)
    ,own_income_change_net = income_own - lag(income_own)
    ,total_income_change_net = income_total - lag(income_total)
    ,total_income_change = round((income_total / lag(income_total)) - 1,2)
  ) %>%
  ungroup() %>%
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = budget_code) %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("budget_code")
  ) %>%
  relocate(budget_code, budget_name, hromada_name, hromada_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, region_code_en) %>%
  arrange(oblast_name, raion_code, hromada_name, year)


#+ ---- adding metadata-old ---------------------------------------------------------

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
                 'Absolute change of own income (from 2021)', 'Absolute change of total income (from 2021)',
                 'Percent change of total income (from 2021)')

#metadata <- data.frame(variables, description)


#+ tweak-data-5  ---- 

ds4_long_a <- 
  ds3_long %>% 
  mutate(
    transfert    = str_detect(tax_code, "^4.+")
    #,target_segment = month %in% c(3:7) & year %in% c(2021, 2022) # i think
    # this should be done after data at the most granualar level are processed
    ,military_tax = tax_code %in% c('11010200')
    ,income_tax   = str_detect(tax_code, "^1101.+")
    ,unified_tax  = str_detect(tax_code, "^1805.+")
    ,property_tax = str_detect(tax_code, "^1801.+")
    ,excise_duty  = str_detect(tax_code, "^140.+")
    ,corporate_tax = str_detect(tax_code, "^1102.+")
  )

ds4_long %>% glimpse(80)

ds5_long_temp <- 
  ds4_long_a %>%
  group_by(budget_code, budget_name, year, month, date) %>%
  summarize(
    income_total         = sum(amount, na.rm = T)
    ,income_transfert    = sum(amount*transfert, na.rm = T)
    ,income_military     = sum(amount*military_tax, na.rm = T)
    ,income_pdfo         = sum(amount*income_tax, na.rm = T)
    ,income_unified_tax  = sum(amount*unified_tax, na.rm = T)
    ,income_property_tax = sum(amount*property_tax, na.rm = T)
    ,income_excise_duty  = sum(amount*excise_duty, na.rm = T)
    ,income_corporate_tax = sum(amount*corporate_tax, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(
    income_own           = income_total - income_transfert
    ,own_income_prop     = round(income_own/income_total,2)
    ,transfert_prop      = round(income_transfert/income_total,2)
    ,military_tax_prop   = round(income_military/income_total,2)
    ,pdfo_prop           = round(income_pdfo/income_total,2)
    ,unified_tax_prop    = round(income_unified_tax/income_total,2)
    ,property_tax_prop   = round(income_property_tax/income_total,2)
    ,excise_duty_prop    = round(income_excise_duty/income_total,2)
  ) %>%
  group_by(budget_code) %>% 
  mutate(
    own_income_change        = round((income_own / lag(income_own)) - 1,2)
    ,own_prop_change         = own_income_prop - lag(own_income_prop)
    ,own_income_change_net   = income_own - lag(income_own)
    ,total_income_change_net = income_total - lag(income_total)
    ,total_income_change     = round((income_total / lag(income_total)) - 1,2)
  ) %>%
  ungroup() 

ds5_long_temp %>% glimpse()

ds_admin <-  
  ds_admin_full %>% 
  mutate(budget_code = budget_code) %>%
  select(
    budget_code, 
    hromada_code, hromada_name, 
    raion_code,   raion_name,
    oblast_code,  oblast_name, 
    oblast_name_en,
    region_code_en, region_en
  ) %>% 
  distinct()
ds_admin %>% glimpse()


target_id <- "02537000000"
ds5_long_temp %>% filter(budget_code == target_id) %>% glimpse()
# ds5_long_temp %>% filter(budget_code == target_id) %>% View()
ds_admin %>% filter(budget_code == target_id) %>% glimpse()

# ds5_long <-
#  ds5_long_temp %>%
#  left_join(
#    ds_admin
#    ,by = c("budget_code")
#  ) %>%
#  relocate(budget_code, budget_name, hromada_name, hromada_code, raion_name,
#          raion_code, oblast_name, oblast_name_en, oblast_code, region_en, region_code_en)

# arrange(oblast_name, raion_code, hromada_name, date) # this kept crashing my
# session, so I will avoid it for now 

ds5_long %>% glimpse()
#rm(ds5_long_temp)
# ----- adding-metadata ----------------------------------------------------
# the above method of pairing names and descriptions depends on the order of variables
# it's not very transparent to inspection and will hide the error if we change the number or 
# order of columns in ds3 or ds5. Like, for example, when we group ds4 into ds5
# by year vs by date. I advocate for making the links within pairs explicit.
# there is a very helpful function to get a head start on this process:
# run the line below and copy-paste the output from console into source and modify
# ds3 %>% select(1:14) %>%  OuhscMunge::column_rename_headstart() 

variable_labels_ds3 <- c(
  "hromada_name"    = "Hromada name"
  ,"hromada_code"   = "Hromada code from Codifier of administrative-territorial units (CATUTTC)"
  ,"budget_name"    = "Hromada budget name"
  ,"budget_code"    = "Hromada budget code"
  ,"raion_name"     = "Raion name"
  ,"raion_code"     = "Raion code from Codifier of administrative-territorial units (CATUTTC)"
  ,"oblast_name"    = "Oblast name"
  ,"oblast_name_en" = "Oblast name Eng"
  ,"oblast_code"    = "Oblast code from Codifier of administrative-territorial units (CATUTTC)"
  ,"region_en"      = "Region name Eng"
  ,"region_code_en" = "Region short code"
  ,"year"           = "Year"
  ,"month"          = "Month"
  ,"date"           = "Date"
  ,"tax_code"       = "Tax code"
) 
ds_meta_ds3 <- 
  tibble::tibble(
    "variable_name"   = variable_labels_ds3 %>% names()
    ,"variable_label" = variable_labels_ds3 %>% as.character()
  )
metadata_dis <- ds_meta_ds3 # to match old names

# ds5_long %>% OuhscMunge::column_rename_headstart() # run to get a headstart
variable_labels_ds5_long <- c(
  "budget_code"              = "Hromada budget code"                                                     
  ,"budget_name"             = "Hromada budget name"                                                     
  ,"hromada_name"            = "Hromada name"                                                            
  ,"hromada_code"            = "Hromada code from Codifier of administrative-territorial units (CATUTTC)"
  ,"raion_name"              = "Raion name"                                                              
  ,"raion_code"              = "Raion code from Codifier of administrative-territorial units (CATUTTC)"  
  ,"oblast_name"             = "Oblast name"                                                             
  ,"oblast_name_en"          = "Oblast name Eng"                                                         
  ,"oblast_code"             = "Oblast code from Codifier of administrative-territorial units (CATUTTC)" 
  ,"region_en"               = "Region name Eng"                                                         
  ,"region_code_en"          = "Region short code"                                                       
  ,"year"                    = "Year"                                                                    
  ,"month"                   = "Month" # turn off when aggregating over years 
  ,"date"                    = "Date"  # turn off when aggregating over years
  ,"income_total"            = "Total revenue amount"                                                    
  ,"income_transfert"        = "Revenue from tranferts"                                                  
  ,"income_military"         = "Revenue from tax on military personnel income"                           
  ,"income_pdfo"             = "Revenue from income tax"                                                 
  ,"income_unified_tax"      = "Revenue from unified tax"                                                
  ,"income_property_tax"     = "Revenue from property tax"                                               
  ,"income_excise_duty"      = "Revenue from excise duty"                                                
  ,"income_own"              = "Own income (w/o tranferts)"                                              
  ,"own_income_prop"         = "Share of own income"                                                     
  ,"transfert_prop"          = "Share of tranferts"                                                      
  ,"military_tax_prop"       = "Share of tax on military personnel income"                               
  ,"pdfo_prop"               = "Share of income tax"                                                     
  ,"unified_tax_prop"        = "Share of unified tax"                                                    
  ,"property_tax_prop"       = "Share of property tax"                                                   
  ,"excise_duty_prop"        = "Share of excise duty"                                                    
  ,"own_income_change"       = "Percent change of own income (from 2021)"                                
  ,"own_prop_change"         = "Change of own income share (from 2021)"                                  
  ,"own_income_change_net"   = "Absolute change of own income (from 2021)"                               
  ,"total_income_change_net" = "Absolute change of total income (from 2021)"                             
  ,"total_income_change"     = "Percent change of total income (from 2021)" 
)
ds_meta_ds5_long <- 
  tibble::tibble(
    "variable_name"   = variable_labels_ds3 %>% names()
    ,"variable_label" = variable_labels_ds3 %>% as.character()
  )
metadata <- ds_meta_ds5_long # to match old names
# this approach allows for a quicker familiarization with the data set when 
# one starts reading the code

#+ derive-the-former -------

# preivous version of ds5_long focused on a specific segments of time:
#,target_segment = month %in% c(3:7) & year %in% c(2021, 2022) 
# I think it is more practical to have a clean data set at the highest
# level of resolution (month), which then can be summarized into the desired form
ds5_long %>% glimpse()

metrics <- ds5_long %>% select(income_total:total_income_change) %>% names()
grouping_stem <- ds5_long %>% select(budget_code:year) %>% names()

# ds5_former <- 
#   ds5_long %>% 
#   mutate(
#     target_segment = month %in% c(3:9) & year %in% c(2021, 2022) # i think
#   ) %>% 
#   filter(target_segment) %>% 
#   group_by_at(grouping_stem) %>% 
#   summarize(
#     across(
#       .cols = metrics
#       ,.fns  = ~sum(.,na.rm=T) 
#     )
#   ) %>% 
#   ungroup() %>% 
#   glimpse()

a <- ds5_long_temp %>% 
  filter(year!=2020) %>% 
  group_by(budget_code, month) %>%
  mutate(income_own_no_mil_change_YoY = ((income_own - income_military) -
                                           (income_own[year==2021] - income_military[year==2021])),
         income_own_no_military_tax = income_own - income_military,
         income_own_no_military_tax_corp_tax = income_own - income_military - income_corporate_tax)

i <- a %>% group_by(year, month) %>% summarise(x = sum(income_corporate_tax, na.rm=TRUE))

b <- a %>%
  group_by(budget_code) %>%
  mutate(own_income_no_mil_change_YoY_jun_aug = ((((income_own_no_military_tax[month=="8"&year==2022]+
                                                      income_own_no_military_tax[month=="7"&year==2022]+
                                                      income_own_no_military_tax[month=="6"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="8"&year==2021]+
                                                        income_own_no_military_tax[month=="7"&year==2021]+
                                                        income_own_no_military_tax[month=="6"&year==2021]))-1)*100),
         own_income_no_mil_change_YoY_mar_may = ((((income_own_no_military_tax[month=="3"&year==2022]+
                                                      income_own_no_military_tax[month=="4"&year==2022]+
                                                      income_own_no_military_tax[month=="5"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="3"&year==2021]+
                                                        income_own_no_military_tax[month=="4"&year==2021]+
                                                        income_own_no_military_tax[month=="5"&year==2021]))-1)*100),
         own_income_no_mil_change_YoY_mar_apr = ((((income_own_no_military_tax_corp_tax[month=="3"&year==2022]+
                                                      income_own_no_military_tax_corp_tax[month=="4"&year==2022]) / 
                                                     (income_own_no_military_tax_corp_tax[month=="3"&year==2021]+
                                                        income_own_no_military_tax_corp_tax[month=="4"&year==2021]))-1)*100),
         own_income_no_mil_change_YoY_jan_feb = ((((income_own_no_military_tax[month=="1"&year==2022]+
                                                      income_own_no_military_tax[month=="2"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="1"&year==2021]+
                                                        income_own_no_military_tax[month=="2"&year==2021]))-1)*100),
         own_income_no_mil_change_YoY_jul_sep = ((((income_own_no_military_tax[month=="9"&year==2022]+
                                                      income_own_no_military_tax[month=="8"&year==2022]+
                                                      income_own_no_military_tax[month=="7"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="9"&year==2021]+
                                                        income_own_no_military_tax[month=="8"&year==2021]+
                                                        income_own_no_military_tax[month=="7"&year==2021]))-1)*100),
         
         own_income_no_mil_change_YoY_oct_jan = ((((income_own_no_military_tax[month=="10"&year==2022]+
                                                      income_own_no_military_tax[month=="11"&year==2022]+
                                                      income_own_no_military_tax[month=="12"&year==2022]+
                                                      income_own_no_military_tax[month=="1"&year==2023]) / 
                                                     (income_own_no_military_tax[month=="10"&year==2021]+
                                                        income_own_no_military_tax[month=="11"&year==2021]+
                                                        income_own_no_military_tax[month=="12"&year==2021]+
                                                        income_own_no_military_tax[month=="1"&year==2022]))-1)*100),
         own_income_no_mil_change_YoY_oct_dec = ((((income_own_no_military_tax[month=="10"&year==2022]+
                                                      income_own_no_military_tax[month=="11"&year==2022]+
                                                      income_own_no_military_tax[month=="12"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="10"&year==2021]+
                                                        income_own_no_military_tax[month=="11"&year==2021]+
                                                        income_own_no_military_tax[month=="12"&year==2021]))-1)*100),
         own_income_no_mil_change_YoY_may_feb = ((((income_own_no_military_tax[month=="5"&year==2022]+
                                                      income_own_no_military_tax[month=="6"&year==2022]+
                                                      income_own_no_military_tax[month=="7"&year==2022]+
                                                      income_own_no_military_tax[month=="8"&year==2022]+
                                                      income_own_no_military_tax[month=="9"&year==2022]+
                                                      income_own_no_military_tax[month=="10"&year==2022]+
                                                      income_own_no_military_tax[month=="11"&year==2022]+
                                                      income_own_no_military_tax[month=="12"&year==2022]+
                                                      income_own_no_military_tax[month=="1"&year==2023]+
                                                      income_own_no_military_tax[month=="2"&year==2023]) / 
                                                     
                                                     (income_own_no_military_tax[month=="5"&year==2021]+
                                                        income_own_no_military_tax[month=="6"&year==2021]+
                                                        income_own_no_military_tax[month=="7"&year==2021]+
                                                        income_own_no_military_tax[month=="8"&year==2021]+
                                                        income_own_no_military_tax[month=="9"&year==2021]+
                                                        income_own_no_military_tax[month=="10"&year==2021]+
                                                        income_own_no_military_tax[month=="11"&year==2021]+
                                                        income_own_no_military_tax[month=="12"&year==2021]+
                                                        income_own_no_military_tax[month=="1"&year==2022]+
                                                        income_own_no_military_tax[month=="2"&year==2022]))-1)*100),
         
         own_income_no_mil_change_YoY_adapt = own_income_no_mil_change_YoY_jul_sep - own_income_no_mil_change_YoY_mar_apr,
         own_income_no_mil_change_YoY_jun_aug = case_when(own_income_no_mil_change_YoY_jun_aug <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_jun_aug),
         own_income_no_mil_change_YoY_mar_may = case_when(own_income_no_mil_change_YoY_mar_may <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_mar_may),
         own_income_no_mil_change_YoY_mar_apr = case_when(own_income_no_mil_change_YoY_mar_apr <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_mar_apr),
         own_income_no_mil_change_YoY_jan_feb = case_when(own_income_no_mil_change_YoY_jan_feb <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_jan_feb),
         own_income_no_mil_change_YoY_jul_sep = case_when(own_income_no_mil_change_YoY_jul_sep <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_jul_sep),
         own_income_no_mil_change_YoY_oct_jan = case_when(own_income_no_mil_change_YoY_oct_jan <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_oct_jan),
         own_income_no_mil_change_YoY_oct_dec = case_when(own_income_no_mil_change_YoY_oct_dec <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_oct_dec),
         own_income_no_mil_change_YoY_may_feb = case_when(own_income_no_mil_change_YoY_may_feb <= -100 ~ -100, TRUE ~ own_income_no_mil_change_YoY_may_feb),
         
         ) %>%
  group_by(budget_code, year) %>%
  mutate(income_own_full_year = sum(income_own, na.rm = TRUE),
         income_total_full_year = sum(income_total, na.rm = TRUE),
         own_income_prop_full_year = (income_own_full_year/sum(income_total, na.rm = TRUE))) %>% ungroup()

d <- readr::read_csv("./data-public/derived/full_dataset.csv")
check_per_capita <- b %>% left_join(
  d %>% select(budget_code, total_population_2022)
  ,by = "budget_code"
) %>% mutate(income_total_per_cap = (income_total_full_year/total_population_2022)) %>%
  select(c("budget_name", "year", "income_total_per_cap")) %>%
  filter(budget_name == "Бюджет Новороздільської міської територіальної громади" |
           budget_name ==                 "Бюджет Краснокутської селищної територіальної громади" |
           budget_name ==                 "Бюджет Боярської міської територіальної громади" |
         budget_name ==               "Бюджет Нововолинської міської територіальної громади" |
         budget_name ==                "Бюджет Новоушицької селищної територіальної громади" |
         budget_name ==                "Бюджет Городенківської міської територіальної громади") %>% 
  distinct(budget_name, year, .keep_all = TRUE) %>% filter(year != 2023)
 

# b$own_income_no_mil_change_YoY_jun_aug[b$year == 2021] <- NA
# b$own_income_no_mil_change_YoY_mar_may[b$year == 2021] <- NA
# b$own_income_no_mil_change_YoY_adapt[b$year == 2021] <- NA
# b$own_income_no_mil_change_YoY_jan_feb[b$year == 2021] <- NA
# b$own_income_no_mil_change_YoY_mar_apr[b$year == 2021] <- NA

c <- b %>% 
  distinct(budget_code, year, .keep_all = TRUE) %>%
  select(budget_code,
         year,
         own_income_no_mil_change_YoY_jan_feb,
         own_income_no_mil_change_YoY_mar_apr,
         own_income_no_mil_change_YoY_mar_may,
         own_income_no_mil_change_YoY_jun_aug,
         own_income_no_mil_change_YoY_jul_sep,
         own_income_no_mil_change_YoY_oct_jan,
         own_income_no_mil_change_YoY_oct_dec,
         own_income_no_mil_change_YoY_may_feb,
         own_income_no_mil_change_YoY_adapt,
         income_own_full_year,
         own_income_prop_full_year)

q <- c %>% filter(year==2022) %>%
  group_by() %>%
  summarise(own_income_no_mil_change_YoY_jan_feb = mean(own_income_no_mil_change_YoY_jan_feb),
            own_income_no_mil_change_YoY_mar_apr = mean(own_income_no_mil_change_YoY_mar_apr),
            own_income_no_mil_change_YoY_mar_may = mean(own_income_no_mil_change_YoY_mar_may),
            own_income_no_mil_change_YoY_jun_aug = mean(own_income_no_mil_change_YoY_jun_aug),
            own_income_no_mil_change_YoY_jul_sep = mean(own_income_no_mil_change_YoY_jul_sep),
            own_income_no_mil_change_YoY_adapt = mean(own_income_no_mil_change_YoY_adapt))

ds_5_add <- ds3 %>% 
  mutate(revenue_total = rowSums(across(starts_with(c('1','2', '3', '4', '5'))), na.rm = TRUE),
                           revenue_own = revenue_total - rowSums(across(starts_with(c('4'))), na.rm = TRUE),
         revenue_military_tax = case_when(is.na(`11010200`)~0, TRUE ~ `11010200`),
         revenue_corporate_tax = case_when(is.na(`11020200`)~0, TRUE ~ `11020200`),
         revenue_own_no_mil_tax = revenue_own - revenue_military_tax,
         revenue_own_no_mil_tax_corp_tax = revenue_own - revenue_military_tax - revenue_corporate_tax) %>%
  select(c("hromada_code",
           "budget_name",
           "oblast_name",
           "raion_name",
           "year",
           "month",
           "revenue_total",
           "revenue_own",
           "revenue_military_tax",
           "revenue_own_no_mil_tax",
           "revenue_own_no_mil_tax_corp_tax"))

CPI_path <- "./data-public/raw/CPI_region_monthly.xlsx"
CPI <- readxl::read_xlsx(CPI_path)

CPI <- CPI %>% mutate(year = as.character(year),
                      month = as.character(month))

ds_5_add_CPI <- ds_5_add %>%
  left_join(CPI, by = c("oblast_name","year", "month")) %>%
  mutate(revenue_total_const = case_when(revenue_total >= 0 ~ (revenue_total / CPI_index_base_2021_1),
                                         revenue_total < 0 ~ revenue_total),
         revenue_own_const = case_when(revenue_own >= 0 ~ (revenue_own / CPI_index_base_2021_1),
                                       revenue_own < 0 ~ revenue_own),
         revenue_military_tax_const = case_when(revenue_military_tax >= 0 ~ (revenue_military_tax / CPI_index_base_2021_1),
                                                revenue_military_tax < 0 ~ revenue_military_tax),
         revenue_own_no_mil_tax_const = case_when(revenue_own_no_mil_tax >= 0 ~ (revenue_own_no_mil_tax / CPI_index_base_2021_1),
                                                  revenue_own_no_mil_tax < 0 ~ revenue_own_no_mil_tax),
         revenue_own_no_mil_tax_corp_tax_const = case_when(revenue_own_no_mil_tax_corp_tax >= 0 ~ (revenue_own_no_mil_tax_corp_tax / CPI_index_base_2021_1),
                                                               revenue_own_no_mil_tax_corp_tax < 0 ~ revenue_own_no_mil_tax_corp_tax))


ds_5_add_CPI <- ds_5_add_CPI %>% mutate(month = as.numeric(month),
                                        year = as.numeric(year))

average_change_rates <- ds_5_add_CPI %>%
  filter(year == 2022 & month >= 4 | year == 2023 & month <= 3) %>%
  group_by(hromada_code) %>%
  summarize(average_change = mean((revenue_own_no_mil_tax_const - lag(revenue_own_no_mil_tax_const)) / lag(revenue_own_no_mil_tax_const), na.rm = TRUE)) %>%
  mutate(average_change = replace(average_change, is.infinite(average_change) | average_change > 0.2, 0.2))

# Create a data frame with monthly projections for April 2023 to December 2024
ds_5_add_CPI_short <- ds_5_add_CPI %>% select(c("hromada_code",
                                                "year",
                                                "month",
                                                "revenue_own_no_mil_tax_const"))




projection_df <- ds_5_add_CPI_short %>%
  filter(year == 2023 & month == 3) %>%
  bind_rows(
    expand.grid(
      hromada_code = unique(ds_5_add_CPI_short$hromada_code),
      month = 1:12,
      year = 2023:2024,
      revenue_own_no_mil_tax_const = 0
    ) %>%
      filter(!(year == 2023 & month <= 3))
  )


# Join the projection data frame with the average change rates to calculate the projected revenues
options(scipen=999)
projected_revenues <- projection_df %>%
  left_join( average_change_rates, by = "hromada_code") %>%
  group_by(hromada_code) %>%
  mutate(revenue_own_no_mil_tax_const = revenue_own_no_mil_tax_const[1] * cumprod(1 + average_change),
         year = as.integer(year),
         month = as.integer(month)) %>%
  select(hromada_code, month, year, revenue_own_no_mil_tax_const)%>%
  filter(!(year == 2023 & month <= 3))

ds_5_add_CPI_1 <- rbind(ds_5_add_CPI_short, projected_revenues)

# d_5_add_CPI_dist <- ds_5_add_CPI %>%
#   group_by(hromada_code, month) %>%
#   filter((year == 2022 & month >= 3 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) |
#            (year == 2023 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021])) %>%
#   group_by(hromada_code) %>%
#   slice_head(n = 1) %>%
#   ungroup() %>%
#   mutate(month_distance = ifelse(year %in% c(2022, 2023), (year - 2022) * 12 + month - 3, NA)) %>%
#   select(c("hromada_code",
#            "month_distance"))


d_5_add_CPI_dist <- ds_5_add_CPI_1 %>%
  group_by(hromada_code, month) %>%
  filter((year == 2022 & month >= 3 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) |
           (year == 2023 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) |
           (year == 2024 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021])) %>%
  group_by(hromada_code) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(recovery_month_distance = ifelse(year %in% c(2022, 2023, 2024), (year - 2022) * 12 + month - 3, NA)) %>%
  select(hromada_code, recovery_month_distance)

d_5_add_CPI_dist_alt <- ds_5_add_CPI_1 %>%
  filter(year >= 2021) %>%
  group_by(hromada_code) %>%
  mutate(recovery_month_distance_2 = ifelse(
    (year == 2022 & month >= 3 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) &
      ((lead(revenue_own_no_mil_tax_const, 1) > revenue_own_no_mil_tax_const) &
         (lead(revenue_own_no_mil_tax_const, 2) > revenue_own_no_mil_tax_const)),
    (year - 2022) * 12 + month - 3,
    NA
  )) %>%
  filter(!is.na(recovery_month_distance_2)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(hromada_code, recovery_month_distance_2)


d_5_add_CPI_dist_add <- ds_5_add_CPI_short %>%
  group_by(hromada_code, month) %>%
  filter((year == 2022 & month >= 3 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) |
           (year == 2023 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021]) |
           (year == 2024 & revenue_own_no_mil_tax_const > revenue_own_no_mil_tax_const[year == 2021])) %>%
  group_by(hromada_code) %>% summarise(count_recovery = n())





# View the result

ds5_final <- ds5_long %>%
  left_join(c, by = c("budget_code","year")) %>%
  left_join(d_5_add_CPI_dist, by = "hromada_code") %>%
  mutate(diversification_income_score = corporate_tax_own_prop^2 +
           parking_fee_own_prop^2 +
           tourist_fee_own_prop^2 +
           eco_tax_own_prop^2 +
           non_tax_own_prop^2 +
           capital_proceedings_own_prop^2 +
           special_funds_own_prop^2 +
           excise_duty_own_prop^2 +
           pdfo_own_prop^2 +
           unified_tax_own_prop^2 +
           property_tax_own_prop^2
           ) %>%
  left_join(d_5_add_CPI_dist_add, by = "hromada_code") %>%
  left_join(d_5_add_CPI_dist_alt, by = "hromada_code") %>%
  mutate(count_recovery = case_when(!is.na(count_recovery) ~ count_recovery,
                                     is.na(count_recovery) ~ 0))
  

cases_up_down <- ds_5_add_CPI_short %>%
  group_by(hromada_code, year, month) %>%
  summarize(revenue_own_no_mil_tax_const = sum(revenue_own_no_mil_tax_const)) %>%
  arrange(hromada_code, year, month) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(prev_revenue = lag(revenue_own_no_mil_tax_const)) %>%
  group_by(hromada_code) %>%
  mutate(
    prev_revenue = ifelse(is.na(prev_revenue) & month == 1, lag(prev_revenue, n = 12), prev_revenue),
    prev_revenue = ifelse(is.na(prev_revenue), lag(prev_revenue, default = last(prev_revenue)), prev_revenue),
    is_above_prev_year = ifelse(revenue_own_no_mil_tax_const > prev_revenue, 1, 0),
    is_below_prev_year = ifelse(revenue_own_no_mil_tax_const < prev_revenue, 1, 0),
    is_prev_above_curr_below = lag(is_above_prev_year) == 1 & is_below_prev_year == 1
  ) %>%
  filter(is_prev_above_curr_below) %>%
  summarize(num_unique_budgets = n_distinct(hromada_code))

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
dataset_names_dis <- list('Data' = ds3, 'Metadata' = metadata_dis)

dataset_names <- list('Data' = ds5_final, 'Metadata' = metadata)

library(openxlsx)

openxlsx::write.xlsx(dataset_names_dis, './data-public/derived/hromada_budget_2020_2022_taxes.xlsx')
openxlsx::write.xlsx(dataset_names, './data-public/derived/hromada_budget_2020_2022.xlsx')
readr::write_csv(ds5_final, './data-public/derived/hromada_budget_2020_2022.csv')

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


