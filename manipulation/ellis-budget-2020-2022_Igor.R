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

#+ tweak-data-5-old ---- 
# `5` because it produces `ds5` form
ds4_long <- 
  ds3_long %>% 
  mutate(
    transfert = str_detect(tax_code, "^4.+")
    ,target_segment = month %in% c(3:9) & year %in% c(2021, 2022)
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
    ,total_income_change_net = income_total - lag(income_total)
    ,total_income_change = round((income_total / lag(income_total)) - 1,2)
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

metadata <- data.frame(variables, description)


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
  mutate(budget_code = paste0(budget_code,"0")) %>%
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

ds5_former <- 
  ds5_long %>% 
  mutate(
    target_segment = month %in% c(3:9) & year %in% c(2021, 2022) # i think
  ) %>% 
  filter(target_segment) %>% 
  group_by_at(grouping_stem) %>% 
  summarize(
    across(
      .cols = metrics
      ,.fns  = ~sum(.,na.rm=T) 
    )
  ) %>% 
  ungroup() %>% 
  glimpse()

a <- ds5_long_temp %>% 
  filter(year!=2020) %>% 
  group_by(budget_code, month) %>%
  mutate(income_own_no_mil_change_YoY = ((income_own - income_military) -
                                           (income_own[year==2021] - income_military[year==2021])),
         income_own_no_military_tax = income_own - income_military)

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
         own_income_no_mil_change_YoY_mar_apr = ((((income_own_no_military_tax[month=="3"&year==2022]+
                                                      income_own_no_military_tax[month=="4"&year==2022]) / 
                                                     (income_own_no_military_tax[month=="3"&year==2021]+
                                                        income_own_no_military_tax[month=="4"&year==2021]))-1)*100),
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
         own_income_no_mil_change_YoY_adapt = own_income_no_mil_change_YoY_jul_sep - own_income_no_mil_change_YoY_mar_apr,
         income_own_full_year = income_own,
         own_income_prop_full_year = own_income_prop)

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

ds5_final <- ds5_long %>%
  left_join(c, by = c("budget_code","year"))

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
dataset_names_dis <- list('Data' = ds3, 'Metadata' = metadata_dis)

dataset_names <- list('Data' = ds5_final, 'Metadata' = metadata)

library(openxlsx)

openxlsx::write.xlsx(dataset_names_dis, './data-public/derived/hromada_budget_2020_2022_taxes.xlsx')
openxlsx::write.xlsx(dataset_names, 'C:/GitHub/ua-de-center/data-public/derived/hromada_budget_2020_2022.xlsx')
readr::write_csv(ds5_final, 'C:/GitHub/ua-de-center/data-public/derived/hromada_budget_2020_2022.csv')

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


