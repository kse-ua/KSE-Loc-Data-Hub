
#' ---
#' title: "Ellis Education Institutions"
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

#+ load-data, eval=eval_chunks ----------------------------------------------

# Data from register of educational institutions
# https://registry.edbo.gov.ua/opendata/institutions/

#General education institutions
path_gen_education <- "./data-public/raw/education/General_secondary_education_institutions_08.06.2023.xlsx"
#Higher education institutions
path_higher_education <- "./data-public/raw/education/institutions_higher_education_08.06.2023.xlsx"
#Professional education institutions
path_prof_education <- "./data-public/raw/education/Institutions_professional_education_08.06.2023.xlsx"
#Professional higher education institutions
path_high_prof_education <- "./data-public/raw/education/Institutions_professional_higher_education_08.06.2023.xlsx"
#Postgraduate education institutions
path_postgrad_education <- "./data-public/raw/education/Postgraduate_education_institutions_08.06.2023.xlsx"
#Science  institutions
path_science_education <- "./data-public/raw/education/science_institutes_08.06.2023.xlsx"

# Admin data for settlements codes
ds_admin_map <- readr::read_csv("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/ua-admin-map-2020.csv")


#+ General education -----------------------------------------------------------

ds_gen_educ <- readxl::read_xlsx(path_gen_education)

colnames(ds_gen_educ) <- c('full_name', "edebo", "ouo_validated", "short_name", 
                           "status", "type", "property_type", "koatuu", "oblast", "settlement", 
                           "address", "main_institution", "managing_body", "phone", 
                           "fax", "mail", "site", "director", "is_core", "is_rural", 
                           "is_mountain", "is_internat", "licensed_volume")

ds1 <- ds_gen_educ %>% 
  left_join(
    ds_admin_map %>% 
      distinct(settlement_code_old, settlement_name, settlement_code, hromada_name,
             hromada_code, raion_name, raion_code, oblast_name, oblast_code),
    by = c("koatuu" = "settlement_code_old"), multiple = 'all') %>% 
  filter(settlement_name != "Ценжів" | is.na(settlement_name))

# identifying kyiv
pattern <- "^80"

ds1 <- ds1 %>% 
  mutate(match_status_koatuu = ifelse(is.na(settlement_name), 'unmatched', 'matched')) %>% 
  mutate(settlement_name = case_when(grepl(pattern, koatuu) ~ 'Київ', 
                                     .default = settlement_name),
         settlement_code = case_when(grepl(pattern, koatuu) ~ 'UA80000000000093317', 
                                     .default = settlement_code),
         hromada_name = case_when(grepl(pattern, koatuu) ~ 'Київ', 
                                     .default = hromada_name),
         hromada_code = case_when(grepl(pattern, koatuu) ~ 'UA80000000000093317', 
                                     .default = hromada_code))

# dataset with unmatched koatuu
unmatched_koatuu <- ds1 %>%  
  filter(match_status_koatuu == 'unmatched') %>% 
  distinct(koatuu)

# 30% unmatched - sad!
nrow(ds1[ds1$match_status_koatuu == "unmatched",])/nrow(ds1[ds1$match_status_koatuu == "matched",])

# unmatched schools
unmatched_schools <- ds1 %>% filter(match_status_koatuu == "unmatched")

#+ matching unmatching cases ---------------------------------------------------
# matching cities
# filter only cities
city_schools <- unmatched_schools %>% filter(!grepl("с\\. |с-ще|смт", settlement)) %>% 
  mutate(settlement_new = str_extract(settlement, "^[^,]+"),
         oblast_new = str_extract(oblast, "^[^\\s]+"),
         key_code = paste0(settlement_new, oblast_new),
         key_code = str_replace_all(key_code, "'", "ʼ"))

ds_admin_city <- ds_admin_map %>% filter(settlement_type == 'місто') %>% 
  filter(oblast_name_en != 'Crimea') %>% 
  mutate(key_code = paste0(settlement_name, oblast_name),
         key_code = case_when(key_code == "Кам’янськеДніпропетровська" ~ "КамʼянськеДніпропетровська",
                              .default = key_code))

matched_schools_city <- city_schools %>% select(-c(24:31)) %>% 
  left_join(
    ds_admin_city %>% 
      distinct(settlement_name, settlement_code, hromada_name,
               hromada_code, raion_name, raion_code, oblast_name, oblast_code, key_code),
    by = c("key_code"))

# matching villages

village_schools <- unmatched_schools %>% filter(grepl("с\\. |с-ще |смт ", settlement)) %>% 
  mutate(settlement_new = str_extract(settlement, "(?<=\\s)[^,]+"),
         oblast_new = str_extract(oblast, "^[^\\s]+"),
         raion_new = str_extract(settlement, "(?<=,\\s)([^,]+)(?=\\s)"),
         settlement_type = case_when(grepl("с\\.", settlement) ~ 'село',
                          grepl("смт", settlement) ~ 'селище міського типу',
                          grepl("с-ще", settlement) ~ 'селище'),
         key_code = paste0(settlement_new, settlement_type, raion_new, oblast_new))

ds_admin_villages <- ds_admin_map %>% filter(settlement_type != 'місто') %>% 
  filter(oblast_name_en != 'Crimea') %>% 
  mutate(key_code = paste0(settlement_name, settlement_type, raion_name, oblast_name))


duplicates <- ds_admin_villages %>%
  mutate(n = n(), .by = key_code) %>% 
  filter(n>1) %>% 
  arrange(key_code)


matched_schools_villages <- village_schools %>% select(-c(24:31)) %>% 
  left_join(
    ds_admin_villages %>% 
      distinct(settlement_name, settlement_code, hromada_name,
               hromada_code, raion_name, raion_code, oblast_name, oblast_code, key_code),
    by = c("key_code"))

write.csv(unmatched_schools, 'unmatched_schools.csv')
