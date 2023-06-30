
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
library(sf) #geospatial operations
library(tmap) #visualisation on maps

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

# #load polygons of hromadas
polygons <-  st_read("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/shapefiles/admin/terhromad_fin.geojson") %>%
  janitor::clean_names()

#+ General education -----------------------------------------------------------

ds_gen_educ <- readxl::read_xlsx(path_gen_education)

colnames(ds_gen_educ) <- c('full_name', "edebo", "ouo_validated", "short_name", 
                           "status", "type", "property_type", "koatuu", "oblast", "settlement", 
                           "address", "main_institution", "managing_body", "phone", 
                           "fax", "mail", "site", "director", "is_core", "is_rural", 
                           "is_mountain", "is_internat", "licensed_volume")

#

ds1 <- ds_gen_educ %>% 
  mutate(location = paste(oblast, settlement, address, sep = ', '))









#

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
                                     .default = hromada_code),
         match_status_koatuu = case_when(settlement_name == "Київ" ~ 'matched', 
                                         .default = match_status_koatuu))

matched_schools <- ds1 %>% filter(match_status_koatuu == "matched")


  
# dataset with unmatched koatuu
unmatched_koatuu <- ds1 %>%  
  filter(match_status_koatuu == 'unmatched') %>% 
  distinct(koatuu)

# 24% unmatched - sad!
nrow(ds1[ds1$match_status_koatuu == "unmatched",])/nrow(ds1)

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

# check
matched_schools_city %>% filter(is.na(hromada_name))


# matching villages

village_schools <- unmatched_schools %>% 
  select(-c(settlement_name, settlement_code, hromada_name,
         hromada_code, raion_name, raion_code, oblast_name, oblast_code)) %>% 
  filter(grepl("с\\. |с-ще |смт ", settlement)) %>% 
  mutate(settlement_new = str_extract(settlement, "(?<=\\s)[^,]+"),
         oblast_new = str_extract(oblast, "^[^\\s]+"),
         raion_new = str_extract(settlement, "(?<=,\\s)([^,]+)(?=\\s)"),
         raion_new = gsub("'", "’", raion_new),
         settlement_type = case_when(grepl("с\\.", settlement) ~ 'село',
                          grepl("смт", settlement) ~ 'селище міського типу',
                          grepl("с-ще", settlement) ~ 'селище'),
         key_code = paste0(raion_new, oblast_new))

ds_admin_villages <- ds_admin_map %>% filter(settlement_type != 'місто') %>% 
  filter(oblast_name_en != 'Crimea') %>% 
  mutate(key_code = paste0(settlement_name, settlement_type, raion_name, oblast_name))

ds_admin_raion <- ds_admin_map %>% filter(oblast_name_en != 'Crimea') %>% 
  mutate(key_code = paste0(raion_name, oblast_name)) %>% 
  distinct(raion_name, raion_code, oblast_name, 
           oblast_code, key_code)

village_schools %>% 
  distinct(key_code) %>% 
  filter(grepl("’", key_code)) %>% select(key_code)

# Old raions in schools dataset!!!
old_raions <- village_schools %>% 
  anti_join(ds_admin_raion,
            by = c("key_code")) %>% 
  distinct(oblast_new, raion_new, key_code)

# reading file with old and new names comparison
raions <- readxl::read_excel("raions.xlsx")

d <- village_schools %>%
  rename(raion_old = raion_new) %>%
  left_join(raions %>% select(key_code, raion_new),
            by = 'key_code') %>% 
  mutate(raion_old = case_when(!is.na(raion_new) ~ raion_new,
                               .default = raion_old),
         raion_old = gsub("'", "’", raion_old),
         key_code = paste0(raion_old, oblast_new))

village_schools_raions <- d %>% 
  left_join(
    ds_admin_raion,
    by = c("raion_old" = 'raion_name', 'oblast_new' = 'oblast_name')) %>% 
  mutate(settlement_new = gsub("'", "’", settlement_new),
         key_code = paste0(settlement_new, settlement_type, raion_old, oblast_new))

ds_admin_villages_hromadas_unique <- ds_admin_villages %>% 
  distinct(hromada_code, hromada_name, raion_name, raion_code, oblast_name, 
           oblast_code, key_code) %>% 
  mutate(n = n(), .by = c('key_code')) %>% 
  filter(n == 1) %>% 
  arrange(key_code) %>% 
  select(-n)

matched_schools_villages <- 
  village_schools_raions %>%
  rename(raion_name = raion_old, oblast_name = oblast_new) %>% 
  left_join(
    ds_admin_villages_hromadas_unique %>% select(hromada_name, hromada_code, key_code),
    by = c("key_code")) %>% 
  mutate(match_status_hromadas = ifelse(is.na(hromada_code), 'unmatched', 'matched'))
  
unmatched_schools_villages <- matched_schools_villages %>% 
  filter(match_status_hromadas == 'unmatched') %>% 
  select(-c(26:31))

# merging it all togetherіі
colnames(matched_schools)
colnames(matched_schools_city)
colnames(matched_schools_villages)

d1 <- matched_schools %>% 
  select(-c(settlement_code))
  
d2 <- matched_schools_city %>% 
  select(-c(settlement_new, oblast_new, key_code, settlement_code))

d3 <- matched_schools_villages %>% 
  rename(settlement_name = settlement_new) %>% 
  select(-c(key_code, match_status_hromadas, key_code.x, key_code.y, 
            raion_new, settlement_type))

fin <- rbind(d1, d2, d3)  

# 2% unmatched - great!
nrow(fin[is.na(fin$hromada_code),])/nrow(fin)

write.csv(fin, './data-private/derived/matched_schools.csv')

# 


