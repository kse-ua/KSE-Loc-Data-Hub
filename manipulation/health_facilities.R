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
library(stringr)


#+ declare-globals -------------------------------------------------------------

path_tsnap   <- "./data-public/raw/rejestr-cnap.xlsx"

cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

#health facilities datasets

pmg_1 <- read.csv("./ua-de-center/data-public/raw/pmg_legal_entity_divisions_info.csv")
pmg_2 <- read.csv("./ua-de-center/data-public/raw/pmg_legal_entity_info.csv")

#main admin dataset
ds_admin <- read_csv("ua-de-center/data-public/derived/ua-admin-map-2020.csv") %>% 
  select(settlement_name, hromada_name, raion_name, oblast_name)
  
# clearing the data
  
pmg_1 <- pmg_1 %>% 
  mutate(settlement_name = gsub("'", "’", 
                                gsub("\\([^()]*\\)", "", str_to_title(residence_settlement))), 
         oblast_name = case_when(
           residence_area == "М.КИЇВ" ~ "м.Київ",
           TRUE ~ str_to_title(residence_area)), 
         settlement_name = case_when(
           settlement_name == "Мукачеве" ~ "Мукачево",
           settlement_name == "Звягель" ~ "Новоград-Волинський", 
           settlement_name == "Володимир-Волинський" ~ "Володимир", 
           TRUE ~ settlement_name
         )) %>%
  select(division_type, settlement_name, division_name, residence_settlement_type, 
         oblast_name) 

# joining the data
pmg_1 <- pmg_1 %>%
  left_join(ds_admin, by = c("settlement_name", "oblast_name")) %>%
  mutate(oblast_name = case_when(
    settlement_name == "Київ" ~ "м.Київ",
    TRUE ~ oblast_name),
    hromada_name = case_when(
      settlement_name == "Київ" ~ "м.Київ", 
      TRUE ~ hromada_name)) %>%
  na.omit(hromada_name)

# Row 17 of `x` matches multiple rows in `y`. - idk how crucial


pmg_2 <- pmg_2 %>% 
  mutate(settlement_name = gsub("'", "’", 
                                gsub("\\([^()]*\\)", "", str_to_title(registration_settlement))), 
         oblast_name = str_to_title(registration_area,), 
         settlement_name = case_when(
           settlement_name == "Мукачеве" ~ "Мукачево",
           settlement_name == "Звягель" ~ "Новоград-Волинський", 
           settlement_name == "Володимир-Волинський" ~ "Володимир", 
           settlement_name == "Кіровоград" ~ "Кропивницький",
           TRUE ~ settlement_name)) %>%
  select(legal_entity_name, care_type, settlement_name, property_type,
         oblast_name)

pmg_2 <- pmg_2 %>%
  left_join(ds_admin, by = c("settlement_name", "oblast_name")) %>%
  mutate(oblast_name = case_when(
    settlement_name == "Київ" ~ "м.Київ",
    TRUE ~ oblast_name), 
    hromada_name = case_when(
      settlement_name == "Київ" ~ "м.Київ", 
      TRUE ~ hromada_name)) %>%
  na.omit(hromada_name)

cat("\n# 3.Grouping ")
# grouping for types of facility
division_col <- pmg_1 %>% 
  group_by(hromada_name, division_type) %>%
  count() %>%
  pivot_wider(names_from = division_type, values_from = n)

# grouping for placing of the facility
resid_col <- pmg_1 %>% 
  group_by(hromada_name, residence_settlement_type) %>%
  count() %>%
  pivot_wider(names_from = residence_settlement_type, values_from = n)

# grouping for care level of facility
care_col <- pmg_2 %>% 
  group_by(hromada_name, care_type) %>%
  count() %>%
  pivot_wider(names_from = care_type, values_from = n)

# grouping for ownership
property_col <- pmg_2 %>% 
  group_by(hromada_name, property_type) %>%
  count() %>%
  pivot_wider(names_from = property_type, values_from = n)

cat("\n# 4.Final combination")
health_property <- inner_join(division_col, resid_col, by = "hromada_name") %>%
  inner_join(care_col, by = "hromada_name") %>%
  inner_join(property_col, by = "hromada_name") %>%
  mutate_all(~ifelse(is.na(.), 0, .))


file_path <- "./ua-de-center/data-public/derived/health_facilities.csv"

# Save the dataset to a CSV file
write.csv(health_property, file = file_path, row.names = FALSE)
