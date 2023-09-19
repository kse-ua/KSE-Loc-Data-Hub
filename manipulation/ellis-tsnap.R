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

path_tsnap   <- "~/Documents/ua-de-center/data-public/raw/rejestr-cnap.xlsx"


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------

ds0 <- readxl::read_excel(path_tsnap,skip = 1)

#main admin dataset
ds_admin <- readr::read_csv("~/Documents/ua-de-center/data-public/derived/ua-admin-map-2020.csv")

#hromada dataset
ds_hromada <- readr::read_csv("~/Documents/ua-de-center/data-public/derived/ua-admin-hromada.csv")

#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------
#make metadata object

metadata <- readxl::read_excel(path_tsnap, n_max = 1) %>% 
  t() 

# BIG DATA ORGANISING

# Part 1 - extracting from address + getting rid of Kyiv
ds1 <- ds0 %>% 
  mutate(across(c(name, address), ~gsub("'", "’", .))) %>% # replacing for better processing
  mutate(
    hromada_name = str_extract(address, "((\\p{L}+|[’'-]\\p{L}+)(?=\\s+територіальна громада)|'[’'](\\p{L}+\\s+центр послуг)\\s+м.\\p{L}+)")
    ,settlement = str_extract(address, "(м\\.|с\\.|смт)\\s+([\\w’'-]+|м\\.[\\w’'-]+)")
    ,council = str_extract(name, "([а-яА-ЯіІїЇєЄґҐ’'-]+)\\s+(селищної|сільської|міської)")
    ,oblast_name = str_extract(address, "[а-яА-ЯіІїЇєЄґҐ’'-]+(?= область)")
  ) %>% 
  mutate(oblast_name = case_when(
    settlement == "м. Київ" ~ "м.Київ",
    TRUE ~ oblast_name),
    hromada_name = case_when(
      settlement == "м. Київ" ~ "м.Київ", 
      TRUE ~ hromada_name))

ds1 %>% filter(is.na(hromada_name)) %>% view()
# 527
ds1 %>% filter(is.na(settlement), is.na(hromada_name)) %>% view()
# 527

#Part 2 getting more settlements
ds1 <- ds1 %>%
  mutate(settlement = case_when(
    is.na(settlement) ~ str_extract(name, "(м\\.|с\\.|смт)\\s+([\\w’'-]+|м\\.[\\w’'-]+)"),
    TRUE ~ settlement)) %>%
  mutate(settlement = case_when(
    is.na(settlement) ~ str_extract(name, "(?<=с\\.)\\w+"), 
    TRUE ~ settlement)) %>%
  select(code, name, address, formation_form, workers_total, square_total, 
         state, hromada_name, settlement, council, oblast_name)

ds1 %>% filter(is.na(settlement), is.na(hromada_name)) %>% view()
#313

# Part 3 - Finding hromada by council 

# 3.1 - getting council parts from main dataset
ds_council <- ds_admin %>%
  mutate(council = str_extract(full_name, "([а-яА-ЯіІїЇєЄґҐ’'-]+)\\s+(сільради|сільської|міської|селищної)")
         ,council = str_replace(council, "сільради", "сільської")) %>%
  mutate(council = gsub("'", "’", council)) %>% 
  select(council, oblast_name, hromada_code, hromada_name)#

# 3.2 - getting rid of those council+oblast that duplicates
ommit_hromada <- ds_council %>% 
  distinct(council, oblast_name, hromada_code) %>% 
  group_by(council, oblast_name) %>% count() %>%
  filter(n > 1)
ds_council <- ds_council %>% anti_join(ommit_hromada, by = c("council", "oblast_name"))
         
# 3.3 - finally joining all of those that corresponds
joining_hromada <- ds1 %>% filter(is.na(hromada_name)) %>%
  merge(ds_council, by = c("council", "oblast_name")) %>%
  select(code, name, address, formation_form, workers_total, square_total, 
         state, hromada_name.y, settlement, council, oblast_name, code) %>%
  rename(hromada_name = hromada_name.y) %>%
  distinct(code, .keep_all = TRUE) %>%

# i did smth wrong cause it shouldn't be with distinct but i'm too tired to dig more

ds2 <- ds1 %>% anti_join(joining_hromada, by = "code") %>%
  bind_rows(joining_hromada)

ds2 %>% filter(is.na(hromada_name)) %>% view()
# 156 remained NA

# Part 4 - joining by settlement admin ' j ’

# 4.1 cleaning ds2
joining_settlement <- ds2 %>% filter(is.na(hromada_name)) %>%
  mutate(settlement_name = gsub("^(\\s*с\\.\\s*|\\s*м\\.\\s*|\\s*смт\\s*)", "", settlement)) %>%
  select(code, name, address, formation_form, workers_total, square_total, 
         state, hromada_name, settlement_name, council, oblast_name)

# 4.2 - getting rid of those settlement+oblast that duplicates
ommit_settlemet <- ds_admin %>% 
  distinct(settlement_name, oblast_name, hromada_code) %>% 
  group_by(settlement_name, oblast_name) %>% count() %>%
  filter(n > 1)

ds_settlement <- ds_admin %>% anti_join(ommit_settlemet, by = c("settlement_name", "oblast_name"))

# 4.3 - joining
joining_settlement <- joining_settlement %>% 
  merge(ds_settlement, by = c("settlement_name", "oblast_name")) %>%
  select(code, name, address, formation_form, workers_total, square_total, 
       state, hromada_name.y, settlement_name, council, oblast_name, code) %>%
  rename(hromada_name = hromada_name.y) %>%
  distinct(code, .keep_all = TRUE) 

ds3 <- ds2 %>% 
  rename(settlement_name = settlement) %>% 
  anti_join(joining_settlement, by = "code") %>%
  bind_rows(joining_settlement)
# finally agregating 

ds3 %>% filter(is.na(hromada_name)) %>% view()

# 90 are still with NA

# n in hromada 
#without 527 rows
ready <- ds3 %>% filter(!is.na(hromada_name)) %>% 
  group_by(hromada_name) %>% 
  summarise(workers_total = sum(workers_total), 
            square_total = sum(square_total)) %>%
  left_join(ds_hromada, by = "hromada_name") # if no data put 0

ready %>% filter(is.na(hromada_code)) %>% view() 
# 33 з проблемами в апострофвх і тире знайти через ni


# not informative, maybe replacing with our values - цнап чи відокремлене місце 
formation_form <- ds3 %>% group_by(hromada_name, formation_form) %>%
  count() %>% pivot_wider(names_from = formation_form, values_from = n)
# зробити структурний підрозділ + постійно діючий робочий орган + відалене робоче місце