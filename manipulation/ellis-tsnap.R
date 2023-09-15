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
  
ds1 <- ds0 %>% 
  mutate(
    hromada_name = str_extract(gsub("[\\'’]", "’", gsub("\\([^()]*\\)", "", address)),
                          "((\\p{L}+|\\p{L}+’\\p{L}+)(?=\\s+територіальна громада)|'(\\p{L}+\\s+центр послуг)\\s+м.\\p{L}+)")
    ,settlement = str_extract(gsub("[\\'’]", "’", gsub("\\([^()]*\\)", "", address)), "(м\\.|с\\.|смт)\\s+(\\w+|м\\.[[:alpha:]]+)")
    ,council = str_extract(name, "([а-яА-ЯіІїЇєЄґҐ’]+)\\s+(селищної ради|сільської ради|міської ради)")
    ,oblast = str_extract(address, "[а-яА-ЯіІїЇєЄґҐ-]+(?= область)")
  ) %>% 
  mutate(settlement = case_when(
    is.na(settlement) ~ str_extract(name, "(м\\.|с\\.|смт\\.міста)\\s+(\\w+)"),
    TRUE ~ settlement
  )) %>%
  select(name, address, formation_form, workers_total, square_total, 
         state, hromada_name, settlement, council, oblast)

ds1 %>% filter(is.na(hromada), is.na(settlement)) %>% view()


# from councils and settlements
joining_hromada <- ds1 %>% filter(is.na(hromada)) %>%
  mutate(hromada = case_when(
    council %in% ds_admin$full_name ~ ds_admin$hromada_name, # not working
    settlement %in% ds_admin$settlement_name ~ ds_admin$hromada_name, 
    TRUE ~ "я не знаю")
  )


#without 527 rows
ready <- ds1 %>% filter(!is.na(hromada_name)) %>% 
  group_by(hromada_name) %>% 
  summarise(workers_total = sum(workers_total), 
            square_total = sum(square_total)) %>%
  left_join(ds_hromada, by = "hromada_name")

ready %>% filter(is.na(raion_name))%>% view()

# not informative, maybe replacing with our values?
formation_form <- ds1 %>% group_by(hromada, formation_form) %>%
  count() %>% pivot_wider(names_from = formation_form, values_from = n)
