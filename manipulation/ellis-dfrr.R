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

# Define utils
'%ni%' <- Negate(`%in%`)
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "n/a", "NOt available", '<NA>')

#+ load-packages -----------------------------------------------------------
library(tidyverse)


#+ declare-globals -------------------------------------------------------------
names_dfrr2021 <- c(
  "oblast_code"
  ,"description"
  ,"budget_plan"
  ,"budget_opened"
  ,"budget_executed"
  ,"debt"
  ,"completed"
)

names_dfrr2020 <- c(
  "oblast_code"
  ,"id"
  ,"description"
  ,"budget_plan"
  ,"quantity"
  ,"completed"
  ,"budget_executed"
  ,"sphere"
  ,"work_type"
  ,"rkmu"
  ,"note"
)

names_dfrr2019 <- c(
  "oblast_code"
  ,"id"
  ,"description"
  ,"timeline"
  ,"budget_plan"
  ,"budget_plan_common"
  ,"budget_plan_special"
  ,"budget_executed"
  ,"budget_executed_common"
  ,"budget_executed_special"
  ,"quantity"
  ,"completed"
  ,"note"
)

names_dfrr2018<- c(
  "oblast_code"
  ,"description"
  ,"timeline"
  ,"budget_plan_common"
  ,"budget_plan_special"
  ,"budget_executed_common"
  ,"budget_executed_special"
  ,"quantity"
  ,"completed"
)

names_dfrr2017<- c(
  "oblast_code"
  ,"id"
  ,"description"
  ,"timeline"
  ,"budget_plan_common"
  ,"budget_plan_special"
  ,"budget_opened_common"
  ,"budget_opened_special"
  ,"budget_executed_common"
  ,"budget_executed_special"
  ,"budget_actual_common"
  ,"budget_actual_special"
  ,"debt_common"
  ,"debt_special"
  ,"date"
  ,"act_date"
  ,"certificate_date"
)

names_dfrr2016<- c(
  "oblast_code"
  ,"id"
  ,"description"
  ,"timeline"
  ,"budget_plan"
  ,"budget_opened"
  ,"budget_received"
  ,"budget_executed"
  ,"budget_actual"
  ,"debt"
  ,"date"
  ,"act_date"
  ,"certificate_date"
)

names_dfrr2015<- c(
  "oblast_code"
  ,"id"
  ,"description"
  ,"timeline"
  ,"budget_plan"
  ,"budget_opened"
  ,"budget_received"
  ,"budget_executed"
  ,"budget_executed_debt"
  ,"date"
  ,"act_date"
  ,"certificate_date"
)

path_dfrr2021   <- "./data-private/raw/dfrr-2021.xlsx"
path_dfrr2020   <- "./data-private/raw/dfrr-2020.xlsx"
path_dfrr2019   <- "./data-private/raw/dfrr-2019.xlsx"
path_dfrr2018   <- "./data-private/raw/dfrr-2018.xlsx"
path_dfrr2017   <- "./data-private/raw/dfrr-2017.xlsx"
path_dfrr2016   <- "./data-private/raw/dfrr-2016.xlsx"
path_dfrr2015   <- "./data-private/raw/dfrr-2015.xlsx"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
ds21_0 <- readxl::read_excel(path_dfrr2021, col_names = names_dfrr2021, skip=9)
ds20_0 <- readxl::read_excel(path_dfrr2020, col_names = names_dfrr2020, skip=6)
ds19_0 <- readxl::read_excel(path_dfrr2019, col_names = names_dfrr2019, skip=6)
ds18_0 <- readxl::read_excel(path_dfrr2018, col_names = names_dfrr2018, skip=11)
ds17_0 <- readxl::read_excel(path_dfrr2017, col_names = names_dfrr2017, skip=14)
ds16_0 <- readxl::read_excel(path_dfrr2016, col_names = names_dfrr2016, skip=14)
ds15_0 <- readxl::read_excel(path_dfrr2015, col_names = names_dfrr2015, skip=14)

#main admin dataset
ds_admin <- readr::read_csv("./data-public/derived/ua-admin-map-2020.csv")

#old admin dataset
ds_admin_old <- readr::read_csv("./data-public/derived/ua-admin-old.csv")

#hromada dataset
ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv")

#+ inspect-data ----------------------------------------------------------------
ds20_0 %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------
ds21_1 <- 
  ds21_0 %>% 
  mutate(oblast = str_to_title(str_extract(description , '.+(?= область)|^Київ$'))) %>% 
  fill(oblast) %>% 
  filter(!(is.na(oblast_code) | str_detect(description , "область$"))) %>%
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,year = 2021
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)


ds20_1 <- 
  ds20_0 %>% 
  mutate(oblast = str_to_title(str_extract(description , '.+(?= ОБЛАСТЬ)|КИЇВ'))) %>%
  fill(oblast) %>% 
  filter(!(is.na(id) & str_detect(description , "Усього|усього"))) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,year = 2020
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)
  
ds19_1 <- 
  ds19_0 %>% 
  mutate(oblast = str_to_title(str_extract(description , '.+(?= ОБЛАСТЬ)|КИЇВ'))) %>%
  fill(oblast) %>% 
  filter(!(is.na(id) & str_detect(description , "Усього|усього|Перелік із змінами"))) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,year = 2019
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)

ds18_1 <- 
  ds18_0 %>% 
  mutate(oblast = str_to_title(str_extract(description , '.+(?= область)|^Київ'))) %>%
  fill(oblast) %>% 
  filter(!is.na(timeline)) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,budget_plan = as.numeric(budget_plan_common) + as.numeric(budget_plan_special)
    ,budget_executed = as.numeric(budget_executed_common) + as.numeric(budget_executed_special)
    ,year = 2018
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)


ds17_1 <- 
  ds17_0 %>% 
  mutate(oblast = str_to_title(ifelse(str_detect(id, "\\d"), NA, id))) %>% 
  fill(oblast) %>% 
  filter(!is.na(timeline)) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,budget_plan = as.numeric(budget_plan_common) + as.numeric(budget_plan_special)
    ,budget_executed = as.numeric(budget_executed_common) + as.numeric(budget_executed_special)
    ,year = 2017
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)

ds16_1 <- 
  ds16_0 %>% 
  mutate(oblast = str_to_title(ifelse(str_detect(id, "\\d"), NA, id))) %>% 
  fill(oblast) %>% 
  filter(!is.na(timeline)) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,year = 2016
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)

ds15_1 <- 
  ds15_0 %>% 
  mutate(oblast = str_to_title(ifelse(str_detect(id, "\\d"), NA, id))) %>% 
  fill(oblast) %>% 
  filter(!is.na(timeline)) %>% 
  mutate(
    settlement = str_extract(description , '(м(\\.|\\.\\s)|(?<= )с(\\.|\\.\\s)|смт\\s)([^\\s]+)')
    ,raion = str_extract(description , '[\\p{Cyrillic}]+(?= району)')
    ,year = 2015
  )  %>% 
  select(oblast, raion, settlement, description, budget_plan, budget_executed, year)

#+ combine ---------------------------------------------------------------------
ds_1 <- rbind(ds21_1,ds20_1,ds19_1,ds18_1,ds17_1,ds16_1,ds15_1)

#+ save-to-disk-for-coding, eval=eval_chunks-----------------------------------------------
openxlsx::write.xlsx(ds_1, "./data-private/raw/dfrr-2015-2022-coding.xlsx")

#+ tweak-coded-data, eval=eval_chunks ------------------------------------------------
ds_2 <- readxl::read_excel("./data-private/raw/dfrr-2015-2022-coding-coded.xlsx", na = "NA")
  
#select cities codes for merge - all combinations oblast-settlement for cities are unique
cities_for_merge <- 
  ds_admin_old %>% 
    filter(category_label == "місто") %>% 
    mutate(
      key = paste(oblast_name, category_label, settlement_name)
    ) %>% 
    select(key, settlement_code)

#select unique settlements for merge
settlement_duplicated <- 
  ds_admin_old %>% 
    filter(category_label == "селище міського типу") %>% 
    mutate(
      key = paste(oblast_name, category_label, settlement_name)
    ) %>% 
    count(key) %>% 
    filter(n > 1) %>% 
    pull(key)

settlements_for_merge <- 
  ds_admin_old %>% 
  mutate(
    key = paste(oblast_name, category_label, settlement_name)
  ) %>% 
  filter(key %ni% settlement_duplicated) %>% 
  select(key, settlement_code)

#select unique villages for merge
villages_duplicated <- 
  ds_admin_old %>% 
  mutate(
    category_label = ifelse(category_label == "селище", "село", category_label)
  ) %>%
  filter(category_label == "село") %>% 
  mutate(
    key = paste(oblast_name, raion_name, category_label, settlement_name)
  ) %>% 
  count(key) %>% 
  filter(n > 1) %>% 
  pull(key)

villages_for_merge <- 
  ds_admin_old %>% 
  mutate(
    key = paste(oblast_name, raion_name, category_label, settlement_name)
  ) %>% 
  filter(key %ni% villages_duplicated) %>% 
  select(key, settlement_code)

#combine in one df
for_merge <- rbind(cities_for_merge, settlements_for_merge, villages_for_merge)


ds_3 <-
  ds_2 %>%
  filter(!(is.na(settlement) & is.na(hromada))) %>%
  mutate(
    settlement_type = case_when(
      str_detect(settlement, "м\\.") ~ "місто"
      ,str_detect(settlement, "с\\.") ~ "село"
      ,str_detect(settlement, "смт |cмт ") ~ "селище міського типу"
      ,str_detect(settlement, "Київ") ~ "місто"
    )
    ,settlement = str_remove(settlement, "м\\.\\s|с\\.\\s|c\\.\\s|смт\\s|м\\.\\s")
    ,settlement = str_replace(settlement, "'", "’")
    ,raion = str_replace(raion, "'", "’")
    ,key = case_when(
      is.na(hromada) == T & settlement_type %in% c("місто", "селище міського типу") ~ paste(oblast, settlement_type, settlement)
      ,is.na(hromada) == T & settlement_type == "село" ~ paste(oblast, raion, settlement_type, settlement)
      ,TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    for_merge
    ,by = "key"
  )

#final coding of settlements which haven't merged
# openxlsx::write.xlsx(ds_3 %>% filter(is.na(settlement_code) & !is.na(key))
#                      , "./data-private/raw/dfrr-2015-2022-coding.xlsx")

ds_coded <- readxl::read_excel("./data-private/raw/dfrr-2015-2022-coding_v2_coded.xlsx", na = "NA")

ds_4 <- 
  ds_3 %>% 
  filter(!(is.na(settlement_code) & !is.na(key))) %>% 
  rbind(ds_coded) %>%
  left_join(
    ds_admin %>% select(settlement_code_old, hromada_code, hromada_name)
    ,by = c("settlement_code"="settlement_code_old")
  ) %>% 
  mutate(
    key = paste(oblast, hromada)
  ) %>% 
  left_join(
    ds_hromada %>% select(hromada_code, oblast_name, hromada_name, type) %>% 
      mutate(
        key = paste(oblast_name, hromada_name, type, "громада"), oblast_name = NULL, type = NULL
      )
    ,by = "key"
  ) %>% 
  mutate(
    hromada_name = case_when(
      settlement_code == "8000000000" ~ "Київ"
      ,is.na(hromada_name.x) == T ~ hromada_name.y
      ,TRUE ~ hromada_name.x
    )
    ,hromada_code = case_when(
      settlement_code == "8000000000" ~ "UA80000000000093317"
      ,is.na(hromada_code.x) == T ~ hromada_code.y
      ,TRUE ~ hromada_code.x
    )
  ) %>% 
  select(-c(hromada_name.x, hromada_name.y, hromada_code.x, hromada_code.y))

#+ aggregate-data, eval=eval_chunks ------------------------------------------------
ds_5 <- 
  ds_4 %>% 
  group_by(hromada_code, hromada_name, year) %>% 
  summarise(budget_planned = sum(budget_plan), budget_executed = sum(budget_executed))

#+ save-data, eval=eval_chunks ------------------------------------------------
readr::write_csv(ds_5, "./data-private/derived/dfrr_hromadas.csv")


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

