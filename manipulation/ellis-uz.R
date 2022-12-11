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

#+ declare-globals -------------------------------------------------------------
names_uz<- c(
  "train_number"
  ,"type"
  ,"date_departure"
  ,"station_departure"
  ,"station_arrival"
  ,"car_type"
  ,"ticket_type"
  ,"n_passangers"
)

path_admin <- "./data-public/derived/ua-admin-map-2020.csv"


#+ load-data, eval=eval_chunks -------------------------------------------------
paths_uz <-  list.files("./data-private/raw/uz-data",full.names = T)

ls_import <- list()
for(i in seq_along(paths_uz)){
  
  ls_import[[i]] <- 
    readxl::read_xlsx(
      path = paths_uz[[i]]
      ,col_names = names_uz
      ,skip=1
    ) %>% 
    janitor::clean_names() %>% 
    select(-c(date_departure, car_type, ticket_type)) %>% 
    mutate_all(.funs = as.character)
}

ds_admin <- readr::read_csv(path_admin)
  
#+ tweak-data ------------------------------------------------------------------
#join files
d0 <- bind_rows(ls_import,.id = "file_number") 

skimr::skim(d0)

#count aggregated number of passangers who arrived in 2021 per station and unique number of trains
d1 <- d0 %>% 
  filter(type == "Внутрішньодержавне") %>% 
  select(-type, -file_number, -station_departure) %>% 
  mutate(
    n_passangers = as.numeric(n_passangers)
  ) %>% 
  group_by(station_arrival) %>% 
  mutate(
    total_passangers = sum(n_passangers)
    ,unique_trains = n_distinct(train_number)
  ) %>% 
  select(-n_passangers, -train_number) %>% 
  distinct(station_arrival, .keep_all = T) %>% 
  filter(total_passangers > 100) #filter out statoion with <100 passangers in 2021


#assign stations to hromadas
unique_settlements <- 
  ds_admin %>% 
  mutate(settlement_name = str_replace(settlement_name, "’", "ʼ")) %>% 
  group_by(settlement_name) %>% 
  mutate(name_count = n()) %>%
  ungroup() %>% 
  filter(name_count == 1) %>% 
  select(settlement_name, hromada_code)

d2 <- d1 %>% 
  mutate(
    station_arrival = str_to_title(station_arrival)
    ,station_arrival = str_remove(station_arrival, "(-(?!Дністровський|Барятинське|Франківськ|Подільський|Бузька|Зоря|Бугаз).*)|( \\d)")
    ,station_arrival = str_replace(station_arrival, "'", "'")
    ,station_arrival = case_when(
      station_arrival == "Миколаїв-Дністровський" ~ "Миколаїв"
      ,station_arrival == "Новоград" ~ "Новоград-Волинський"
      ,TRUE ~ station_arrival
    )
  ) %>% 
  left_join(
    unique_settlements
    ,by = c("station_arrival"="settlement_name")
  )

#save file for coding of settlement names
openxlsx::write.xlsx(d2, "./data-private/raw/uz-stations-for-coding.xlsx")


#read coded file
d3 <-  readxl::read_xlsx("./data-private/raw/uz-stations-for-coding-coded.xlsx") %>% 
  left_join(
    unique_settlements
    ,by = c("right_name"="settlement_name")
  ) %>% 
  mutate(
    hromada_code2 = case_when(
      is.na(hromada_code.x) ~ hromada_code.y
      ,is.na(hromada_code.y) ~ hromada_code.x
      ,TRUE ~ NA_character_
    )
  ) %>% 
  select(-hromada_code.x, -hromada_code.y)

#save file for coding of the rest of settlement names
openxlsx::write.xlsx(d3, "./data-private/raw/uz-stations-for-coding_v2.xlsx")

d4 <-  readxl::read_xlsx("./data-private/raw/uz-stations-for-coding_v2_coded.xlsx") %>% 
  group_by(hromada_code2) %>% 
  summarise(passangers_2021 = sum(total_passangers)) %>% 
  filter(!is.na(hromada_code2)) %>% 
  rename(hromada_code = hromada_code2)


#+ write-data ------------------------------------------------------------------

readr::write_csv(d4, "./data-private/derived/passangers.csv")

