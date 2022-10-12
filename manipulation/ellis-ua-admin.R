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
Sys.setlocale("LC_CTYPE", "russian")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
path_file <- "./data-private/raw/ua-admin-codes.csv"


#Сomparison between old (before 2020) and new admin codifiers,
#source: https://docs.google.com/spreadsheets/d/1Cu_ANPCunoQywhz2-NUkKAtT7eemR1Mt/edit?usp=sharing&ouid=108294388934909170871&rtpof=true&sd=true
path_admin_comp <- "./data-private/raw/admin-comp.csv" 
#Old (before 2020) admin codifier,
#source: https://docs.google.com/spreadsheets/d/1fezJP9iJ0Yjp4REsz722czsMD5AoORmv/edit?usp=sharing&ouid=108294388934909170871&rtpof=true&sd=true
path_admin_old <- "./data-private/raw/ua-admin-codes-old.csv"
#Current codifier of budget codes of hromadas by the Ministry of Finance,
#source: https://docs.google.com/spreadsheets/d/1tONOFUsX0Q-0AwIabPZET8lmZ9INA1Rh/edit?usp=sharing&ouid=108294388934909170871&rtpof=true&sd=true
path_admin_fin <- "./data-private/raw/admin-fin.xlsx" 
#Old codifier of budget codes as of 01.01.2019 (voluntarily formed hromadas + radas/separate settlements)
#source:https://docs.google.com/spreadsheets/d/13gDuQTUuCjE0TUuRHS1sLvqkLYdL7gGW/edit?usp=sharing&ouid=108294388934909170871&rtpof=true&sd=true
path_admin_fin_old <- "./data-private/raw/admin-fin-old.xlsx" 
# Kодифікатор. tab "області"
# https://docs.google.com/spreadsheets/d/1_M-MOSIOkpiBHrP0ieiK0iFmm1_gnP_7/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true 
path_oblast <- "./data-private/raw/oblast.csv"
path_geography <- "./data-private/raw/terhromad_fin.geojson"


names_admin_ua <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"level_extra"
  ,"object_category"
  ,"object_name"
)

names_admin_comp <- c(
  "new_code"
  ,"old_code"
  ,"object_category"
  ,"object_name"
)

names_admin_old <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"object_category"
  ,"object_name"
)

names_admin_fin <- c(
  "territory_code"
  ,"Uncontrolled"
  ,"budget_feature"
  ,"n_budgets"
  ,"n_budget_state_link"
  ,"budget_code"
  ,"budget_name"
  ,"object_name"
  ,"object_code"
  ,"full_name"
)

names_admin_fin_old <- c(
  "territory_code"
  ,"uncontrolled"
  ,"budget_feature"
  ,"n_budgets"
  ,"n_budget_state_link"
  ,"budget_code_old"
  ,"budget_name"
  ,"object_name"
  ,"object_code_old"
  ,"full_name"
)


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_file, col_names = names_admin_ua, skip=1)
ds_comp0 <- readr::read_csv(path_admin_comp, col_names = names_admin_comp, skip=1)
ds_old0 <- readr::read_csv(path_admin_old, col_names = names_admin_old, skip=1)
ds_fin0 <- readxl::read_excel(path_admin_fin, sheet = "codes", col_names = names_admin_fin, skip=10)
ds_fin_old <- readxl::read_excel(path_admin_fin_old, sheet = "codes", col_names = names_admin_fin_old, skip=11)
ds0_oblast <- readr::read_csv(path_oblast, skip=0)
ds_geography <- sf::st_read(path_geography) %>% janitor::clean_names() %>% 
  mutate_at(vars(admin_1:type), ~str_replace_all(.,c("a" = "а", "o" = "о", "p"="р", "e"="е", "i"="і", "'" = "’")))
  

#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()
ds_comp0 %>% glimpse()
ds_old0 %>% glimpse()
ds_fin0 %>% glimpse()

ds0 %>% count(object_category)
ds_comp0 %>% count(object_category)
ds_old0 %>% count(object_category)
ds_fin0 %>% count(object_category)

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds1 <- 
  ds0 %>% 
  mutate(
    category_label = case_when(
      object_category =="O"  ~ "область"
      , object_category =="K"  ~ "місто (спец статус)"
      , object_category =="P"  ~ "район"
      , object_category =="H"  ~ "громада"
      , object_category =="M"  ~ "місто"
      , object_category =="T"  ~ "селище міського типу"
      , object_category =="C"  ~ "село"
      , object_category =="X"  ~ "селище"
      , object_category =="B"  ~ "район міста"
      , TRUE ~ NA_character_
    )
  )
ds1 %>% group_by(object_category, category_label) %>% tally()

ds1 %>% glimpse()

#transform old admin dataset - added category label
ds_old1 <- 
  ds_old0 %>% 
  mutate(
    category_label = case_when(
      object_category =="Р"  ~ "район міста"
      , object_category =="М"  ~ "місто"
      , object_category =="Т"  ~ "селище міського типу"
      , object_category =="С"  ~ "село"
      , object_category =="Щ"  ~ "селище"
      , str_detect(object_name, "РАЙОН/") ~ "район"
      , str_detect(object_name, "(?<!(ОБЛАСТЬ|КРИМ))\\/(СМТ|С\\.|М\\.|С-ЩЕ)") ~ "рада"
      , str_detect(object_name, "(ОБЛАСТЬ|КРИМ)\\/") ~ "область"
      , is.na(object_category) == T & !str_detect(object_name, "(Р-НУ|РАЙОНИ|МІСТА|ПIДПОРЯДКОВАНI|ПІДПОРЯДКОВАНІ|\\/)") ~ "місто"
      , str_detect(object_name, "АВТОНОМНА РЕСПУБЛІКА КРИМ\\/М.СІМФЕРОПОЛЬ") ~ "область"
      , TRUE ~ NA_character_
    )    
  ) %>% 
  mutate(
    category_label = case_when(
      str_detect(object_name, "(?<!\\/)М\\.КИЇВ") ~ "місто (спец статус)"
      , str_detect(object_name, "(?<!\\s)М\\.СЕВАСТОПОЛЬ") ~ "місто (спец статус)"
      , TRUE ~ as.character(category_label)
    )
  ) %>%
  filter(is.na(category_label) == F)

ds_old1  %>% group_by(category_label) %>% tally()

#transform comparison dataset
ds_comp1 <- 
  ds_comp0 %>% 
  mutate(
    category_label = case_when(
      object_category =="О"  ~ "область"
      , object_category =="К"  ~ "місто (спец статус)"
      , object_category =="Р"  ~ "район"
      , object_category =="Н"  ~ "громада"
      , object_category =="М"  ~ "місто"
      , object_category =="Т"  ~ "селище міського типу"
      , object_category =="С"  ~ "село"
      , object_category =="Х"  ~ "селище"
      , object_category =="В"  ~ "район міста"
      , TRUE ~ NA_character_
    )
  ) %>%
  add_row(
    new_code = "UA51120010030018173"
    , old_code = "5120280501"
    , object_category = "С"
    , object_name = "Ананьїв Другий"
    , category_label = "село"
  ) #one village consisted of two radas/villages in old and budget datasets, but has just one in comparison and new datasets

ds_comp1  %>% group_by(category_label) %>% tally()

#transform new fin dataset
ds_fin <- 
  ds_fin0 %>% 
  select(budget_code:full_name) %>% 
  filter(
    !(budget_code == "0552800000" & object_code == "UA14100070260032560") #added to Mariupol in 2015
    ,!str_detect(full_name, "село Велика Білозерка-(2|3|4)") #several codes for one settlement (only in this dataset)
  ) %>%
  add_row(
    budget_code ="0951900000"
    ,budget_name = "Бюджет Ямницької сільської територіальної громади"
    ,object_name = "Ямницька сільська рада"
    ,object_code = "UA26040390080042180"
    ,full_name = "село Ценжів у складі Ямницької сільської територіальної громади у Івано-Франківському районі у Івано-Франківській області"
  ) #added code for the new settlement created in 2021


#+ table-1 ---------------------------------------------------------------------

ds_oblast <- 
  ds1 %>% 
  filter(object_category == "O") %>% 
  distinct(oblast_code = level_1, oblast_name = object_name)

ds_oblast 

ds_raion <-
  ds1 %>% 
  filter(object_category == "P") %>% 
  distinct(raion_code = level_2, raion_name = object_name)
ds_raion

ds_hromada <-
  ds1 %>% 
  filter(object_category == "H") %>% 
  distinct(hromada_code = level_3, hromada_name = object_name)
ds_hromada

ds_settlement <-
  ds1 %>% 
  filter(object_category %in% c("X","C","T","M") ) %>% 
  distinct(settlement_code = level_4, settlement_name = object_name, 
           settlement_type = category_label)
ds_settlement


#+ table-2 ---------------------------------------------------------------------

ds_oblast_old <- 
  ds_old1 %>% 
  filter(category_label == "область") %>% 
  distinct(oblast_code = level_1, oblast_name = object_name)
ds_oblast_old 

ds_raion_old <-
  ds_old1 %>% 
  filter(category_label == "район") %>% 
  distinct(raion_code = level_2, raion_name = object_name)
ds_raion_old

ds_rada_old <-
  ds_old1 %>% 
  filter(category_label == "рада" 
         | category_label == "місто" 
         | (category_label == "селище міського типу" & is.na(level_4) == T)) %>%
  mutate(
    rada_code = case_when(
      category_label %in% c("село", "селище", "селище міського типу") ~ level_3
      ,category_label == "місто" & is.na(level_3) == F  ~ level_3
      ,category_label == "місто" & is.na(level_3) == T  ~ level_2
      ,category_label == "рада" ~ level_3
    )
    , rada_name = object_name
  ) %>% 
  select(rada_code, rada_name)
ds_rada_old

ds_settlement_rada_old <-
  ds_old1 %>% 
  filter(category_label %in% c("місто","село","селище","селище міського типу") ) %>%
  mutate(
    settlement_code = case_when(
      category_label == "місто" & is.na(level_3) == T ~ level_2
      ,category_label == "місто" & is.na(level_3) == F ~ level_3
      ,category_label == "село" ~ level_4
      ,category_label == "селище" ~ level_4
      ,category_label == "селище міського типу" & is.na(level_4) == T ~ level_3
      ,category_label == "селище міського типу" & is.na(level_4) == F ~ level_4
    )
  ) %>% 
  mutate(
    rada_code = case_when(
      category_label %in% c("село", "селище", "селище міського типу") ~ level_3
      ,category_label == "місто" & is.na(level_3) == F  ~ level_3
      ,category_label == "місто" & is.na(level_3) == T  ~ level_2
    )
  ) %>% 
  left_join(
    ds_rada_old
    ,by = "rada_code"
  ) %>%
  select(settlement_name = object_name, settlement_code, rada_name, rada_code)

ds_settlement_rada_old

#+ table-3 ---------------------------------------------------------------------

ds_oblast_comp <- 
  ds_comp1 %>% 
  filter(object_category == "О") %>% 
  distinct(oblast_code = new_code, oblast_code_old = old_code, oblast_name = object_name)
ds_oblast_comp 

ds_raion_comp <-
  ds_comp1 %>% 
  filter(object_category == "Р") %>% 
  distinct(raion_code = new_code, raion_code_old = old_code, raion_name = object_name)
ds_raion_comp

ds_hromada_comp <-
  ds_comp1 %>% 
  filter(object_category == "Н") %>% 
  distinct(hromada_code = new_code, hromada_code_old = old_code, hromada_name = object_name)
ds_hromada_comp

ds_settlement_comp <-
  ds_comp1 %>% 
  filter(object_category %in% c("Х","С","Т","М") ) %>% 
  distinct(settlement_code = new_code, settlement_code_old = old_code, settlement_name = object_name, 
           settlement_type = category_label)
ds_settlement_comp


#+ combine ---------------------------------------------------------------------

#new admin dataset
ds_map_hromada <- 
  #1
  ds_hromada %>% 
  left_join(
    ds1 %>% distinct(raion_code = level_2, hromada_code = level_3)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_raion
    ,by = "raion_code"
  ) %>% 
  # 2
  left_join(
    ds1 %>% distinct(oblast_code = level_1, hromada_code = level_3)
    ,by = "hromada_code"
  ) %>% 
  left_join(
    ds_oblast
    ,by = "oblast_code"
  ) %>% 
  left_join(
    as_tibble(ds_geography) %>% 
      select(cod_3,type) %>% 
      mutate(type = str_extract(type, "сільська|селищна|міська"))
    ,by = c('hromada_code' = "cod_3")
  )
ds_map_hromada

ds_map_settlement <- 
  #1
  ds_settlement %>% 
  inner_join(
    ds1 %>% distinct(settlement_code = level_4, hromada_code = level_3)
    ,by = "settlement_code"
  ) %>% 
  inner_join(
    ds_map_hromada
    ,by = "hromada_code"
  )
ds_map_settlement

# demonstrate that ds_map_hromada can be devided from ds_map_settlement
identical(
  ds_map_hromada
  ,ds_map_settlement %>% 
    select(!starts_with("settlement_")) %>% 
    distinct()
)
# Therefore we will use ds_map_settlement as the primary file

ds_admin <- 
  ds_map_settlement %>% 
  left_join(ds0_oblast, by = c("oblast_code", "oblast_name")) %>% 
  mutate(
    oblast_name_display = paste0(region_ua," - ",oblast_name)
    ,oblast_name_display = fct_reorder(oblast_name_display, map_position)
  ) 
ds_admin %>% glimpse(90)


#combine all together with old classification (rada name and code)
ds_admin_old_new <-
  ds_admin %>% 
  left_join(
    ds_settlement_comp %>% select(settlement_code, settlement_code_old, settlement_name)
    ,by = "settlement_code"
  ) %>%
  mutate(
    settlement_code_old = case_when(
      settlement_name.x == "Ценжів" ~ "2625882701"
      , TRUE ~ as.character(settlement_code_old)
    )  #added old code for the settlement which was created in 2021
  ) %>%
  left_join(
    ds_settlement_rada_old
    ,by = c("settlement_code_old" = "settlement_code")
  )

#adding information on budget codes: final after 2020 and before as of 01.01.2019 - 
#does not include codes for settlements which formed hromadas voluntarily - SOLUTION IS NEEDED
ds_admin_full <-
  ds_admin_old_new %>% 
  left_join(
    ds_fin %>% select(object_code, budget_code, budget_name, full_name)
    ,by = c("settlement_code" = "object_code")
  ) %>%
  filter(
    !(settlement_code_old == "5120280401" & str_detect(full_name, "Ананьїв Другий"))
    ,!(settlement_code_old == "5120280501" & str_detect(full_name, "Ананьїв Перший"))
  ) %>% 
  left_join(
    ds_fin_old %>% select(object_code_old, budget_code_old)
    ,by= c("settlement_code_old" = "object_code_old")
  ) %>% 
  select(-c(settlement_name.y, settlement_name)) %>% 
  rename(settlement_name = settlement_name.x) %>% 
  mutate(settlement_name = case_when(
    settlement_code_old == "5120280501" ~ "Ананьїв Другий"
    ,TRUE ~ settlement_name)
  ) %>% 
  mutate(
   budget_code = case_when(
     settlement_code == "UA01160030020068769" ~ "0120140100"
     ,settlement_code == "UA01160030030037634" ~ "0120140100"
     ,settlement_code == "UA01160330020049936" ~ "0120110000"
     ,settlement_code == "UA01180770020090437" ~ "0121040400"
     ,settlement_code == "UA01200070020040385" ~ "0121140200"
     ,settlement_code == "UA01200130020054332" ~ "0121140400"
     ,settlement_code == "UA01200130030013528" ~ "0121140400"
     ,settlement_code == "UA01200130040083175" ~ "0121140400"
     ,settlement_code == "UA01200210020027504" ~ "0121140500"
     ,settlement_code == "UA01200210030026825" ~ "0121140500"
     ,settlement_code == "UA01200210040055601" ~ "0121140500"
     ,settlement_code == "UA01200210050032694" ~ "0121140500"
     ,settlement_code == "UA01200270020097082" ~ "0121140600"
     ,settlement_code == "UA01200270030090294" ~ "0121140600"
     ,settlement_code == "UA01200270040095864" ~ "0121140600"
     ,settlement_code == "UA01200270050040024" ~ "0121140600"
     ,settlement_code == "UA01200270060060495" ~ "0121140600"
     ,settlement_code == "UA01200290020021637" ~ "0121140700"
     ,settlement_code == "UA05040250260015923" ~ "0253500000"
     ,settlement_code == "UA65060110020037465" ~ "2154100000"
     ,TRUE ~ budget_code
   )
  )




ds_admin_full %>% filter(is.na(budget_code_old) ) %>% View()

#identification of relevant settlement name among three variables
# ds_admin_full %>% 
#   select(oblast_name, settlement_name.x, settlement_name.y, settlement_name) %>% 
#   mutate_all(.funs = ~str_replace(.,"'","’")) %>% 
#   mutate(settlement_name.x = tolower(settlement_name.x)
#          ,settlement_name.y = tolower(settlement_name.y)
#          ,settlement_name = tolower(settlement_name)
#   ) %>% 
#   filter(!(settlement_name.x == settlement_name.y) | 
#            !(settlement_name.x == settlement_name) | 
#            !(settlement_name.y == settlement_name)) %>% View()
#the most of renames are Crimean Tatar names instead of Soviet ones in Crimea


#+ save-to-disk, eval=eval_chunks-----------------------------------------------
#all dataset withour bdget_code_all - for work with OpenBudget data from 2020 onwords
ds_admin_full %>% 
  select(-budget_code_old) %>% 
  readr::write_csv("./data-public/derived/ua-admin-map-2020.csv")

ds_admin_full %>% 
  readr::write_csv("./data-private/derived/ua-admin-map.csv")

ds_map_hromada %>% 
  readr::write_csv("./data-private/derived/hromada.csv")



#+ sanity-check, eval=F, echo=F -------------------------------
# rm(list = ls(all.names = TRUE))
# cat("\014") # Clear the console
# library(tidyverse)
# 
# ds_map <- readr::read_rds("./data-private/derived/ua-admin-map.rds")
# 
# ds_map_hromada <- 
#   ds_map %>% 
#   select(!starts_with("settlement")) %>% 
#   filter(!is.na(hromada_name)) 
# ds_map_hromada
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

