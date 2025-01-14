---
title: "Resilience Survey Overview"
author: 
- "Valentyn Hatsko"
- "Andriy Koval"  
date: "Last updated: 2022-12-27"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: show
    theme: cerulean
    highlight: zenburn
editor_options: 
  chunk_output_type: console
---

> This report visualizes key information about Resilience Survey

***Important Definitions***

> Research Sample: Hromadas who responded to the survey.

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->





# Environment

> Reviews the components of the working environment of the report. Non-technical readers are welcomed to skip. Come back if you need to understand the origins of custom functions, scripts, or data objects.

<details>

<summary>Packages used </summary>

Packages used in current report


```r
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
library(tidyr)     # data wrangling
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(survey)
library(fastDummies)
library(gt)
```

</details>

<details>

<summary>External scripts </summary>

Collection of custom functions used in current repository (`sda-information-requests`)


```r
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling
```

</details>

<details>

<summary>Global values </summary>

Values used throughout the report.


```r
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-overview/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
```

</details>

<details>

<summary>Functions </summary>

Custom functions defined for use in this report.


```r
'%ni%' <- Negate(`%in%`)


make_corr_matrix <- function(d,na_action="remove", method="pearson"){
  
  item_names <- names(d)
  # browser()
  d1 <- 
    d %>% 
    select(all_of(item_names)) %>% 
    mutate(
      across(
        .cols = everything()
        ,.fns = ~as.numeric(.)
      )
    ) 
  
  if(na_action == "remove"){
    d2 <- d1 %>% drop_na()
  }
  
  if(na_action == "replace"){
    d2 <-
      d1 %>%
      mutate(
        across(
          .cols = everything()
          ,.fns = ~replace_na(.,0L)
        )
      )
  }
  cormat <- cor(d2,method = method)
  # row.names(cormat) <- item_names
  return(cormat)
}


make_corr_plot <- function (
    corr,
    lower="number",
    upper="number",
    bg="white",
    addgrid.col="gray"
    ,title 
    , ...
){
  corrplot::corrplot(
    corr
    , add=F
    , type   = "lower"
    , method = lower
    , diag   = TRUE
    , tl.pos = "lt"
    , cl.pos = "n"
    # ,order = "hclust"
    # ,addrect = 3
    ,...
  )
  corrplot::corrplot(
    corr
    ,add=T
    , type="upper"
    , method=upper
    , diag=TRUE
    , tl.pos="n"
    # ,order = "hclust"
    # ,addrect = 3
    ,title = title  
    , ...
  )
  
}
```

</details>

# Data

## Input


```r
# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")
# 
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")

# meta_oblast <- googlesheets4::read_sheet(sheet_name,"choices",skip = 0)

# Originally, we pulled the meta data object from Kobo front end and stored to 
# survey_xls  <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
# we put this on google drive now, to control manually
googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# https://googlesheets4.tidyverse.org/ 
# https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)
```

<details>

<summary>click to glimpse </summary>


```{.r .fold-show}
ds_survey %>% glimpse()
```

```
Rows: 138
Columns: 278
$ index                                              <dbl> 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15~
$ today                                              <dttm> 2022-10-12, 2022-10-12, 2022-10-19, 20~
$ `_id`                                              <dbl> 191541757, 191560222, 193449048, 193452~
$ hromada_code                                       <chr> "UA12060190000043514", "UA4606037000006~
$ hromada_name                                       <chr> "Лозуватська", "Пустомитівська", "Поміч~
$ hromada_full_name                                  <chr> "Лозуватська сільська громада", "Пустом~
$ raion_code                                         <chr> "UA12060000000022633", "UA4606000000004~
$ raion_name                                         <chr> "Криворізький", "Львівський", "Новоукра~
$ oblast_code                                        <chr> "UA12000000000090473", "UA4600000000002~
$ oblast_name                                        <chr> "Дніпропетровська", "Львівська", "Кіров~
$ type                                               <chr> "сільська", "міська", "міська", "селищн~
$ occupation                                         <chr> "not_occupied", NA, "not_occupied", "no~
$ military_action                                    <chr> "no_combat", NA, "no_combat", "no_comba~
$ population_text                                    <dbl> 18957, 16133, 12000, 14500, 4742, 18851~
$ partners_text                                      <dbl> 0, 1, 0, 0, 5, 0, 0, 0, 0, 0, NA, 0, 4,~
$ friends_text                                       <dbl> 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 4,~
$ state_communication                                <chr> "yes", "yes", "no", "yes", "yes", "yes"~
$ prep_first_aid_water                               <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 0, 1, 1, ~
$ prep_first_aid_fuel                                <dbl> 2, 1, 1, 0, 2, 0, 2, NA, 1, 1, 0, NA, 1~
$ prep_reaction_plan                                 <dbl> 2, 2, 2, 2, 2, 2, 0, 1, 1, 1, 0, 1, 2, ~
$ prep_evacuation_plan                               <dbl> 2, 0, 2, 2, 1, 2, 1, 1, 1, 1, 0, 1, 2, ~
$ prep_reaction_plan_oth_hromadas                    <dbl> 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, ~
$ prep_reaction_plan_oda                             <dbl> 2, 0, 2, 1, 1, 2, 0, 1, 1, 1, 0, 0, 1, ~
$ prep_dftg_creation                                 <dbl> NA, 1, 1, 1, 1, 0, 1, 1, 1, 0, 2, 1, 1,~
$ prep_national_resistance                           <dbl> 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, ~
$ prep_starosta_meeting                              <dbl> 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 0, 2, 1, ~
$ prep_communal_meetiing                             <dbl> 2, 1, 1, 1, 1, 1, 1, NA, 1, 1, 0, 2, 1,~
$ prep_online_map                                    <dbl> 2, 2, 1, 1, 2, 0, 0, NA, 0, 0, 0, 0, 0,~
$ prep_shelter_list                                  <dbl> 2, 2, 1, 1, 2, 2, 1, 1, 0, 1, 0, 0, 2, ~
$ prep_notification_check                            <dbl> 2, 2, 1, 1, 0, 0, 1, NA, 1, 1, 0, 0, 2,~
$ prep_backup                                        <dbl> 2, 0, 1, 0, 2, 0, 0, NA, 0, 0, 0, 0, 1,~
$ prep_partly_backup                                 <dbl> NA, NA, 1, 1, 2, 0, 0, 1, 1, 0, 0, 0, 2~
$ shelter_capacity_before_text                       <chr> "близько 1600 осіб", "1518 - місткість ~
$ shelter_capacity_now_text                          <chr> "близько 1600 осіб (+найпростіші)", "15~
$ telegram                                           <dbl> 2, 0, 1, 1, 2, 0, 0, 0, 0, 2, 0, 0, 2, ~
$ viber                                              <dbl> 2, 0, 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, ~
$ facebook                                           <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, ~
$ chat_help                                          <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, ~
$ hotline                                            <dbl> 0, 2, 0, 0, 2, 0, 1, 2, 2, 0, 0, 2, 2, ~
$ telegram_link                                      <chr> "https://t.me/loz_sirena/", NA, "https:~
$ facebook_link                                      <chr> "https://www.facebook.com/lozuvatka.otg~
$ head_hromada_communication                         <chr> "few_times_a_week", "once_a_day", "few_~
$ dftg_creation                                      <chr> "still_not", "yes", "still_not", "yes",~
$ dftg_creation_date                                 <dttm> NA, 2022-02-25, NA, 2022-02-25, 2022-0~
$ help_for_military                                  <chr> "rooms transport money products other",~
$ `help_for_military/rooms`                          <dbl> 1, 1, 1, 1, 0, NA, 1, 0, 1, 0, NA, 1, N~
$ `help_for_military/transport`                      <dbl> 1, 0, 1, 1, 0, NA, 1, 0, 1, 1, NA, 0, N~
$ `help_for_military/money`                          <dbl> 1, 1, 1, 1, 1, NA, 1, 0, 1, 0, NA, 1, N~
$ `help_for_military/products`                       <dbl> 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, NA, 1, N~
$ `help_for_military/other`                          <dbl> 1, 0, 0, 0, 0, NA, 1, 1, 0, 1, NA, 1, N~
$ `help_for_military/none`                           <dbl> 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 1, N~
$ help_for_military_text                             <chr> "Амуніція, різні пристрої та апаратура"~
$ transport_help_communal                            <chr> "5", NA, "4", "-", NA, NA, "1", NA, "1"~
$ transport_help_bought                              <chr> "0", NA, "0", "3-4", NA, NA, "0", NA, "~
$ percent_working_march                              <dbl> 95.0, 98.0, 100.0, 100.0, 100.0, 90.0, ~
$ percent_working_now                                <dbl> 95.0, 100.0, 100.0, 100.0, 100.0, 85.0,~
$ commun_between_hromadas                            <chr> "Daily", "Daily", "Several times a mont~
$ evacuation                                         <chr> "no", "no", "no", "no", "no", "yes_note~
$ idp_accept                                         <chr> "yes", NA, "yes", "yes", "yes", NA, "ye~
$ idp_registration_date                              <dttm> 2022-02-26, NA, 2022-02-25, 2022-02-25~
$ idp_registration_number                            <dbl> 959, NA, 1162, 1600, 370, NA, 101, 1115~
$ idp_real_number                                    <dbl> 1420, NA, 1220, 1600, 410, NA, 101, 111~
$ idp_help                                           <chr> "communal_placement humanitar_help empl~
$ `idp_help/communal_placement`                      <dbl> 1, NA, 1, 1, 1, NA, 0, 0, 1, 0, NA, 0, ~
$ `idp_help/private_placement`                       <dbl> 0, NA, 1, 1, 1, NA, 1, 0, 1, 1, NA, 1, ~
$ `idp_help/regular_meal`                            <dbl> 0, NA, 0, 1, 0, NA, 0, 0, 0, 0, NA, 0, ~
$ `idp_help/humanitar_help`                          <dbl> 1, NA, 1, 1, 1, NA, 1, 1, 1, 1, NA, 1, ~
$ `idp_help/fundraising`                             <dbl> 0, NA, 0, 0, 0, NA, 0, 0, 1, 0, NA, 0, ~
$ `idp_help/employ`                                  <dbl> 1, NA, 0, 0, 0, NA, 1, 0, 0, 0, NA, 0, ~
$ `idp_help/psych_help`                              <dbl> 1, NA, 1, 1, 1, NA, 1, 1, 1, 0, NA, 0, ~
$ `idp_help/law_help`                                <dbl> 1, NA, 0, 0, 0, NA, 0, 1, 1, 0, NA, 0, ~
$ `idp_help/transit_center`                          <dbl> 1, NA, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0, ~
$ idp_place_rooms                                    <chr> "101_250_beds", NA, "0_100_beds", "over~
$ idp_room_number                                    <chr> NA, NA, NA, "1600", NA, NA, NA, NA, NA,~
$ idp_child_education                                <dbl> 18, NA, 21, 200, 26, NA, 7, NA, 15, 17,~
$ special_fund_relocation                            <chr> "yes", "yes", "no", "no", "no", "no", "~
$ special_fund_relocation_needs                      <chr> "defense public_order", "economic_activ~
$ `special_fund_relocation_needs/state_functions`    <dbl> 0, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/defense`            <dbl> 1, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/public_order`       <dbl> 1, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/economic_activity`  <dbl> 0, 1, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/environment`        <dbl> 0, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/utilities`          <dbl> 0, 1, NA, NA, NA, NA, NA, NA, 1, NA, NA~
$ `special_fund_relocation_needs/spirit_development` <dbl> 0, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/education`          <dbl> 0, 0, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/social_protection`  <dbl> 0, 1, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ `special_fund_relocation_needs/healthcare`         <dbl> 0, 1, NA, NA, NA, NA, NA, NA, 0, NA, NA~
$ relocated_companies_text                           <chr> "0", NA, "0", "0", "0", NA, "0", "0", "~
$ created_jobs                                       <chr> "dk", NA, "dk", "dk", "dk", NA, "dk", "~
$ bussiness_stimules                                 <chr> "tax_benefits", NA, "free_rooms", "othe~
$ `bussiness_stimules/tax_benefits`                  <dbl> 1, NA, 0, 0, 0, NA, 0, 0, 0, 1, NA, 0, ~
$ `bussiness_stimules/free_rooms`                    <dbl> 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, NA, 0, ~
$ `bussiness_stimules/education`                     <dbl> 0, NA, 0, 0, 1, NA, 0, 0, 0, 0, NA, 0, ~
$ `bussiness_stimules/other`                         <dbl> 0, NA, 0, 1, 0, NA, 1, 1, 1, 0, NA, 1, ~
$ bussiness_stimules_none                            <dbl> 0, NA, 0, 1, 0, NA, 0, 0, 1, 0, NA, 1, ~
$ bussiness_stimules_other                           <chr> NA, NA, NA, "-", NA, NA, "відстрочка в ~
$ humanitarian_hub                                   <chr> NA, NA, NA, NA, NA, "no", NA, NA, NA, N~
$ hromada_cooperation                                <chr> NA, NA, NA, NA, NA, "other", NA, NA, NA~
$ `hromada_cooperation/medicine`                     <dbl> NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, ~
$ `hromada_cooperation/food`                         <dbl> NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, ~
$ `hromada_cooperation/pensions`                     <dbl> NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, ~
$ `hromada_cooperation/evacuation`                   <dbl> NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, ~
$ `hromada_cooperation/other`                        <dbl> NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, ~
$ `hromada_cooperation/none`                         <dbl> NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, ~
$ hromada_cooperation_text                           <chr> NA, NA, NA, NA, NA, "співпраця не відбу~
$ is_damaged                                         <chr> "no", "no", "no", "no", "no", "yes", "n~
$ percent_damaged                                    <chr> NA, NA, NA, NA, NA, "0_10_percent", NA,~
$ damage_evaluation_persons                          <chr> NA, NA, NA, NA, NA, "no", NA, "yes", NA~
$ damage_evaluation_communal                         <chr> NA, NA, NA, NA, NA, "no", NA, "yes", NA~
$ damage_evaluation_bussiness                        <chr> NA, NA, NA, NA, NA, "no", NA, "yes", NA~
$ reconstruction_plan                                <chr> NA, NA, NA, NA, NA, NA, NA, "yes", NA, ~
$ reconstruction_financing                           <chr> NA, NA, NA, NA, NA, NA, NA, "no", NA, N~
$ reconstruction_financing_text                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ international_projects                             <chr> "0", NA, "1", "1", "3", NA, "0", NA, "0~
$ percent_reconstructed                              <chr> NA, NA, NA, NA, NA, NA, NA, "76_100_per~
$ finance_school_shelters                            <chr> "0", NA, "50 000 грн.", "не маємо інфор~
$ finance_school_shelters_coded                      <dbl> 0, NA, 50000, NA, 520671, NA, 286900, 7~
$ info_campaign                                      <dbl> 1, NA, 0, 1, 1, NA, 1, 1, 1, 0, NA, 0, ~
$ reserves                                           <dbl> 1, NA, 1, 1, 1, NA, 1, 1, 1, 0, NA, 1, ~
$ count_power_sources                                <dbl> 1, NA, 0, 1, 1, NA, 1, NA, 1, 0, NA, 1,~
$ count_heaters_need                                 <dbl> 0, NA, 0, 1, 1, NA, 0, NA, 0, 0, NA, 0,~
$ solid_fuel_boiler                                  <dbl> 0, NA, 0, 1, 1, NA, 0, NA, NA, 0, NA, 0~
$ no_school_days                                     <chr> "Дистанційно проводились в усі навчальн~
$ no_school_days_coded                               <chr> "0", NA, "0", "0", "81", NA, "0", NA, "~
$ hromada_exp                                        <chr> "yes", "yes", "no", "no", "yes", "no", ~
$ hromada_problem_info                               <chr> "idp bussiness", "citizens bussiness", ~
$ `hromada_problem_info/idp`                         <dbl> 1, 0, NA, NA, 0, NA, NA, NA, 1, 0, NA, ~
$ `hromada_problem_info/citizens`                    <dbl> 0, 1, NA, NA, 1, NA, NA, NA, 0, 1, NA, ~
$ `hromada_problem_info/bussiness`                   <dbl> 1, 1, NA, NA, 1, NA, NA, NA, 0, 1, NA, ~
$ `hromada_problem_info/experts`                     <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_info/ngo`                         <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_info/nobody`                      <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ hromada_problem_consultation                       <chr> "idp", "bussiness", NA, NA, "citizens e~
$ `hromada_problem_consultation/idp`                 <dbl> 1, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_consultation/citizens`            <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_consultation/bussiness`           <dbl> 0, 1, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_consultation/experts`             <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_consultation/ngo`                 <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_consultation/nobody`              <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 1, 1, NA, ~
$ hromada_problem_proposition                        <chr> "citizens", "nobody", NA, NA, "idp citi~
$ `hromada_problem_proposition/idp`                  <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_proposition/citizens`             <dbl> 1, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_proposition/bussiness`            <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 1, 0, NA, ~
$ `hromada_problem_proposition/experts`              <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_proposition/ngo`                  <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_proposition/nobody`               <dbl> 0, 1, NA, NA, 0, NA, NA, NA, 0, 1, NA, ~
$ hromada_problem_system                             <chr> "idp", "bussiness", NA, NA, "citizens b~
$ `hromada_problem_system/idp`                       <dbl> 1, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_system/citizens`                  <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_system/bussiness`                 <dbl> 0, 1, NA, NA, 1, NA, NA, NA, 1, 0, NA, ~
$ `hromada_problem_system/experts`                   <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_system/ngo`                       <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_system/nobody`                    <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 1, NA, ~
$ hromada_problem_feedback                           <chr> "idp", "bussiness", NA, NA, "idp citize~
$ `hromada_problem_feedback/idp`                     <dbl> 1, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_feedback/citizens`                <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_feedback/bussiness`               <dbl> 0, 1, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_feedback/experts`                 <dbl> 0, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_feedback/ngo`                     <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_feedback/nobody`                  <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 1, 1, NA, ~
$ hromada_problem_execution                          <chr> "idp citizens", "bussiness", NA, NA, "c~
$ `hromada_problem_execution/idp`                    <dbl> 1, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_execution/citizens`               <dbl> 1, 0, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_execution/bussiness`              <dbl> 0, 1, NA, NA, 1, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_execution/experts`                <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 0, NA, ~
$ `hromada_problem_execution/ngo`                    <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 1, 0, NA, ~
$ `hromada_problem_execution/nobody`                 <dbl> 0, 0, NA, NA, 0, NA, NA, NA, 0, 1, NA, ~
$ skills_needed                                      <chr> "fundraising project_management", "fund~
$ `skills_needed/fundraising`                        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, ~
$ `skills_needed/project_management`                 <dbl> 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, ~
$ `skills_needed/longterm_planning`                  <dbl> 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, ~
$ `skills_needed/crisis_planning`                    <dbl> 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, ~
$ `skills_needed/data_analysis`                      <dbl> 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, ~
$ `skills_needed/human_resourse`                     <dbl> 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, ~
$ `skills_needed/other`                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ skills_needed_text                                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ contact_text                                       <chr> "Петренко Ігор, 0980913068", "Ірина При~
$ evacuation_001                                     <chr> "no", "no", "no", "no", "no", "yes_note~
$ hromada_exp_problem                                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `_uuid`                                            <chr> "699df016-92c6-406e-a2d2-4c62a7b47e1c",~
$ `_submission_time`                                 <dttm> 2022-10-12 11:35:13, 2022-10-12 12:25:~
$ `_validation_status`                               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `_status`                                          <chr> "submitted_via_web", "submitted_via_web~
$ `_submitted_by`                                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `_tags`                                            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ region_en                                          <chr> "East", "West", "Center", "Center", "Ce~
$ `idp_help/communal_placement_number`               <dbl> 959, NA, 1162, 1600, 370, NA, 0, 0, 565~
$ `idp_help/private_placement_number`                <dbl> 0, NA, 1162, 1600, 370, NA, 101, 0, 565~
$ `idp_help/regular_meal_number`                     <dbl> 0, NA, 0, 1600, 0, NA, 0, 0, 0, 0, NA, ~
$ `idp_help/humanitar_help_number`                   <dbl> 959, NA, 1162, 1600, 370, NA, 101, 1115~
$ `idp_help/fundraising_number`                      <dbl> 0, NA, 0, 0, 0, NA, 0, 0, 565, 0, NA, 0~
$ `idp_help/employ_number`                           <dbl> 959, NA, 0, 0, 0, NA, 101, 0, 0, 0, NA,~
$ `idp_help/psych_help_number`                       <dbl> 959, NA, 1162, 1600, 370, NA, 101, 1115~
$ `idp_help/law_help_number`                         <dbl> 959, NA, 0, 0, 0, NA, 0, 1115, 565, 0, ~
$ `idp_help/transit_center_number`                   <dbl> 959, NA, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0~
$ idp_help_count                                     <dbl> 627, 627, 627, 627, 627, 627, 627, 627,~
$ prep_count                                         <dbl> 23, 14, 16, 14, 20, 13, 9, 10, 12, 10, ~
$ comm_channels_count                                <dbl> 6, 4, 3, 3, 7, 2, 3, 6, 7, 6, 0, 4, 7, ~
$ help_military_count                                <dbl> 5, 3, 4, 4, 2, 0, 5, 2, 4, 3, 0, 4, 0, ~
$ hromada_cooperation_count                          <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 3, ~
$ dftg_creation_time                                 <chr> "2", NA, "1", "1", "7", NA, "0", "36", ~
$ idp_registration_time                              <chr> NA, "1", NA, "1", "1", NA, "0", "5", "1~
$ prep_winter_count                                  <dbl> 3, 0, 1, 5, 5, 0, 3, 2, 3, 0, 0, 2, 0, ~
$ oblast_center                                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
$ hromada_center_code                                <chr> "UA12060190010077883", "UA4606037001003~
$ hromada_center                                     <chr> "Лозуватка", "Пустомити", "Помічна", "Н~
$ lat_center                                         <dbl> 48.06131, 49.71896, 48.24331, 48.66450,~
$ lon_center                                         <dbl> 33.28102, 23.90473, 31.40372, 30.80485,~
$ travel_time                                        <dbl> 160.3, 27.5, 88.1, 112.2, 141.1, 93.8, ~
$ n_settlements                                      <dbl> 32, 10, 4, 21, 10, 8, 9, 33, 15, 3, 68,~
$ square                                             <dbl> 563.8, 95.7, 77.4, 487.6, 209.7, 250.7,~
$ occipied_before_2022                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ total_population_2022                              <dbl> 18464, 15121, 9738, 12161, 4111, 16755,~
$ urban_population_2022                              <dbl> 1312, 9372, 8608, 5882, 0, 10108, 0, 26~
$ urban_pct                                          <dbl> 0.07105719, 0.61980028, 0.88395975, 0.4~
$ budget_code                                        <chr> "04579000000", "13573000000", "11513000~
$ budget_name                                        <chr> "Бюджет Лозуватської сільської територі~
$ oblast_name_en                                     <chr> "Driproptrovska", "Lviv", "Kirovograd",~
$ region_en.x                                        <chr> "East", "West", "Center", "Center", "Ce~
$ region_code_en                                     <chr> "E", "W", "C", "C", "C", "S", "S", "N",~
$ income_total_2021                                  <dbl> 67902340, 83142969, 49372216, 59091396,~
$ income_transfert_2021                              <dbl> 28360223, 26747544, 14842391, 26289209,~
$ income_military_2021                               <dbl> 165437.16, 1471468.72, 161315.18, 45314~
$ income_pdfo_2021                                   <dbl> 21860190, 33743437, 23556614, 18257048,~
$ income_unified_tax_2021                            <dbl> 5038856.5, 7871346.5, 1446555.9, 429707~
$ income_property_tax_2021                           <dbl> 5898847.8, 9690968.4, 6446093.4, 601961~
$ income_excise_duty_2021                            <dbl> 3740238.35, 1989744.71, 1370209.27, 296~
$ income_own_2021                                    <dbl> 39542117, 56395425, 34529825, 32802187,~
$ own_income_prop_2021                               <dbl> 0.58, 0.68, 0.70, 0.56, 0.61, 0.55, 0.3~
$ transfert_prop_2021                                <dbl> 0.42, 0.32, 0.30, 0.44, 0.39, 0.45, 0.6~
$ military_tax_prop_2021                             <dbl> 0.00, 0.02, 0.00, 0.01, 0.00, 0.00, 0.0~
$ pdfo_prop_2021                                     <dbl> 0.32, 0.41, 0.48, 0.31, 0.15, 0.29, 0.1~
$ unified_tax_prop_2021                              <dbl> 0.07, 0.09, 0.03, 0.07, 0.05, 0.06, 0.0~
$ property_tax_prop_2021                             <dbl> 0.09, 0.12, 0.13, 0.10, 0.34, 0.13, 0.1~
$ excise_duty_prop_2021                              <dbl> 0.06, 0.02, 0.03, 0.05, 0.01, 0.05, 0.0~
$ own_income_change                                  <dbl> 0.06, 0.30, -0.02, 0.03, -0.35, -0.55, ~
$ own_prop_change                                    <dbl> 0.03, 0.06, 0.04, 0.05, -0.07, -0.20, -~
$ total_income_change                                <dbl> 0.02, 0.19, -0.07, -0.06, -0.26, -0.29,~
$ income_own                                         <dbl> 41909616, 73349537, 33842591, 33684971,~
$ income_total                                       <dbl> 69264708, 99182114, 45977305, 55515961,~
$ income_transfert                                   <dbl> 27355092, 25832577, 12134714, 21830990,~
$ dfrr_executed                                      <dbl> NA, 51740.635, NA, 8979.148, 611.900, 4~
$ turnout_2020                                       <dbl> 0.3736239, 0.4272969, 0.2801991, 0.3610~
$ sex_head                                           <chr> "female", "female", "female", "female",~
$ age_head                                           <dbl> 45, 40, 62, 56, 65, 64, 66, 63, 61, 54,~
$ education_head                                     <chr> "higher", "higher", "higher", "higher",~
$ incumbent                                          <dbl> 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, ~
$ rda                                                <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ not_from_here                                      <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, ~
$ party                                              <chr> "Слуга народу", "Самовисування", "Самов~
$ enterpreuner                                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ unemployed                                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ priv_work                                          <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
$ polit_work                                         <dbl> 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, ~
$ communal_work                                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ ngo_work                                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ party_national_winner                              <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ no_party                                           <dbl> 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, ~
$ male                                               <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, ~
$ high_educ                                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ sum_osbb_2020                                      <dbl> NA, 29, 28, 6, NA, NA, NA, 12, NA, NA, ~
$ edem_total                                         <dbl> 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, ~
$ edem_petitions                                     <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, ~
$ edem_consultations                                 <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
$ edem_participatory_budget                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ edem_open_hromada                                  <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ youth_councils                                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
$ youth_centers                                      <dbl> 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ business_support_centers                           <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 17,~
$ region_en.y                                        <chr> "East", "West", "Center", "Center", "Ce~
$ creation_date                                      <dttm> 2020-08-16, 2020-08-16, 2017-08-20, 20~
$ creation_year                                      <dbl> 2020, 2020, 2017, 2020, 2017, 2020, 201~
$ time_before_24th                                   <dbl> 556.7917, 556.7917, 1648.7917, 556.7917~
$ voluntary                                          <dbl> 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, ~
$ war_zone_27_04_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, ~
$ war_zone_20_06_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, ~
$ war_zone_23_08_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, ~
$ war_zone_10_10_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, ~
```

```{.r .fold-show}
meta_survey %>% filter(group == 'preamble')
```

```
# A tibble: 1 x 14
  group    item_~1 label~2 label~3 type  name  label requi~4 appea~5 media~6 relev~7 const~8 const~9
  <chr>      <dbl> <chr>   <chr>   <chr> <chr> <chr> <list>  <chr>   <chr>   <chr>   <chr>   <chr>  
1 preamble      NA <NA>    <NA>    note  <NA>  "Суп~ <NULL>  <NA>    kse_in~ <NA>    <NA>    <NA>   
# ... with 1 more variable: hint <chr>, and abbreviated variable names 1: item_number, 2: label_en,
#   3: label_ua, 4: required, 5: appearance, 6: `media::image`, 7: relevant, 8: constraint,
#   9: constraint_message
```

```{.r .fold-show}
ds_survey %>% pull(hromada_code) %>% unique()
```

```
  [1] "UA12060190000043514" "UA46060370000065608" "UA35060190000079777" "UA35020130000045875"
  [5] "UA53060250000043118" "UA65060250000073379" "UA51040110000040346" "UA59080230000084731"
  [9] "UA05100110000070795" "UA51100250000055079" "UA65040010000061104" "UA74040130000094076"
 [13] "UA65100150000057191" "UA12040170000019083" "UA63020090000096294" "UA21120190000082169"
 [17] "UA63100010000016136" "UA07060370000022360" "UA21020070000015036" "UA21080190000094580"
 [21] "UA21080150000014443" "UA26060250000064599" "UA21040250000010914" "UA46060230000093092"
 [25] "UA46080130000077112" "UA21100130000055002" "UA73060250000049790" "UA18020050000058053"
 [29] "UA74040050000013413" "UA23060230000071243" "UA46100070000076013" "UA32120090000034281"
 [33] "UA23060130000058533" "UA12100030000084605" "UA51020170000041393" "UA51120070000097161"
 [37] "UA35020090000039429" "UA21080070000025254" "UA53060070000077527" "UA65020190000013272"
 [41] "UA21060070000049340" "UA12140150000054570" "UA63120210000075842" "UA12040210000039759"
 [45] "UA32140070000012102" "UA35040030000074104" "UA12100050000045992" "UA71080450000083423"
 [49] "UA51120210000054996" "UA59100010000064812" "UA12040030000066040" "UA07080170000013585"
 [53] "UA71040130000029175" "UA71040090000047664" "UA46080090000029798" "UA32080210000074136"
 [57] "UA59020050000012539" "UA53060230000098362" "UA59020110000066430" "UA51120230000084853"
 [61] "UA61060070000098188" "UA74040330000024949" "UA26060090000054411" "UA53040130000097690"
 [65] "UA53040110000034949" "UA26040230000035526" "UA56080150000069525" "UA56060050000010769"
 [69] "UA26040130000010870" "UA73060410000077092" "UA05100170000071290" "UA26060170000091466"
 [73] "UA59080030000075526" "UA05120070000075759" "UA61060130000045755" "UA12080070000077032"
 [77] "UA26120050000087602" "UA32020230000012716" "UA61020030000060484" "UA23060150000085288"
 [81] "UA26040110000023512" "UA05060130000030729" "UA35040190000012514" "UA51020110000041005"
 [85] "UA26100050000019570" "UA63100030000050119" "UA26060110000025739" "UA46020010000073886"
 [89] "UA56060310000066585" "UA51060030000044366" "UA05020110000052014" "UA18060170000069581"
 [93] "UA53040030000088898" "UA26040290000025886" "UA26040270000047749" "UA59040010000075530"
 [97] "UA26060030000011364" "UA26060130000047466" "UA74080150000033167" "UA74100090000064336"
[101] "UA21020130000047547" "UA65100130000084882" "UA26120070000067596" "UA21040110000099623"
[105] "UA21120130000025618" "UA46140010000081849" "UA59060110000049734" "UA12140110000060935"
[109] "UA26040350000024417" "UA12140250000015858" "UA26080070000092582" "UA61060270000084790"
[113] "UA18040410000025491" "UA48040150000011861" "UA32060030000048241" "UA73040170000011490"
[117] "UA71020030000059581" "UA74080130000060606" "UA12140270000072109" "UA46100150000087495"
[121] "UA07020130000036300" "UA74080010000063011" "UA51100070000063635" "UA53060290000047345"
[125] "UA51140110000053825" "UA56060290000044465" "UA46140030000023506" "UA07060290000054842"
[129] "UA68040430000052699" "UA51020030000095942" "UA12020130000022909" "UA59040130000041676"
[133] "UA51120050000071748" "UA65080070000011930" "UA07060390000098670" "UA53040010000091190"
[137] "UA65020150000047137" "UA71080070000050993"
```

</details>

Next, we define useful sets of variable names to be used throughout the report

<details>

<summary>click to see the groups </summary>


```{.r .fold-show}
# create supporting objects for convenient reference of variable groups

# multiple choice questions
mcq <-
  meta_survey%>%
  dplyr::select(type,name)%>%
  dplyr::filter(str_detect(type, "select_multiple"))%>%
  dplyr::select(name)%>%
  pull() %>%  
  print()
```

```
 [1] "help_for_military"             "evacuation_actions"            "idp_help"                     
 [4] "special_fund_relocation_needs" "bussiness_stimules"            "hromada_cooperation"          
 [7] "hromada_problem_inv_labels"    "hromada_problem_info"          "hromada_problem_consultation" 
[10] "hromada_problem_proposition"   "hromada_problem_system"        "hromada_problem_feedback"     
[13] "hromada_problem_execution"     "skills_needed"                
```

```{.r .fold-show}
#vectors of mcq names
preparation <- 
  ds_survey %>% 
  select(starts_with("prep_"), -prep_winter_count, -prep_count) %>% 
  colnames() %>% 
  print()
```

```
 [1] "prep_first_aid_water"            "prep_first_aid_fuel"            
 [3] "prep_reaction_plan"              "prep_evacuation_plan"           
 [5] "prep_reaction_plan_oth_hromadas" "prep_reaction_plan_oda"         
 [7] "prep_dftg_creation"              "prep_national_resistance"       
 [9] "prep_starosta_meeting"           "prep_communal_meetiing"         
[11] "prep_online_map"                 "prep_shelter_list"              
[13] "prep_notification_check"         "prep_backup"                    
[15] "prep_partly_backup"             
```

```{.r .fold-show}
comm_channels <- 
  ds_survey %>% 
  select(telegram:hotline) %>% 
  colnames() %>% 
  print()
```

```
[1] "telegram"  "viber"     "facebook"  "chat_help" "hotline"  
```

```{.r .fold-show}
idp_help <- 
  ds_survey %>%
  select(starts_with('idp_help/'), -ends_with('number')) %>% 
  colnames() %>% 
  print()
```

```
[1] "idp_help/communal_placement" "idp_help/private_placement"  "idp_help/regular_meal"      
[4] "idp_help/humanitar_help"     "idp_help/fundraising"        "idp_help/employ"            
[7] "idp_help/psych_help"         "idp_help/law_help"           "idp_help/transit_center"    
```

```{.r .fold-show}
military_help <- 
  ds_survey %>% 
  select(starts_with('help_for_military/')) %>% 
  colnames() %>% 
  print()
```

```
[1] "help_for_military/rooms"     "help_for_military/transport" "help_for_military/money"    
[4] "help_for_military/products"  "help_for_military/other"     "help_for_military/none"     
```

```{.r .fold-show}
# only for occupied hromadas - few cases
hromada_cooperation <- 
  ds_survey %>% 
  select(starts_with('hromada_cooperation/')) %>% 
  colnames() %>% 
  print()
```

```
[1] "hromada_cooperation/medicine"   "hromada_cooperation/food"      
[3] "hromada_cooperation/pensions"   "hromada_cooperation/evacuation"
[5] "hromada_cooperation/other"      "hromada_cooperation/none"      
```

```{.r .fold-show}
prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')
# vector of income variables 
income <- 
  ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()
```

```
[1] "own_income_prop_2021"   "transfert_prop_2021"    "military_tax_prop_2021"
[4] "pdfo_prop_2021"         "unified_tax_prop_2021"  "property_tax_prop_2021"
[7] "excise_duty_prop_2021" 
```

</details>

<details>

<summary>meta data </summary>


```{.r .fold-show}
meta_survey %>% glimpse()
```

```
Rows: 132
Columns: 14
$ group              <chr> NA, NA, NA, NA, NA, NA, "preamble", "general_information", "general_inf~
$ item_number        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_en           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_ua           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ type               <chr> "start", "end", "today", "deviceid", "begin_group", "begin_group", "not~
$ name               <chr> "start", "end", "today", "deviceid", "general_information", "general_in~
$ label              <chr> NA, NA, NA, NA, NA, NA, "Супротив України у загарбницькій війні Російсь~
$ required           <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, "field-list", <NULL>, TRUE, TR~
$ appearance         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `media::image`     <chr> NA, NA, NA, NA, NA, NA, "kse_institute.png", NA, NA, NA, NA, NA, NA, NA~
$ relevant           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint_message <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hint               <chr> NA, NA, NA, NA, NA, NA, "Заповнення опитувальника займе орієнтовно 25 х~
```

```{.r .fold-show}
meta_survey %>% 
  filter(type %in% c("begin_group","end_group")) %>% 
  select(1:5) %>% 
  print_all()
```

```
# A tibble: 32 x 5
   group item_number label_en label_ua type       
   <chr>       <dbl> <chr>    <chr>    <chr>      
 1 <NA>           NA <NA>     <NA>     begin_group
 2 <NA>           NA <NA>     <NA>     begin_group
 3 <NA>           NA <NA>     <NA>     end_group  
 4 <NA>           NA <NA>     <NA>     end_group  
 5 <NA>           NA <NA>     <NA>     begin_group
 6 <NA>           NA <NA>     <NA>     end_group  
 7 <NA>           NA <NA>     <NA>     begin_group
 8 <NA>           NA <NA>     <NA>     end_group  
 9 <NA>           NA <NA>     <NA>     begin_group
10 <NA>           NA <NA>     <NA>     begin_group
11 <NA>           NA <NA>     <NA>     end_group  
12 <NA>           NA <NA>     <NA>     end_group  
13 <NA>           NA <NA>     <NA>     begin_group
14 <NA>           NA <NA>     <NA>     end_group  
15 <NA>           NA <NA>     <NA>     begin_group
16 <NA>           NA <NA>     <NA>     end_group  
17 <NA>           NA <NA>     <NA>     begin_group
18 <NA>           NA <NA>     <NA>     end_group  
19 <NA>           NA <NA>     <NA>     begin_group
20 <NA>           NA <NA>     <NA>     end_group  
21 <NA>           NA <NA>     <NA>     begin_group
22 <NA>           NA <NA>     <NA>     end_group  
23 <NA>           NA <NA>     <NA>     begin_group
24 <NA>           NA <NA>     <NA>     begin_group
25 <NA>           NA <NA>     <NA>     end_group  
26 <NA>           NA <NA>     <NA>     end_group  
27 <NA>           NA <NA>     <NA>     begin_group
28 <NA>           NA <NA>     <NA>     begin_group
29 <NA>           NA <NA>     <NA>     end_group  
30 <NA>           NA <NA>     <NA>     begin_group
31 <NA>           NA <NA>     <NA>     end_group  
32 <NA>           NA <NA>     <NA>     end_group  
```

```{.r .fold-show}
meta_survey %>% glimpse()
```

```
Rows: 132
Columns: 14
$ group              <chr> NA, NA, NA, NA, NA, NA, "preamble", "general_information", "general_inf~
$ item_number        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_en           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_ua           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ type               <chr> "start", "end", "today", "deviceid", "begin_group", "begin_group", "not~
$ name               <chr> "start", "end", "today", "deviceid", "general_information", "general_in~
$ label              <chr> NA, NA, NA, NA, NA, NA, "Супротив України у загарбницькій війні Російсь~
$ required           <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, "field-list", <NULL>, TRUE, TR~
$ appearance         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `media::image`     <chr> NA, NA, NA, NA, NA, NA, "kse_institute.png", NA, NA, NA, NA, NA, NA, NA~
$ relevant           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint_message <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hint               <chr> NA, NA, NA, NA, NA, NA, "Заповнення опитувальника займе орієнтовно 25 х~
```

</details>

## Transformations

For the state `ds0`, we augment the focal table of the report with additional columns and transform existing variable to better fit visualization/modeling needs


```{.r .fold-hide}
ds_general0 <- 
  ds_general %>% 
  mutate(
    survey_response = case_when(
      hromada_code %in% (ds_survey %>% pull(hromada_code) %>% unique()) ~ TRUE
      ,TRUE ~ FALSE
    )
  )
# ds_general0 %>% group_by(survey_response) %>% count()



ds0 <- 
  ds_survey %>% 
  mutate(
    income_own_per_capita       = income_own_2021         / total_population_2022,
    income_total_per_capita     = income_total_2021       / total_population_2022,
    income_tranfert_per_capita  = income_transfert_2021   / total_population_2022,
    idp_registration_share      = idp_registration_number / total_population_2022,
    idp_real_share              = idp_real_number         / total_population_2022,
    idp_child_share             = idp_child_education     / idp_registration_number
  ) 
```

To make our analysis more nimble we create four alternative versions of `ds1` with Invasion Preparedness questions

<details>
<summary> transformations for PREPAREDNESS scale </summary>

```{.r .fold-show}
# compute total binary score (preparations are made at all, regardless of timing)
d_meta_prep <- 
  meta_survey %>% 
  filter(group=="preparation") %>% 
  select(item_name = name,label_en,label)

ds1_prep <-
  ds0 %>% 
  mutate(
    # sum of 0|1|2 where larger numbers indicate more preparedness
    prep_score_combo = rowSums(across(all_of(preparation)),na.rm = T) 
    ,prep_score_feb = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 0 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
    ,prep_score_oct = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 1 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
  )  %>% 
  select(hromada_code, starts_with("prep_score"),preparation)  
ds1_prep %>% select(1:4)
```

```
# A tibble: 138 x 4
   hromada_code        prep_score_combo prep_score_feb prep_score_oct
   <chr>                          <dbl>          <dbl>          <dbl>
 1 UA12060190000043514               23             10             13
 2 UA46060370000065608               14              4             10
 3 UA35060190000079777               16              3             13
 4 UA35020130000045875               14              2             12
 5 UA53060250000043118               20              6             14
 6 UA65060250000073379               13              6              7
 7 UA51040110000040346                9              1              8
 8 UA59080230000084731               10              0             10
 9 UA05100110000070795               12              0             12
10 UA51100250000055079               10              0             10
# ... with 128 more rows
```

```{.r .fold-show}
## Some handy datasets for quick visualization
# Raw scale (0,1,2) with factors
ds1_prep_ordinal_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "As of Oct"
        ,. == 2 ~ "As of Feb"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","As of Oct","As of Feb",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)

# Binary scale (0,1) with factors
ds1_prep_binary_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0  ~ "No"
        ,. == 1 ~ "Yes"
        ,. == 2 ~ "Yes"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Yes","Not Applicable"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)
```
</details>

>  

<details>
<summary> transformations for INFORMATION scale </summary>

```{.r .fold-show}
d_meta_info <- 
  meta_survey %>% 
  filter(group== "information") %>%
  select(item_name = name,label_en,item_number)
meta_choices %>% filter(list_name=="commun_prep")
```

```
# A tibble: 3 x 4
  list_name   name      label                         label_en
  <chr>       <chr>     <chr>                         <chr>   
1 commun_prep before_24 Було створено до 24 лютого    <NA>    
2 commun_prep after_24  Було створено після 24 лютого <NA>    
3 commun_prep none      Немає                         <NA>    
```

```{.r .fold-show}
item_information <- d_meta_info %>% pull(item_name)

ds1_info <- 
  ds0 %>% 
  mutate(
    across(
      .cols = item_information
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "After Feb 24"
        ,. == 2 ~ "Before Feb 24"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Before Feb 24","After Feb 24",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code,item_information)
```
</details>


<details>
<summary>examine data versions </summary>

</details>


# Variable List

The following variables are present in the processed data table of survey responses:


```{.r .fold-hide}
ds0 %>% explore::describe_all() %>%neat_DT()
```

```{=html}
<div id="htmlwidget-abd51945fa3468239c43" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-abd51945fa3468239c43">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"100\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.83\" data-max=\"191541757\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.01\" data-max=\"197322877.2\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"1288755475.83\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284"],["index","today","_id","hromada_code","hromada_name","hromada_full_name","raion_code","raion_name","oblast_code","oblast_name","type","occupation","military_action","population_text","partners_text","friends_text","state_communication","prep_first_aid_water","prep_first_aid_fuel","prep_reaction_plan","prep_evacuation_plan","prep_reaction_plan_oth_hromadas","prep_reaction_plan_oda","prep_dftg_creation","prep_national_resistance","prep_starosta_meeting","prep_communal_meetiing","prep_online_map","prep_shelter_list","prep_notification_check","prep_backup","prep_partly_backup","shelter_capacity_before_text","shelter_capacity_now_text","telegram","viber","facebook","chat_help","hotline","telegram_link","facebook_link","head_hromada_communication","dftg_creation","dftg_creation_date","help_for_military","help_for_military/rooms","help_for_military/transport","help_for_military/money","help_for_military/products","help_for_military/other","help_for_military/none","help_for_military_text","transport_help_communal","transport_help_bought","percent_working_march","percent_working_now","commun_between_hromadas","evacuation","idp_accept","idp_registration_date","idp_registration_number","idp_real_number","idp_help","idp_help/communal_placement","idp_help/private_placement","idp_help/regular_meal","idp_help/humanitar_help","idp_help/fundraising","idp_help/employ","idp_help/psych_help","idp_help/law_help","idp_help/transit_center","idp_place_rooms","idp_room_number","idp_child_education","special_fund_relocation","special_fund_relocation_needs","special_fund_relocation_needs/state_functions","special_fund_relocation_needs/defense","special_fund_relocation_needs/public_order","special_fund_relocation_needs/economic_activity","special_fund_relocation_needs/environment","special_fund_relocation_needs/utilities","special_fund_relocation_needs/spirit_development","special_fund_relocation_needs/education","special_fund_relocation_needs/social_protection","special_fund_relocation_needs/healthcare","relocated_companies_text","created_jobs","bussiness_stimules","bussiness_stimules/tax_benefits","bussiness_stimules/free_rooms","bussiness_stimules/education","bussiness_stimules/other","bussiness_stimules_none","bussiness_stimules_other","humanitarian_hub","hromada_cooperation","hromada_cooperation/medicine","hromada_cooperation/food","hromada_cooperation/pensions","hromada_cooperation/evacuation","hromada_cooperation/other","hromada_cooperation/none","hromada_cooperation_text","is_damaged","percent_damaged","damage_evaluation_persons","damage_evaluation_communal","damage_evaluation_bussiness","reconstruction_plan","reconstruction_financing","reconstruction_financing_text","international_projects","percent_reconstructed","finance_school_shelters","finance_school_shelters_coded","info_campaign","reserves","count_power_sources","count_heaters_need","solid_fuel_boiler","no_school_days","no_school_days_coded","hromada_exp","hromada_problem_info","hromada_problem_info/idp","hromada_problem_info/citizens","hromada_problem_info/bussiness","hromada_problem_info/experts","hromada_problem_info/ngo","hromada_problem_info/nobody","hromada_problem_consultation","hromada_problem_consultation/idp","hromada_problem_consultation/citizens","hromada_problem_consultation/bussiness","hromada_problem_consultation/experts","hromada_problem_consultation/ngo","hromada_problem_consultation/nobody","hromada_problem_proposition","hromada_problem_proposition/idp","hromada_problem_proposition/citizens","hromada_problem_proposition/bussiness","hromada_problem_proposition/experts","hromada_problem_proposition/ngo","hromada_problem_proposition/nobody","hromada_problem_system","hromada_problem_system/idp","hromada_problem_system/citizens","hromada_problem_system/bussiness","hromada_problem_system/experts","hromada_problem_system/ngo","hromada_problem_system/nobody","hromada_problem_feedback","hromada_problem_feedback/idp","hromada_problem_feedback/citizens","hromada_problem_feedback/bussiness","hromada_problem_feedback/experts","hromada_problem_feedback/ngo","hromada_problem_feedback/nobody","hromada_problem_execution","hromada_problem_execution/idp","hromada_problem_execution/citizens","hromada_problem_execution/bussiness","hromada_problem_execution/experts","hromada_problem_execution/ngo","hromada_problem_execution/nobody","skills_needed","skills_needed/fundraising","skills_needed/project_management","skills_needed/longterm_planning","skills_needed/crisis_planning","skills_needed/data_analysis","skills_needed/human_resourse","skills_needed/other","skills_needed_text","contact_text","evacuation_001","hromada_exp_problem","_uuid","_submission_time","_validation_status","_status","_submitted_by","_tags","region_en","idp_help/communal_placement_number","idp_help/private_placement_number","idp_help/regular_meal_number","idp_help/humanitar_help_number","idp_help/fundraising_number","idp_help/employ_number","idp_help/psych_help_number","idp_help/law_help_number","idp_help/transit_center_number","idp_help_count","prep_count","comm_channels_count","help_military_count","hromada_cooperation_count","dftg_creation_time","idp_registration_time","prep_winter_count","oblast_center","hromada_center_code","hromada_center","lat_center","lon_center","travel_time","n_settlements","square","occipied_before_2022","total_population_2022","urban_population_2022","urban_pct","budget_code","budget_name","oblast_name_en","region_en.x","region_code_en","income_total_2021","income_transfert_2021","income_military_2021","income_pdfo_2021","income_unified_tax_2021","income_property_tax_2021","income_excise_duty_2021","income_own_2021","own_income_prop_2021","transfert_prop_2021","military_tax_prop_2021","pdfo_prop_2021","unified_tax_prop_2021","property_tax_prop_2021","excise_duty_prop_2021","own_income_change","own_prop_change","total_income_change","income_own","income_total","income_transfert","dfrr_executed","turnout_2020","sex_head","age_head","education_head","incumbent","rda","not_from_here","party","enterpreuner","unemployed","priv_work","polit_work","communal_work","ngo_work","party_national_winner","no_party","male","high_educ","sum_osbb_2020","edem_total","edem_petitions","edem_consultations","edem_participatory_budget","edem_open_hromada","youth_councils","youth_centers","business_support_centers","region_en.y","creation_date","creation_year","time_before_24th","voluntary","war_zone_27_04_2022","war_zone_20_06_2022","war_zone_23_08_2022","war_zone_10_10_2022","income_own_per_capita","income_total_per_capita","income_tranfert_per_capita","idp_registration_share","idp_real_share","idp_child_share"],["dbl","dat","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dat","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","chr","chr","chr","dat","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","lgl","chr","dat","lgl","chr","lgl","lgl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dat","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl"],[0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,4,6,1,2,7,7,5,7,2,6,9,4,7,10,10,0,0,0,0,0,0,0,81,6,0,0,41,6,6,6,6,6,6,6,83,46,46,1,0,12,0,8,16,9,16,8,8,8,8,8,8,8,8,8,8,46,131,15,0,77,77,77,77,77,77,77,77,77,77,77,11,8,8,8,8,8,8,8,99,132,132,132,132,132,132,132,132,136,0,100,100,100,100,105,105,129,14,105,8,16,11,13,12,17,27,22,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,130,15,12,138,0,0,138,0,138,138,0,9,9,9,9,9,9,9,9,9,0,0,0,0,0,16,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,16,16],[0,0,0,0,0,0,0,0,0,0,0,0.7,0.7,0.7,0.7,0.7,0,2.9,4.3,0.7,1.4,5.1,5.1,3.6,5.1,1.4,4.3,6.5,2.9,5.1,7.2,7.2,0,0,0,0,0,0,0,58.7,4.3,0,0,29.7,4.3,4.3,4.3,4.3,4.3,4.3,4.3,60.1,33.3,33.3,0.7,0,8.7,0,5.8,11.6,6.5,11.6,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,33.3,94.9,10.9,0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,71.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,98.6,0,72.5,72.5,72.5,72.5,76.1,76.1,93.5,10.1,76.1,5.8,11.6,8,9.4,8.7,12.3,19.6,15.9,18.8,0,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,0,0,0,0,0,0,0,0,94.2,10.9,8.7,100,0,0,100,0,100,100,0,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,0,0,0,0,0,11.6,29.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31.9,0.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6.5,11.6,11.6],[138,30,138,138,135,137,76,76,22,22,3,5,4,120,11,15,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,105,106,3,3,3,3,3,58,133,5,3,51,20,3,3,3,3,3,3,56,23,32,26,29,6,3,2,32,120,110,68,3,3,3,3,3,3,3,3,3,6,8,79,2,45,3,3,3,3,3,3,3,3,3,3,12,5,11,3,3,3,3,3,32,3,5,3,3,2,3,3,3,3,2,5,3,3,3,3,3,8,11,6,109,82,3,3,3,3,3,61,38,2,18,3,3,3,3,3,3,23,3,3,3,3,3,3,21,3,3,3,3,3,3,24,3,3,3,3,3,3,23,3,3,3,3,3,3,22,3,3,3,3,3,3,42,2,2,2,2,2,2,2,9,124,4,1,138,138,1,1,1,1,5,88,79,52,120,36,31,88,77,37,1,22,11,6,4,32,51,6,2,138,138,138,138,134,54,137,1,138,95,96,138,137,22,5,5,138,138,91,138,138,138,138,138,52,52,10,42,12,30,15,71,42,58,138,138,138,95,138,2,35,2,2,2,2,24,2,2,2,2,2,1,2,2,2,2,38,5,2,2,2,2,3,4,9,5,15,6,15,2,2,2,2,2,138,138,138,130,123,117],[2,null,191541757,null,null,null,null,null,null,null,null,null,null,140,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,0,0,0,0,0,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,0,0,null,null,null,null,23,23,null,0,0,0,0,0,0,0,0,0,null,null,0,null,null,0,0,0,0,0,0,0,0,0,0,null,null,null,0,0,0,0,0,null,null,null,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,0,0,0,627,2,0,0,0,null,null,0,0,null,null,45.68,22.49,0,1,42.2,0,3359,0,0,null,null,null,null,null,10846101.81,5163331,0,1056172.94,227066.07,224034.84,8271,3131966.65,0.14,0.14,0,0.09,0.01,0.01,0,-0.83,-0.39,-0.43,1972353.16,11030764.44,5642000,78.5,0.27,null,32,null,0,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,2015,556.79,0,0,0,0,0,507.13,2607.8,962.57,0.01,0.01,0],[78.2,null,197322877.2,null,null,null,null,null,null,null,null,null,null,21136.72,1.34,1.34,null,1.04,1.08,1.41,1,0.55,0.84,1.02,0.56,1.12,1.13,0.78,1.08,1.24,0.55,0.86,null,null,0.6,0.71,1.8,0.33,0.86,null,null,null,null,null,null,0.73,0.7,0.73,0.97,0.42,0.05,null,null,null,89.22,91.77,null,null,null,null,2001.63,2326.16,null,0.71,0.65,0.41,0.99,0.26,0.22,0.69,0.61,0.28,null,null,64.67,null,null,0.23,0.51,0.34,0.13,0.08,0.49,0.03,0.39,0.39,0.28,null,null,null,0.19,0.18,0.45,0.3,0.19,null,null,null,0.33,0.33,0,0.5,0.33,0.33,null,null,null,null,null,null,null,null,null,null,null,null,853167.02,0.86,0.93,0.9,0.43,0.34,null,null,null,null,0.38,0.64,0.58,0.09,0.35,0.08,null,0.34,0.39,0.33,0.09,0.27,0.24,null,0.28,0.46,0.49,0.08,0.29,0.21,null,0.26,0.45,0.55,0.16,0.34,0.16,null,0.36,0.45,0.47,0.11,0.33,0.16,null,0.15,0.37,0.46,0.07,0.38,0.21,null,0.75,0.41,0.32,0.49,0.26,0.32,0.06,null,null,null,null,null,null,null,null,null,null,null,1590.84,1037.82,1107.57,1977.64,461.12,623.09,1618.08,1513.87,784.98,627,13.72,4.29,3.39,0.08,null,null,3.11,0.01,null,null,49.07,29.43,93.67,22.23,410.51,0,22076.86,12499.06,0.35,null,null,null,null,null,91899785.68,37659862.26,1815527.65,31364648.7,6130599.53,8123370.3,3741911.43,54239923.43,0.51,0.49,0.01,0.27,0.06,0.1,0.03,0.04,0.01,-0.01,56792346.01,92278010.92,35485664.91,32738.42,0.42,null,52.36,null,0.54,0.07,0.11,null,0.02,0.02,0.08,0.83,0.04,0,0.16,0.43,0.27,0.93,35.48,0.62,0.22,0.16,0.15,0.09,0.1,0.22,0.56,null,null,2018.22,1209.18,0.58,0.08,0.12,0.12,0.12,2244.72,4224.02,1979.3,0.1,0.1,0.05],[151,null,206471695,null,null,null,null,null,null,null,null,null,null,243000,20,17,null,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,null,null,2,2,2,2,2,null,null,null,null,null,null,1,1,1,1,1,1,null,null,null,100,100,null,null,null,null,20000,60000,null,1,1,1,1,1,1,1,1,1,null,null,800,null,null,1,1,1,1,1,1,1,1,1,1,null,null,null,1,1,1,1,1,null,null,null,1,1,0,1,1,1,null,null,null,null,null,null,null,null,null,null,null,null,13936323,1,1,1,1,1,null,null,null,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,1,null,null,null,null,null,null,null,null,null,null,null,20000,16331,20000,20000,8500,20000,20000,20000,16331,627,29,10,5,3,null,null,5,1,null,null,52.06,36.73,288,97,2497.1,0,317752,305239,1,null,null,null,null,null,1288755475.83,346574777.46,47254976.84,608781726.22,124876522.55,78663469.37,73206177.69,942180698.37,0.86,0.86,0.14,0.59,0.13,0.44,0.27,1.69,0.23,0.89,969725144.97,1248182878.17,315122334.64,757596.25,0.65,null,71,null,1,1,1,null,1,1,1,1,1,0,1,1,1,1,638,4,1,1,1,1,2,4,17,null,null,2020,2383.79,1,1,1,1,1,7418.91,9388.75,3470.86,0.63,0.63,0.33]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>type<\/th>\n      <th>na<\/th>\n      <th>na_pct<\/th>\n      <th>unique<\/th>\n      <th>min<\/th>\n      <th>mean<\/th>\n      <th>max<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"autoWidth":false,"columnDefs":[{"className":"dt-right","targets":[3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```

# 0. Introduction

<mark>0.1</mark> What is the goal of this report?

> This report overviews the responses to the survey conducted by \_\_\_\_\_ in Ukraine during 2022


# 1. General Information


```r
meta_survey %>% filter(group=="preamble") %>% pull(label) %>% cat()
```

```
Супротив України у загарбницькій війні Російської Федерації великою мірою пов’язаний зі стійкістю, яку виявили територіальні громади. 
Щоб оцінити, як різні потрясіння вплинули на громади та перевірити, які фактори впливають на стійкість громад у війні, Київська Школа Економіки проводить опитування.
Ми гарантуємо конфіденційність Ваших відповідей. Всі отримані дані будуть аналізуватись тільки в узагальненому вигляді.
Крайній термін заповнення анкети - 4 листопада.
```

<mark>1.1</mark> How many hromadas contributed responses to so far?

> As of 2022-12-27, 138 hromadas contributed valid response to the survey

<mark>1.2</mark> What oblasts are represented in this sample>? 



```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en, oblast_name_en) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  left_join(
    ds_general %>% 
      group_by(region_en,  oblast_name_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  mutate(
    prop = hromada_count/hromada_count_total
    ,pct = scales::percent(prop, accuracy = .1)
  ) %>% 
  arrange(region_en, oblast_name_en) %>% 
  select(-prop) %>% 
  ungroup() %>%
  # neat_DT()
  neat()
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> region_en </th>
   <th style="text-align:left;"> oblast_name_en </th>
   <th style="text-align:right;"> hromada_count </th>
   <th style="text-align:right;"> hromada_count_total </th>
   <th style="text-align:left;"> pct </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Cherkassy </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> 7.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Khmelnitsk </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 1.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Kirovograd </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> 10.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Poltava </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 13.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Vinnytsia </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> 7.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Driproptrovska </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:left;"> 14.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Kharkiv </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> 7.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Zaporizka </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:left;"> 4.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Chernigiv </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> 12.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Kyiv-oblast </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:left;"> 7.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Sumska </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:left;"> 15.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Zhytomir </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> 4.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Kherson </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> 14.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Mykolayiv </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> 1.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Odesa </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:left;"> 13.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Cherniveska </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> 5.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Ivano-Frankivsk </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> 25.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Lviv </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:left;"> 12.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Rivenska </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:left;"> 6.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Ternopilska </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> 7.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Vonyn </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> 9.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Zakarpatska </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:left;"> 17.2% </td>
  </tr>
</tbody>
</table>



<mark>1.3</mark> What type of hromadas are represented in the sample? 



```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("type"))+
  labs(
    title = "What types of hromadas repsonded to the survey?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/unnamed-chunk-5-1.png)<!-- -->

<mark>1.4</mark> What hromadas experienced military occupation or  military actions? 

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("military_action"))+
  labs(
    title = "How many respondent hromadas have experienced military action at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/unnamed-chunk-6-1.png)<!-- -->

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("occupation"))+
  labs(
    title = "How many respondent hromadas have experienced occupation at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y=NULL
  )
```

![](figure-png-iso/unnamed-chunk-6-2.png)<!-- -->

```{.r .fold-hide}
ds0 %>% make_bi_freq_graph("military_action","occupation")
```

![](figure-png-iso/unnamed-chunk-6-3.png)<!-- -->

```{.r .fold-hide}
ds0 %>% make_bi_freq_graph("occupation","military_action")
```

![](figure-png-iso/unnamed-chunk-6-4.png)<!-- -->



# 2. Preparation




```{.r .fold-hide}
d1 <- 
  ds1_prep_ordinal_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d2 <- 
  ds1_prep_binary_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d12 <- 
  bind_rows(
    d1
    ,d2 %>% filter(value == "Yes")
  ) %>% 
  left_join(
    d_meta_prep
  ) %>% 
  arrange(item_name, value) 

d_in <- 
  d12 %>% 
    mutate(
    display_name = label_en
    ,display_name = factor(
      display_name
      ,levels =  d12 %>%
        left_join(d_meta_prep) %>%
        filter(value == "Yes") %>%
        # filter(value == "Before Feb 24") %>%
        arrange(prop) %>%
        # arrange(desc(item_number)) %>%
        pull(label_en)
    ) # display_name
    ,value = fct_relevel(
      value
      , "As of Oct", "As of Feb","Yes", "No", "Not Applicable",
    ) %>% fct_rev()
  ) 

g <- 
  d_in %>% 
  { 
  ggplot(
    data = (.) %>% filter(value !="Yes") %>% mutate(value=factor(value))
    ,aes(x=prop, y = display_name, fill=value)
  )+
  geom_col(position = position_stack()
           , alpha = .7
           ,data =
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = 1
            , size = 4
            ,color="white"
            ,position = position_stack()
            ,data = . %>% filter(value !="Yes")
            )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = -.5
            ,vjust = .5
            , size = 2
            ,color="black"
            ,position = position_stack()
            ,data = (.) %>% filter(value=="Yes") %>% mutate(value=NA)

            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  scale_fill_viridis_d(
    begin = .8, end = .0, direction = -1
    , option = "plasma", guide= guide_legend(reverse=T)
  )+
  labs(
    title = "Have your hromada made the following preparations?"
    ,x = "Percent of respondents", y = NULL, fill = NULL
    ,caption = "Cummulative percent shown in black"
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
  }

g
```

![](figure-png-iso/preparation-summary-1-1.png)<!-- -->

```{.r .fold-hide}
# g %>% quick_save("2-preparation-summary-yes",w=12,h=5)
```

<mark>2.1</mark> What questions were asked about preparations hromadas made? 


```{.r .fold-hide}
ds0 %>% 
  select(preparation) %>% 
  explore::describe_all() %>% 
  left_join(
    meta_survey %>% filter(group=="preparation") %>% select(name,label_en,label)
    ,by=c("variable"="name")) %>% 
  relocate(c("label_en","label"),.after = "variable") %>% 
  select(1:3) %>%
  neat()
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> label_en </th>
   <th style="text-align:left;"> label </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> prep_first_aid_water </td>
   <td style="text-align:left;"> Water stored (1) </td>
   <td style="text-align:left;"> Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_first_aid_fuel </td>
   <td style="text-align:left;"> Fuel stored (2) </td>
   <td style="text-align:left;"> Сформовані запаси товарів першої необхідності (паливо) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan </td>
   <td style="text-align:left;"> Plan of response (3) </td>
   <td style="text-align:left;"> Оновлено чи затверджено план реагування на надзвичайні ситуації </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_evacuation_plan </td>
   <td style="text-align:left;"> Plan of evacuation (4) </td>
   <td style="text-align:left;"> Складено спеціальний план евакуації населення при загрозі збройного конфлікту </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan_oth_hromadas </td>
   <td style="text-align:left;"> Plan coord w/ oth. Hs (5) </td>
   <td style="text-align:left;"> Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan_oda </td>
   <td style="text-align:left;"> Plan coord w/ Oblast (6) </td>
   <td style="text-align:left;"> Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_dftg_creation </td>
   <td style="text-align:left;"> Territorial Defense (7) </td>
   <td style="text-align:left;"> Розпочато створення (добровольчого) формування територіальної громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_national_resistance </td>
   <td style="text-align:left;"> Plan of resistance (8) </td>
   <td style="text-align:left;"> Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_starosta_meeting </td>
   <td style="text-align:left;"> Meeting with heads (9) </td>
   <td style="text-align:left;"> Проведена зустріч зі старостами з приводу дій у випадку вторгнення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_communal_meetiing </td>
   <td style="text-align:left;"> Meeting with utilities (10) </td>
   <td style="text-align:left;"> Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_online_map </td>
   <td style="text-align:left;"> Shelter map online (11) </td>
   <td style="text-align:left;"> Опублікована онлайн-мапа укриттів в громаді </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_shelter_list </td>
   <td style="text-align:left;"> Shelter list online (12) </td>
   <td style="text-align:left;"> Опублікований перелік адрес укриттів в соцмережах або на сайті громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_notification_check </td>
   <td style="text-align:left;"> Communication tested (13) </td>
   <td style="text-align:left;"> Перевірено засоби оповіщення населення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_backup </td>
   <td style="text-align:left;"> Data backup fully (14) </td>
   <td style="text-align:left;"> Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_partly_backup </td>
   <td style="text-align:left;"> Data backed up partially (15) </td>
   <td style="text-align:left;"> Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру) </td>
  </tr>
</tbody>
</table>

## Item-total correlations 


We can conceptualize hromadas' the preparation for invasion as two quantities:  
- `prep_score_feb` - the number of security measures implemented as of February 2022   
- `prep_score_oct` - the number of security measures implemented as of October 2022


```r
ds1_prep %>% select(1:4) # + individual preparation items
```

```
# A tibble: 138 x 4
   hromada_code        prep_score_combo prep_score_feb prep_score_oct
   <chr>                          <dbl>          <dbl>          <dbl>
 1 UA12060190000043514               23             10             13
 2 UA46060370000065608               14              4             10
 3 UA35060190000079777               16              3             13
 4 UA35020130000045875               14              2             12
 5 UA53060250000043118               20              6             14
 6 UA65060250000073379               13              6              7
 7 UA51040110000040346                9              1              8
 8 UA59080230000084731               10              0             10
 9 UA05100110000070795               12              0             12
10 UA51100250000055079               10              0             10
# ... with 128 more rows
```
We  also compute `prep_score_combo`, which is a sum of `prep_score_feb` and `prep_score_oct`, the quantity equivalent to weighting the implementation of security measures  prior to Feb 24, 2022 **twice as important**.

These three scores are distributed as follows:  

```{.r .fold-hide}
g <-  
  ds1_prep %>%
  select(starts_with("prep_score")) %>% 
  pivot_longer(cols = everything(),names_to = "measure",values_to="value") %>% 
  mutate( 
    measure = factor(measure,
                        levels = c("prep_score_feb","prep_score_oct","prep_score_combo")
                        ,labels = c("..as of February","..as of October", "Combined = Feb + Oct")
                        )
  ) %>% 
  ggplot(aes(x=value))+
  geom_histogram(binwidth = 1, alpha = .4)+
  scale_x_continuous(breaks = seq(0,30,5),minor_breaks = NULL)+
  facet_wrap("measure",ncol =1)+
  labs(
    title = "How many security measures have your hromada implemented..."
    # ,subtitle = "Combined score = February + October"
  )
g
```

![](figure-png-iso/info-score-distribution-1.png)<!-- -->

```{.r .fold-hide}
# g %>%  quick_save("score-distribution",w=3.5, h=6)
```


```{.r .fold-hide}
ds1_prep %>% select(starts_with("prep_score")) %>% GGally::ggpairs()
```

![](figure-png-iso/unnamed-chunk-9-1.png)<!-- -->

```{.r .fold-hide}
# Note that correlation coefficient is Pearson
```

The item-total correlations indicates that all three preparedness scores are adequate unidimensional measures.


```{.r .fold-hide}
# Step 1 - create data sets with re-coded item responses
# As of February 2022, how many of these security steps have been implemented?
d_feb <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 0 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)


# As of October 20200, how many of these security steps have been implemented?
d_oct <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 1 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)

# What is the combined score of preparedness if we give 2 points for having
# a security measure implemented as of February, and 1 point - as of October?
d_combo <- 
  ds_survey %>% 
  select(hromada_code, preparation)



# convert to matrices
m_feb <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_feb) %>% # raw scores have been converted to binary for as of Feb
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_oct <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_oct) %>% # raw scores have been converted to binary for as of Oct
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_combo <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_combo) %>% 
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")


d_item_total <- 
  list(
    "Combination" = m_combo[,"prep_score_combo"]
    ,"February"  = m_feb[,"prep_score_feb"]
    ,"October"  = m_oct[,"prep_score_oct"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_combo)) %>% 
  filter(item_name != c("prep_score_combo","prep_score_feb","prep_score_oct")) %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name) %>% 
  pivot_longer(
    cols = 2:4
    ,names_to = "scenario"
    ,values_to = "correlation"
  ) %>% 
  mutate(
    discrimination = case_when(
      correlation <= 0  ~ "problematic"
      ,correlation > 0 & correlation < .2 ~ "poor"
      ,correlation >=.2 & correlation < .4 ~ "good"
      ,correlation >=.4  ~ "very good"
    ) %>% factor(levels = c("problematic","poor","good","very good"))
    ,scenario = scenario %>% factor(
      labels=c("February","October","Combination"))
    ,item_name = factor(item_name,levels = preparation) %>% fct_rev()
  )

discrimination_levels <- c(
  "problematic" = "#d01c8b"
  ,"poor"        = "#f1b6da"
  ,"good"        = "#b8e186"
  ,"very good"   = "#4dac26"
)

g_item_total <-
  d_item_total %>% 
  ggplot(aes(x = item_name, y = correlation, color = discrimination, group = scenario))+
  geom_line(aes(group = "scenario"))+
  geom_point()+
  geom_text(aes(label=correlation %>% scales::number(accuracy = .01) %>% RemoveLeadingZero()),hjust=-.3
            ,size = 3)+
  geom_hline(aes( yintercept = 0))+ 
  facet_wrap("scenario",nrow=1)+
  scale_y_continuous(limits = c(-.3,.7), expand = expansion(add = c(0,.2)))+
  scale_color_manual(
    values = discrimination_levels
    , limits = names(discrimination_levels)
  )+
  coord_flip() +
  labs(
    title = "Item-total corellations under three scoring scenarios"
    ,y = "Item-total Correlation (Spearman)"
    ,x = NULL
    ,color = "Discrimination"
  )

g_item_total
```

![](figure-png-iso/prep-item-total-1.png)<!-- -->

```{.r .fold-hide}
g_item_total %>% quick_save("item-total",w=8,h=4)
```

While all three metrics should be considered during modeling, the next section demonstrates why and how the interpreations of these scores will differ

## Prep score change
 
Let's us visualize individual scores of invasion preparedness

```{.r .fold-hide}
d <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score_")) %>% 
arrange(prep_score_oct, prep_score_feb) 

make_plot_prep_change_bw <- function(
    d
    ,order_by #= c("prep_score_feb","prep_score_oct")
    ,color_by #= "row_number_ntile"
    ,ntile_count = 10
){
  # browser()
  
  level_order <- d %>% arrange(!!!rlang::syms(order_by)) %>% pull(hromada_code)
  caption_text = paste0("Order by: ", paste0(order_by,collapse = " + "), " | Color by: ", color_by)
  g <- 
    d %>%
    arrange(!!!rlang::syms(order_by)) %>% 
    mutate(
      hromada_code = hromada_code %>% factor(levels = level_order)
      ,prep_score_combo_ntile = ntile(prep_score_combo,ntile_count)        %>% factor()
      ,prep_score_feb_ntile   = ntile(prep_score_feb,ntile_count) %>% factor()
      ,prep_score_oct_ntile   = ntile(prep_score_oct,ntile_count)  %>% factor()
      ,row_number_ntile       = ntile(row_number(),ntile_count)      %>% factor()
    ) %>% 
    # graphing begins
    ggplot(aes(y=hromada_code, color = !!rlang::sym(color_by) ))+
    geom_segment(
      aes(
        y     = hromada_code
        ,yend = hromada_code
        ,x    = prep_score_feb
        ,xend = prep_score_oct
      )
      ,linewidth = 2 ,alpha = 1
    )+
    geom_segment(
      aes(
        y     = hromada_code
        ,yend = hromada_code
        ,x    = 0
        ,xend = prep_score_feb
      )
      ,linewidth = 2 ,alpha = .1
      , color = "black"
    )+
    scale_color_brewer(type="div", palette = "Spectral")+
    scale_x_continuous(
      breaks = seq(0,15,5),minor_breaks = seq(0,15,1)
      # ,limits = c(-10,25)
      )+
    labs(
      title = paste0("The number of security measures implemented by hromadas (N= ",
                     d %>% summarize(n=n_distinct(hromada_code)) %>% pull(n)
                     ,")")
      ,subtitle = caption_text
      ,x = "Each segment starts at February and ends at October preparedness score"
      # ,caption = caption_text
      ,y = NULL
      ,color = "Percentile\nGroup\n"
    )+
    theme(
      axis.text.y = element_blank()
      ,panel.grid.major.y = element_blank()
      ,panel.border = element_blank()
    )+
    guides(color = guide_legend(override.aes = list(linewidth=7), reverse=TRUE))
  return(g)
}
# Ordering by the total score (before + after OR sum(0|1|2)) 
g <- d %>% make_plot_prep_change_bw(order_by = "prep_score_combo",color_by = "prep_score_combo_ntile") # 
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-1-1.png)<!-- -->

```{.r .fold-hide}
# g %>% quick_save("prep-change-segment-bw",w=5.5,h=9)
```

However,this scoring method may not work for operationalizing preparedness as of October


```{.r .fold-hide}
g <- d %>% make_plot_prep_change_bw(order_by = c("prep_score_oct","prep_score_feb"), color_by = "prep_score_combo_ntile")
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-2-1.png)<!-- -->

or as of February


```{.r .fold-hide}
g <- d %>% make_plot_prep_change_bw(order_by = c("prep_score_feb","prep_score_oct"), color_by = "prep_score_combo_ntile")
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-3-1.png)<!-- -->


 
<mark>**Conclusion**</mark> 

> Both `prep_score_feb` and `prep_score_oct` are meaningful, adequate unidimensional measures with a straightforward interpretation: *Number of security measures implemented as of a given date*. 

> The measure `prep_score_combo` is also an adequate unidimensional measure, but it does not have a clear interpretation of its value.

 
 







# 3. Information



```r
meta_survey %>% filter(group=="information_hat") %>% pull(label) %>% cat()
```

```
Який з перелічених нижче засобів використовується громадою для інформування населення...
```


```{.r .fold-hide}
d1 <- 
  ds1_info %>% 
  pivot_longer(
    cols = item_information
    ,names_to = "item_name"
    ,values_to = "item_response"
  ) %>% 
  group_by(item_name,item_response) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d2 <- 
  ds1_info %>% 
   pivot_longer(
    cols = item_information
    ,names_to = "item_name"
    ,values_to = "item_response"
  ) %>% 
  mutate(
    item_response = case_when(
      item_response != "No" ~ "Yes"
    )
  ) %>%
  group_by(item_name,item_response) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d12 <- 
  bind_rows(
    d1
    ,d2 %>% filter(item_response == "Yes")
  ) %>% 
  left_join(
    d_meta_info
  ) %>% 
  arrange(item_name, item_response) 

d_in <- 
  d12 %>% 
    mutate(
    display_name = label_en
    ,display_name = factor(
      display_name
      ,levels =  d12 %>%
        left_join(d_meta_info) %>%
        filter(item_response == "Yes") %>%
        # filter(value == "Before Feb 24") %>%
        arrange(prop) %>%
        # arrange(desc(item_number)) %>%
        pull(label_en)
    ) # display_name
    ,item_response = fct_relevel(
      item_response
      , "Before Feb 24", "After Feb 24","Yes", "No", "Not Applicable",
    ) %>% fct_rev()
  ) 

g <- 
  d_in %>% 
  { 
  ggplot(
    data = (.) %>% filter(item_response !="Yes") %>% mutate(item_response=factor(item_response))
    ,aes(x=prop, y = display_name, fill=item_response)
  )+
  geom_col(position = position_stack()
           , alpha = .7
           ,data =
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = 1
            , size = 4
            ,color="white"
            ,position = position_stack()
            ,data = . %>% filter(item_response !="Yes")
            )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = -.5
            ,vjust = .5
            , size = 2
            ,color="black"
            ,position = position_stack()
            ,data = (.) %>% filter(item_response=="Yes") %>% mutate(item_response=NA)

            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  scale_fill_viridis_d(
    begin = .8, end = .0, direction = -1
    , option = "plasma", guide= guide_legend(reverse=T)
  )+
  labs(
    title = "What channels of communication are used to dissiminate information?"
    ,x = "Percent of respondents", y = NULL, fill = NULL
    ,caption = "Cummulative percent shown in black"
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
  }

g
```

![](figure-png-iso/information-summary-1-1.png)<!-- -->



```r
meta_survey %>% filter(group=="information_freq") %>% pull(label) %>% cat()
```

```
Як часто в середньому голова громади звертався (відео чи повідомлення) до населення громади стосовно стану справ у перший місяць від повномасштабного вторгнення?
```



```{.r .fold-hide}
(ds0 %>% 
  mutate(
    
    head_hromada_communication = fct_recode(
      head_hromada_communication,
       "Once a week"      = "once_a_week"
      ,"Once a day"       = "once_a_day"
      ,"Never"            = "none"
      ,"Few times a week" = "few_times_a_week"
      ,"2-3 times a day"  = "2_3_times"
    ) %>% factor( levels = c(
       "Never"           
       ,"Once a week"     
       ,"Few times a week"
       ,"Once a day"      
       ,"2-3 times a day"
    )
    )
  ) %>% 
  make_bi_freq_graph("head_hromada_communication") )+
  labs(
    title = "How frequently did hromada head communicated in the frist month of invasion?"
    ,x = NULL
  )
```

![](figure-png-iso/info-1 -1.png)<!-- -->



```{.r .fold-hide}
d <- 
  ds0 %>%  
  select(hromada_code,head_hromada_communication, facebook,viber,telegram ) %>% 
  pivot_longer(cols = c("facebook","viber","telegram")) %>% 
  mutate(
    had_sn_before = case_when(
      value %in% c(0,1) ~ "No cccount before Feb24"
      ,value %in% c(2)  ~ "Had cccount before Feb24"
    )
  ) %>% 
  arrange(hromada_code, head_hromada_communication) %>% 
  group_by(hromada_code,head_hromada_communication) %>% 
  mutate(
    had_any_sn_before_feb24 = sum(value==2,na.rm = T)>0
  ) %>% 
  ungroup() %>% 
  distinct(hromada_code, head_hromada_communication,had_any_sn_before_feb24) %>% 
  mutate(
    time_per_week = fct_recode(head_hromada_communication,
        "1"  = "once_a_week"    
      , "7"  = "once_a_day"   
      , "0"  = "none"   
      , "3"  = "few_times_a_week"  
      , "15" = "2_3_times"   
    ) %>% as.character() %>% as.integer()
    , head_hromada_communication = fct_recode(
      head_hromada_communication,
       "Once a week"      = "once_a_week"
      ,"Once a day"       = "once_a_day"
      ,"Never"            = "none"
      ,"Few times a week" = "few_times_a_week"
      ,"2-3 times a day"  = "2_3_times"
    ) %>% factor( levels = c(
       "Never"           
       ,"Once a week"     
       ,"Few times a week"
       ,"Once a day"      
       ,"2-3 times a day"
    )
    )
  ) 
(d %>% make_bi_freq_graph("had_any_sn_before_feb24")) +
  labs(
    title = "Did hromadas have account on any social network?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, fill = "Had account"
  )
```

![](figure-png-iso/info-2-1.png)<!-- -->


```{.r .fold-hide}
d %>% 
  group_by(had_any_sn_before_feb24) %>% 
  summarize(mean_times_per_week = mean(time_per_week,na.rm =T)) %>% 
  ggplot(aes(x=mean_times_per_week, y= had_any_sn_before_feb24,
             fill = had_any_sn_before_feb24))+
  geom_col()+
  geom_text(aes(label=scales::comma(mean_times_per_week)))+
  labs(
    title = "How freqently did heads of hromadas communicate with the community during the first month of invasion?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, x = "Average times per week", fill = "Had account on\nany social network"
  )
```

![](figure-png-iso/info-3 -1.png)<!-- -->


```{.r .fold-hide}
d %>% 
  group_by(head_hromada_communication,had_any_sn_before_feb24) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(had_any_sn_before_feb24) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ggplot(aes(x=prop, y = head_hromada_communication, fill = had_any_sn_before_feb24))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = -.5,position = position_dodge(width = .9))+
  scale_x_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T)
    )+
  labs(
    title = "How frequently did heads of hromadas communicated during the first month of invasion?"
    ,fill = "Had accounts\non social networks\nbefore Feb 24"
    ,x = "Percent of respondents in each group"
    , y = NULL
  )
```

![](figure-png-iso/info-4 -1.png)<!-- -->


# 4. National Resistance

## 4.1 Transport Help

# 5. Administrative Adaptation

# 6. Evacuation

# 7. IDP

# 8. Economics

# 9. Humanitarian

# 10. Reconstructioin

## 10.1 Demage Evaluation

# 11. Current Challenges

## 11.1 Heating season

## 11.2 Problem Involvement


```r
meta_survey %>% filter(group=="preparation") %>% pull(name)
```

```
 [1] "prep_first_aid_water"            "prep_first_aid_fuel"            
 [3] "prep_reaction_plan"              "prep_evacuation_plan"           
 [5] "prep_reaction_plan_oth_hromadas" "prep_reaction_plan_oda"         
 [7] "prep_dftg_creation"              "prep_national_resistance"       
 [9] "prep_starosta_meeting"           "prep_communal_meetiing"         
[11] "prep_online_map"                 "prep_shelter_list"              
[13] "prep_notification_check"         "prep_backup"                    
[15] "prep_partly_backup"             
```

# Session Information {#session-info}

For the sake of documentation and reproducibility, the current report was rendered in the following environment. Click the line below to expand.

<details>

<summary>Environment </summary>


```
- Session info -----------------------------------------------------------------------------------
 setting  value
 version  R version 4.2.2 (2022-10-31 ucrt)
 os       Windows 10 x64 (build 19045)
 system   x86_64, mingw32
 ui       RTerm
 language (EN)
 collate  Ukrainian_Ukraine.utf8
 ctype    Ukrainian_Ukraine.1251
 tz       Europe/Helsinki
 date     2022-12-27
 pandoc   2.19.2 @ C:/Program Files/RStudio/bin/quarto/bin/tools/ (via rmarkdown)

- Packages ---------------------------------------------------------------------------------------
 ! package       * version date (UTC) lib source
 D archive         1.1.5   2022-05-06 [1] CRAN (R 4.2.2)
   assertthat      0.2.1   2019-03-21 [1] CRAN (R 4.2.2)
   backports       1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
   bit             4.0.5   2022-11-15 [1] CRAN (R 4.2.2)
   bit64           4.0.5   2020-08-30 [1] CRAN (R 4.2.2)
   broom           1.0.1   2022-08-29 [1] CRAN (R 4.2.2)
   bslib           0.4.1   2022-11-02 [1] CRAN (R 4.2.2)
   cachem          1.0.6   2021-08-19 [1] CRAN (R 4.2.2)
   callr           3.7.3   2022-11-02 [1] CRAN (R 4.2.2)
   cellranger      1.1.0   2016-07-27 [1] CRAN (R 4.2.2)
   cli             3.4.1   2022-09-23 [1] CRAN (R 4.2.2)
   colorspace      2.0-3   2022-02-21 [1] CRAN (R 4.2.2)
   crayon          1.5.2   2022-09-29 [1] CRAN (R 4.2.2)
   crosstalk       1.2.0   2021-11-04 [1] CRAN (R 4.2.2)
   curl            4.3.3   2022-10-06 [1] CRAN (R 4.2.2)
   DBI             1.1.3   2022-06-18 [1] CRAN (R 4.2.2)
   dbplyr          2.2.1   2022-06-27 [1] CRAN (R 4.2.2)
   devtools        2.4.5   2022-10-11 [1] CRAN (R 4.2.2)
   dichromat     * 2.0-0.1 2022-05-02 [1] CRAN (R 4.2.0)
   digest          0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
   dplyr         * 1.0.10  2022-09-01 [1] CRAN (R 4.2.2)
   DT              0.26    2022-10-19 [1] CRAN (R 4.2.2)
   ellipsis        0.3.2   2021-04-29 [1] CRAN (R 4.2.2)
   evaluate        0.18    2022-11-07 [1] CRAN (R 4.2.2)
   explore         1.0.0   2022-11-11 [1] CRAN (R 4.2.2)
   fansi           1.0.3   2022-03-24 [1] CRAN (R 4.2.2)
   farver          2.1.1   2022-07-06 [1] CRAN (R 4.2.2)
   fastDummies   * 1.6.3   2020-11-29 [1] CRAN (R 4.2.2)
   fastmap         1.1.0   2021-01-25 [1] CRAN (R 4.2.2)
   forcats       * 0.5.2   2022-08-19 [1] CRAN (R 4.2.2)
   fs              1.5.2   2021-12-08 [1] CRAN (R 4.2.2)
   gargle          1.2.1   2022-09-08 [1] CRAN (R 4.2.2)
   generics        0.1.3   2022-07-05 [1] CRAN (R 4.2.2)
   ggplot2       * 3.4.0   2022-11-04 [1] CRAN (R 4.2.2)
   glue            1.6.2   2022-02-24 [1] CRAN (R 4.2.2)
   googledrive     2.0.0   2021-07-08 [1] CRAN (R 4.2.2)
   googlesheets4   1.0.1   2022-08-13 [1] CRAN (R 4.2.2)
   gridExtra       2.3     2017-09-09 [1] CRAN (R 4.2.2)
   gt            * 0.8.0   2022-11-16 [1] CRAN (R 4.2.2)
   gtable          0.3.1   2022-09-01 [1] CRAN (R 4.2.2)
   haven           2.5.1   2022-08-22 [1] CRAN (R 4.2.2)
   highr           0.9     2021-04-16 [1] CRAN (R 4.2.2)
   hms             1.1.2   2022-08-19 [1] CRAN (R 4.2.2)
   htmltools       0.5.4   2022-12-07 [1] CRAN (R 4.2.2)
   htmlwidgets     1.5.4   2021-09-08 [1] CRAN (R 4.2.2)
   httpuv          1.6.6   2022-09-08 [1] CRAN (R 4.2.2)
   httr            1.4.4   2022-08-17 [1] CRAN (R 4.2.2)
   import          1.3.0   2022-05-23 [1] CRAN (R 4.2.2)
   janitor         2.1.0   2021-01-05 [1] CRAN (R 4.2.2)
   jquerylib       0.1.4   2021-04-26 [1] CRAN (R 4.2.2)
   jsonlite        1.8.4   2022-12-06 [1] CRAN (R 4.2.2)
   kableExtra      1.3.4   2021-02-20 [1] CRAN (R 4.2.2)
   knitr         * 1.41    2022-11-18 [1] CRAN (R 4.2.2)
   labeling        0.4.2   2020-10-20 [1] CRAN (R 4.2.0)
   labelled      * 2.10.0  2022-09-14 [1] CRAN (R 4.2.2)
   later           1.3.0   2021-08-18 [1] CRAN (R 4.2.2)
   lattice         0.20-45 2021-09-22 [2] CRAN (R 4.2.2)
   lifecycle       1.0.3   2022-10-07 [1] CRAN (R 4.2.2)
   lubridate     * 1.9.0   2022-11-06 [1] CRAN (R 4.2.2)
   magrittr        2.0.3   2022-03-30 [1] CRAN (R 4.2.2)
   Matrix        * 1.5-1   2022-09-13 [2] CRAN (R 4.2.2)
   memoise         2.0.1   2021-11-26 [1] CRAN (R 4.2.2)
   mime            0.12    2021-09-28 [1] CRAN (R 4.2.0)
   miniUI          0.1.1.1 2018-05-18 [1] CRAN (R 4.2.2)
   mitools         2.4     2019-04-26 [1] CRAN (R 4.2.2)
   modelr          0.1.10  2022-11-11 [1] CRAN (R 4.2.2)
   munsell         0.5.0   2018-06-12 [1] CRAN (R 4.2.2)
   pacman          0.5.1   2019-03-11 [1] CRAN (R 4.2.2)
   pillar          1.8.1   2022-08-19 [1] CRAN (R 4.2.2)
   pkgbuild        1.4.0   2022-11-27 [1] CRAN (R 4.2.2)
   pkgconfig       2.0.3   2019-09-22 [1] CRAN (R 4.2.2)
   pkgload         1.3.2   2022-11-16 [1] CRAN (R 4.2.2)
   prettyunits     1.1.1   2020-01-24 [1] CRAN (R 4.2.2)
   processx        3.8.0   2022-10-26 [1] CRAN (R 4.2.2)
   profvis         0.3.7   2020-11-02 [1] CRAN (R 4.2.2)
   promises        1.2.0.1 2021-02-11 [1] CRAN (R 4.2.2)
   ps              1.7.2   2022-10-26 [1] CRAN (R 4.2.2)
   purrr         * 0.3.5   2022-10-06 [1] CRAN (R 4.2.2)
   R6              2.5.1   2021-08-19 [1] CRAN (R 4.2.2)
   RColorBrewer  * 1.1-3   2022-04-03 [1] CRAN (R 4.2.0)
   Rcpp            1.0.9   2022-07-08 [1] CRAN (R 4.2.2)
   readr         * 2.1.3   2022-10-01 [1] CRAN (R 4.2.2)
   readxl        * 1.4.1   2022-08-17 [1] CRAN (R 4.2.2)
   remotes         2.4.2   2021-11-30 [1] CRAN (R 4.2.2)
   reprex          2.0.2   2022-08-17 [1] CRAN (R 4.2.2)
   rlang           1.0.6   2022-09-24 [1] CRAN (R 4.2.2)
   rmarkdown       2.18    2022-11-09 [1] CRAN (R 4.2.2)
   rstudioapi      0.14    2022-08-22 [1] CRAN (R 4.2.2)
   rvest           1.0.3   2022-08-19 [1] CRAN (R 4.2.2)
   sass            0.4.4   2022-11-24 [1] CRAN (R 4.2.2)
   scales          1.2.1   2022-08-20 [1] CRAN (R 4.2.2)
   sessioninfo     1.2.2   2021-12-06 [1] CRAN (R 4.2.2)
   shiny           1.7.3   2022-10-25 [1] CRAN (R 4.2.2)
   snakecase       0.11.0  2019-05-25 [1] CRAN (R 4.2.2)
   stringi         1.7.8   2022-07-11 [1] CRAN (R 4.2.1)
   stringr       * 1.5.0   2022-12-02 [1] CRAN (R 4.2.2)
   survey        * 4.1-1   2021-07-19 [1] CRAN (R 4.2.2)
   survival      * 3.4-0   2022-08-09 [2] CRAN (R 4.2.2)
   svglite         2.1.0   2022-02-03 [1] CRAN (R 4.2.2)
   systemfonts     1.0.4   2022-02-11 [1] CRAN (R 4.2.2)
   testit          0.13    2021-04-14 [1] CRAN (R 4.2.2)
   tibble        * 3.1.8   2022-07-22 [1] CRAN (R 4.2.2)
   tidyr         * 1.2.1   2022-09-08 [1] CRAN (R 4.2.2)
   tidyselect      1.2.0   2022-10-10 [1] CRAN (R 4.2.2)
   tidyverse     * 1.3.2   2022-07-18 [1] CRAN (R 4.2.2)
   timechange    * 0.1.1   2022-11-04 [1] CRAN (R 4.2.2)
   tzdb            0.3.0   2022-03-28 [1] CRAN (R 4.2.2)
   urlchecker      1.0.1   2021-11-30 [1] CRAN (R 4.2.2)
   usethis         2.1.6   2022-05-25 [1] CRAN (R 4.2.2)
   utf8            1.2.2   2021-07-24 [1] CRAN (R 4.2.2)
   vctrs           0.5.1   2022-11-16 [1] CRAN (R 4.2.2)
   viridisLite     0.4.1   2022-08-22 [1] CRAN (R 4.2.2)
   vroom           1.6.0   2022-09-30 [1] CRAN (R 4.2.2)
   webshot         0.5.4   2022-09-26 [1] CRAN (R 4.2.2)
   withr           2.5.0   2022-03-03 [1] CRAN (R 4.2.2)
   xfun            0.35    2022-11-16 [1] CRAN (R 4.2.2)
   xml2            1.3.3   2021-11-30 [1] CRAN (R 4.2.2)
   xtable          1.8-4   2019-04-21 [1] CRAN (R 4.2.2)
   yaml            2.3.6   2022-10-18 [1] CRAN (R 4.2.2)

 [1] C:/Users/Valentyn Hatsko/AppData/Local/R/win-library/4.2
 [2] C:/Program Files/R/R-4.2.2/library

 D -- DLL MD5 mismatch, broken installation.

--------------------------------------------------------------------------------------------------
```

</details>



Report rendered by Valentyn Hatsko at 2022-12-27, 15:30 +0200 in 12 seconds.
