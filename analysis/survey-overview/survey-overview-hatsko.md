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


make_corr_matrix <- function(d,metaData=d_meta,item_names,display_var="label_en", method="pearson"){
  # browser()
  # d <- ds0
  # metaData <- d_meta
  # item_names <- (d_meta %>% pull(item_name) %>% as.character() )[1:3]
  # add_short_label <- TRUE
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select(all_of(item_names))
  d2 <- d1[complete.cases(d1),]
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = !!rlang::sym(display_var))
  # rownames <- rownames[,"display_name"]
  # rownames <- rownames %>% as.list() %>% unlist() %>% as.character()
  rownames <- rownames %>% pull(display_name)
  d3 <- sapply(d2, as.numeric)
  # d3 %>% glimpse()
  cormat <- cor(d3,method = method)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
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



<details>

<summary>click to glimpse </summary>


```{.r .fold-show}
ds_survey %>% glimpse()
```

```
Rows: 138
Columns: 277
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
[4] "help_for_military/products"  "help_for_military/other"    
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

ds1_winter_prep <- ds0 %>% 
  mutate(
    winter_prep_count = rowSums(across(info_campaign:solid_fuel_boiler), na.rm = T)
    ,winter_prep_count = case_when(
      occupation =='not_occupied' | occupation == 'occupied_april' ~ winter_prep_count
      ,TRUE ~ NA_real_
    )
  )

ds1_problem <- ds0 %>% 
  mutate(
    hromada_exp = ifelse(hromada_exp == "yes", 1, 0)
    ,problem_info_index = rowSums(across(contains("hromada_problem_info/")))
    ,problem_consultation_index = rowSums(across(contains("hromada_problem_consultation/")))
    ,problem_proposition_index = rowSums(across(contains("hromada_problem_proposition/")))
    ,problem_system_index = rowSums(across(contains("hromada_problem_system/")))
    ,problem_feedback_index = rowSums(across(contains("hromada_problem_feedback/")))
    ,problem_execution_index = rowSums(across(contains("hromada_problem_execution/")))
  )
```

To make our analysis more nimble we create four alternative versions of `ds1` with Invasion Preparedness questions

<details>

<summary>show transformations </summary>


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
    prep_score = rowSums(across(preparation),na.rm = T) 
    ,prep_score_before = rowSums(
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
    ,prep_score_after = rowSums(
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
  # to normalize the metric, making every scale to be out of 10 points maximum
  # mutate(
  #   prep_score = prep_score / 3 # because 15 items, maximum 2 points each
  #   ,prep_score_before =prep_score_before /1.5 # because 15 items, maximum 1 point each
  #   ,prep_score_after = prep_score_after /1.5 # because 15 items, maximum 1 point each
  # ) %>%
  select(hromada_code, starts_with("prep_score"),preparation) %>% 
  relocate(c("prep_score","prep_score_before","prep_score_after"),.after=1)
ds1_prep %>% select(2:4)
```

```
# A tibble: 138 x 3
   prep_score prep_score_before prep_score_after
        <dbl>             <dbl>            <dbl>
 1         23                10               13
 2         14                 4               10
 3         16                 3               13
 4         14                 2               12
 5         20                 6               14
 6         13                 6                7
 7          9                 1                8
 8         10                 0               10
 9         12                 0               12
10         10                 0               10
# ... with 128 more rows
```

```{.r .fold-show}
# Raw scale (0,1,2) with factors
ds1_prep_ordinal_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "After Feb 24"
        ,. == 2 ~ "Before Feb 24"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Before Feb 24","After Feb 24",  "Not Applicable"))
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



m_prep <- 
  ds1_prep %>% 
  select(-hromada_code) %>%
  # you would recode into binary at this point, but we dont' in this case
  make_corr_matrix(
    item_names = names(.)
    ,metaData=d_meta_prep %>% bind_rows(
      list(
        "item_name" = c("prep_score","prep_score_before","prep_score_after")
        ,"label_en" = c("Prep Score","Prep Score (Before)","Prep Score (After)")
      ) %>% as_tibble()
    )
    ,method = "spearman"
  ) 

# TO test a hypothesis that binary measure of prepration item is better (not)
m_prep_binary <- 
  ds1_prep %>% 
  select(-hromada_code) %>%
  # recode individual items into binary
  mutate(
    across(
      .cols = preparation
      ,.fns = ~ case_when(.==2~1,T~.)
    )
  ) %>% 
  make_corr_matrix(
    item_names = names(.)
    ,metaData=d_meta_prep %>% bind_rows(
      list(
        "item_name" = c("prep_score","prep_score_before","prep_score_after")
        ,"label_en" = c("Prep Score","Prep Score (Before)","Prep Score (After)")
      ) %>% as_tibble()
    )
    ,method = "spearman"
  )  

d_item_total <- 
  list(
    "Total" = m_prep[,"Prep Score"]
    ,"Before"= m_prep[,"Prep Score (Before)"]
    ,"After" = m_prep[,"Prep Score (After)"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_prep)) %>% 
  filter(item_name != "Total Prep Score") %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name)

d_item_total_binary <- 
  list(
    "Total"  = m_prep_binary[,"Prep Score"]
    ,"Before"= m_prep_binary[,"Prep Score (Before)"]
    ,"After" = m_prep_binary[,"Prep Score (After)"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_prep)) %>% 
  filter(item_name != "Total Prep Score") %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name)
```


```{.r .fold-show}
d_meta_info <- 
  meta_survey %>% 
  filter(group== "information") %>%
  select(item_name = name,label_en,item_number)
meta_choices %>% filter(list_name=="commun_prep")
```

```
# A tibble: 3 x 3
  list_name   name      label                        
  <chr>       <chr>     <chr>                        
1 commun_prep before_24 Було створено до 24 лютого   
2 commun_prep after_24  Було створено після 24 лютого
3 commun_prep none      Немає                        
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

<summary>examine the versions </summary>



</details>

# Variable List

The following variables are present in the processed data table of survey responses:


```{.r .fold-hide}
ds0 %>% explore::describe_all() %>%neat_DT()
```

```{=html}
<div id="htmlwidget-25a9e7fe66138b152988" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-25a9e7fe66138b152988">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"100\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.83\" data-max=\"191541757\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.01\" data-max=\"197322877.2\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"1288755475.83\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283"],["index","today","_id","hromada_code","hromada_name","hromada_full_name","raion_code","raion_name","oblast_code","oblast_name","type","occupation","military_action","population_text","partners_text","friends_text","state_communication","prep_first_aid_water","prep_first_aid_fuel","prep_reaction_plan","prep_evacuation_plan","prep_reaction_plan_oth_hromadas","prep_reaction_plan_oda","prep_dftg_creation","prep_national_resistance","prep_starosta_meeting","prep_communal_meetiing","prep_online_map","prep_shelter_list","prep_notification_check","prep_backup","prep_partly_backup","shelter_capacity_before_text","shelter_capacity_now_text","telegram","viber","facebook","chat_help","hotline","telegram_link","facebook_link","head_hromada_communication","dftg_creation","dftg_creation_date","help_for_military","help_for_military/rooms","help_for_military/transport","help_for_military/money","help_for_military/products","help_for_military/other","help_for_military_text","transport_help_communal","transport_help_bought","percent_working_march","percent_working_now","commun_between_hromadas","evacuation","idp_accept","idp_registration_date","idp_registration_number","idp_real_number","idp_help","idp_help/communal_placement","idp_help/private_placement","idp_help/regular_meal","idp_help/humanitar_help","idp_help/fundraising","idp_help/employ","idp_help/psych_help","idp_help/law_help","idp_help/transit_center","idp_place_rooms","idp_room_number","idp_child_education","special_fund_relocation","special_fund_relocation_needs","special_fund_relocation_needs/state_functions","special_fund_relocation_needs/defense","special_fund_relocation_needs/public_order","special_fund_relocation_needs/economic_activity","special_fund_relocation_needs/environment","special_fund_relocation_needs/utilities","special_fund_relocation_needs/spirit_development","special_fund_relocation_needs/education","special_fund_relocation_needs/social_protection","special_fund_relocation_needs/healthcare","relocated_companies_text","created_jobs","bussiness_stimules","bussiness_stimules/tax_benefits","bussiness_stimules/free_rooms","bussiness_stimules/education","bussiness_stimules/other","bussiness_stimules_none","bussiness_stimules_other","humanitarian_hub","hromada_cooperation","hromada_cooperation/medicine","hromada_cooperation/food","hromada_cooperation/pensions","hromada_cooperation/evacuation","hromada_cooperation/other","hromada_cooperation/none","hromada_cooperation_text","is_damaged","percent_damaged","damage_evaluation_persons","damage_evaluation_communal","damage_evaluation_bussiness","reconstruction_plan","reconstruction_financing","reconstruction_financing_text","international_projects","percent_reconstructed","finance_school_shelters","finance_school_shelters_coded","info_campaign","reserves","count_power_sources","count_heaters_need","solid_fuel_boiler","no_school_days","no_school_days_coded","hromada_exp","hromada_problem_info","hromada_problem_info/idp","hromada_problem_info/citizens","hromada_problem_info/bussiness","hromada_problem_info/experts","hromada_problem_info/ngo","hromada_problem_info/nobody","hromada_problem_consultation","hromada_problem_consultation/idp","hromada_problem_consultation/citizens","hromada_problem_consultation/bussiness","hromada_problem_consultation/experts","hromada_problem_consultation/ngo","hromada_problem_consultation/nobody","hromada_problem_proposition","hromada_problem_proposition/idp","hromada_problem_proposition/citizens","hromada_problem_proposition/bussiness","hromada_problem_proposition/experts","hromada_problem_proposition/ngo","hromada_problem_proposition/nobody","hromada_problem_system","hromada_problem_system/idp","hromada_problem_system/citizens","hromada_problem_system/bussiness","hromada_problem_system/experts","hromada_problem_system/ngo","hromada_problem_system/nobody","hromada_problem_feedback","hromada_problem_feedback/idp","hromada_problem_feedback/citizens","hromada_problem_feedback/bussiness","hromada_problem_feedback/experts","hromada_problem_feedback/ngo","hromada_problem_feedback/nobody","hromada_problem_execution","hromada_problem_execution/idp","hromada_problem_execution/citizens","hromada_problem_execution/bussiness","hromada_problem_execution/experts","hromada_problem_execution/ngo","hromada_problem_execution/nobody","skills_needed","skills_needed/fundraising","skills_needed/project_management","skills_needed/longterm_planning","skills_needed/crisis_planning","skills_needed/data_analysis","skills_needed/human_resourse","skills_needed/other","skills_needed_text","contact_text","evacuation_001","hromada_exp_problem","_uuid","_submission_time","_validation_status","_status","_submitted_by","_tags","region_en","idp_help/communal_placement_number","idp_help/private_placement_number","idp_help/regular_meal_number","idp_help/humanitar_help_number","idp_help/fundraising_number","idp_help/employ_number","idp_help/psych_help_number","idp_help/law_help_number","idp_help/transit_center_number","idp_help_count","prep_count","comm_channels_count","help_military_count","hromada_cooperation_count","dftg_creation_time","idp_registration_time","prep_winter_count","oblast_center","hromada_center_code","hromada_center","lat_center","lon_center","travel_time","n_settlements","square","occipied_before_2022","total_population_2022","urban_population_2022","urban_pct","budget_code","budget_name","oblast_name_en","region_en.x","region_code_en","income_total_2021","income_transfert_2021","income_military_2021","income_pdfo_2021","income_unified_tax_2021","income_property_tax_2021","income_excise_duty_2021","income_own_2021","own_income_prop_2021","transfert_prop_2021","military_tax_prop_2021","pdfo_prop_2021","unified_tax_prop_2021","property_tax_prop_2021","excise_duty_prop_2021","own_income_change","own_prop_change","total_income_change","income_own","income_total","income_transfert","dfrr_executed","turnout_2020","sex_head","age_head","education_head","incumbent","rda","not_from_here","party","enterpreuner","unemployed","priv_work","polit_work","communal_work","ngo_work","party_national_winner","no_party","male","high_educ","sum_osbb_2020","edem_total","edem_petitions","edem_consultations","edem_participatory_budget","edem_open_hromada","youth_councils","youth_centers","business_support_centers","region_en.y","creation_date","creation_year","time_before_24th","voluntary","war_zone_27_04_2022","war_zone_20_06_2022","war_zone_23_08_2022","war_zone_10_10_2022","income_own_per_capita","income_total_per_capita","income_tranfert_per_capita","idp_registration_share","idp_real_share","idp_child_share"],["dbl","dat","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dat","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","chr","chr","chr","dat","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","lgl","chr","dat","lgl","chr","lgl","lgl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dat","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl"],[0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,4,6,1,2,7,7,5,7,2,6,9,4,7,10,10,0,0,0,0,0,0,0,81,6,0,0,41,6,6,6,6,6,6,83,46,46,1,0,12,0,8,16,9,16,8,8,8,8,8,8,8,8,8,8,46,131,15,0,77,77,77,77,77,77,77,77,77,77,77,11,8,8,8,8,8,8,8,99,132,132,132,132,132,132,132,132,136,0,100,100,100,100,105,105,129,14,105,8,16,11,13,12,17,27,22,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,130,15,12,138,0,0,138,0,138,138,0,9,9,9,9,9,9,9,9,9,0,0,0,0,0,16,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,16,16],[0,0,0,0,0,0,0,0,0,0,0,0.7,0.7,0.7,0.7,0.7,0,2.9,4.3,0.7,1.4,5.1,5.1,3.6,5.1,1.4,4.3,6.5,2.9,5.1,7.2,7.2,0,0,0,0,0,0,0,58.7,4.3,0,0,29.7,4.3,4.3,4.3,4.3,4.3,4.3,60.1,33.3,33.3,0.7,0,8.7,0,5.8,11.6,6.5,11.6,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,33.3,94.9,10.9,0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,71.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,98.6,0,72.5,72.5,72.5,72.5,76.1,76.1,93.5,10.1,76.1,5.8,11.6,8,9.4,8.7,12.3,19.6,15.9,18.8,0,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,0,0,0,0,0,0,0,0,94.2,10.9,8.7,100,0,0,100,0,100,100,0,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,0,0,0,0,0,11.6,29.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31.9,0.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6.5,11.6,11.6],[138,30,138,138,135,137,76,76,22,22,3,5,4,120,11,15,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,105,106,3,3,3,3,3,58,133,5,3,51,20,3,3,3,3,3,56,23,32,26,29,6,3,2,32,120,110,68,3,3,3,3,3,3,3,3,3,6,8,79,2,45,3,3,3,3,3,3,3,3,3,3,12,5,11,3,3,3,3,3,32,3,5,3,3,2,3,3,3,3,2,5,3,3,3,3,3,8,11,6,109,82,3,3,3,3,3,61,38,2,18,3,3,3,3,3,3,23,3,3,3,3,3,3,21,3,3,3,3,3,3,24,3,3,3,3,3,3,23,3,3,3,3,3,3,22,3,3,3,3,3,3,42,2,2,2,2,2,2,2,9,124,4,1,138,138,1,1,1,1,5,88,79,52,120,36,31,88,77,37,1,22,11,6,4,32,51,6,2,138,138,138,138,134,54,137,1,138,95,96,138,137,22,5,5,138,138,91,138,138,138,138,138,52,52,10,42,12,30,15,71,42,58,138,138,138,95,138,2,35,2,2,2,2,24,2,2,2,2,2,1,2,2,2,2,38,5,2,2,2,2,3,4,9,5,15,6,15,2,2,2,2,2,138,138,138,130,123,117],[2,null,191541757,null,null,null,null,null,null,null,null,null,null,140,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,0,0,0,0,0,null,null,null,null,null,null,0,0,0,0,0,null,null,null,0,0,null,null,null,null,23,23,null,0,0,0,0,0,0,0,0,0,null,null,0,null,null,0,0,0,0,0,0,0,0,0,0,null,null,null,0,0,0,0,0,null,null,null,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,0,0,0,627,2,0,0,0,null,null,0,0,null,null,45.68,22.49,0,1,42.2,0,3359,0,0,null,null,null,null,null,10846101.81,5163331,0,1056172.94,227066.07,224034.84,8271,3131966.65,0.14,0.14,0,0.09,0.01,0.01,0,-0.83,-0.39,-0.43,1972353.16,11030764.44,5642000,78.5,0.27,null,32,null,0,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,2015,556.79,0,0,0,0,0,507.13,2607.8,962.57,0.01,0.01,0],[78.2,null,197322877.2,null,null,null,null,null,null,null,null,null,null,21136.72,1.34,1.34,null,1.04,1.08,1.41,1,0.55,0.84,1.02,0.56,1.12,1.13,0.78,1.08,1.24,0.55,0.86,null,null,0.6,0.71,1.8,0.33,0.86,null,null,null,null,null,null,0.73,0.7,0.73,0.97,0.42,null,null,null,89.22,91.77,null,null,null,null,2001.63,2326.16,null,0.71,0.65,0.41,0.99,0.26,0.22,0.69,0.61,0.28,null,null,64.67,null,null,0.23,0.51,0.34,0.13,0.08,0.49,0.03,0.39,0.39,0.28,null,null,null,0.19,0.18,0.45,0.3,0.19,null,null,null,0.33,0.33,0,0.5,0.33,0.33,null,null,null,null,null,null,null,null,null,null,null,null,853167.02,0.86,0.93,0.9,0.43,0.34,null,null,null,null,0.38,0.64,0.58,0.09,0.35,0.08,null,0.34,0.39,0.33,0.09,0.27,0.24,null,0.28,0.46,0.49,0.08,0.29,0.21,null,0.26,0.45,0.55,0.16,0.34,0.16,null,0.36,0.45,0.47,0.11,0.33,0.16,null,0.15,0.37,0.46,0.07,0.38,0.21,null,0.75,0.41,0.32,0.49,0.26,0.32,0.06,null,null,null,null,null,null,null,null,null,null,null,1590.84,1037.82,1107.57,1977.64,461.12,623.09,1618.08,1513.87,784.98,627,13.72,4.29,3.39,0.08,null,null,3.11,0.01,null,null,49.07,29.43,93.67,22.23,410.51,0,22076.86,12499.06,0.35,null,null,null,null,null,91899785.68,37659862.26,1815527.65,31364648.7,6130599.53,8123370.3,3741911.43,54239923.43,0.51,0.49,0.01,0.27,0.06,0.1,0.03,0.04,0.01,-0.01,56792346.01,92278010.92,35485664.91,32738.42,0.42,null,52.36,null,0.54,0.07,0.11,null,0.02,0.02,0.08,0.83,0.04,0,0.16,0.43,0.27,0.93,35.48,0.62,0.22,0.16,0.15,0.09,0.1,0.22,0.56,null,null,2018.22,1209.18,0.58,0.08,0.12,0.12,0.12,2244.72,4224.02,1979.3,0.1,0.1,0.05],[151,null,206471695,null,null,null,null,null,null,null,null,null,null,243000,20,17,null,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,null,null,2,2,2,2,2,null,null,null,null,null,null,1,1,1,1,1,null,null,null,100,100,null,null,null,null,20000,60000,null,1,1,1,1,1,1,1,1,1,null,null,800,null,null,1,1,1,1,1,1,1,1,1,1,null,null,null,1,1,1,1,1,null,null,null,1,1,0,1,1,1,null,null,null,null,null,null,null,null,null,null,null,null,13936323,1,1,1,1,1,null,null,null,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,1,null,null,null,null,null,null,null,null,null,null,null,20000,16331,20000,20000,8500,20000,20000,20000,16331,627,29,10,5,3,null,null,5,1,null,null,52.06,36.73,288,97,2497.1,0,317752,305239,1,null,null,null,null,null,1288755475.83,346574777.46,47254976.84,608781726.22,124876522.55,78663469.37,73206177.69,942180698.37,0.86,0.86,0.14,0.59,0.13,0.44,0.27,1.69,0.23,0.89,969725144.97,1248182878.17,315122334.64,757596.25,0.65,null,71,null,1,1,1,null,1,1,1,1,1,0,1,1,1,1,638,4,1,1,1,1,2,4,17,null,null,2020,2383.79,1,1,1,1,1,7418.91,9388.75,3470.86,0.63,0.63,0.33]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>type<\/th>\n      <th>na<\/th>\n      <th>na_pct<\/th>\n      <th>unique<\/th>\n      <th>min<\/th>\n      <th>mean<\/th>\n      <th>max<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"autoWidth":false,"columnDefs":[{"className":"dt-right","targets":[3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```

# 0. Introduction

<mark>0.1</mark> What is the goal of this report?

> This report overviews the responses to the survey conducted by KSE Institute in Ukraine during 2022


# 1. General Information


```{.r .fold-hide}
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

<mark>1.2</mark> What regions are represented in this sample? 



```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general %>% 
      group_by(region_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  filter(!is.na(region_en)) %>%
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
  ) %>%
  arrange(region_en, desc(hromada_survey_prop)) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(region_en = 'Region',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 hromada_total_pct = 'Proportion of Region \nin the General Population',
                 hromada_survey_pct = 'Proportion of Region \nin the Survey'
  )
```

```{=html}
<div id="fjhgtlvjiq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fjhgtlvjiq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fjhgtlvjiq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fjhgtlvjiq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fjhgtlvjiq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fjhgtlvjiq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fjhgtlvjiq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhgtlvjiq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fjhgtlvjiq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fjhgtlvjiq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fjhgtlvjiq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fjhgtlvjiq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fjhgtlvjiq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fjhgtlvjiq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fjhgtlvjiq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fjhgtlvjiq .gt_from_md > :first-child {
  margin-top: 0;
}

#fjhgtlvjiq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fjhgtlvjiq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fjhgtlvjiq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhgtlvjiq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fjhgtlvjiq .gt_row_group_first td {
  border-top-width: 2px;
}

#fjhgtlvjiq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhgtlvjiq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fjhgtlvjiq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fjhgtlvjiq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhgtlvjiq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhgtlvjiq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fjhgtlvjiq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fjhgtlvjiq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhgtlvjiq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fjhgtlvjiq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhgtlvjiq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fjhgtlvjiq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhgtlvjiq .gt_left {
  text-align: left;
}

#fjhgtlvjiq .gt_center {
  text-align: center;
}

#fjhgtlvjiq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fjhgtlvjiq .gt_font_normal {
  font-weight: normal;
}

#fjhgtlvjiq .gt_font_bold {
  font-weight: bold;
}

#fjhgtlvjiq .gt_font_italic {
  font-style: italic;
}

#fjhgtlvjiq .gt_super {
  font-size: 65%;
}

#fjhgtlvjiq .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#fjhgtlvjiq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fjhgtlvjiq .gt_indent_1 {
  text-indent: 5px;
}

#fjhgtlvjiq .gt_indent_2 {
  text-indent: 10px;
}

#fjhgtlvjiq .gt_indent_3 {
  text-indent: 15px;
}

#fjhgtlvjiq .gt_indent_4 {
  text-indent: 20px;
}

#fjhgtlvjiq .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Region">Region</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Region &#10;in the Survey">Proportion of Region 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Region &#10;in the General Population">Proportion of Region 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="hromada_count" class="gt_row gt_right">24</td>
<td headers="hromada_count_total" class="gt_row gt_right">298</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">17.4%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">20.7%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="hromada_count" class="gt_row gt_right">19</td>
<td headers="hromada_count_total" class="gt_row gt_right">281</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">13.8%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">19.5%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="hromada_count" class="gt_row gt_right">23</td>
<td headers="hromada_count_total" class="gt_row gt_right">243</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">16.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">16.9%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">South</td>
<td headers="hromada_count" class="gt_row gt_right">20</td>
<td headers="hromada_count_total" class="gt_row gt_right">192</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">14.5%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">13.4%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="hromada_count" class="gt_row gt_right">52</td>
<td headers="hromada_count_total" class="gt_row gt_right">424</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">37.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">29.5%</td></tr>
  </tbody>
  
  
</table>
</div>
```

<mark>1.3</mark> What oblasts are represented in this sample? 


```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en, oblast_name_en) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general %>% 
      group_by(region_en, oblast_name_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  filter(!is.na(region_en)) %>%
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
  ) %>%
  arrange(region_en, desc(hromada_survey_prop)) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(region_en = 'Region',
                 oblast_name_en = 'Oblast',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 hromada_total_pct = 'Proportion of Oblast \nin the General Population',
                 hromada_survey_pct = 'Proportion of Oblast \nin the Survey'
  )
```

```{=html}
<div id="nuctadiyti" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#nuctadiyti .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#nuctadiyti .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#nuctadiyti .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nuctadiyti .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#nuctadiyti .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#nuctadiyti .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nuctadiyti .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#nuctadiyti .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#nuctadiyti .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#nuctadiyti .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nuctadiyti .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nuctadiyti .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#nuctadiyti .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#nuctadiyti .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#nuctadiyti .gt_from_md > :first-child {
  margin-top: 0;
}

#nuctadiyti .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nuctadiyti .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#nuctadiyti .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#nuctadiyti .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#nuctadiyti .gt_row_group_first td {
  border-top-width: 2px;
}

#nuctadiyti .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nuctadiyti .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nuctadiyti .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nuctadiyti .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nuctadiyti .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nuctadiyti .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nuctadiyti .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nuctadiyti .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nuctadiyti .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#nuctadiyti .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nuctadiyti .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#nuctadiyti .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nuctadiyti .gt_left {
  text-align: left;
}

#nuctadiyti .gt_center {
  text-align: center;
}

#nuctadiyti .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nuctadiyti .gt_font_normal {
  font-weight: normal;
}

#nuctadiyti .gt_font_bold {
  font-weight: bold;
}

#nuctadiyti .gt_font_italic {
  font-style: italic;
}

#nuctadiyti .gt_super {
  font-size: 65%;
}

#nuctadiyti .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#nuctadiyti .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nuctadiyti .gt_indent_1 {
  text-indent: 5px;
}

#nuctadiyti .gt_indent_2 {
  text-indent: 10px;
}

#nuctadiyti .gt_indent_3 {
  text-indent: 15px;
}

#nuctadiyti .gt_indent_4 {
  text-indent: 20px;
}

#nuctadiyti .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Region">Region</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Oblast">Oblast</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Oblast &#10;in the Survey">Proportion of Oblast 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Oblast &#10;in the General Population">Proportion of Oblast 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="oblast_name_en" class="gt_row gt_left">Poltava</td>
<td headers="hromada_count" class="gt_row gt_right">8</td>
<td headers="hromada_count_total" class="gt_row gt_right">60</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">5.8%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.2%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="oblast_name_en" class="gt_row gt_left">Cherkassy</td>
<td headers="hromada_count" class="gt_row gt_right">5</td>
<td headers="hromada_count_total" class="gt_row gt_right">66</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.6%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="oblast_name_en" class="gt_row gt_left">Kirovograd</td>
<td headers="hromada_count" class="gt_row gt_right">5</td>
<td headers="hromada_count_total" class="gt_row gt_right">49</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.4%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="oblast_name_en" class="gt_row gt_left">Vinnytsia</td>
<td headers="hromada_count" class="gt_row gt_right">5</td>
<td headers="hromada_count_total" class="gt_row gt_right">63</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.4%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="oblast_name_en" class="gt_row gt_left">Khmelnitsk</td>
<td headers="hromada_count" class="gt_row gt_right">1</td>
<td headers="hromada_count_total" class="gt_row gt_right">60</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">0.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.2%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="oblast_name_en" class="gt_row gt_left">Driproptrovska</td>
<td headers="hromada_count" class="gt_row gt_right">12</td>
<td headers="hromada_count_total" class="gt_row gt_right">86</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">8.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">6.0%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="oblast_name_en" class="gt_row gt_left">Kharkiv</td>
<td headers="hromada_count" class="gt_row gt_right">4</td>
<td headers="hromada_count_total" class="gt_row gt_right">56</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.9%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="oblast_name_en" class="gt_row gt_left">Zaporizka</td>
<td headers="hromada_count" class="gt_row gt_right">3</td>
<td headers="hromada_count_total" class="gt_row gt_right">67</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.7%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="oblast_name_en" class="gt_row gt_left">Donetks</td>
<td headers="hromada_count" class="gt_row gt_right">0</td>
<td headers="hromada_count_total" class="gt_row gt_right">46</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">NA</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.2%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="oblast_name_en" class="gt_row gt_left">Luhansk</td>
<td headers="hromada_count" class="gt_row gt_right">0</td>
<td headers="hromada_count_total" class="gt_row gt_right">26</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">NA</td>
<td headers="hromada_total_pct" class="gt_row gt_right">1.8%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="oblast_name_en" class="gt_row gt_left">Sumska</td>
<td headers="hromada_count" class="gt_row gt_right">8</td>
<td headers="hromada_count_total" class="gt_row gt_right">51</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">5.8%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.5%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="oblast_name_en" class="gt_row gt_left">Chernigiv</td>
<td headers="hromada_count" class="gt_row gt_right">7</td>
<td headers="hromada_count_total" class="gt_row gt_right">57</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">5.1%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.0%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="oblast_name_en" class="gt_row gt_left">Kyiv-oblast</td>
<td headers="hromada_count" class="gt_row gt_right">5</td>
<td headers="hromada_count_total" class="gt_row gt_right">69</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.8%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="oblast_name_en" class="gt_row gt_left">Zhytomir</td>
<td headers="hromada_count" class="gt_row gt_right">3</td>
<td headers="hromada_count_total" class="gt_row gt_right">66</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.6%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">South</td>
<td headers="oblast_name_en" class="gt_row gt_left">Odesa</td>
<td headers="hromada_count" class="gt_row gt_right">12</td>
<td headers="hromada_count_total" class="gt_row gt_right">91</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">8.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">6.3%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">South</td>
<td headers="oblast_name_en" class="gt_row gt_left">Kherson</td>
<td headers="hromada_count" class="gt_row gt_right">7</td>
<td headers="hromada_count_total" class="gt_row gt_right">49</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">5.1%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.4%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">South</td>
<td headers="oblast_name_en" class="gt_row gt_left">Mykolayiv</td>
<td headers="hromada_count" class="gt_row gt_right">1</td>
<td headers="hromada_count_total" class="gt_row gt_right">52</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">0.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.6%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Ivano-Frankivsk</td>
<td headers="hromada_count" class="gt_row gt_right">16</td>
<td headers="hromada_count_total" class="gt_row gt_right">62</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">11.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.3%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Zakarpatska</td>
<td headers="hromada_count" class="gt_row gt_right">11</td>
<td headers="hromada_count_total" class="gt_row gt_right">64</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">8.0%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.5%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Lviv</td>
<td headers="hromada_count" class="gt_row gt_right">9</td>
<td headers="hromada_count_total" class="gt_row gt_right">73</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">6.5%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">5.1%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Vonyn</td>
<td headers="hromada_count" class="gt_row gt_right">5</td>
<td headers="hromada_count_total" class="gt_row gt_right">54</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.8%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Rivenska</td>
<td headers="hromada_count" class="gt_row gt_right">4</td>
<td headers="hromada_count_total" class="gt_row gt_right">64</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">4.5%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Ternopilska</td>
<td headers="hromada_count" class="gt_row gt_right">4</td>
<td headers="hromada_count_total" class="gt_row gt_right">55</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.8%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="oblast_name_en" class="gt_row gt_left">Cherniveska</td>
<td headers="hromada_count" class="gt_row gt_right">3</td>
<td headers="hromada_count_total" class="gt_row gt_right">52</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">3.6%</td></tr>
  </tbody>
  
  
</table>
</div>
```

<mark>1.3</mark> What type of hromadas are represented in the sample? 


```{.r .fold-hide}
ds_survey %>% 
  group_by(type) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general %>% 
      group_by(type) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
    ,type = case_when(type == 'міська' ~ "urban",
                      type == 'селищна' ~ "urban village",
                      type == 'сільська' ~ "village"
                      )
  ) %>%
  arrange(desc(hromada_survey_prop)) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(type = 'Type of Hromada',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 hromada_total_pct = 'Proportion of Hromada Type \nin the General Population',
                 hromada_survey_pct = 'Proportion of Hromada Type \nin the Survey'
  )
```

```{=html}
<div id="hcpxelokyd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hcpxelokyd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hcpxelokyd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hcpxelokyd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hcpxelokyd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hcpxelokyd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hcpxelokyd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hcpxelokyd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hcpxelokyd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hcpxelokyd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hcpxelokyd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hcpxelokyd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hcpxelokyd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hcpxelokyd .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hcpxelokyd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hcpxelokyd .gt_from_md > :first-child {
  margin-top: 0;
}

#hcpxelokyd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hcpxelokyd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hcpxelokyd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hcpxelokyd .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hcpxelokyd .gt_row_group_first td {
  border-top-width: 2px;
}

#hcpxelokyd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hcpxelokyd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hcpxelokyd .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hcpxelokyd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hcpxelokyd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hcpxelokyd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hcpxelokyd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hcpxelokyd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hcpxelokyd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hcpxelokyd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hcpxelokyd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hcpxelokyd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hcpxelokyd .gt_left {
  text-align: left;
}

#hcpxelokyd .gt_center {
  text-align: center;
}

#hcpxelokyd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hcpxelokyd .gt_font_normal {
  font-weight: normal;
}

#hcpxelokyd .gt_font_bold {
  font-weight: bold;
}

#hcpxelokyd .gt_font_italic {
  font-style: italic;
}

#hcpxelokyd .gt_super {
  font-size: 65%;
}

#hcpxelokyd .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#hcpxelokyd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hcpxelokyd .gt_indent_1 {
  text-indent: 5px;
}

#hcpxelokyd .gt_indent_2 {
  text-indent: 10px;
}

#hcpxelokyd .gt_indent_3 {
  text-indent: 15px;
}

#hcpxelokyd .gt_indent_4 {
  text-indent: 20px;
}

#hcpxelokyd .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Type of Hromada">Type of Hromada</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Hromada Type &#10;in the Survey">Proportion of Hromada Type 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Hromada Type &#10;in the General Population">Proportion of Hromada Type 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="type" class="gt_row gt_left">urban village</td>
<td headers="hromada_count" class="gt_row gt_right">46</td>
<td headers="hromada_count_total" class="gt_row gt_right">435</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">33.3%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">29.6%</td></tr>
    <tr><td headers="type" class="gt_row gt_left">urban</td>
<td headers="hromada_count" class="gt_row gt_right">46</td>
<td headers="hromada_count_total" class="gt_row gt_right">409</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">33.3%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">27.8%</td></tr>
    <tr><td headers="type" class="gt_row gt_left">village</td>
<td headers="hromada_count" class="gt_row gt_right">46</td>
<td headers="hromada_count_total" class="gt_row gt_right">625</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">33.3%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">42.5%</td></tr>
  </tbody>
  
  
</table>
</div>
```


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("type"))+
  labs(
    title = "What types of hromadas repsonded to the survey?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/unnamed-chunk-4-1.png)<!-- -->

<mark>1.4</mark> What hromadas experienced military occupation or  military actions? 

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("military_action"))+
  labs(
    title = "How many respondent hromadas have experienced military action at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/unnamed-chunk-5-1.png)<!-- -->

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("occupation"))+
  labs(
    title = "How many respondent hromadas have experienced occupation at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y=NULL
  )
```

![](figure-png-iso/unnamed-chunk-5-2.png)<!-- -->

```{.r .fold-hide}
ds0 %>% make_bi_freq_graph("military_action","occupation")
```

![](figure-png-iso/unnamed-chunk-5-3.png)<!-- -->

```{.r .fold-hide}
ds0 %>% make_bi_freq_graph("occupation","military_action")
```

![](figure-png-iso/unnamed-chunk-5-4.png)<!-- -->



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
      , "Before Feb 24", "After Feb 24","Yes", "No", "Not Applicable",
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

## Item-total correlation 

We can conceptualize the preparation for invation as two scores: the first received for steps taken prior to Feb 24, 2022 (the `Before` prep score) and the second for the steps undertaken after Feb 24 (the `After` prep score), as recorded at the time of data collection ( October - November 20200). 



```r
ds1_prep %>% select(1:4) # + individual preparation items
```

```
# A tibble: 138 x 4
   hromada_code        prep_score prep_score_before prep_score_after
   <chr>                    <dbl>             <dbl>            <dbl>
 1 UA12060190000043514         23                10               13
 2 UA46060370000065608         14                 4               10
 3 UA35060190000079777         16                 3               13
 4 UA35020130000045875         14                 2               12
 5 UA53060250000043118         20                 6               14
 6 UA65060250000073379         13                 6                7
 7 UA51040110000040346          9                 1                8
 8 UA59080230000084731         10                 0               10
 9 UA05100110000070795         12                 0               12
10 UA51100250000055079         10                 0               10
# ... with 128 more rows
```

```r
# prep_score =  0 to  30, sum of (0|1|2) for each of 15 items, 2 pts for prepping before 2022-02-24 1 pts after.
# prep_score_before = 0 to 15, sum of (0|1) items, where 1 = prepping before Feb 24
# prep_score_after = 0 to 15, sum of (0|1) items, where 1 = prepping eventually (Nov 2022)
```

 These  scores have a convenient conceptualization and a straightforward interpretation
 
 - `Before` - number of preparatory items on the prep list completed before Feb 24
 - `AFter` - number of preparatory steps completed by the hromada at the time of the interview
 - `Total` - the sum of `Before` and `After`. Evaluates the readiness of the hromada at the time of the interview, valuing steps undertaken prior to Feb 24 twice as influential in the final score

`Before` and `After` scores have a more clear conceptualization, but th `Total` score has a more appealing distribution shape, making it more useful for statistical modeling. 


```{.r .fold-hide}
g <-  ds1_prep %>%
  # To standardize the metrics of each scale : 0 to 10, where 10 - most prepared
  mutate(
     prep_score = prep_score / 3 # because 15 items, maximum 2 points each
    ,prep_score_before =prep_score_before /1.5 # because 15 items, maximum 1 point each
    ,prep_score_after = prep_score_after /1.5 # because 15 items, maximum 1 point each
  ) %>% 
  select(starts_with("prep_score")) %>% 
  pivot_longer(cols = everything(),names_to = "measure",values_to="value") %>% 
  mutate( 
    measure = factor(measure,
                        levels = c("prep_score_before","prep_score_after","prep_score")
                        ,labels = c("Prep Score (Before)","Prep Score (After)", "Prep Score")
                        )
  ) %>% 
  ggplot(aes(x=value))+
  geom_histogram(binwidth = 1, alpha = .4)+
  scale_x_continuous(breaks = seq(0,10,1))+
  facet_wrap("measure",ncol =1)
g
```

![](figure-png-iso/info-score-distribution-1.png)<!-- -->

```{.r .fold-hide}
g %>%  quick_save("score-distribution",w=4, h=6)
```

The item-total correlation also indicates that psychometrically the `Total` score is a better choice - having no negative values and generally having a higher discrimination capacity of items.   


```{.r .fold-hide}
(d_item_total%>% 
  slice(1:15) %>% 
  pivot_longer(
     cols = c("Total","Before","After")
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
    ,scenario = scenario %>% factor(level=c("Before","After","Total"))
    ,item_name = factor(item_name, levels = d_meta_prep %>% pull(label_en)) %>% 
      fct_rev()
  ) %>% 
  ggplot(aes(x = item_name, y = correlation, color = discrimination, group = scenario))+
  geom_line(aes(group = "scenario"))+
  geom_point()+
  geom_text(aes(label=correlation %>% scales::number(accuracy = .01) %>% RemoveLeadingZero()),hjust=-.3
            ,size = 3)+
  geom_hline(aes(x=0, yintercept = 0))+ 
  facet_wrap("scenario",nrow=1)+
  scale_y_continuous(limits = c(-.3,.6), expand = expansion(add = c(0,.2)))+
  scale_color_brewer(type ="div",palette = "RdYlGn")+
  coord_flip() +
    labs(
      title = "Item-total corellations under three measurement scenarios"
      ,subtitle = "Before = prior to Feb 24, After = at time of interview, Oct-Nov 2022, Total = Before + After"
      ,y = "Item-total Correlation (Spearman)"
      ,x = NULL
      ,color = "Discrimination"
    )
  ) %>% 
  print() %>% 
  quick_save("item-total",w=8,h=4)
```

![](figure-png-iso/prep-item-total-1.png)<!-- -->

While all three metrics should be considered during modeling, our current understanding of the data sugggests that we should prefer the `Total` score in relating hromada's preparedness to other attributes. 


# 3. Information



```{.r .fold-hide}
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
    title = "Did hromadas have account on any social network before the invasion?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, x = NULL, fill = "Had account"
  ) +
  scale_x_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account')) +
  scale_fill_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account'))
```

![](figure-png-iso/info-2-1.png)<!-- -->



```{.r .fold-hide}
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
    title = "How frequently did hromada head communicated in the first month of invasion?"
    ,x = NULL, fill = NULL
  )
```

![](figure-png-iso/info-1 -1.png)<!-- -->


```{.r .fold-hide}
d %>% 
  group_by(had_any_sn_before_feb24) %>% 
  summarize(mean_times_per_week = mean(time_per_week,na.rm =T)) %>% 
  ggplot(aes(x=mean_times_per_week, y= had_any_sn_before_feb24,
             fill = had_any_sn_before_feb24))+
  geom_col()+
  geom_text(aes(label=scales::comma(mean_times_per_week)))+
  labs(
    title = "How frequently did heads of hromadas communicate with the community during the first month of invation \ndepending on having any social account before 24th February?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, x = "Average times per week", fill = "Had account on\nany social network"
  ) +
  scale_y_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account')) +
  scale_fill_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account'))
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
                                ,labels = c('TRUE' = 'Had account', 'FALSE' = 'No account')
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


```{.r .fold-hide}
meta_survey %>% filter(name=="dftg_creation") %>% pull(label) %>% cat()
```

```
Чи було у вашій громаді офіційно сформоване добровольче формування територіальної громади?
```


```{.r .fold-hide}
d <- ds0 %>% 
  select(hromada_code, dftg_creation, type) %>%
  mutate(dftg_creation = factor(dftg_creation, levels = c('not_able', 'still_not', 'yes')),
         type = factor(type, levels = c('сільська', "селищна", "міська")),
         dftg_creation = fct_recode(dftg_creation,
             'Yes' = 'yes'
            ,"Didn't due to quick occupation" = 'not_able'
            ,'Still not created' = 'still_not')
  )

(d %>% make_bi_freq_graph('dftg_creation')) +
  labs(
    title = "Did hromadas create a voluntary formation of a territorial community?"
    ,y = NULL, x = NULL, fill = NULL
  )
```

![](figure-png-iso/dftg-1-1.png)<!-- -->

Many village communities didn't have a voluntary formation of a territorial community at the time of the survey


```{.r .fold-hide}
d %>%
  group_by(type) %>%
  count(dftg_creation) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(x=prop, y = dftg_creation, fill = type))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = -.5,position = position_dodge(width = .9))+
  scale_x_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T)
  )+
  labs(
    title = "Did hromadas create a voluntary formation of a territorial community?"
    ,fill = "Hromada Type"
    ,x = "Percent of respondents in each group"
    , y = NULL
  )
```

![](figure-png-iso/dftg-2-1.png)<!-- -->


```{.r .fold-hide}
d <- ds0 %>% select(dftg_creation_date) %>% group_by(dftg_creation_date) %>% 
  summarise(n = n()) %>%
  filter(!is.na(dftg_creation_date) & dftg_creation_date > '2021-12-28') %>%
  mutate(cum = cumsum(n))

p <- d %>%
  ggplot(aes(x = dftg_creation_date, y = cum)) +
  geom_point() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = as.POSIXct('2021-12-29')), 
             color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.02) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct('2021-12-01'), as.POSIXct('2022-10-15')), date_breaks = '1 month') +
  annotate(geom = 'label', x = as.POSIXct('2021-12-31'), y = 80, size = 3,
           label = 'December 29th: \nThe order of formation and \nactivity of the voluntary \nformations of ATC was defined') +
  annotate(geom = 'label', x = as.POSIXct('2022-02-28'), y = 70, size = 3,
           label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-03-26'), y = 90, hjust = 0, size = 3,
           label = '59 voluntary formations of ATC \nafter a first month', fontface = 'italic') +
  labs(title = 'Number of DFTG created by ATCs')
p
```

![](figure-png-iso/dftg-date-1.png)<!-- -->


```{.r .fold-hide}
d <- ds0 %>% select(dftg_creation_date, type) %>% group_by(dftg_creation_date, type) %>% 
  filter(!is.na(dftg_creation_date) & dftg_creation_date > '2021-12-28') %>%
  summarise(n = n()) %>%
  pivot_wider(values_from = n, names_from = type) %>%
  rename(c = 'міська', v = 'сільська', s = 'селищна') %>%
  ungroup() %>% 
  mutate(c = replace_na(c, 0),
         v = replace_na(v, 0),
         s = replace_na(s, 0),
         c_cum = cumsum(c),
         s_cum = cumsum(s),
         v_cum = cumsum(v)) %>% 
  select(-c(c,v,s)) %>%
  pivot_longer(-dftg_creation_date, values_to = 'n', names_to = 'type')

p <- d %>%
  ggplot(aes(x = dftg_creation_date, y = n, color = type)) +
  geom_point() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = as.POSIXct('2021-12-29')), 
             color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.007) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'top') +
  scale_x_datetime(limits = c(as.POSIXct('2021-12-01'), as.POSIXct('2022-10-15')), date_breaks = '1 month') +
  scale_y_continuous(limits = c(0,40)) +
  scale_color_discrete(labels = c('c_cum' = 'City', 's_cum' = 'Urban Village', 'v_cum' = 'Village')) +
  annotate(geom = 'label', x = as.POSIXct('2021-12-31'), y = 36, size = 3,
           label = 'December 29th: \nThe order of formation and \nactivity of the voluntary \nformations of ATC was defined') +
  annotate(geom = 'label', x = as.POSIXct('2022-02-28'), y = 37, size = 3,
           label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-03-26'), y = 37, hjust = 0, size = 3,
           label = '59 voluntary formations of ATC \nafter a first month', fontface = 'italic') +
  labs(title = 'Number of DFTG created by ATCs', color = NULL)
p
```

![](figure-png-iso/dftg-date-1-1.png)<!-- -->

## 4.1 Transport Help

# 5. Administrative Adaptation

```{.r .fold-hide}
g <-  ds0 %>%
  select(starts_with("percent_working")) %>% 
  pivot_longer(cols = everything(),names_to = "measure",values_to="value") %>% 
  mutate( 
    measure = factor(measure,
                        levels = c("percent_working_march","percent_working_now")
                        ,labels = c("Pecent of stuff working in March 2022",
                                    "Pecent of stuff working as of now")
                        )
  ) %>% 
  ggplot(aes(x=value))+
  geom_histogram(binwidth = 100, alpha = .5, breaks=c(10,20,30,40,50,60,70,80,90,100))+
  facet_wrap("measure",ncol =5)
g
```

![](figure-png-iso/admin-percent-working-1.png)<!-- -->

```{.r .fold-hide}
g %>%  quick_save("score-distribution",w=4, h=6)
```


```{.r .fold-hide}
(ds0 %>% 
   filter(!is.na(commun_between_hromadas)) %>%
  mutate(
    `Frequency of communication with other hromadas` = fct_recode(
      commun_between_hromadas,
       "Daily"      = "Daily"
      ,"Several times a week"       = "Several times a week"
      ,"Several times a month" = "Several times a month"
      ,"Once a month and less" = "Once a month and less"
      ,"No meetings/calls" = "No meetings/calls"
    ) %>% factor( levels = c(
       "Daily"           
       ,"Several times a week"
       ,"Several times a month"
       ,"Once a month and less"
       ,"No meetings/calls"
    )
    )
  ) %>% 
  make_bi_freq_graph("military_action","Frequency of communication with other hromadas") )+
  labs(
    title = "Frequency of communication with representatives of other hromadas"
    ,x = NULL
  )
```

![](figure-png-iso/comm-between-hromadas-1-1.png)<!-- -->

# 6. Evacuation

```{.r .fold-hide}
(ds0 %>% 
  mutate(
    `Need of evacuation among hromadas` = fct_recode(
      evacuation,
       "No need"      = "no"
      ,"Yes, and executed the evacuation"       = "yes_executed"
      ,"Yes, but did not manage to execute the evacuation" = "yes_notexecuted"
    ) %>% factor( levels = c(
       "No need"           
       ,"Yes, and executed the evacuation"
       ,"Yes, but did not manage to execute the evacuation"
    )
    )
  ) %>% 
  make_bi_freq_graph("military_action","Need of evacuation among hromadas") )+
  labs(
    title = "Need of evacuation among hromadas"
    ,x = NULL
  )
```

![](figure-png-iso/evacuation-1.png)<!-- -->
# 7. IDP

# 8. Economics


```{.r .fold-hide}
(ds0 %>% 
  mutate(
    `Special fund's expenditures were relocated` = fct_recode(
      special_fund_relocation,
       "Yes"      = "yes"
      ,"No"       = "no"
    ) %>% factor( levels = c(
       "Yes"           
       ,"No"     
    )
    )
  ) %>% 
  make_bi_freq_graph("military_action","Special fund's expenditures were relocated") )+
  labs(
    title = "Were budget expenditures from the special fund streamed to finance other, current needs?"
    ,x = NULL
  )
```

![](figure-png-iso/special-fund-1-1.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% filter(military_action=="no_combat") %>%
  mutate(
    `Special fund's expenditures were relocated` = case_when(special_fund_relocation=="no"~0,
                                                             special_fund_relocation=="yes"~1)
  ) %>% 
  ggplot(aes(x = prep_count, y = `Special fund's expenditures were relocated`)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between preparation and special fund realocation for non-combat hromadas') +
  xlab("counted preparations"))
```

![](figure-png-iso/special-fund-no-combat-1.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% filter(military_action=="no_combat") %>%
  mutate(
    `Special fund's expenditures were relocated` = case_when(special_fund_relocation=="no"~0,
                                                             special_fund_relocation=="yes"~1)
  ) %>% 
  ggplot(aes(x = urban_pct, y = `Special fund's expenditures were relocated`)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between urbanisation and special fund realocation for non-combat hromadas') +
  xlab("share of urban population"))
```

![](figure-png-iso/special-fund-urbanisation-1.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% filter(military_action=="no_combat") %>%
  mutate(
    `Special fund's expenditures were relocated` = case_when(special_fund_relocation=="no"~0,
                                                             special_fund_relocation=="yes"~1)
  ) %>% 
  ggplot(aes(x = own_income_prop_2021, y = `Special fund's expenditures were relocated`)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between financial autonomy and special fund realocation for non-combat hromadas') +
  xlab("share of own revenue in total budget in 2021"))
```

![](figure-png-iso/special-fund-own-income-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  filter(special_fund_relocation == "yes") %>% 
  select(hromada_name, `special_fund_relocation_needs/state_functions`:`special_fund_relocation_needs/healthcare`) %>% 
  pivot_longer(-c(hromada_name), names_to = "sector", values_to = "cut") %>% 
  mutate(
    sector = str_to_title(str_remove(sector, "sectors_"))
  ) %>% 
  group_by(sector) %>% 
  summarise(`Number of Hromadas` = sum(cut), .groups = "drop") %>% 
  filter(`Number of Hromadas` > 0)  %>% 
 mutate(sector = case_when(sector=="Special_fund_relocation_needs/Defense"~"Defense",
                           sector=="Special_fund_relocation_needs/Economic_activity"~"Economic Activity",
                           sector=="Special_fund_relocation_needs/Education"~"Education",
                           sector=="Special_fund_relocation_needs/Environment"~"Environment",
                           sector=="Special_fund_relocation_needs/Healthcare"~"Healthcare",
                           sector=="Special_fund_relocation_needs/Social_protection"~"Social Protection",
                           sector=="Special_fund_relocation_needs/Spirit_development"~"Spirit Development",
                           sector=="Special_fund_relocation_needs/State_functions"~"State Functions",
                           sector=="Special_fund_relocation_needs/Utilities"~"Utilities",
                           sector=="Special_fund_relocation_needs/Public_order"~"Public Order",
                           TRUE~sector)
 ) %>%
  ggplot(aes(x = `Number of Hromadas`, y = fct_reorder(sector, `Number of Hromadas`))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw()+
  labs(
    title = "Sectors for which the funds of the special fund were redistributed"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = "Sector"
  )+
  geom_col(position = position_stack()
           , alpha = .5
           ,data =
  )
```

![](figure-png-iso/special-fund-sectors-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>%
      group_by(region_en,  oblast_name_en ) %>% 
      summarize(`Relocated companies` = sum(relocated_companies, na.rm = TRUE)) %>% 
  filter(`Relocated companies`>0) %>%
    ggplot(aes(x = `Relocated companies`, y = fct_reorder(oblast_name_en, `Relocated companies`))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw()+
  labs(
    title = "Relocated businesses by destination region"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = "Oblast"
  )+
  geom_col(position = position_stack()
           , alpha = .5
           ,data =
  )
```

![](figure-png-iso/relocated-bussiness-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>% 
  filter(relocated_companies>0) %>%
    ggplot(aes(y = relocated_companies, x = own_income_prop_2021 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between financial autonomy and business relocation inflow') +
  xlab("share of own revenue in total budget in 2021")
```

![](figure-png-iso/relocated-companies-own-income-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% filter(military_action=="no_combat") %>%
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>% 
  filter(relocated_companies>0) %>%
    ggplot(aes(y = relocated_companies, x = prep_count )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between preparations and business relocation inflow')
```

![](figure-png-iso/relocated-companies-preparation-1.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% 
   mutate(
    `Jobs created` = fct_recode(
      created_jobs,
       "Don't know" = "dk",
      "0-50" = "0_50_jobs",
      "51-100" = "51_100_jobs",
      "101-250" = "101_250_jobs"
    ) %>% factor( levels = c(
       "Don't know"           
       ,"0-50" ,
       "51-100",
       "101-250"
    )
    )
  ) %>%
   make_bi_freq_graph("Jobs created"))+
  labs(
    title = "How many jobs were created in the hromada thanks to the relocated enterprises?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/created-jobs-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% filter(bussiness_stimules_none == 0) %>%
  select(hromada_name, `bussiness_stimules/tax_benefits`:`bussiness_stimules/other`) %>% 
  pivot_longer(-c(hromada_name), names_to = "type", values_to = "done") %>% 
  mutate(
    type = str_to_title(str_remove(type, "types_"))
  ) %>% 
  group_by(type) %>% 
  summarise(`number of hromadas` = sum(done), .groups = "drop") %>% 
  filter(`number of hromadas`>0) %>% 
  mutate(type = case_when(type == "Bussiness_stimules/Education" ~ "Organized Educational Events",
                          type == "Bussiness_stimules/Free_rooms" ~ "Provided Premises for Free",
                          type == "Bussiness_stimules/Tax_benefits" ~ "Provided with Tax Benefits",
                          type == "Bussiness_stimules/Other" ~ "Other Methods",
  TRUE~type)
  ) %>%
  ggplot(aes(x = `number of hromadas`, y = fct_reorder(type, `number of hromadas`))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw()+
  geom_col(position = position_stack()
           , alpha = .5
           ,data =
  )+
  labs(
    title = "Which incentives have been used to support business in the community since February 24?"
    ,x = "Number of Hromadas", y = NULL, fill = NULL
  )+
  theme(
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
```

![](figure-png-iso/bussiness-stimules-1.png)<!-- -->

# 9. Humanitarian

# 10. Reconstructioin

# small number of answers (38) - omit at this stage

## 10.1 Demage Evaluation


```{.r .fold-hide}
ds0 %>% 
  count(region_en, is_damaged) %>% 
  ggplot(aes(x = region_en, y = n, fill = is_damaged)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Were there any destructions in the hromada as a result of military actions?"
    ,y = "Number of Hromadas", x = NULL, fill = NULL
  ) +
  theme_bw()
```

![](figure-png-iso/damage-1-1.png)<!-- -->



# 11. Current Challenges


```{.r .fold-hide}
# ds0 %>% 
#   count(region_en, is_damaged) %>% 
#   ggplot(aes(x = region_en, y = n, fill = is_damaged)) +
#   geom_bar(stat = "identity") +
#   labs(
#     title = "Were there any destructions in the community as a result of military actions?"
#     ,y = "Number of Hromadas", x = NULL, fill = NULL
#   ) +
#   theme_bw()
```

#poor quality of answerts regarding days without schooling - consider to omit at all

## 11.1 Heating season


```{.r .fold-hide}
ds1_winter_prep %>% 
  select(hromada_name, all_of(prep_for_winter)) %>% 
  pivot_longer(
    -hromada_name
    ,names_to = "action"
    ,values_to = "response"
  ) %>% 
  group_by(action) %>% 
  summarise(n = sum(response, na.rm = T), .groups = "drop") %>% 
  ggplot(aes(x = n, y = fct_reorder(action, n))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Which of the following measures were implemented to prepare hromada for the heating season?"
    ,y = NULL, x = NULL, fill = NULL
  ) +
  theme_bw()
```

![](figure-png-iso/heating-1-1.png)<!-- -->


```{.r .fold-hide}
ds1_winter_prep %>% 
  count(winter_prep_count) %>% 
  filter(!is.na(winter_prep_count)) %>% 
  ggplot(aes(x = factor(winter_prep_count), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Number of measures implemented to prepare hromada for the heating season"
    ,y = "Number of hromadas", x = "Number of measures", fill = NULL
  ) +
  xlab("winter_prep_count")
```

![](figure-png-iso/heating-2-1.png)<!-- -->


```{.r .fold-hide}
m1_heating <-lm(data = ds1_winter_prep, 
       winter_prep_count ~ log(income_total_2021) + own_income_prop_2021 + 
         type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_heating <-lm(data = ds1_winter_prep, 
             winter_prep_count ~ log(income_total_2021) + own_income_prop_2021 + 
               turnout_2020 + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m3_heating <-lm(data = ds1_winter_prep,
             winter_prep_count ~ log(income_total_2021) + own_income_prop_2021 +
               turnout_2020 + + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda)

m4_heating <-lm(data = ds1_winter_prep,
             winter_prep_count ~ log(income_total_2021) + own_income_prop_2021 +
               turnout_2020 + + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda +
               youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer::stargazer(m1_heating, m2_heating, m3_heating, m4_heating, single.row = T, type = 'html')
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">winter_prep_count</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">log(income_total_2021)</td><td>-0.041 (0.608)</td><td>0.091 (0.609)</td><td>-0.155 (0.632)</td><td>1.041 (0.908)</td></tr>
<tr><td style="text-align:left">own_income_prop_2021</td><td>-0.797 (1.098)</td><td>-1.270 (1.114)</td><td>-0.947 (1.152)</td><td>-2.188 (1.577)</td></tr>
<tr><td style="text-align:left">turnout_2020</td><td></td><td>3.298<sup>*</sup> (1.724)</td><td>3.437<sup>*</sup> (1.771)</td><td>6.324<sup>***</sup> (2.251)</td></tr>
<tr><td style="text-align:left">travel_time</td><td></td><td>0.002 (0.002)</td><td>0.003 (0.002)</td><td>0.005<sup>*</sup> (0.003)</td></tr>
<tr><td style="text-align:left">typeселищна</td><td></td><td></td><td></td><td>-0.979 (0.724)</td></tr>
<tr><td style="text-align:left">typeміська</td><td>-0.222 (0.318)</td><td>-0.243 (0.315)</td><td>-0.233 (0.325)</td><td>-1.489<sup>*</sup> (0.867)</td></tr>
<tr><td style="text-align:left">typeсільська</td><td>0.115 (0.421)</td><td>0.251 (0.422)</td><td>0.174 (0.432)</td><td></td></tr>
<tr><td style="text-align:left">square</td><td>0.0002 (0.001)</td><td>-0.0001 (0.001)</td><td>0.00000 (0.001)</td><td>0.0001 (0.001)</td></tr>
<tr><td style="text-align:left">log(total_population_2022)</td><td>0.202 (0.619)</td><td>0.310 (0.619)</td><td>0.599 (0.642)</td><td>-0.240 (0.877)</td></tr>
<tr><td style="text-align:left">urban_pct</td><td>-0.110 (0.771)</td><td>0.169 (0.774)</td><td>-0.004 (0.804)</td><td>0.514 (1.050)</td></tr>
<tr><td style="text-align:left">n_settlements</td><td>0.001 (0.010)</td><td>0.003 (0.010)</td><td>0.0004 (0.010)</td><td>-0.010 (0.011)</td></tr>
<tr><td style="text-align:left">region_enEast</td><td>0.206 (0.344)</td><td>0.427 (0.351)</td><td>0.442 (0.357)</td><td>-0.015 (0.648)</td></tr>
<tr><td style="text-align:left">region_enNorth</td><td>0.372 (0.413)</td><td>0.322 (0.411)</td><td>0.298 (0.417)</td><td>1.285 (0.799)</td></tr>
<tr><td style="text-align:left">region_enSouth</td><td>-0.521 (0.397)</td><td>-0.542 (0.396)</td><td>-0.548 (0.400)</td><td>0.508 (0.972)</td></tr>
<tr><td style="text-align:left">region_enWest</td><td>0.176 (0.359)</td><td>0.022 (0.378)</td><td>0.072 (0.394)</td><td>-0.256 (0.503)</td></tr>
<tr><td style="text-align:left">occupationoccupied_april</td><td>-0.296 (0.576)</td><td>-0.439 (0.572)</td><td>-0.448 (0.592)</td><td>-1.404 (1.283)</td></tr>
<tr><td style="text-align:left">military_actioncombat_now</td><td>0.136 (0.710)</td><td>0.246 (0.701)</td><td>0.197 (0.708)</td><td>-0.709 (1.097)</td></tr>
<tr><td style="text-align:left">military_actionno_combat</td><td>0.196 (0.612)</td><td>0.075 (0.605)</td><td>-0.006 (0.628)</td><td>-0.628 (1.525)</td></tr>
<tr><td style="text-align:left">voluntary</td><td>-0.042 (0.209)</td><td>0.047 (0.213)</td><td>0.036 (0.217)</td><td>-0.151 (0.279)</td></tr>
<tr><td style="text-align:left">sex_headmale</td><td></td><td></td><td>0.056 (0.230)</td><td>-0.052 (0.326)</td></tr>
<tr><td style="text-align:left">age_head</td><td></td><td></td><td>-0.008 (0.012)</td><td>-0.011 (0.018)</td></tr>
<tr><td style="text-align:left">education_headnon-higher</td><td></td><td></td><td>0.399 (0.418)</td><td>0.315 (0.546)</td></tr>
<tr><td style="text-align:left">incumbent</td><td></td><td></td><td>0.191 (0.224)</td><td>0.087 (0.302)</td></tr>
<tr><td style="text-align:left">rda</td><td></td><td></td><td>-0.489 (0.423)</td><td>0.073 (0.485)</td></tr>
<tr><td style="text-align:left">youth_councils</td><td></td><td></td><td></td><td>-0.075 (0.488)</td></tr>
<tr><td style="text-align:left">youth_centers</td><td></td><td></td><td></td><td>-0.103 (0.299)</td></tr>
<tr><td style="text-align:left">sum_osbb_2020</td><td></td><td></td><td></td><td>0.009 (0.020)</td></tr>
<tr><td style="text-align:left">region_enEast:sum_osbb_2020</td><td></td><td></td><td></td><td>0.027 (0.034)</td></tr>
<tr><td style="text-align:left">region_enNorth:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.035 (0.024)</td></tr>
<tr><td style="text-align:left">region_enSouth:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.211<sup>**</sup> (0.089)</td></tr>
<tr><td style="text-align:left">region_enWest:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.006 (0.019)</td></tr>
<tr><td style="text-align:left">Constant</td><td>2.215 (5.497)</td><td>-2.473 (5.852)</td><td>-0.619 (6.138)</td><td>-12.656 (8.927)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>130</td><td>129</td><td>129</td><td>72</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.089</td><td>0.136</td><td>0.168</td><td>0.546</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>-0.040</td><td>-0.006</td><td>-0.014</td><td>0.213</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>1.075 (df = 113)</td><td>1.059 (df = 110)</td><td>1.064 (df = 105)</td><td>0.916 (df = 41)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>0.691 (df = 16; 113)</td><td>0.958 (df = 18; 110)</td><td>0.923 (df = 23; 105)</td><td>1.641<sup>*</sup> (df = 30; 41)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## 11.2 Problem Involvement

```{.r .fold-hide}
m1_problem <-glm(data = ds1_problem, 
             hromada_exp ~ log(income_total_2021) + own_income_prop_2021 + 
             type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary,
             family = "binomial")

m2_problem  <-glm(data = ds1_problem, 
             hromada_exp ~ log(income_total_2021) + own_income_prop_2021 + 
               turnout_2020 + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary,
             family = "binomial")

m3_problem  <-glm(data = ds1_problem,
             hromada_exp ~ log(income_total_2021) + own_income_prop_2021 +
               turnout_2020 + + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda,
             family = "binomial")

m4_problem  <-glm(data = ds1_problem,
             hromada_exp ~ log(income_total_2021) + own_income_prop_2021 +
               turnout_2020 + + travel_time +
               type + square + log(total_population_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda +
               youth_councils + youth_centers + region_en * sum_osbb_2020,
             family = "binomial")

stargazer::stargazer(m1_problem, m2_problem, m3_problem, m4_problem, single.row = T, type = 'html')
```

```

<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">hromada_exp</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">log(income_total_2021)</td><td>0.135 (1.503)</td><td>0.093 (1.517)</td><td>-0.736 (1.677)</td><td>-84.725 (692,099.900)</td></tr>
<tr><td style="text-align:left">own_income_prop_2021</td><td>0.107 (2.818)</td><td>0.181 (2.838)</td><td>1.581 (2.970)</td><td>114.139 (730,084.900)</td></tr>
<tr><td style="text-align:left">turnout_2020</td><td></td><td>-0.826 (4.510)</td><td>-2.146 (4.699)</td><td>135.893 (2,141,099.000)</td></tr>
<tr><td style="text-align:left">travel_time</td><td></td><td>0.002 (0.005)</td><td>0.001 (0.005)</td><td>-0.343 (3,853.388)</td></tr>
<tr><td style="text-align:left">typeселищна</td><td></td><td></td><td></td><td>-4.701 (819,133.200)</td></tr>
<tr><td style="text-align:left">typeміська</td><td>-0.755 (0.889)</td><td>-0.845 (0.885)</td><td>-0.836 (0.949)</td><td>-27.317 (1,039,895.000)</td></tr>
<tr><td style="text-align:left">typeсільська</td><td>-1.061 (1.127)</td><td>-1.019 (1.146)</td><td>-1.402 (1.230)</td><td></td></tr>
<tr><td style="text-align:left">square</td><td>-0.003<sup>**</sup> (0.002)</td><td>-0.003<sup>**</sup> (0.002)</td><td>-0.003<sup>*</sup> (0.002)</td><td>-0.060 (265.870)</td></tr>
<tr><td style="text-align:left">log(total_population_2022)</td><td>0.459 (1.513)</td><td>0.516 (1.557)</td><td>1.281 (1.709)</td><td>125.880 (495,023.100)</td></tr>
<tr><td style="text-align:left">urban_pct</td><td>-0.679 (2.098)</td><td>-0.732 (2.123)</td><td>-1.165 (2.271)</td><td>7.480 (1,382,325.000)</td></tr>
<tr><td style="text-align:left">n_settlements</td><td>0.048 (0.030)</td><td>0.054<sup>*</sup> (0.032)</td><td>0.050 (0.033)</td><td>0.270 (14,526.640)</td></tr>
<tr><td style="text-align:left">region_enEast</td><td>1.519 (1.175)</td><td>1.595 (1.187)</td><td>1.294 (1.218)</td><td>29.804 (367,404.000)</td></tr>
<tr><td style="text-align:left">region_enNorth</td><td>0.529 (1.207)</td><td>0.600 (1.224)</td><td>0.461 (1.263)</td><td>56.732 (691,508.000)</td></tr>
<tr><td style="text-align:left">region_enSouth</td><td>0.383 (0.936)</td><td>0.486 (0.949)</td><td>0.395 (0.971)</td><td>-45.057 (371,854.300)</td></tr>
<tr><td style="text-align:left">region_enWest</td><td>-0.103 (0.893)</td><td>0.062 (0.947)</td><td>0.187 (1.025)</td><td>32.596 (512,285.700)</td></tr>
<tr><td style="text-align:left">occupationoccupied_april</td><td>-1.840 (1.633)</td><td>-1.811 (1.649)</td><td>-1.800 (1.712)</td><td>-25.166 (621,634.500)</td></tr>
<tr><td style="text-align:left">occupationoccupied_august</td><td>-19.982 (3,956.181)</td><td>-20.257 (3,956.181)</td><td>-21.439 (6,522.639)</td><td></td></tr>
<tr><td style="text-align:left">occupationoccupied_now</td><td>-1.855 (1.777)</td><td>-1.854 (1.803)</td><td>-1.905 (1.884)</td><td>-49.613 (460,278.900)</td></tr>
<tr><td style="text-align:left">military_actioncombat_now</td><td>-16.271 (1,403.286)</td><td>-16.298 (1,402.541)</td><td>-17.289 (2,250.865)</td><td>-43.202 (606,589.200)</td></tr>
<tr><td style="text-align:left">military_actionno_combat</td><td>-17.568 (1,403.286)</td><td>-17.583 (1,402.541)</td><td>-18.544 (2,250.864)</td><td>37.331 (652,535.300)</td></tr>
<tr><td style="text-align:left">voluntary</td><td>0.614 (0.578)</td><td>0.691 (0.591)</td><td>0.901 (0.641)</td><td>42.242 (212,131.500)</td></tr>
<tr><td style="text-align:left">sex_headmale</td><td></td><td></td><td>-0.026 (0.640)</td><td>-7.225 (289,236.300)</td></tr>
<tr><td style="text-align:left">age_head</td><td></td><td></td><td>-0.036 (0.034)</td><td>-4.669 (29,066.870)</td></tr>
<tr><td style="text-align:left">education_headnon-higher</td><td></td><td></td><td>17.630 (1,980.788)</td><td>15.497 (478,058.400)</td></tr>
<tr><td style="text-align:left">incumbent</td><td></td><td></td><td>0.421 (0.592)</td><td>-34.180 (285,656.300)</td></tr>
<tr><td style="text-align:left">rda</td><td></td><td></td><td>-0.308 (1.097)</td><td>-19.316 (352,081.200)</td></tr>
<tr><td style="text-align:left">youth_councils</td><td></td><td></td><td></td><td>31.807 (358,561.700)</td></tr>
<tr><td style="text-align:left">youth_centers</td><td></td><td></td><td></td><td>5.807 (252,011.000)</td></tr>
<tr><td style="text-align:left">sum_osbb_2020</td><td></td><td></td><td></td><td>0.133 (9,351.328)</td></tr>
<tr><td style="text-align:left">region_enEast:sum_osbb_2020</td><td></td><td></td><td></td><td>1.884 (20,336.010)</td></tr>
<tr><td style="text-align:left">region_enNorth:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.734 (24,690.320)</td></tr>
<tr><td style="text-align:left">region_enSouth:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.171 (10,036.550)</td></tr>
<tr><td style="text-align:left">region_enWest:sum_osbb_2020</td><td></td><td></td><td></td><td>-0.917 (9,813.355)</td></tr>
<tr><td style="text-align:left">Constant</td><td>12.911 (1,403.351)</td><td>13.097 (1,402.612)</td><td>23.316 (2,250.921)</td><td>498.071 (7,397,994.000)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>137</td><td>136</td><td>136</td><td>75</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-54.208</td><td>-53.722</td><td>-50.852</td><td>-0.000</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>146.415</td><td>149.444</td><td>153.703</td><td>64.000</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>
```


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
 package       * version date (UTC) lib source
 assertthat      0.2.1   2019-03-21 [1] CRAN (R 4.2.2)
 backports       1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
 broom           1.0.1   2022-08-29 [1] CRAN (R 4.2.2)
 bslib           0.4.1   2022-11-02 [1] CRAN (R 4.2.2)
 cachem          1.0.6   2021-08-19 [1] CRAN (R 4.2.2)
 callr           3.7.3   2022-11-02 [1] CRAN (R 4.2.2)
 cellranger      1.1.0   2016-07-27 [1] CRAN (R 4.2.2)
 cli             3.4.1   2022-09-23 [1] CRAN (R 4.2.2)
 colorspace      2.0-3   2022-02-21 [1] CRAN (R 4.2.2)
 crayon          1.5.2   2022-09-29 [1] CRAN (R 4.2.2)
 crosstalk       1.2.0   2021-11-04 [1] CRAN (R 4.2.2)
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
 mgcv            1.8-41  2022-10-21 [2] CRAN (R 4.2.2)
 mime            0.12    2021-09-28 [1] CRAN (R 4.2.0)
 miniUI          0.1.1.1 2018-05-18 [1] CRAN (R 4.2.2)
 mitools         2.4     2019-04-26 [1] CRAN (R 4.2.2)
 modelr          0.1.10  2022-11-11 [1] CRAN (R 4.2.2)
 munsell         0.5.0   2018-06-12 [1] CRAN (R 4.2.2)
 nlme            3.1-160 2022-10-10 [2] CRAN (R 4.2.2)
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
 stargazer       5.2.3   2022-03-04 [1] CRAN (R 4.2.0)
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
 webshot         0.5.4   2022-09-26 [1] CRAN (R 4.2.2)
 withr           2.5.0   2022-03-03 [1] CRAN (R 4.2.2)
 xfun            0.35    2022-11-16 [1] CRAN (R 4.2.2)
 xml2            1.3.3   2021-11-30 [1] CRAN (R 4.2.2)
 xtable          1.8-4   2019-04-21 [1] CRAN (R 4.2.2)
 yaml            2.3.6   2022-10-18 [1] CRAN (R 4.2.2)

 [1] C:/Users/Valentyn Hatsko/AppData/Local/R/win-library/4.2
 [2] C:/Program Files/R/R-4.2.2/library

--------------------------------------------------------------------------------------------------
```

</details>



Report rendered by Valentyn Hatsko at 2022-12-27, 14:51 +0200 in 35 seconds.
