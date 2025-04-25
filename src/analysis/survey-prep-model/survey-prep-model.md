---
title: "Modeling Hromada Preparedness"
author: 
- "Andriy Koval"  
date: "Last updated: 2022-12-12"
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

> This report searches for association between Preparedness Index of hromadas and their 

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


```r
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")

# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

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
ds_general %>% glimpse(80)
```

```
Rows: 1,469
Columns: 84
$ hromada_code              <chr> "UA05020010000053508", "UA05020030000031457"~
$ hromada_name              <chr> "Агрономічна", "Вінницька", "Вороновицька", ~
$ raion_code                <chr> "UA05020000000026686", "UA05020000000026686"~
$ raion_name                <chr> "Вінницький", "Вінницький", "Вінницький", "В~
$ oblast_code               <chr> "UA05000000000010236", "UA05000000000010236"~
$ oblast_name               <chr> "Вінницька", "Вінницька", "Вінницька", "Вінн~
$ type                      <chr> "сільська", "міська", "селищна", "міська", "~
$ hromada_full_name         <chr> "Агрономічна сільська громада", "Вінницька м~
$ oblast_center             <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ hromada_center_code       <chr> "UA05020010010058853", "UA05020030010063857"~
$ hromada_center            <chr> "Агрономічне", "Вінниця", "Вороновиця", "Гні~
$ lat_center                <dbl> 49.19277, 49.23202, 49.11179, 49.09337, 49.1~
$ lon_center                <dbl> 28.37203, 28.46797, 28.68894, 28.33834, 29.2~
$ travel_time               <dbl> 13.9, 0.0, 28.9, 27.3, 65.0, 47.9, 38.8, 12.~
$ n_settlements             <dbl> 6, 9, 21, 11, 30, 39, 36, 13, 53, 52, 60, 13~
$ area                      <dbl> 86.1, 256.6, 373.5, 244.5, 469.3, 614.8, 587~
$ occipied_before_2022      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ total_popultaion_2022     <dbl> 9361, 387439, 17127, 18446, 21123, 20302, 21~
$ urban_popultaion_2022     <dbl> 0, 371004, 6246, 12191, 11095, 7958, 6547, 0~
$ urban_pct                 <dbl> 0.0000000, 0.9575804, 0.3646873, 0.6609021, ~
$ budget_code               <chr> "02547000000", "02536000000", "02508000000",~
$ budget_name               <chr> "Бюджет Агрономічної сільської територіально~
$ oblast_name_en            <chr> "Vinnytsia", "Vinnytsia", "Vinnytsia", "Vinn~
$ region_en                 <chr> "Center", "Center", "Center", "Center", "Cen~
$ region_code_en            <chr> "C", "C", "C", "C", "C", "C", "C", "C", "C",~
$ income_total_2021         <dbl> 34270.96, 1952592.70, 51515.95, 84179.36, 96~
$ income_transfert_2021     <dbl> 14845.52, 372456.14, 24807.73, 42646.94, 328~
$ income_military_2021      <dbl> 445.1480, 84391.3586, 163.1763, 1356.8441, 4~
$ income_pdfo_2021          <dbl> 10426.996, 983232.207, 16243.950, 21544.526,~
$ income_unified_tax_2021   <dbl> 4181.4541, 226354.5078, 2503.8871, 4652.1029~
$ income_property_tax_2021  <dbl> 1694.522, 153913.766, 4271.128, 7717.273, 69~
$ income_excise_duty_2021   <dbl> 498.95954, 107701.98123, 2686.67991, 2435.05~
$ income_own_2021           <dbl> 19425.438, 1580136.553, 26708.218, 41532.414~
$ own_income_prop_2021      <dbl> 0.57, 0.81, 0.52, 0.49, 0.66, 0.58, 0.51, 0.~
$ transfert_prop_2021       <dbl> 0.43, 0.19, 0.48, 0.51, 0.34, 0.42, 0.49, 0.~
$ military_tax_prop_2021    <dbl> 0.01, 0.04, 0.00, 0.02, 0.00, 0.01, 0.02, 0.~
$ pdfo_prop_2021            <dbl> 0.30, 0.50, 0.32, 0.26, 0.47, 0.34, 0.30, 0.~
$ unified_tax_prop_2021     <dbl> 0.12, 0.12, 0.05, 0.06, 0.06, 0.07, 0.06, 0.~
$ property_tax_prop_2021    <dbl> 0.05, 0.08, 0.08, 0.09, 0.07, 0.12, 0.05, 0.~
$ excise_duty_prop_2021     <dbl> 0.01, 0.06, 0.05, 0.03, 0.02, 0.03, 0.08, 0.~
$ own_income_change         <dbl> -0.05, 0.20, 0.09, 0.18, 0.01, 0.04, 0.01, -~
$ own_prop_change           <dbl> -0.02, 0.01, 0.01, 0.15, 0.02, 0.02, 0.05, -~
$ total_income_change       <dbl> -0.02, 0.17, 0.08, -0.08, -0.02, 0.01, -0.07~
$ income_own_2022           <dbl> 18424.450, 1890998.749, 29235.734, 49192.849~
$ income_total_2022         <dbl> 33467.49, 2293558.72, 55667.66, 77227.53, 93~
$ income_transfert_2022     <dbl> 15043.042, 402559.972, 26431.931, 28034.678,~
$ dfrr_executed             <dbl> 6300.823, 1188859.851, 3889.246, 3200.000, 1~
$ turnout_2020              <dbl> 0.4641189, 0.3145251, 0.4279812, 0.3854180, ~
$ sex_head                  <chr> "male", "male", "male", "male", "male", "mal~
$ age_head                  <dbl> 54, 54, 54, 56, 62, 61, 54, 33, 57, 48, 61, ~
$ education_head            <chr> "higher", "higher", "higher", "higher", "hig~
$ incumbent                 <dbl> 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0,~
$ rda                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,~
$ not_from_here             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,~
$ party                     <chr> "Самовисування", "Українська стратегія Гройс~
$ enterpreuner              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ unemployed                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ priv_work                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,~
$ polit_work                <dbl> 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1,~
$ communal_work             <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,~
$ ngo_work                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ party_national_winner     <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ no_party                  <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0,~
$ male                      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,~
$ high_educ                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
$ sum_osbb_2020             <dbl> 8, 656, 1, 24, 4, 6, NA, NA, 41, NA, 3, 5, N~
$ edem_total                <dbl> 2, 3, 1, 0, 2, 1, 2, 0, 0, 0, 1, 0, 2, 0, 0,~
$ edem_petitions            <dbl> 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0,~
$ edem_consultations        <dbl> 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,~
$ edem_participatory_budget <dbl> 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,~
$ edem_open_hromada         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ youth_councils            <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ youth_centers             <dbl> 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ business_support_centers  <dbl> 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
$ creation_date             <date> 2020-08-16, 2018-09-20, 2016-10-09, 2017-08~
$ creation_year             <dbl> 2020, 2018, 2016, 2017, 2016, 2020, 2019, 20~
$ time_before_24th          <dbl> 556.9167, 1252.9167, 1963.9167, 1648.9167, 2~
$ voluntary                 <dbl> 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0,~
$ war_zone_27_04_2022       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ war_zone_20_06_2022       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ war_zone_23_08_2022       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ war_zone_10_10_2022       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
$ passangers_2021           <dbl> NA, 670569, 10676, 5043, 3887, 276, 7874, NA~
$ train_station             <dbl> 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0,~
```

```{.r .fold-show}
ds_survey %>% glimpse(80)
```

```
Rows: 138
Columns: 277
$ index                                              <dbl> 2, 3, 5, 6, 7, 8, 9~
$ today                                              <dttm> 2022-10-12, 2022-1~
$ `_id`                                              <dbl> 191541757, 19156022~
$ hromada_code                                       <chr> "UA1206019000004351~
$ hromada_name                                       <chr> "Лозуватська", "Пус~
$ hromada_full_name                                  <chr> "Лозуватська сільсь~
$ raion_code                                         <chr> "UA1206000000002263~
$ raion_name                                         <chr> "Криворізький", "Ль~
$ oblast_code                                        <chr> "UA1200000000009047~
$ oblast_name                                        <chr> "Дніпропетровська",~
$ type                                               <chr> "сільська", "міська~
$ occupation                                         <chr> "not_occupied", NA,~
$ military_action                                    <chr> "no_combat", NA, "n~
$ population_text                                    <dbl> 18957, 16133, 12000~
$ partners_text                                      <dbl> 0, 1, 0, 0, 5, 0, 0~
$ friends_text                                       <dbl> 0, 3, 0, 0, 0, 0, 0~
$ state_communication                                <chr> "yes", "yes", "no",~
$ prep_first_aid_water                               <dbl> 1, 1, 1, 1, 1, 2, 1~
$ prep_first_aid_fuel                                <dbl> 2, 1, 1, 0, 2, 0, 2~
$ prep_reaction_plan                                 <dbl> 2, 2, 2, 2, 2, 2, 0~
$ prep_evacuation_plan                               <dbl> 2, 0, 2, 2, 1, 2, 1~
$ prep_reaction_plan_oth_hromadas                    <dbl> 1, 0, 0, 1, 1, 0, 0~
$ prep_reaction_plan_oda                             <dbl> 2, 0, 2, 1, 1, 2, 0~
$ prep_dftg_creation                                 <dbl> NA, 1, 1, 1, 1, 0, ~
$ prep_national_resistance                           <dbl> 1, 1, 0, 0, 1, 0, 0~
$ prep_starosta_meeting                              <dbl> 2, 1, 1, 1, 1, 2, 1~
$ prep_communal_meetiing                             <dbl> 2, 1, 1, 1, 1, 1, 1~
$ prep_online_map                                    <dbl> 2, 2, 1, 1, 2, 0, 0~
$ prep_shelter_list                                  <dbl> 2, 2, 1, 1, 2, 2, 1~
$ prep_notification_check                            <dbl> 2, 2, 1, 1, 0, 0, 1~
$ prep_backup                                        <dbl> 2, 0, 1, 0, 2, 0, 0~
$ prep_partly_backup                                 <dbl> NA, NA, 1, 1, 2, 0,~
$ shelter_capacity_before_text                       <chr> "близько 1600 осіб"~
$ shelter_capacity_now_text                          <chr> "близько 1600 осіб ~
$ telegram                                           <dbl> 2, 0, 1, 1, 2, 0, 0~
$ viber                                              <dbl> 2, 0, 0, 0, 0, 0, 0~
$ facebook                                           <dbl> 2, 2, 2, 2, 2, 2, 2~
$ chat_help                                          <dbl> 0, 0, 0, 0, 1, 0, 0~
$ hotline                                            <dbl> 0, 2, 0, 0, 2, 0, 1~
$ telegram_link                                      <chr> "https://t.me/loz_s~
$ facebook_link                                      <chr> "https://www.facebo~
$ head_hromada_communication                         <chr> "few_times_a_week",~
$ dftg_creation                                      <chr> "still_not", "yes",~
$ dftg_creation_date                                 <dttm> NA, 2022-02-25, NA~
$ help_for_military                                  <chr> "rooms transport mo~
$ `help_for_military/rooms`                          <dbl> 1, 1, 1, 1, 0, NA, ~
$ `help_for_military/transport`                      <dbl> 1, 0, 1, 1, 0, NA, ~
$ `help_for_military/money`                          <dbl> 1, 1, 1, 1, 1, NA, ~
$ `help_for_military/products`                       <dbl> 1, 1, 1, 1, 1, NA, ~
$ `help_for_military/other`                          <dbl> 1, 0, 0, 0, 0, NA, ~
$ help_for_military_text                             <chr> "Амуніція, різні пр~
$ transport_help_communal                            <chr> "5", NA, "4", "-", ~
$ transport_help_bought                              <chr> "0", NA, "0", "3-4"~
$ percent_working_march                              <dbl> 95.0, 98.0, 100.0, ~
$ percent_working_now                                <dbl> 95.0, 100.0, 100.0,~
$ commun_between_hromadas                            <chr> "Daily", "Daily", "~
$ evacuation                                         <chr> "no", "no", "no", "~
$ idp_accept                                         <chr> "yes", NA, "yes", "~
$ idp_registration_date                              <dttm> 2022-02-26, NA, 20~
$ idp_registration_number                            <dbl> 959, NA, 1162, 1600~
$ idp_real_number                                    <dbl> 1420, NA, 1220, 160~
$ idp_help                                           <chr> "communal_placement~
$ `idp_help/communal_placement`                      <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/private_placement`                       <dbl> 0, NA, 1, 1, 1, NA,~
$ `idp_help/regular_meal`                            <dbl> 0, NA, 0, 1, 0, NA,~
$ `idp_help/humanitar_help`                          <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/fundraising`                             <dbl> 0, NA, 0, 0, 0, NA,~
$ `idp_help/employ`                                  <dbl> 1, NA, 0, 0, 0, NA,~
$ `idp_help/psych_help`                              <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/law_help`                                <dbl> 1, NA, 0, 0, 0, NA,~
$ `idp_help/transit_center`                          <dbl> 1, NA, 0, 0, 0, NA,~
$ idp_place_rooms                                    <chr> "101_250_beds", NA,~
$ idp_room_number                                    <chr> NA, NA, NA, "1600",~
$ idp_child_education                                <dbl> 18, NA, 21, 200, 26~
$ special_fund_relocation                            <chr> "yes", "yes", "no",~
$ special_fund_relocation_needs                      <chr> "defense public_ord~
$ `special_fund_relocation_needs/state_functions`    <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/defense`            <dbl> 1, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/public_order`       <dbl> 1, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/economic_activity`  <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/environment`        <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/utilities`          <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/spirit_development` <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/education`          <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/social_protection`  <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/healthcare`         <dbl> 0, 1, NA, NA, NA, N~
$ relocated_companies_text                           <chr> "0", NA, "0", "0", ~
$ created_jobs                                       <chr> "dk", NA, "dk", "dk~
$ bussiness_stimules                                 <chr> "tax_benefits", NA,~
$ `bussiness_stimules/tax_benefits`                  <dbl> 1, NA, 0, 0, 0, NA,~
$ `bussiness_stimules/free_rooms`                    <dbl> 0, NA, 1, 0, 0, NA,~
$ `bussiness_stimules/education`                     <dbl> 0, NA, 0, 0, 1, NA,~
$ `bussiness_stimules/other`                         <dbl> 0, NA, 0, 1, 0, NA,~
$ bussiness_stimules_none                            <dbl> 0, NA, 0, 1, 0, NA,~
$ bussiness_stimules_other                           <chr> NA, NA, NA, "-", NA~
$ humanitarian_hub                                   <chr> NA, NA, NA, NA, NA,~
$ hromada_cooperation                                <chr> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/medicine`                     <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/food`                         <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/pensions`                     <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/evacuation`                   <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/other`                        <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/none`                         <dbl> NA, NA, NA, NA, NA,~
$ hromada_cooperation_text                           <chr> NA, NA, NA, NA, NA,~
$ is_damaged                                         <chr> "no", "no", "no", "~
$ percent_damaged                                    <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_persons                          <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_communal                         <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_bussiness                        <chr> NA, NA, NA, NA, NA,~
$ reconstruction_plan                                <chr> NA, NA, NA, NA, NA,~
$ reconstruction_financing                           <chr> NA, NA, NA, NA, NA,~
$ reconstruction_financing_text                      <chr> NA, NA, NA, NA, NA,~
$ international_projects                             <chr> "0", NA, "1", "1", ~
$ percent_reconstructed                              <chr> NA, NA, NA, NA, NA,~
$ finance_school_shelters                            <chr> "0", NA, "50 000 гр~
$ finance_school_shelters_coded                      <dbl> 0, NA, 50000, NA, 5~
$ info_campaign                                      <dbl> 1, NA, 0, 1, 1, NA,~
$ reserves                                           <dbl> 1, NA, 1, 1, 1, NA,~
$ count_power_sources                                <dbl> 1, NA, 0, 1, 1, NA,~
$ count_heaters_need                                 <dbl> 0, NA, 0, 1, 1, NA,~
$ solid_fuel_boiler                                  <dbl> 0, NA, 0, 1, 1, NA,~
$ no_school_days                                     <chr> "Дистанційно провод~
$ no_school_days_coded                               <chr> "0", NA, "0", "0", ~
$ hromada_exp                                        <chr> "yes", "yes", "no",~
$ hromada_problem_info                               <chr> "idp bussiness", "c~
$ `hromada_problem_info/idp`                         <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_info/citizens`                    <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_info/bussiness`                   <dbl> 1, 1, NA, NA, 1, NA~
$ `hromada_problem_info/experts`                     <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_info/ngo`                         <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_info/nobody`                      <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_consultation                       <chr> "idp", "bussiness",~
$ `hromada_problem_consultation/idp`                 <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_consultation/citizens`            <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/bussiness`           <dbl> 0, 1, NA, NA, 0, NA~
$ `hromada_problem_consultation/experts`             <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/ngo`                 <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/nobody`              <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_proposition                        <chr> "citizens", "nobody~
$ `hromada_problem_proposition/idp`                  <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/citizens`             <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/bussiness`            <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/experts`              <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_proposition/ngo`                  <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_proposition/nobody`               <dbl> 0, 1, NA, NA, 0, NA~
$ hromada_problem_system                             <chr> "idp", "bussiness",~
$ `hromada_problem_system/idp`                       <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_system/citizens`                  <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/bussiness`                 <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_system/experts`                   <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/ngo`                       <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/nobody`                    <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_feedback                           <chr> "idp", "bussiness",~
$ `hromada_problem_feedback/idp`                     <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/citizens`                <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/bussiness`               <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_feedback/experts`                 <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/ngo`                     <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_feedback/nobody`                  <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_execution                          <chr> "idp citizens", "bu~
$ `hromada_problem_execution/idp`                    <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/citizens`               <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_execution/bussiness`              <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_execution/experts`                <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/ngo`                    <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/nobody`                 <dbl> 0, 0, NA, NA, 0, NA~
$ skills_needed                                      <chr> "fundraising projec~
$ `skills_needed/fundraising`                        <dbl> 1, 1, 1, 1, 1, 1, 1~
$ `skills_needed/project_management`                 <dbl> 1, 1, 1, 1, 0, 0, 0~
$ `skills_needed/longterm_planning`                  <dbl> 0, 0, 1, 1, 0, 0, 0~
$ `skills_needed/crisis_planning`                    <dbl> 0, 1, 0, 1, 0, 0, 1~
$ `skills_needed/data_analysis`                      <dbl> 0, 1, 0, 1, 0, 0, 0~
$ `skills_needed/human_resourse`                     <dbl> 0, 0, 1, 1, 0, 0, 1~
$ `skills_needed/other`                              <dbl> 0, 0, 0, 0, 0, 0, 0~
$ skills_needed_text                                 <chr> NA, NA, NA, NA, NA,~
$ contact_text                                       <chr> "Петренко Ігор, 098~
$ evacuation_001                                     <chr> "no", "no", "no", "~
$ hromada_exp_problem                                <lgl> NA, NA, NA, NA, NA,~
$ `_uuid`                                            <chr> "699df016-92c6-406e~
$ `_submission_time`                                 <dttm> 2022-10-12 11:35:1~
$ `_validation_status`                               <lgl> NA, NA, NA, NA, NA,~
$ `_status`                                          <chr> "submitted_via_web"~
$ `_submitted_by`                                    <lgl> NA, NA, NA, NA, NA,~
$ `_tags`                                            <lgl> NA, NA, NA, NA, NA,~
$ region_en                                          <chr> "East", "West", "Ce~
$ `idp_help/communal_placement_number`               <dbl> 959, NA, 1162, 1600~
$ `idp_help/private_placement_number`                <dbl> 0, NA, 1162, 1600, ~
$ `idp_help/regular_meal_number`                     <dbl> 0, NA, 0, 1600, 0, ~
$ `idp_help/humanitar_help_number`                   <dbl> 959, NA, 1162, 1600~
$ `idp_help/fundraising_number`                      <dbl> 0, NA, 0, 0, 0, NA,~
$ `idp_help/employ_number`                           <dbl> 959, NA, 0, 0, 0, N~
$ `idp_help/psych_help_number`                       <dbl> 959, NA, 1162, 1600~
$ `idp_help/law_help_number`                         <dbl> 959, NA, 0, 0, 0, N~
$ `idp_help/transit_center_number`                   <dbl> 959, NA, 0, 0, 0, N~
$ idp_help_count                                     <dbl> 627, 627, 627, 627,~
$ prep_count                                         <dbl> 23, 14, 16, 14, 20,~
$ comm_channels_count                                <dbl> 6, 4, 3, 3, 7, 2, 3~
$ help_military_count                                <dbl> 5, 3, 4, 4, 2, 0, 5~
$ hromada_cooperation_count                          <dbl> 0, 0, 0, 0, 0, 2, 0~
$ dftg_creation_time                                 <chr> "2", NA, "1", "1", ~
$ idp_registration_time                              <chr> NA, "1", NA, "1", "~
$ prep_winter_count                                  <dbl> 3, 0, 1, 5, 5, 0, 3~
$ oblast_center                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ hromada_center_code                                <chr> "UA1206019001007788~
$ hromada_center                                     <chr> "Лозуватка", "Пусто~
$ lat_center                                         <dbl> 48.06131, 49.71896,~
$ lon_center                                         <dbl> 33.28102, 23.90473,~
$ travel_time                                        <dbl> 160.3, 27.5, 88.1, ~
$ n_settlements                                      <dbl> 32, 10, 4, 21, 10, ~
$ square                                             <dbl> 563.8, 95.7, 77.4, ~
$ occipied_before_2022                               <dbl> 0, 0, 0, 0, 0, 0, 0~
$ total_population_2022                              <dbl> 18464, 15121, 9738,~
$ urban_population_2022                              <dbl> 1312, 9372, 8608, 5~
$ urban_pct                                          <dbl> 0.07105719, 0.61980~
$ budget_code                                        <chr> "04579000000", "135~
$ budget_name                                        <chr> "Бюджет Лозуватсько~
$ oblast_name_en                                     <chr> "Driproptrovska", "~
$ region_en.x                                        <chr> "East", "West", "Ce~
$ region_code_en                                     <chr> "E", "W", "C", "C",~
$ income_total_2021                                  <dbl> 67902340, 83142969,~
$ income_transfert_2021                              <dbl> 28360223, 26747544,~
$ income_military_2021                               <dbl> 165437.2, 1471468.7~
$ income_pdfo_2021                                   <dbl> 21860190, 33743437,~
$ income_unified_tax_2021                            <dbl> 5038856.5, 7871346.~
$ income_property_tax_2021                           <dbl> 5898847.8, 9690968.~
$ income_excise_duty_2021                            <dbl> 3740238.35, 1989744~
$ income_own_2021                                    <dbl> 39542117, 56395425,~
$ own_income_prop_2021                               <dbl> 0.58, 0.68, 0.70, 0~
$ transfert_prop_2021                                <dbl> 0.42, 0.32, 0.30, 0~
$ military_tax_prop_2021                             <dbl> 0.00, 0.02, 0.00, 0~
$ pdfo_prop_2021                                     <dbl> 0.32, 0.41, 0.48, 0~
$ unified_tax_prop_2021                              <dbl> 0.07, 0.09, 0.03, 0~
$ property_tax_prop_2021                             <dbl> 0.09, 0.12, 0.13, 0~
$ excise_duty_prop_2021                              <dbl> 0.06, 0.02, 0.03, 0~
$ own_income_change                                  <dbl> 0.06, 0.30, -0.02, ~
$ own_prop_change                                    <dbl> 0.03, 0.06, 0.04, 0~
$ total_income_change                                <dbl> 0.02, 0.19, -0.07, ~
$ income_own                                         <dbl> 41909616, 73349537,~
$ income_total                                       <dbl> 69264708, 99182114,~
$ income_transfert                                   <dbl> 27355092, 25832577,~
$ dfrr_executed                                      <dbl> NA, 51740.635, NA, ~
$ turnout_2020                                       <dbl> 0.3736239, 0.427296~
$ sex_head                                           <chr> "female", "female",~
$ age_head                                           <dbl> 45, 40, 62, 56, 65,~
$ education_head                                     <chr> "higher", "higher",~
$ incumbent                                          <dbl> 0, 1, 1, 1, 1, 1, 0~
$ rda                                                <dbl> 1, 0, 0, 0, 0, 0, 0~
$ not_from_here                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ party                                              <chr> "Слуга народу", "Са~
$ enterpreuner                                       <dbl> 0, 0, 0, 0, 0, 0, 0~
$ unemployed                                         <dbl> 0, 0, 0, 0, 0, 0, 0~
$ priv_work                                          <dbl> 0, 1, 0, 0, 0, 0, 0~
$ polit_work                                         <dbl> 1, 0, 1, 1, 0, 1, 1~
$ communal_work                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ ngo_work                                           <dbl> 0, 0, 0, 0, 0, 0, 0~
$ party_national_winner                              <dbl> 1, 0, 0, 0, 0, 0, 0~
$ no_party                                           <dbl> 0, 1, 1, 1, 1, 1, 0~
$ male                                               <dbl> 0, 0, 0, 0, 1, 0, 0~
$ high_educ                                          <dbl> 1, 1, 1, 1, 1, 1, 1~
$ sum_osbb_2020                                      <dbl> NA, 29, 28, 6, NA, ~
$ edem_total                                         <dbl> 1, 2, 0, 0, 0, 0, 0~
$ edem_petitions                                     <dbl> 0, 1, 0, 0, 0, 0, 0~
$ edem_consultations                                 <dbl> 1, 0, 0, 0, 0, 0, 0~
$ edem_participatory_budget                          <dbl> 0, 0, 0, 0, 0, 0, 0~
$ edem_open_hromada                                  <dbl> 0, 1, 0, 0, 0, 0, 0~
$ youth_councils                                     <dbl> 0, 0, 0, 0, 0, 0, 0~
$ youth_centers                                      <dbl> 0, 2, 0, 0, 0, 0, 0~
$ business_support_centers                           <dbl> 0, 1, 0, 0, 0, 1, 0~
$ region_en.y                                        <chr> "East", "West", "Ce~
$ creation_date                                      <dttm> 2020-08-16, 2020-0~
$ creation_year                                      <dbl> 2020, 2020, 2017, 2~
$ time_before_24th                                   <dbl> 556.7917, 556.7917,~
$ voluntary                                          <dbl> 0, 0, 1, 0, 1, 0, 1~
$ war_zone_27_04_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_20_06_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_23_08_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_10_10_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
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
  select(type, name) %>% 
  print_all()
```

```
# A tibble: 32 x 2
   type        name                     
   <chr>       <chr>                    
 1 begin_group general_information      
 2 begin_group general_information_text 
 3 end_group   general_information_text 
 4 end_group   general_information      
 5 begin_group preparation              
 6 end_group   preparation              
 7 begin_group information              
 8 end_group   information              
 9 begin_group national_resistance      
10 begin_group transport_help           
11 end_group   transport_help           
12 end_group   national_resistance      
13 begin_group administrative_adaptation
14 end_group   administrative_adaptation
15 begin_group evacuation_section       
16 end_group   evacuation               
17 begin_group idp                      
18 end_group   idp                      
19 begin_group economics                
20 end_group   economics                
21 begin_group humanitarian             
22 end_group   humanitarian             
23 begin_group reconstruction           
24 begin_group damage_evaluation        
25 end_group   damage_evaluation        
26 end_group   reconstruction           
27 begin_group current_challenges       
28 begin_group heat_season              
29 end_group   heat_season              
30 begin_group problem_involvement      
31 end_group   problem_involvement      
32 end_group   current_challenges       
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

ds0 %>% glimpse(80)
```

```
Rows: 138
Columns: 283
$ index                                              <dbl> 2, 3, 5, 6, 7, 8, 9~
$ today                                              <dttm> 2022-10-12, 2022-1~
$ `_id`                                              <dbl> 191541757, 19156022~
$ hromada_code                                       <chr> "UA1206019000004351~
$ hromada_name                                       <chr> "Лозуватська", "Пус~
$ hromada_full_name                                  <chr> "Лозуватська сільсь~
$ raion_code                                         <chr> "UA1206000000002263~
$ raion_name                                         <chr> "Криворізький", "Ль~
$ oblast_code                                        <chr> "UA1200000000009047~
$ oblast_name                                        <chr> "Дніпропетровська",~
$ type                                               <chr> "сільська", "міська~
$ occupation                                         <chr> "not_occupied", NA,~
$ military_action                                    <chr> "no_combat", NA, "n~
$ population_text                                    <dbl> 18957, 16133, 12000~
$ partners_text                                      <dbl> 0, 1, 0, 0, 5, 0, 0~
$ friends_text                                       <dbl> 0, 3, 0, 0, 0, 0, 0~
$ state_communication                                <chr> "yes", "yes", "no",~
$ prep_first_aid_water                               <dbl> 1, 1, 1, 1, 1, 2, 1~
$ prep_first_aid_fuel                                <dbl> 2, 1, 1, 0, 2, 0, 2~
$ prep_reaction_plan                                 <dbl> 2, 2, 2, 2, 2, 2, 0~
$ prep_evacuation_plan                               <dbl> 2, 0, 2, 2, 1, 2, 1~
$ prep_reaction_plan_oth_hromadas                    <dbl> 1, 0, 0, 1, 1, 0, 0~
$ prep_reaction_plan_oda                             <dbl> 2, 0, 2, 1, 1, 2, 0~
$ prep_dftg_creation                                 <dbl> NA, 1, 1, 1, 1, 0, ~
$ prep_national_resistance                           <dbl> 1, 1, 0, 0, 1, 0, 0~
$ prep_starosta_meeting                              <dbl> 2, 1, 1, 1, 1, 2, 1~
$ prep_communal_meetiing                             <dbl> 2, 1, 1, 1, 1, 1, 1~
$ prep_online_map                                    <dbl> 2, 2, 1, 1, 2, 0, 0~
$ prep_shelter_list                                  <dbl> 2, 2, 1, 1, 2, 2, 1~
$ prep_notification_check                            <dbl> 2, 2, 1, 1, 0, 0, 1~
$ prep_backup                                        <dbl> 2, 0, 1, 0, 2, 0, 0~
$ prep_partly_backup                                 <dbl> NA, NA, 1, 1, 2, 0,~
$ shelter_capacity_before_text                       <chr> "близько 1600 осіб"~
$ shelter_capacity_now_text                          <chr> "близько 1600 осіб ~
$ telegram                                           <dbl> 2, 0, 1, 1, 2, 0, 0~
$ viber                                              <dbl> 2, 0, 0, 0, 0, 0, 0~
$ facebook                                           <dbl> 2, 2, 2, 2, 2, 2, 2~
$ chat_help                                          <dbl> 0, 0, 0, 0, 1, 0, 0~
$ hotline                                            <dbl> 0, 2, 0, 0, 2, 0, 1~
$ telegram_link                                      <chr> "https://t.me/loz_s~
$ facebook_link                                      <chr> "https://www.facebo~
$ head_hromada_communication                         <chr> "few_times_a_week",~
$ dftg_creation                                      <chr> "still_not", "yes",~
$ dftg_creation_date                                 <dttm> NA, 2022-02-25, NA~
$ help_for_military                                  <chr> "rooms transport mo~
$ `help_for_military/rooms`                          <dbl> 1, 1, 1, 1, 0, NA, ~
$ `help_for_military/transport`                      <dbl> 1, 0, 1, 1, 0, NA, ~
$ `help_for_military/money`                          <dbl> 1, 1, 1, 1, 1, NA, ~
$ `help_for_military/products`                       <dbl> 1, 1, 1, 1, 1, NA, ~
$ `help_for_military/other`                          <dbl> 1, 0, 0, 0, 0, NA, ~
$ help_for_military_text                             <chr> "Амуніція, різні пр~
$ transport_help_communal                            <chr> "5", NA, "4", "-", ~
$ transport_help_bought                              <chr> "0", NA, "0", "3-4"~
$ percent_working_march                              <dbl> 95.0, 98.0, 100.0, ~
$ percent_working_now                                <dbl> 95.0, 100.0, 100.0,~
$ commun_between_hromadas                            <chr> "Daily", "Daily", "~
$ evacuation                                         <chr> "no", "no", "no", "~
$ idp_accept                                         <chr> "yes", NA, "yes", "~
$ idp_registration_date                              <dttm> 2022-02-26, NA, 20~
$ idp_registration_number                            <dbl> 959, NA, 1162, 1600~
$ idp_real_number                                    <dbl> 1420, NA, 1220, 160~
$ idp_help                                           <chr> "communal_placement~
$ `idp_help/communal_placement`                      <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/private_placement`                       <dbl> 0, NA, 1, 1, 1, NA,~
$ `idp_help/regular_meal`                            <dbl> 0, NA, 0, 1, 0, NA,~
$ `idp_help/humanitar_help`                          <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/fundraising`                             <dbl> 0, NA, 0, 0, 0, NA,~
$ `idp_help/employ`                                  <dbl> 1, NA, 0, 0, 0, NA,~
$ `idp_help/psych_help`                              <dbl> 1, NA, 1, 1, 1, NA,~
$ `idp_help/law_help`                                <dbl> 1, NA, 0, 0, 0, NA,~
$ `idp_help/transit_center`                          <dbl> 1, NA, 0, 0, 0, NA,~
$ idp_place_rooms                                    <chr> "101_250_beds", NA,~
$ idp_room_number                                    <chr> NA, NA, NA, "1600",~
$ idp_child_education                                <dbl> 18, NA, 21, 200, 26~
$ special_fund_relocation                            <chr> "yes", "yes", "no",~
$ special_fund_relocation_needs                      <chr> "defense public_ord~
$ `special_fund_relocation_needs/state_functions`    <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/defense`            <dbl> 1, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/public_order`       <dbl> 1, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/economic_activity`  <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/environment`        <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/utilities`          <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/spirit_development` <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/education`          <dbl> 0, 0, NA, NA, NA, N~
$ `special_fund_relocation_needs/social_protection`  <dbl> 0, 1, NA, NA, NA, N~
$ `special_fund_relocation_needs/healthcare`         <dbl> 0, 1, NA, NA, NA, N~
$ relocated_companies_text                           <chr> "0", NA, "0", "0", ~
$ created_jobs                                       <chr> "dk", NA, "dk", "dk~
$ bussiness_stimules                                 <chr> "tax_benefits", NA,~
$ `bussiness_stimules/tax_benefits`                  <dbl> 1, NA, 0, 0, 0, NA,~
$ `bussiness_stimules/free_rooms`                    <dbl> 0, NA, 1, 0, 0, NA,~
$ `bussiness_stimules/education`                     <dbl> 0, NA, 0, 0, 1, NA,~
$ `bussiness_stimules/other`                         <dbl> 0, NA, 0, 1, 0, NA,~
$ bussiness_stimules_none                            <dbl> 0, NA, 0, 1, 0, NA,~
$ bussiness_stimules_other                           <chr> NA, NA, NA, "-", NA~
$ humanitarian_hub                                   <chr> NA, NA, NA, NA, NA,~
$ hromada_cooperation                                <chr> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/medicine`                     <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/food`                         <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/pensions`                     <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/evacuation`                   <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/other`                        <dbl> NA, NA, NA, NA, NA,~
$ `hromada_cooperation/none`                         <dbl> NA, NA, NA, NA, NA,~
$ hromada_cooperation_text                           <chr> NA, NA, NA, NA, NA,~
$ is_damaged                                         <chr> "no", "no", "no", "~
$ percent_damaged                                    <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_persons                          <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_communal                         <chr> NA, NA, NA, NA, NA,~
$ damage_evaluation_bussiness                        <chr> NA, NA, NA, NA, NA,~
$ reconstruction_plan                                <chr> NA, NA, NA, NA, NA,~
$ reconstruction_financing                           <chr> NA, NA, NA, NA, NA,~
$ reconstruction_financing_text                      <chr> NA, NA, NA, NA, NA,~
$ international_projects                             <chr> "0", NA, "1", "1", ~
$ percent_reconstructed                              <chr> NA, NA, NA, NA, NA,~
$ finance_school_shelters                            <chr> "0", NA, "50 000 гр~
$ finance_school_shelters_coded                      <dbl> 0, NA, 50000, NA, 5~
$ info_campaign                                      <dbl> 1, NA, 0, 1, 1, NA,~
$ reserves                                           <dbl> 1, NA, 1, 1, 1, NA,~
$ count_power_sources                                <dbl> 1, NA, 0, 1, 1, NA,~
$ count_heaters_need                                 <dbl> 0, NA, 0, 1, 1, NA,~
$ solid_fuel_boiler                                  <dbl> 0, NA, 0, 1, 1, NA,~
$ no_school_days                                     <chr> "Дистанційно провод~
$ no_school_days_coded                               <chr> "0", NA, "0", "0", ~
$ hromada_exp                                        <chr> "yes", "yes", "no",~
$ hromada_problem_info                               <chr> "idp bussiness", "c~
$ `hromada_problem_info/idp`                         <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_info/citizens`                    <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_info/bussiness`                   <dbl> 1, 1, NA, NA, 1, NA~
$ `hromada_problem_info/experts`                     <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_info/ngo`                         <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_info/nobody`                      <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_consultation                       <chr> "idp", "bussiness",~
$ `hromada_problem_consultation/idp`                 <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_consultation/citizens`            <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/bussiness`           <dbl> 0, 1, NA, NA, 0, NA~
$ `hromada_problem_consultation/experts`             <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/ngo`                 <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_consultation/nobody`              <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_proposition                        <chr> "citizens", "nobody~
$ `hromada_problem_proposition/idp`                  <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/citizens`             <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/bussiness`            <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_proposition/experts`              <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_proposition/ngo`                  <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_proposition/nobody`               <dbl> 0, 1, NA, NA, 0, NA~
$ hromada_problem_system                             <chr> "idp", "bussiness",~
$ `hromada_problem_system/idp`                       <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_system/citizens`                  <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/bussiness`                 <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_system/experts`                   <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/ngo`                       <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_system/nobody`                    <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_feedback                           <chr> "idp", "bussiness",~
$ `hromada_problem_feedback/idp`                     <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/citizens`                <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/bussiness`               <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_feedback/experts`                 <dbl> 0, 0, NA, NA, 1, NA~
$ `hromada_problem_feedback/ngo`                     <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_feedback/nobody`                  <dbl> 0, 0, NA, NA, 0, NA~
$ hromada_problem_execution                          <chr> "idp citizens", "bu~
$ `hromada_problem_execution/idp`                    <dbl> 1, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/citizens`               <dbl> 1, 0, NA, NA, 1, NA~
$ `hromada_problem_execution/bussiness`              <dbl> 0, 1, NA, NA, 1, NA~
$ `hromada_problem_execution/experts`                <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/ngo`                    <dbl> 0, 0, NA, NA, 0, NA~
$ `hromada_problem_execution/nobody`                 <dbl> 0, 0, NA, NA, 0, NA~
$ skills_needed                                      <chr> "fundraising projec~
$ `skills_needed/fundraising`                        <dbl> 1, 1, 1, 1, 1, 1, 1~
$ `skills_needed/project_management`                 <dbl> 1, 1, 1, 1, 0, 0, 0~
$ `skills_needed/longterm_planning`                  <dbl> 0, 0, 1, 1, 0, 0, 0~
$ `skills_needed/crisis_planning`                    <dbl> 0, 1, 0, 1, 0, 0, 1~
$ `skills_needed/data_analysis`                      <dbl> 0, 1, 0, 1, 0, 0, 0~
$ `skills_needed/human_resourse`                     <dbl> 0, 0, 1, 1, 0, 0, 1~
$ `skills_needed/other`                              <dbl> 0, 0, 0, 0, 0, 0, 0~
$ skills_needed_text                                 <chr> NA, NA, NA, NA, NA,~
$ contact_text                                       <chr> "Петренко Ігор, 098~
$ evacuation_001                                     <chr> "no", "no", "no", "~
$ hromada_exp_problem                                <lgl> NA, NA, NA, NA, NA,~
$ `_uuid`                                            <chr> "699df016-92c6-406e~
$ `_submission_time`                                 <dttm> 2022-10-12 11:35:1~
$ `_validation_status`                               <lgl> NA, NA, NA, NA, NA,~
$ `_status`                                          <chr> "submitted_via_web"~
$ `_submitted_by`                                    <lgl> NA, NA, NA, NA, NA,~
$ `_tags`                                            <lgl> NA, NA, NA, NA, NA,~
$ region_en                                          <chr> "East", "West", "Ce~
$ `idp_help/communal_placement_number`               <dbl> 959, NA, 1162, 1600~
$ `idp_help/private_placement_number`                <dbl> 0, NA, 1162, 1600, ~
$ `idp_help/regular_meal_number`                     <dbl> 0, NA, 0, 1600, 0, ~
$ `idp_help/humanitar_help_number`                   <dbl> 959, NA, 1162, 1600~
$ `idp_help/fundraising_number`                      <dbl> 0, NA, 0, 0, 0, NA,~
$ `idp_help/employ_number`                           <dbl> 959, NA, 0, 0, 0, N~
$ `idp_help/psych_help_number`                       <dbl> 959, NA, 1162, 1600~
$ `idp_help/law_help_number`                         <dbl> 959, NA, 0, 0, 0, N~
$ `idp_help/transit_center_number`                   <dbl> 959, NA, 0, 0, 0, N~
$ idp_help_count                                     <dbl> 627, 627, 627, 627,~
$ prep_count                                         <dbl> 23, 14, 16, 14, 20,~
$ comm_channels_count                                <dbl> 6, 4, 3, 3, 7, 2, 3~
$ help_military_count                                <dbl> 5, 3, 4, 4, 2, 0, 5~
$ hromada_cooperation_count                          <dbl> 0, 0, 0, 0, 0, 2, 0~
$ dftg_creation_time                                 <chr> "2", NA, "1", "1", ~
$ idp_registration_time                              <chr> NA, "1", NA, "1", "~
$ prep_winter_count                                  <dbl> 3, 0, 1, 5, 5, 0, 3~
$ oblast_center                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ hromada_center_code                                <chr> "UA1206019001007788~
$ hromada_center                                     <chr> "Лозуватка", "Пусто~
$ lat_center                                         <dbl> 48.06131, 49.71896,~
$ lon_center                                         <dbl> 33.28102, 23.90473,~
$ travel_time                                        <dbl> 160.3, 27.5, 88.1, ~
$ n_settlements                                      <dbl> 32, 10, 4, 21, 10, ~
$ square                                             <dbl> 563.8, 95.7, 77.4, ~
$ occipied_before_2022                               <dbl> 0, 0, 0, 0, 0, 0, 0~
$ total_population_2022                              <dbl> 18464, 15121, 9738,~
$ urban_population_2022                              <dbl> 1312, 9372, 8608, 5~
$ urban_pct                                          <dbl> 0.07105719, 0.61980~
$ budget_code                                        <chr> "04579000000", "135~
$ budget_name                                        <chr> "Бюджет Лозуватсько~
$ oblast_name_en                                     <chr> "Driproptrovska", "~
$ region_en.x                                        <chr> "East", "West", "Ce~
$ region_code_en                                     <chr> "E", "W", "C", "C",~
$ income_total_2021                                  <dbl> 67902340, 83142969,~
$ income_transfert_2021                              <dbl> 28360223, 26747544,~
$ income_military_2021                               <dbl> 165437.2, 1471468.7~
$ income_pdfo_2021                                   <dbl> 21860190, 33743437,~
$ income_unified_tax_2021                            <dbl> 5038856.5, 7871346.~
$ income_property_tax_2021                           <dbl> 5898847.8, 9690968.~
$ income_excise_duty_2021                            <dbl> 3740238.35, 1989744~
$ income_own_2021                                    <dbl> 39542117, 56395425,~
$ own_income_prop_2021                               <dbl> 0.58, 0.68, 0.70, 0~
$ transfert_prop_2021                                <dbl> 0.42, 0.32, 0.30, 0~
$ military_tax_prop_2021                             <dbl> 0.00, 0.02, 0.00, 0~
$ pdfo_prop_2021                                     <dbl> 0.32, 0.41, 0.48, 0~
$ unified_tax_prop_2021                              <dbl> 0.07, 0.09, 0.03, 0~
$ property_tax_prop_2021                             <dbl> 0.09, 0.12, 0.13, 0~
$ excise_duty_prop_2021                              <dbl> 0.06, 0.02, 0.03, 0~
$ own_income_change                                  <dbl> 0.06, 0.30, -0.02, ~
$ own_prop_change                                    <dbl> 0.03, 0.06, 0.04, 0~
$ total_income_change                                <dbl> 0.02, 0.19, -0.07, ~
$ income_own                                         <dbl> 41909616, 73349537,~
$ income_total                                       <dbl> 69264708, 99182114,~
$ income_transfert                                   <dbl> 27355092, 25832577,~
$ dfrr_executed                                      <dbl> NA, 51740.635, NA, ~
$ turnout_2020                                       <dbl> 0.3736239, 0.427296~
$ sex_head                                           <chr> "female", "female",~
$ age_head                                           <dbl> 45, 40, 62, 56, 65,~
$ education_head                                     <chr> "higher", "higher",~
$ incumbent                                          <dbl> 0, 1, 1, 1, 1, 1, 0~
$ rda                                                <dbl> 1, 0, 0, 0, 0, 0, 0~
$ not_from_here                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ party                                              <chr> "Слуга народу", "Са~
$ enterpreuner                                       <dbl> 0, 0, 0, 0, 0, 0, 0~
$ unemployed                                         <dbl> 0, 0, 0, 0, 0, 0, 0~
$ priv_work                                          <dbl> 0, 1, 0, 0, 0, 0, 0~
$ polit_work                                         <dbl> 1, 0, 1, 1, 0, 1, 1~
$ communal_work                                      <dbl> 0, 0, 0, 0, 0, 0, 0~
$ ngo_work                                           <dbl> 0, 0, 0, 0, 0, 0, 0~
$ party_national_winner                              <dbl> 1, 0, 0, 0, 0, 0, 0~
$ no_party                                           <dbl> 0, 1, 1, 1, 1, 1, 0~
$ male                                               <dbl> 0, 0, 0, 0, 1, 0, 0~
$ high_educ                                          <dbl> 1, 1, 1, 1, 1, 1, 1~
$ sum_osbb_2020                                      <dbl> NA, 29, 28, 6, NA, ~
$ edem_total                                         <dbl> 1, 2, 0, 0, 0, 0, 0~
$ edem_petitions                                     <dbl> 0, 1, 0, 0, 0, 0, 0~
$ edem_consultations                                 <dbl> 1, 0, 0, 0, 0, 0, 0~
$ edem_participatory_budget                          <dbl> 0, 0, 0, 0, 0, 0, 0~
$ edem_open_hromada                                  <dbl> 0, 1, 0, 0, 0, 0, 0~
$ youth_councils                                     <dbl> 0, 0, 0, 0, 0, 0, 0~
$ youth_centers                                      <dbl> 0, 2, 0, 0, 0, 0, 0~
$ business_support_centers                           <dbl> 0, 1, 0, 0, 0, 1, 0~
$ region_en.y                                        <chr> "East", "West", "Ce~
$ creation_date                                      <dttm> 2020-08-16, 2020-0~
$ creation_year                                      <dbl> 2020, 2020, 2017, 2~
$ time_before_24th                                   <dbl> 556.7917, 556.7917,~
$ voluntary                                          <dbl> 0, 0, 1, 0, 1, 0, 1~
$ war_zone_27_04_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_20_06_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_23_08_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ war_zone_10_10_2022                                <dbl> 0, 0, 0, 0, 0, 1, 0~
$ income_own_per_capita                              <dbl> 2141.5792, 3729.609~
$ income_total_per_capita                            <dbl> 3677.553, 5498.510,~
$ income_tranfert_per_capita                         <dbl> 1535.974, 1768.900,~
$ idp_registration_share                             <dbl> 0.051938908, NA, 0.~
$ idp_real_share                                     <dbl> 0.076906412, NA, 0.~
$ idp_child_share                                    <dbl> 0.018769552, NA, 0.~
```

To make our analysis more nimble we create four alternative versions of `ds1` with Invasion Preparedness questions

<details>

<summary>show transformations </summary>


```{.r .fold-show}
# Select the focal section of the investigation - Invasion Preparation Block
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
<div id="htmlwidget-c1946fd0e51726544559" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c1946fd0e51726544559">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"100\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.83\" data-max=\"191541757\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.01\" data-max=\"197322877.2\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"1288755475.83\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283"],["index","today","_id","hromada_code","hromada_name","hromada_full_name","raion_code","raion_name","oblast_code","oblast_name","type","occupation","military_action","population_text","partners_text","friends_text","state_communication","prep_first_aid_water","prep_first_aid_fuel","prep_reaction_plan","prep_evacuation_plan","prep_reaction_plan_oth_hromadas","prep_reaction_plan_oda","prep_dftg_creation","prep_national_resistance","prep_starosta_meeting","prep_communal_meetiing","prep_online_map","prep_shelter_list","prep_notification_check","prep_backup","prep_partly_backup","shelter_capacity_before_text","shelter_capacity_now_text","telegram","viber","facebook","chat_help","hotline","telegram_link","facebook_link","head_hromada_communication","dftg_creation","dftg_creation_date","help_for_military","help_for_military/rooms","help_for_military/transport","help_for_military/money","help_for_military/products","help_for_military/other","help_for_military_text","transport_help_communal","transport_help_bought","percent_working_march","percent_working_now","commun_between_hromadas","evacuation","idp_accept","idp_registration_date","idp_registration_number","idp_real_number","idp_help","idp_help/communal_placement","idp_help/private_placement","idp_help/regular_meal","idp_help/humanitar_help","idp_help/fundraising","idp_help/employ","idp_help/psych_help","idp_help/law_help","idp_help/transit_center","idp_place_rooms","idp_room_number","idp_child_education","special_fund_relocation","special_fund_relocation_needs","special_fund_relocation_needs/state_functions","special_fund_relocation_needs/defense","special_fund_relocation_needs/public_order","special_fund_relocation_needs/economic_activity","special_fund_relocation_needs/environment","special_fund_relocation_needs/utilities","special_fund_relocation_needs/spirit_development","special_fund_relocation_needs/education","special_fund_relocation_needs/social_protection","special_fund_relocation_needs/healthcare","relocated_companies_text","created_jobs","bussiness_stimules","bussiness_stimules/tax_benefits","bussiness_stimules/free_rooms","bussiness_stimules/education","bussiness_stimules/other","bussiness_stimules_none","bussiness_stimules_other","humanitarian_hub","hromada_cooperation","hromada_cooperation/medicine","hromada_cooperation/food","hromada_cooperation/pensions","hromada_cooperation/evacuation","hromada_cooperation/other","hromada_cooperation/none","hromada_cooperation_text","is_damaged","percent_damaged","damage_evaluation_persons","damage_evaluation_communal","damage_evaluation_bussiness","reconstruction_plan","reconstruction_financing","reconstruction_financing_text","international_projects","percent_reconstructed","finance_school_shelters","finance_school_shelters_coded","info_campaign","reserves","count_power_sources","count_heaters_need","solid_fuel_boiler","no_school_days","no_school_days_coded","hromada_exp","hromada_problem_info","hromada_problem_info/idp","hromada_problem_info/citizens","hromada_problem_info/bussiness","hromada_problem_info/experts","hromada_problem_info/ngo","hromada_problem_info/nobody","hromada_problem_consultation","hromada_problem_consultation/idp","hromada_problem_consultation/citizens","hromada_problem_consultation/bussiness","hromada_problem_consultation/experts","hromada_problem_consultation/ngo","hromada_problem_consultation/nobody","hromada_problem_proposition","hromada_problem_proposition/idp","hromada_problem_proposition/citizens","hromada_problem_proposition/bussiness","hromada_problem_proposition/experts","hromada_problem_proposition/ngo","hromada_problem_proposition/nobody","hromada_problem_system","hromada_problem_system/idp","hromada_problem_system/citizens","hromada_problem_system/bussiness","hromada_problem_system/experts","hromada_problem_system/ngo","hromada_problem_system/nobody","hromada_problem_feedback","hromada_problem_feedback/idp","hromada_problem_feedback/citizens","hromada_problem_feedback/bussiness","hromada_problem_feedback/experts","hromada_problem_feedback/ngo","hromada_problem_feedback/nobody","hromada_problem_execution","hromada_problem_execution/idp","hromada_problem_execution/citizens","hromada_problem_execution/bussiness","hromada_problem_execution/experts","hromada_problem_execution/ngo","hromada_problem_execution/nobody","skills_needed","skills_needed/fundraising","skills_needed/project_management","skills_needed/longterm_planning","skills_needed/crisis_planning","skills_needed/data_analysis","skills_needed/human_resourse","skills_needed/other","skills_needed_text","contact_text","evacuation_001","hromada_exp_problem","_uuid","_submission_time","_validation_status","_status","_submitted_by","_tags","region_en","idp_help/communal_placement_number","idp_help/private_placement_number","idp_help/regular_meal_number","idp_help/humanitar_help_number","idp_help/fundraising_number","idp_help/employ_number","idp_help/psych_help_number","idp_help/law_help_number","idp_help/transit_center_number","idp_help_count","prep_count","comm_channels_count","help_military_count","hromada_cooperation_count","dftg_creation_time","idp_registration_time","prep_winter_count","oblast_center","hromada_center_code","hromada_center","lat_center","lon_center","travel_time","n_settlements","square","occipied_before_2022","total_population_2022","urban_population_2022","urban_pct","budget_code","budget_name","oblast_name_en","region_en.x","region_code_en","income_total_2021","income_transfert_2021","income_military_2021","income_pdfo_2021","income_unified_tax_2021","income_property_tax_2021","income_excise_duty_2021","income_own_2021","own_income_prop_2021","transfert_prop_2021","military_tax_prop_2021","pdfo_prop_2021","unified_tax_prop_2021","property_tax_prop_2021","excise_duty_prop_2021","own_income_change","own_prop_change","total_income_change","income_own","income_total","income_transfert","dfrr_executed","turnout_2020","sex_head","age_head","education_head","incumbent","rda","not_from_here","party","enterpreuner","unemployed","priv_work","polit_work","communal_work","ngo_work","party_national_winner","no_party","male","high_educ","sum_osbb_2020","edem_total","edem_petitions","edem_consultations","edem_participatory_budget","edem_open_hromada","youth_councils","youth_centers","business_support_centers","region_en.y","creation_date","creation_year","time_before_24th","voluntary","war_zone_27_04_2022","war_zone_20_06_2022","war_zone_23_08_2022","war_zone_10_10_2022","income_own_per_capita","income_total_per_capita","income_tranfert_per_capita","idp_registration_share","idp_real_share","idp_child_share"],["dbl","dat","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dat","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","chr","chr","chr","dat","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","lgl","chr","dat","lgl","chr","lgl","lgl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dat","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl"],[0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,4,6,1,2,7,7,5,7,2,6,9,4,7,10,10,0,0,0,0,0,0,0,81,6,0,0,41,6,6,6,6,6,6,83,46,46,1,0,12,0,8,16,9,16,8,8,8,8,8,8,8,8,8,8,46,131,15,0,77,77,77,77,77,77,77,77,77,77,77,11,8,8,8,8,8,8,8,99,132,132,132,132,132,132,132,132,136,0,100,100,100,100,105,105,129,14,105,8,16,11,13,12,17,27,22,26,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,130,15,12,138,0,0,138,0,138,138,0,9,9,9,9,9,9,9,9,9,0,0,0,0,0,16,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,16,16],[0,0,0,0,0,0,0,0,0,0,0,0.7,0.7,0.7,0.7,0.7,0,2.9,4.3,0.7,1.4,5.1,5.1,3.6,5.1,1.4,4.3,6.5,2.9,5.1,7.2,7.2,0,0,0,0,0,0,0,58.7,4.3,0,0,29.7,4.3,4.3,4.3,4.3,4.3,4.3,60.1,33.3,33.3,0.7,0,8.7,0,5.8,11.6,6.5,11.6,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,33.3,94.9,10.9,0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,71.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,98.6,0,72.5,72.5,72.5,72.5,76.1,76.1,93.5,10.1,76.1,5.8,11.6,8,9.4,8.7,12.3,19.6,15.9,18.8,0,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,0,0,0,0,0,0,0,0,94.2,10.9,8.7,100,0,0,100,0,100,100,0,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,0,0,0,0,0,11.6,29.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31.9,0.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6.5,11.6,11.6],[138,30,138,138,135,137,76,76,22,22,3,5,4,120,11,15,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,105,106,3,3,3,3,3,58,133,5,3,51,20,3,3,3,3,3,56,23,32,26,29,6,3,2,32,120,110,68,3,3,3,3,3,3,3,3,3,6,8,79,2,45,3,3,3,3,3,3,3,3,3,3,12,5,11,3,3,3,3,3,32,3,5,3,3,2,3,3,3,3,2,5,3,3,3,3,3,8,11,6,109,82,3,3,3,3,3,61,38,2,18,3,3,3,3,3,3,23,3,3,3,3,3,3,21,3,3,3,3,3,3,24,3,3,3,3,3,3,23,3,3,3,3,3,3,22,3,3,3,3,3,3,42,2,2,2,2,2,2,2,9,124,4,1,138,138,1,1,1,1,5,88,79,52,120,36,31,88,77,37,1,22,11,6,4,32,51,6,2,138,138,138,138,134,54,137,1,138,95,96,138,137,22,5,5,138,138,91,138,138,138,138,138,52,52,10,42,12,30,15,71,42,58,138,138,138,95,138,2,35,2,2,2,2,24,2,2,2,2,2,1,2,2,2,2,38,5,2,2,2,2,3,4,9,5,15,6,15,2,2,2,2,2,138,138,138,130,123,117],[2,null,191541757,null,null,null,null,null,null,null,null,null,null,140,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,0,0,0,0,0,null,null,null,null,null,null,0,0,0,0,0,null,null,null,0,0,null,null,null,null,23,23,null,0,0,0,0,0,0,0,0,0,null,null,0,null,null,0,0,0,0,0,0,0,0,0,0,null,null,null,0,0,0,0,0,null,null,null,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,0,0,0,627,2,0,0,0,null,null,0,0,null,null,45.68,22.49,0,1,42.2,0,3359,0,0,null,null,null,null,null,10846101.81,5163331,0,1056172.94,227066.07,224034.84,8271,3131966.65,0.14,0.14,0,0.09,0.01,0.01,0,-0.83,-0.39,-0.43,1972353.16,11030764.44,5642000,78.5,0.27,null,32,null,0,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,2015,556.79,0,0,0,0,0,507.13,2607.8,962.57,0.01,0.01,0],[78.2,null,197322877.2,null,null,null,null,null,null,null,null,null,null,21136.72,1.34,1.34,null,1.04,1.08,1.41,1,0.55,0.84,1.02,0.56,1.12,1.13,0.78,1.08,1.24,0.55,0.86,null,null,0.6,0.71,1.8,0.33,0.86,null,null,null,null,null,null,0.73,0.7,0.73,0.97,0.42,null,null,null,89.22,91.77,null,null,null,null,2001.63,2326.16,null,0.71,0.65,0.41,0.99,0.26,0.22,0.69,0.61,0.28,null,null,64.67,null,null,0.23,0.51,0.34,0.13,0.08,0.49,0.03,0.39,0.39,0.28,null,null,null,0.19,0.18,0.45,0.3,0.19,null,null,null,0.33,0.33,0,0.5,0.33,0.33,null,null,null,null,null,null,null,null,null,null,null,null,853167.02,0.86,0.93,0.9,0.43,0.34,null,null,null,null,0.38,0.64,0.58,0.09,0.35,0.08,null,0.34,0.39,0.33,0.09,0.27,0.24,null,0.28,0.46,0.49,0.08,0.29,0.21,null,0.26,0.45,0.55,0.16,0.34,0.16,null,0.36,0.45,0.47,0.11,0.33,0.16,null,0.15,0.37,0.46,0.07,0.38,0.21,null,0.75,0.41,0.32,0.49,0.26,0.32,0.06,null,null,null,null,null,null,null,null,null,null,null,1590.84,1037.82,1107.57,1977.64,461.12,623.09,1618.08,1513.87,784.98,627,13.72,4.29,3.39,0.08,null,null,3.11,0.01,null,null,49.07,29.43,93.67,22.23,410.51,0,22076.86,12499.06,0.35,null,null,null,null,null,91899785.68,37659862.26,1815527.65,31364648.7,6130599.53,8123370.3,3741911.43,54239923.43,0.51,0.49,0.01,0.27,0.06,0.1,0.03,0.04,0.01,-0.01,56792346.01,92278010.92,35485664.91,32738.42,0.42,null,52.36,null,0.54,0.07,0.11,null,0.02,0.02,0.08,0.83,0.04,0,0.16,0.43,0.27,0.93,35.48,0.62,0.22,0.16,0.15,0.09,0.1,0.22,0.56,null,null,2018.22,1209.18,0.58,0.08,0.12,0.12,0.12,2244.72,4224.02,1979.3,0.1,0.1,0.05],[151,null,206471695,null,null,null,null,null,null,null,null,null,null,243000,20,17,null,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,null,null,2,2,2,2,2,null,null,null,null,null,null,1,1,1,1,1,null,null,null,100,100,null,null,null,null,20000,60000,null,1,1,1,1,1,1,1,1,1,null,null,800,null,null,1,1,1,1,1,1,1,1,1,1,null,null,null,1,1,1,1,1,null,null,null,1,1,0,1,1,1,null,null,null,null,null,null,null,null,null,null,null,null,13936323,1,1,1,1,1,null,null,null,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,1,null,null,null,null,null,null,null,null,null,null,null,20000,16331,20000,20000,8500,20000,20000,20000,16331,627,29,10,5,3,null,null,5,1,null,null,52.06,36.73,288,97,2497.1,0,317752,305239,1,null,null,null,null,null,1288755475.83,346574777.46,47254976.84,608781726.22,124876522.55,78663469.37,73206177.69,942180698.37,0.86,0.86,0.14,0.59,0.13,0.44,0.27,1.69,0.23,0.89,969725144.97,1248182878.17,315122334.64,757596.25,0.65,null,71,null,1,1,1,null,1,1,1,1,1,0,1,1,1,1,638,4,1,1,1,1,2,4,17,null,null,2020,2383.79,1,1,1,1,1,7418.91,9388.75,3470.86,0.63,0.63,0.33]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>type<\/th>\n      <th>na<\/th>\n      <th>na_pct<\/th>\n      <th>unique<\/th>\n      <th>min<\/th>\n      <th>mean<\/th>\n      <th>max<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"autoWidth":false,"columnDefs":[{"className":"dt-right","targets":[3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
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

> As of 2022-12-12, 138 hromadas contributed valid response to the survey

<mark>1.2</mark> What oblasts are represented in this sample>? 



```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en, oblast_name_en) %>% 
  summarize(
    hormada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  left_join(
    ds_general %>% 
      group_by(region_en,  oblast_name_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  mutate(
    prop = hormada_count/hromada_count_total
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
   <th style="text-align:right;"> hormada_count </th>
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

## Prep score change
 

```r
# Continuous - good for spreading out
comparison_vars_continuous <- c(
   "income_own_per_capita"           
  ,"income_total_per_capita"         
  ,"income_tranfert_per_capita"      
  ,"idp_registration_share"
  ,"idp_real_share"
  ,"idp_child_share"
  
  
  ,"square"
  ,"n_settlements"
  ,"travel_time"
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"                              
  ,"sum_osbb_2020"                                      
  ,"turnout_2020"
  ,"age_head"
  ,"time_before_24th"
)
# Categorical - for color
comparison_vars_discreate <- c(
   "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
)
comparison_vars <- c(
  comparison_vars_discreate
   ,comparison_vars_continuous
)

d <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score")) %>% 
  left_join(ds0 %>% select(hromada_code,all_of(comparison_vars))) %>% glimpse() %>% 
  mutate(
    across(
      .cols = comparison_vars_discreate
      ,.fns = ~factor(.)
    )
  ) %>%
  pivot_longer(
    cols = comparison_vars_continuous
    ,names_to = "item_name"
    ,values_to = "item_value"
  ) %>% glimpse()

make_plot_prepvs <- function(
    d
    ,xvar    # "prep_score"
    ,yvar    # "item_value"
    ,fillvar # "region_en"
    )
{
  g <- 
  d %>% 
  ggplot(aes(
      x     = !!rlang::sym(xvar)
      ,y    = !!rlang::sym(yvar)
      ,fill = !!rlang::sym(fillvar)
      ))+
  ggplot2::scale_fill_viridis_d(
    begin = 0, end = .8, direction = -1
    ,option = "plasma",guide= guide_legend(reverse=T)
  )+
  facet_wrap(facets = "item_name", scales = "free_y")+
  geom_point(shape=21,color = "black", size =3, alpha = .5, position=position_jitter(seed=42))+
    labs(
      title = paste0("Relationship between Invasion Preparedness Score (horizontal) and other attributes of hromadas")
    )
}  
# To see how it works
d %>% 
  make_plot_prepvs(
    xvar     = "prep_score"
    ,yvar    = "item_value"
    ,fillvar = "region_en"
  )  

# To execution multiple scenarios
for(i in comparison_vars_discreate){
  
  for(ii in c("prep_score","prep_score_before","prep_score_after")){
    g <- 
      d %>% 
      make_plot_prepvs(
        xvar     = ii
        ,yvar    = "item_value"
        ,fillvar = i
      )  %>% 
      file_name <- paste0(ii,"-",i)
    g %>% quick_save(paste0("/1/",file_name),w=12,h=8)
    }
}
```



```r
# Continuous - good for spreading out
comparison_vars_continuous <- c(
   "income_own_per_capita"           
  ,"income_total_per_capita"         
  ,"income_tranfert_per_capita"      
  ,"idp_registration_share"
  ,"idp_real_share"
  ,"idp_child_share"
  
  
  ,"square"
  ,"n_settlements"
  ,"travel_time"
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"                              
  ,"sum_osbb_2020"                                      
  ,"turnout_2020"
  ,"age_head"
  ,"time_before_24th"
)
# Categorical - for color
comparison_vars_discreate <- c(
   "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
)
comparison_vars <- c(
  comparison_vars_discreate
   ,comparison_vars_continuous
)

d <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score")) %>% 
  left_join(ds0 %>% select(hromada_code,all_of(comparison_vars))) 

d %>% glimpse()

make_plot_prep_change <- function(
  d
  ,ordervar = "prep_score"
  ,colorvar = "region_en"
){
# browser()
g <- 
  d %>% 
  mutate(
    hromada_code = hromada_code %>% factor() %>% fct_reorder(!!rlang::sym(ordervar))
  ) %>% 
  # sample_n(10) %>% 
  # slice(1:10) %>% 
  ggplot(aes(y=hromada_code, color = !!rlang::sym(colorvar)))+
  geom_segment(
    aes(
      y     = hromada_code
      ,yend = hromada_code
      ,x    = prep_score_before
      ,xend = prep_score_after
      # ,x    = 0                                   # to see only after 
      # ,xend = prep_score_after-prep_score_before  # to see only after
    )
    ,linewidth = 2 ,alpha = .6
  )+
  labs(
    title = paste0("The number of preparedness items secured by hromadas (N= ",
                   d %>% summarize(n=n_distinct(hromada_code)) %>% pull(n)
                   ,") before and after full scale invasion")
    ,subtitle = "Scale guide: (Before) = prior to Feb 24, (After) = at time of interview, Oct-Nov 2022, (Total) = Before + After"
    ,x = "Each segment starts at (Before) score and ends at (After)"
  )+
  # scale_color_viridis_d(
  #   begin = .8, end = .0, direction = -1
  #   , option = "plasma", guide= guide_legend(reverse=T)
  # )+
  scale_color_viridis_c(
    # begin = .8, end = .0, direction = -1
    # , option = "plasma", guide= guide_legend(reverse=T)
  )+
  # scale_color_brewer(type="qual", palette = "Dark2")+
  theme(
    axis.text.y = element_blank()
  )
return(g)
}
(
  d %>% 
  make_plot_prep_change(
    ordervar = "prep_score"
    ,colorvar = "income_own_per_capita"
  )
) %>% 
  quick_save("prep-change-segment-color",w=6,h=9)
```



```r
d <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score")) %>% 
  left_join(ds0 %>% select(hromada_code,all_of(comparison_vars))) 

# d %>% glimpse()

make_plot_prep_change_bw <- function(
    d
  ){
  # browser()
  # level_order <- d %>% arrange(prep_score_after, prep_score_before) %>% pull(hromada_code)
  level_order <- d %>% arrange(prep_score_before, prep_score_after) %>% pull(hromada_code)
  
  g <- 
    d %>% 
    mutate(
      hromada_code = hromada_code %>% factor(levels = level_order)
    ) %>% 
    ggplot(aes(y=hromada_code))+
    geom_segment(
      aes(
        y     = hromada_code
        ,yend = hromada_code
        ,x    = prep_score_before
        ,xend = prep_score_after
        # ,x    = 0                                   # to see only after 
        # ,xend = prep_score_after-prep_score_before  # to see only after
      )
      ,linewidth = 2 ,alpha = .2
    )+
    labs(
      title = paste0("The number of preparedness items secured by hromadas (N= ",
                     d %>% summarize(n=n_distinct(hromada_code)) %>% pull(n)
                     ,")")
      ,subtitle = "(Before) = prior to Feb 24, (After) = at time of interview, Oct-Nov 2022"
      ,x = "Each segment starts at (Before) score and ends at (After)"
      ,caption = "Ordered by Before + After"
      # ,caption = "Ordered by After + Before"
      ,y = NULL
    )+
    theme(
      axis.text.y = element_blank()
      ,panel.grid.major.y = element_blank()
    )
  return(g)
}
(d %>% 
  make_plot_prep_change_bw()
  ) %>% 
   quick_save("prep-change-segment-bw-before",w=5.5,h=9)
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
 date     2022-12-12
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



Report rendered by Valentyn Hatsko at 2022-12-12, 23:54 +0200 in 11 seconds.
