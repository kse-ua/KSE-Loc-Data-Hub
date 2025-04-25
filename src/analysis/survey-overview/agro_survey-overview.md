---
title: "Agro Survey Overview"
author: 
- "Valentyn Hatsko"
- "Andriy Koval"  
- "Serhii Tytiuk"  
date: "Last updated: 2023-01-26"
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

> This report visualizes key information about the survey conducted by the Center for Food and Land Use Research (KSE Agrocenter) in June-August 2023

***Important Definitions***

> Research Sample: Hromadas who responded to the survey. In total, 477 hromadas participated in the survey out of 1438 hromadas eligible to take part in the survey

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->





# Environment

> Reviews the components of the working environment of the report. Non-technical readers are welcomed to skip. Come back if you need to understand the origins of custom functions, scripts, or data objects.

<details>

<summary>

Packages used

</summary>

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
# import::from("magrittr", "%>%")
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
# library(gt)
library(stargazer)
```

</details>

<details>

<summary>

External scripts

</summary>

Collection of custom functions used in current repository (`sda-information-requests`)


```r
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling
```

</details>

<details>

<summary>

Global values

</summary>

Values used throughout the report.


```r
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-hromada-analysis/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
```

</details>

<details>

<summary>

Functions

</summary>

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
# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

# the product of ./manipulation/ellis-agro-survey.R
ds_survey <- readr::read_csv("./data-private/derived/agro-survey-full.csv")

#original survey dataset with variables and question types
meta_survey <- readxl::read_excel("./data-private/raw/agro-survey.xlsx", sheet = "variables")


# meta_oblast <- googlesheets4::read_sheet(sheet_name,"choices",skip = 0)

# Originally, we pulled the meta data object from Kobo front end and stored to 
# survey_xls  <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
# # we put this on google drive now, to control manually
# googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# # https://googlesheets4.tidyverse.org/ 
# # https://docs.google.com/spreadsheets/d/1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo/edit?usp=sharing
# survey_url <- "1GaP92b7P1AI5nIYmlG0XoKYVV9AF4PDV8pVW3IeySFo"
# meta_survey <- googlesheets4::read_sheet(survey_url,"survey",skip = 0)
# meta_choices <- googlesheets4::read_sheet(survey_url,"choices",skip = 0)
```

<details>

<summary>

click to glimpse

</summary>


```{.r .fold-show}
ds_survey %>% glimpse()
```

```
Rows: 477
Columns: 273
$ oblast                             <chr> "Харківська", "Дніпропетровська", "Черкаська", "Чернігі…
$ raion                              <chr> "Богодухівський", "Криворізький", "Звенигородський", "Н…
$ hromada_name                       <chr> "Золочівська селищна громада", "Зеленодольська міська г…
$ military_action                    <chr> "Так, бойові дії відбуваються дотепер", "Так, бойові ді…
$ occupation                         <chr> "Так, громада (або частина громади) і наразі перебуває …
$ occupation_duration                <dbl> 65, NA, NA, 33, NA, NA, NA, NA, NA, NA, NA, 36, NA, NA,…
$ income_change_march                <chr> "Зменшилися на 50-69%", "Зменшилися на 0-9%", "Не зменш…
$ income_change_april                <chr> "Зменшилися на 50-69%", "Зменшилися на 0-9%", "Не зменш…
$ income_change_may                  <chr> "Зменшилися на 50-69%", "Зменшилися на 0-9%", "Зменшили…
$ biggest_drop_type                  <chr> "ПДФО", "Єдиний податок", "Єдиний податок", "ПДФО", "ПД…
$ biggest_drop_type_other            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "плата …
$ budget_deficit_estimate            <chr> "Дефіцит оцінюється у розмірі понад 30% бюджету", "Дефі…
$ budget_cut                         <chr> "Так", "Так", "Ні", "Ні", "Так", "Так", "Ні", "Ні", "Ні…
$ budget_cut_pct                     <dbl> 20, 28, NA, NA, 15, 30, NA, NA, NA, NA, NA, 15, NA, 18,…
$ budget_cut_sphere                  <chr> "Освіта", "Економічна діяльність Охорона навколишнього …
$ budget_cut_sphere.no_cuts          <dbl> 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0…
$ budget_cut_sphere.state_functions  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1…
$ budget_cut_sphere.public_safety    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
$ budget_cut_sphere.economic         <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1…
$ budget_cut_sphere.environment      <dbl> 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1…
$ budget_cut_sphere.gkh              <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1…
$ budget_cut_sphere.healthcare       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
$ budget_cut_sphere.phys_development <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1…
$ budget_cut_sphere.education        <dbl> 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0…
$ budget_cut_sphere.social           <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0…
$ budget_cut_sphere.other            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
$ budget_cut_sphere.other_text       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ budget_cut_type                    <chr> "Оплата праці Оплата комунальних послуг Капітальні вида…
$ budget_cut_type.no_cuts            <dbl> 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0…
$ budget_cut_type.wages              <dbl> 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0…
$ budget_cut_type.utilities          <dbl> 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1…
$ budget_cut_type.social_payments    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0…
$ budget_cut_type.subsidies          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1…
$ budget_cut_type.capital            <dbl> 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1…
$ budget_cut_type.other              <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0…
$ budget_cut_type.other_text         <chr> NA, "Поточні видатки, оплата послуг", NA, NA, NA, NA, N…
$ financial_support                  <chr> "Офіційно зверталися та не отримували", "Офіційно зверт…
$ population_change                  <chr> "Зменшилася суттєво", "Зменшилася суттєво", "Збільшилас…
$ combatant_families                 <chr> "Так", "Так", "Так", "Так", "Так", "Ні", "Так", "Ні", "…
$ combatant_families_number          <dbl> 8, 4, 1, 13, 14, NA, 50, NA, NA, -1, 12, 400, 310, 1, 1…
$ idp_pct                            <chr> "5-10% від населення громади", "До 5% від населення гро…
$ idp_number                         <dbl> 3000, 553, 3608, 3299, 10000, 250, 60000, 6000, 1500, 7…
$ admin_services_stopped             <chr> "Так", "Ні", "Так", "Ні", "Ні", "Ні", "Ні", "Ні", "Ні",…
$ admin_services_resumed             <chr> "Ні", NA, "Так", NA, NA, NA, NA, NA, NA, "Надання адмін…
$ admin_services_time                <dbl> 146, NA, 60, NA, NA, NA, NA, NA, NA, 0, NA, 42, 112, NA…
$ mining_area                        <dbl> 12000, 15481, 0, 3500, 0, 0, 0, 0, 0, 0, 100, 78354000,…
$ garbage_interruptions              <chr> "Так", "Ні", "Ні", "Так", "Так", "Ні", "Ні", "Ні", "Ні"…
$ garbage_functional                 <chr> "Ні", NA, NA, "Так", "Так", NA, NA, NA, NA, NA, NA, "Та…
$ doctors_change                     <chr> "Кількість сімейних лікарів зменшилася", "Кількість сім…
$ volunteers_number                  <dbl> 60, 26, 53, 10, 15, 2, 2000, 5, 10, 0, 7, 50, 500, 500,…
$ humanitarian_hubs                  <dbl> 1, 2, 4, 1, 7, 0, 20, 1, 3, 1, 0, 10, 5, 4, 20, 20, 2, …
$ ingo                               <chr> "FAO (Продовольча та сільськогосподарська організація О…
$ ingo_ac                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_acf                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_acted                         <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
$ ingo_alima                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_cadena                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_care                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_drc                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_dtl                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_echo                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_erc                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_fao                           <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_fca                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_fcdo                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_gc                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_ger3                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_giz                           <dbl> 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_goal                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_hi                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_hia                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_hias                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_ifrc                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
$ ingo_imc                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
$ ingo_impact                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_intersos                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_iom                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_irc                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_loop                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_lwf                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_mdm                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_medair                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_msf                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0…
$ ingo_nrc                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1…
$ ingo_osce                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_pin                           <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_pui                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_r2p                           <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_react                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_reach                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_si                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_tri                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_tsf                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_unwomen                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1…
$ ingo_undp                          <dbl> 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1…
$ ingo_unfpa                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
$ ingo_unhcr                         <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_unicef                        <dbl> 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1…
$ ingo_wfp                           <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0…
$ ingo_who                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_wck                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_red_cross                     <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_ulead                         <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ingo_usaid                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_caritas                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_adra                          <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_other                         <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
$ ingo_other_text                    <chr> NA, "ULEAD,  ADRA, DTEK", NA, "ICRC (Міжнародний Коміте…
$ foreign_aid                        <chr> "Ні", "Ні", "Ні", "Ні", "Так", "Ні", "Так", "Ні", "Так"…
$ countries                          <chr> NA, NA, NA, NA, "Німеччина Польща", NA, "Австралія Іспа…
$ countries_australia                <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_austria                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 0, 1, NA…
$ countries_azerbaijan               <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_albania                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_andorra                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_belgium                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_bulgaria                 <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_bosnia_herzegovina       <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_vatican                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_uk                       <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_armenia                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_greece                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 1, 0, NA…
$ countries_georgia                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 1, 0, NA…
$ countries_denmark                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 0, 0, NA…
$ countries_estonia                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 0, 0, NA…
$ countries_ireland                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_iceland                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_spain                    <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 0, 0, 0, 1, NA…
$ countries_italy                    <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_canada                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 0, 0, NA…
$ countries_cyprus                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_latvia                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 1, 0, 0, NA…
$ countries_lithuania                <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 1, 1, 0, NA…
$ countries_liechtenstein            <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_luxembourg               <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_malta                    <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_moldova                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_monaco                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_netherlands              <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 1, 0, NA…
$ countries_germany                  <dbl> NA, NA, NA, NA, 1, NA, 1, NA, 0, NA, NA, 1, 1, 1, 1, NA…
$ `countries_new zealand`            <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_norway                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_northern_macedonia       <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 1, NA…
$ countries_poland                   <dbl> NA, NA, NA, NA, 1, NA, 1, NA, 1, NA, NA, 1, 1, 1, 1, NA…
$ countries_portugal                 <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_romania                  <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 1, 0, 0, 1, NA…
$ `countries_san marino`             <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_serbia                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_slovakia                 <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 0, 1, NA…
$ countries_slovenia                 <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 1, 0, 0, NA…
$ countries_usa                      <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 1, 1, NA…
$ countries_turkey                   <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 1, 1, 1, 0, NA…
$ countries_hungary                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_ukraine                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 1, NA, NA, 1, 1, 0, 1, NA…
$ countries_finland                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_france                   <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 0, 0, 1, 0, NA…
$ countries_croatia                  <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ `countries_czech republic`         <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 0, 1, 0, 0, NA…
$ countries_montenegro               <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, NA…
$ countries_switzerland              <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 0, 1, 1, 0, NA…
$ countries_sweden                   <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_japan                    <dbl> NA, NA, NA, NA, 0, NA, 0, NA, 0, NA, NA, 1, 0, 0, 0, NA…
$ countries_other                    <dbl> NA, NA, NA, NA, 0, NA, 1, NA, 0, NA, NA, 0, 0, 1, 0, NA…
$ countries_other_text               <chr> NA, NA, NA, NA, NA, NA, "Корея, Македонія", NA, NA, NA,…
$ aid_received                       <chr> "Так", "Так", "Так", "Так", "Так", "Так", "Так", "Так",…
$ aid_received_volume                <dbl> 500, 120, 70, 21, 20, 9, 8000, 5, 5, 3, 2, 2000, 2000, …
$ aid_sent                           <chr> "Ні", "Так", "Так", "Ні", "Ні", "Ні", "Так", "Так", "Ні…
$ aid_sent_volume                    <dbl> NA, 13, 52141, NA, NA, NA, 4000, 3000, NA, 1, 46, 200, …
$ enterprises_relocated              <dbl> 1, 0, 2, 0, 0, 0, 45, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0,…
$ new_projects                       <chr> "На території громади ведуться активні бойові дії", "В …
$ `__version__`                      <chr> "vMaafktyMtoymXBWUfZySy", "vMaafktyMtoymXBWUfZySy", "vM…
$ `_id`                              <dbl> 311243135, 318603789, 311635451, 319024528, 308840419, …
$ `_uuid`                            <chr> "d8835a6a-9c93-497a-a7a8-215289ff14f7", "f85f271c-20ae-…
$ `_submission_time`                 <dttm> 2022-07-19 12:06:42, 2022-08-08 12:20:51, 2022-07-20 1…
$ `_validation_status`               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ `_notes`                           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ `_status`                          <chr> "submitted_via_web", "submitted_via_web", "submitted_vi…
$ `_submitted_by`                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ `_tags`                            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ index                              <dbl> 385, 465, 407, 518, 280, 337, 428, 434, 441, 66, 463, 1…
$ key                                <chr> "Харківська Богодухівський Золочівська селищна громада"…
$ hromada_code                       <chr> "UA63020050000045982", "UA12060130000012028", "UA710200…
$ type                               <chr> "селищна", "міська", "міська", "міська", "міська", "сел…
$ oblast_center                      <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0…
$ hromada_center_code                <chr> "UA63020050010064235", "UA12060130010067974", "UA710200…
$ hromada_center                     <chr> "Золочів", "Зеленодольськ", "Ватутіне", "Бобровиця", "Н…
$ lat_center                         <dbl> 50.27905, 47.56681, 49.01725, 50.74150, 48.63585, 48.63…
$ lon_center                         <dbl> 35.98243, 33.64746, 31.06185, 31.38609, 35.25955, 36.98…
$ travel_time                        <dbl> 52.8, 176.0, 99.6, 102.2, 38.5, 80.8, 0.0, 109.3, 147.1…
$ n_settlements                      <dbl> 73, 4, 5, 41, 1, 20, 3, 37, 13, 35, 35, 14, 1, 1, 6, 45…
$ area                               <dbl> 970.2, 310.4, 107.7, 1055.5, 36.0, 280.0, 180.4, 731.4,…
$ occipied_before_2022               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ total_population_2022              <dbl> 23696, 18324, 20469, 24234, 69855, 10510, 269381, 19416…
$ urban_popultaion_2022              <dbl> 7744, 12692, 15763, 10541, 69855, 5597, 264298, 4209, 9…
$ urban_pct                          <dbl> 0.3268062, 0.6926435, 0.7700914, 0.4349674, 1.0000000, …
$ budget_code                        <chr> "20508000000", "04506000000", "23560000000", "255210000…
$ budget_name                        <chr> "Бюджет Золочівської селищної територіальної громади", …
$ oblast_name_en                     <chr> "Kharkiv", "Driproptrovska", "Cherkassy", "Chernigiv", …
$ region_en                          <chr> "East", "East", "Center", "North", "East", "East", "Wes…
$ region_code_en                     <chr> "E", "E", "C", "N", "E", "E", "W", "N", "E", "E", "N", …
$ income_total_2021                  <dbl> 91134.88, 63839.99, 60305.90, 101959.96, 240865.70, 416…
$ income_transfert_2021              <dbl> 41595.552, 24157.236, 34154.556, 39750.354, 85275.444, …
$ income_military_2021               <dbl> 2731.8950, 451.5495, 132.5750, 311.0066, 5303.1764, 0.0…
$ income_pdfo_2021                   <dbl> 26410.601, 19092.887, 13932.936, 36255.927, 87258.471, …
$ income_unified_tax_2021            <dbl> 7747.989, 3024.817, 5046.173, 6620.351, 21385.819, 1990…
$ income_property_tax_2021           <dbl> 8333.909, 6208.526, 3876.447, 12618.963, 29759.227, 349…
$ income_excise_duty_2021            <dbl> 1919.5743, 1602.1408, 1524.7784, 1781.7577, 10376.3207,…
$ income_own_2021                    <dbl> 49539.33, 39682.76, 26151.34, 62209.61, 155590.26, 2427…
$ own_income_prop_2021               <dbl> 0.54, 0.62, 0.43, 0.61, 0.65, 0.58, 0.77, 0.52, 0.46, 0…
$ transfert_prop_2021                <dbl> 0.46, 0.38, 0.57, 0.39, 0.35, 0.42, 0.23, 0.48, 0.54, 0…
$ military_tax_prop_2021             <dbl> 0.03, 0.01, 0.00, 0.00, 0.02, 0.00, 0.03, 0.01, 0.01, 0…
$ pdfo_prop_2021                     <dbl> 0.29, 0.30, 0.23, 0.36, 0.36, 0.43, 0.45, 0.30, 0.25, 0…
$ unified_tax_prop_2021              <dbl> 0.09, 0.05, 0.08, 0.06, 0.09, 0.05, 0.10, 0.07, 0.05, 0…
$ property_tax_prop_2021             <dbl> 0.09, 0.10, 0.06, 0.12, 0.12, 0.08, 0.08, 0.08, 0.07, 0…
$ excise_duty_prop_2021              <dbl> 0.02, 0.03, 0.03, 0.02, 0.04, 0.00, 0.06, 0.02, 0.03, 0…
$ own_income_change                  <dbl> -0.36, -0.04, 0.02, -0.11, 0.17, -0.43, 0.23, -0.04, -0…
$ own_prop_change                    <dbl> -0.12, -0.04, 0.00, 0.00, 0.01, -0.04, 0.03, 0.02, 0.01…
$ total_income_change                <dbl> -0.19, 0.02, 0.01, -0.11, 0.15, -0.38, 0.17, -0.08, -0.…
$ income_own_2022                    <dbl> 31491.68, 37898.23, 26595.25, 55646.78, 181858.10, 1387…
$ income_total_2022                  <dbl> 74120.34, 64965.67, 61208.66, 91227.90, 276239.32, 2581…
$ income_transfert_2022              <dbl> 42628.664, 27067.437, 34613.410, 35581.119, 94381.221, …
$ dfrr_executed                      <dbl> 34430.801, NA, 3509.664, 755.363, 130602.958, 47443.994…
$ turnout_2020                       <dbl> 0.3798101, 0.3275138, 0.3100812, 0.3840267, 0.3097961, …
$ sex_head                           <chr> "male", "male", "male", "female", "male", "male", "male…
$ age_head                           <dbl> 58, 36, 59, 66, 48, 35, 50, 49, 56, 35, 40, 50, 40, 57,…
$ education_head                     <chr> "higher", "higher", "higher", "higher", "higher", "high…
$ incumbent                          <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, NA, 1, …
$ rda                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, …
$ not_from_here                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, …
$ party                              <chr> "Блок Світличної \"Разом\"", "Сила Людей", "Самовисуван…
$ enterpreuner                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ unemployed                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ priv_work                          <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0…
$ polit_work                         <dbl> 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1…
$ communal_work                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ ngo_work                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ party_national_winner              <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, NA, 0, …
$ no_party                           <dbl> 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, NA, 1, …
$ male                               <dbl> 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, NA, 0, …
$ high_educ                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, …
$ sum_osbb_2020                      <dbl> 31, 1, 4, NA, 61, NA, 331, NA, 2, 1, NA, 31, 711, 1327,…
$ edem_total                         <dbl> 0, 2, 1, 0, 4, 0, 2, 0, 2, 0, 0, 1, 1, 0, 0, 1, 2, 0, 1…
$ edem_petitions                     <dbl> 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1…
$ edem_consultations                 <dbl> 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ edem_participatory_budget          <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0…
$ edem_open_hromada                  <dbl> 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ youth_councils                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0…
$ youth_centers                      <dbl> 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 2, 4, 0, 7, 0, 0…
$ business_support_centers           <dbl> 0, 1, 0, 8, 0, 0, 12, 2, 0, 0, 0, 2, 17, 3, 5, 0, 5, 0,…
$ creation_date                      <date> 2017-08-20, 2015-08-16, 2020-08-16, 2017-08-20, 2020-0…
$ creation_year                      <dbl> 2017, 2015, 2020, 2017, 2020, 2020, 2020, 2020, 2016, 2…
$ time_before_24th                   <dbl> 1648.9167, 2383.9167, 556.9167, 1648.9167, 556.9167, 55…
$ voluntary                          <dbl> 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1…
$ war_zone_27_04_2022                <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1…
$ war_zone_20_06_2022                <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1…
$ war_zone_23_08_2022                <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1…
$ war_zone_10_10_2022                <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1…
$ passangers_2021                    <dbl> NA, NA, NA, 46130, NA, NA, NA, NA, 539, NA, 5204, NA, N…
$ train_station                      <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0…
$ budget_cut_sphere_count            <dbl> 1, 3, 0, 0, 2, 5, 0, 1, 0, 1, 0, 4, 0, 9, 0, 3, 5, 1, 5…
$ budget_cut_type_count              <dbl> 3, 1, 0, 0, 1, 2, 0, 2, 0, 1, 0, 3, 0, 5, 0, 2, 2, 2, 3…
$ ingo_count                         <dbl> 2, 5, 1, 3, 5, 2, 3, 1, 4, 0, 1, 4, 1, 5, 9, 0, 2, 1, 4…
$ countries_count                    <dbl> NA, NA, NA, NA, 2, NA, 10, NA, 2, NA, NA, 16, 15, 11, 9…
$ population_categories              <chr> "10-25k", "10-25k", "10-25k", "10-25k", "50-100k", "10-…
$ age_categories                     <chr> "45-59", "26-44", "45-59", "60+", "45-59", "26-44", "45…
$ income_categories                  <chr> "0-25 mln", "0-25 mln", "0-25 mln", "0-25 mln", "0-25 m…
$ turnout_categories                 <chr> "35-50%", "<35%", "<35%", "35-50%", "<35%", "35-50%", "…
$ travel_time_categories             <chr> "0-60 min", "120+ min", "60-120 min", "60-120 min", "0-…
$ rk_type                            <chr> "селищна", "міська", "міська", "міська", "міська", "сел…
$ rk_population_categories           <chr> "10-25k", "10-25k", "10-25k", "10-25k", "50-100k", "10-…
$ rk_war_zone_10_10_2022             <dbl> 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1…
$ rk_voluntary                       <dbl> 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1…
$ rk_income_categories               <chr> "0-25 mln", "0-25 mln", "0-25 mln", "0-25 mln", "0-25 m…
```

```{.r .fold-show}
ds_survey %>% pull(hromada_code) %>% unique()
```

```
  [1] "UA63020050000045982" "UA12060130000012028" "UA71020030000059581" "UA74040050000013413"
  [5] "UA12100070000011492" "UA14120150000071686" "UA73060610000066784" "UA18020130000096761"
  [9] "UA12140250000015858" "UA12080130000071360" "UA18040610000049084" "UA32080070000050759"
 [13] "UA48060150000071713" "UA51100270000073549" "UA12060170000091033" "UA74080190000091939"
 [17] "UA44120110000010163" "UA35040010000022631" "UA14020090000073988" "UA59040130000041676"
 [21] "UA74020030000069340" "UA21120130000025618" "UA26080070000092582" "UA63100050000079487"
 [25] "UA63140170000087785" "UA26100010000076570" "UA26060170000091466" "UA46100230000047305"
 [29] "UA32080130000056481" "UA23060130000058533" "UA56080170000055864" "UA46140050000054478"
 [33] "UA61020150000036935" "UA21060070000049340" "UA07040050000080830" "UA14160230000060952"
 [37] "UA59040110000026694" "UA61020090000020422" "UA63020090000096294" "UA23100170000031289"
 [41] "UA63140050000096783" "UA74080130000060606" "UA14020110000075529" "UA51060030000044366"
 [45] "UA48040070000056126" "UA61060250000062700" "UA14160070000097326" "UA12100110000043499"
 [49] "UA32080090000011038" "UA07040010000033507" "UA07020010000096841" "UA68020110000071202"
 [53] "UA59020090000085438" "UA12120070000057518" "UA59080090000092991" "UA07080250000053335"
 [57] "UA32140030000074165" "UA44120030000094047" "UA59060070000040784" "UA63040070000092606"
 [61] "UA59040010000075530" "UA21020110000091104" "UA18080010000090720" "UA74080170000055617"
 [65] "UA53080450000087891" "UA74040070000086652" "UA23060070000082704" "UA26080170000041182"
 [69] "UA18080150000022496" "UA12080050000062712" "UA32060010000086043" "UA68060330000029010"
 [73] "UA46060090000017373" "UA74040270000069132" "UA61040430000018305" "UA14160110000031920"
 [77] "UA26040210000094444" "UA46080150000050321" "UA46040070000032075" "UA53040090000048649"
 [81] "UA63100030000050119" "UA18040470000074046" "UA35020050000014205" "UA07020150000092012"
 [85] "UA74080150000033167" "UA05100150000050790" "UA12040150000015189" "UA12040110000091524"
 [89] "UA12020330000076598" "UA61060130000045755" "UA26040270000047749" "UA53060330000036503"
 [93] "UA14160170000023385" "UA18060150000038188" "UA46060170000014754" "UA71060170000092613"
 [97] "UA48040090000070960" "UA12060190000043514" "UA48020110000020969" "UA53060110000084138"
[101] "UA53020010000035494" "UA59100070000019079" "UA12060010000090992" "UA23060110000049870"
[105] "UA74100110000030364" "UA63120230000080742" "UA18060170000069581" "UA71020010000037786"
[109] "UA74080110000023749" "UA51080030000048246" "UA07060430000041345" "UA59100090000041284"
[113] "UA05080070000010150" "UA12020250000010438" "UA74100170000043593" "UA05020110000052014"
[117] "UA73040130000087963" "UA46060370000065608" "UA32060110000050416" "UA14120050000033910"
[121] "UA63100010000016136" "UA48060090000097375" "UA56060450000083945" "UA48040250000044166"
[125] "UA68040470000053519" "UA53080010000088479" "UA14020010000088496" "UA53060030000045288"
[129] "UA48020130000080862" "UA23060030000037089" "UA48020150000051082" "UA12140210000089021"
[133] "UA74100230000068041" "UA59040150000076482" "UA63060110000043554" "UA51100410000059549"
[137] "UA05060090000058521" "UA32020190000084133" "UA51100170000087238" "UA21080130000093757"
[141] "UA07020030000052032" "UA32140150000040954" "UA61040350000078247" "UA18040350000040746"
[145] "UA74100190000086457" "UA74080090000092409" "UA46100150000087495" "UA07060310000037369"
[149] "UA07080030000051738" "UA61040390000066619" "UA46120090000051307" "UA68020050000040518"
[153] "UA12060210000077302" "UA32120110000025177" "UA68040230000081901" "UA05080030000089348"
[157] "UA35040230000015609" "UA18040050000049937" "UA74100390000073425" "UA26120150000021671"
[161] "UA12100050000045992" "UA59020010000024157" "UA48020170000082529" "UA59020110000066430"
[165] "UA32120090000034281" "UA12060270000017282" "UA46060390000035967" "UA18060130000061781"
[169] "UA53080330000062155" "UA23100090000029171" "UA14160270000099007" "UA18020150000084693"
[173] "UA71040070000034468" "UA14160050000083011" "UA71080390000078978" "UA05020090000067118"
[177] "UA12020270000063450" "UA46100070000076013" "UA14160150000038750" "UA05040070000093188"
[181] "UA12140150000054570" "UA51100370000040590" "UA71080490000022110" "UA61040170000086062"
[185] "UA35060230000086264" "UA51120090000035170" "UA56020010000027956" "UA26020030000088465"
[189] "UA74040190000070909" "UA46080030000048662" "UA68060170000075276" "UA74080210000098421"
[193] "UA32020110000085875" "UA51060010000033212" "UA73060190000043347" "UA46060230000093092"
[197] "UA48060210000081533" "UA05120090000060589" "UA05020290000092626" "UA12020150000050274"
[201] "UA53060130000040567" "UA18080070000029133" "UA73020030000022011" "UA18020010000059904"
[205] "UA32080210000074136" "UA12020050000026654" "UA26040230000035526" "UA74020050000033944"
[209] "UA26040070000065724" "UA48040010000016403" "UA35020030000022644" "UA23040210000019462"
[213] "UA59080290000021284" "UA07020190000043658" "UA32060050000030591" "UA32020090000022264"
[217] "UA48060230000098265" "UA59020050000012539" "UA12120010000087180" "UA21120030000076462"
[221] "UA05020070000010139" "UA74080010000063011" "UA12140070000025486" "UA05120150000069474"
[225] "UA68040390000011141" "UA46060330000065074" "UA26060130000047466" "UA53080390000027656"
[229] "UA18060010000047628" "UA68040210000089917" "UA26080230000035462" "UA12120050000027007"
[233] "UA53060210000044776" "UA35020090000039429" "UA53040010000091190" "UA12100130000095653"
[237] "UA59060050000060884" "UA12100030000084605" "UA21120150000079563" "UA12120090000031622"
[241] "UA59060090000012687" "UA32120070000050717" "UA12120130000021632" "UA61060170000051011"
[245] "UA12140130000052990" "UA53080050000011754" "UA32140130000030648" "UA46060030000020652"
[249] "UA46060450000037997" "UA12040170000019083" "UA35060250000057436" "UA35040170000065192"
[253] "UA21020090000083647" "UA05120050000063063" "UA74020090000083820" "UA21100150000019483"
[257] "UA05020230000029909" "UA51080110000079212" "UA51020070000019684" "UA21020070000015036"
[261] "UA53040030000088898" "UA32080030000070006" "UA05080090000089922" "UA53020210000023587"
[265] "UA05040190000039450" "UA32020170000020698" "UA26040010000011271" "UA68060110000046608"
[269] "UA18040130000075048" "UA56020130000066130" "UA26060030000011364" "UA53080270000062586"
[273] "UA12020190000075117" "UA35080010000032386" "UA56040230000032182" "UA51100010000023950"
[277] "UA56040330000091100" "UA74040150000010608" "UA14120110000031497" "UA46120010000054878"
[281] "UA56020150000079107" "UA56060150000079268" "UA07020070000082628" "UA53060170000064030"
[285] "UA53060270000016329" "UA73060290000034305" "UA68040130000024471" "UA18040290000063354"
[289] "UA74040330000024949" "UA48060290000080246" "UA21100250000028639" "UA53040130000097690"
[293] "UA61060290000038953" "UA12060110000015305" "UA56040350000014945" "UA68060310000022172"
[297] "UA05120030000061384" "UA51120190000044451" "UA26120070000067596" "UA61020010000028670"
[301] "UA12100150000039396" "UA26060090000054411" "UA61040290000047781" "UA26020050000098694"
[305] "UA32020150000040878" "UA74040130000094076" "UA51040250000099866" "UA18040090000077574"
[309] "UA51100030000027628" "UA12140370000059352" "UA73060310000074904" "UA71080210000074132"
[313] "UA51120030000075494" "UA68040510000019366" "UA12140090000053443" "UA48060050000074873"
[317] "UA21020130000047547" "UA68020150000089666" "UA12020030000073022" "UA26060190000099075"
[321] "UA35040330000014430" "UA51100210000074534" "UA05100090000029847" "UA18040010000031446"
[325] "UA18080030000019784" "UA74100010000063674" "UA05080050000023603" "UA12060070000012840"
[329] "UA35060010000018331" "UA12040090000029346" "UA56060390000090915" "UA32040210000086086"
[333] "UA51020210000019597" "UA51060110000077844" "UA12140190000047361" "UA21120230000041982"
[337] "UA56020090000066575" "UA51100430000081326" "UA26120030000018265" "UA68040090000087566"
[341] "UA35060030000099117" "UA26080150000032640" "UA05040110000042036" "UA23100050000041545"
[345] "UA53060150000093603" "UA71040050000016509" "UA07020090000019505" "UA05020270000044358"
[349] "UA46060270000027170" "UA56060490000031684" "UA53060230000098362" "UA71020310000027379"
[353] "UA71060110000085867" "UA46040110000093394" "UA12020130000022909" "UA12140110000060935"
[357] "UA26040310000066064" "UA68060070000048275" "UA12040030000066040" "UA74020070000037186"
[361] "UA23040090000031823" "UA71020290000032540" "UA32040150000073245" "UA53080410000094938"
[365] "UA46080170000064605" "UA26060250000064599" "UA51100290000027993" "UA21080090000024861"
[369] "UA23060150000085288" "UA23060230000071243" "UA12120110000010894" "UA07080050000014573"
[373] "UA53060250000043118" "UA61060190000016283" "UA51020150000090298" "UA07040090000043197"
[377] "UA12060030000081794" "UA23060250000058722" "UA61040010000042862" "UA18040390000055900"
[381] "UA12140310000078816" "UA26100050000019570" "UA14120030000057882" "UA56020050000087443"
[385] "UA32100050000082904" "UA32140010000049369" "UA18040310000093693" "UA21020170000069101"
[389] "UA07040030000028082" "UA12060150000015198" "UA21100110000018765" "UA26040110000023512"
[393] "UA18040070000059677" "UA35040030000074104" "UA18040270000050167" "UA07060290000054842"
[397] "UA18020030000035625" "UA26040290000025886" "UA51140110000053825" "UA65100110000015344"
[401] "UA12040070000083117" "UA21120210000094884" "UA48040170000057262" "UA21020190000086409"
[405] "UA51080050000022808" "UA23040030000061166" "UA74100370000041962" "UA05060030000026125"
[409] "UA32020070000027895" "UA21040070000029344" "UA07060150000038564" "UA12060230000032833"
[413] "UA18040530000045696" "UA32040190000043384" "UA61040210000084270" "UA48040030000013036"
[417] "UA56040050000074861" "UA56040210000047101" "UA56040030000031906" "UA73040150000056182"
[421] "UA73040050000080963" "UA05080130000024964" "UA53080070000036333" "UA32040170000034173"
[425] "UA53060290000047345" "UA53080290000024637" "UA35060050000025668" "UA46040010000033885"
[429] "UA74080030000040714" "UA32040050000077950" "UA12140270000072109" "UA63120210000075842"
[433] "UA73060110000070017" "UA35060090000091976" "UA63060050000033240" "UA12060050000031212"
[437] "UA05040270000051651" "UA56060190000073111" "UA59020150000078955" "UA12080030000022466"
[441] "UA61020110000084342" "UA07080190000072811" "UA53060070000077527" "UA46140010000081849"
[445] "UA61060110000078824" "UA63120170000093573" "UA05080010000038400" "UA46080190000096121"
[449] "UA56060110000087064" "UA61040310000059301" "UA32040090000083028" "UA18040410000025491"
[453] "UA71060030000053648" "UA46060070000047720" "UA18020190000022756" "UA32080050000052660"
[457] "UA14160250000075646" "UA56060210000017052" "UA12140330000063909" "UA53060310000047988"
[461] "UA35080150000074379" "UA07020210000033081" "UA26080010000036750" "UA53020190000090557"
[465] "UA23020110000062653" "UA46120050000027080" "UA12140290000032227" "UA59100050000059594"
[469] "UA61060010000070653" "UA07080290000088287" "UA68060050000037587" "UA51020170000041393"
[473] "UA05020150000074651" "UA21020010000055068" "UA32060070000092009" "UA51140090000048720"
[477] "UA44140130000089782"
```

</details>

Next, we define useful sets of variable names to be used throughout the report

<details>

<summary>

click to see the groups

</summary>


```{.r .fold-show}
# create supporting objects for convenient reference of variable groups

# multiple choice questions
# mcq <-
#   meta_survey%>%
#   dplyr::select(type,name)%>%
#   dplyr::filter(str_detect(type, "select_multiple"))%>%
#   dplyr::select(name)%>%
#   pull() %>%  
#   print()

#vectors of mcq names
budget_cut_spheres <- 
  ds_survey %>% 
  select(starts_with("budget_cut_sphere"), -ends_with("text"), -budget_cut_sphere, -budget_cut_sphere_count) %>% 
  colnames() %>% 
  print()
```

```
 [1] "budget_cut_sphere.no_cuts"          "budget_cut_sphere.state_functions" 
 [3] "budget_cut_sphere.public_safety"    "budget_cut_sphere.economic"        
 [5] "budget_cut_sphere.environment"      "budget_cut_sphere.gkh"             
 [7] "budget_cut_sphere.healthcare"       "budget_cut_sphere.phys_development"
 [9] "budget_cut_sphere.education"        "budget_cut_sphere.social"          
[11] "budget_cut_sphere.other"           
```

```{.r .fold-show}
budget_cut_types <- 
  ds_survey %>% 
  select(starts_with("budget_cut_type"), , -ends_with("text"), -budget_cut_type, -budget_cut_type_count) %>% 
  colnames() %>% 
  print()
```

```
[1] "budget_cut_type.no_cuts"         "budget_cut_type.wages"          
[3] "budget_cut_type.utilities"       "budget_cut_type.social_payments"
[5] "budget_cut_type.subsidies"       "budget_cut_type.capital"        
[7] "budget_cut_type.other"          
```

```{.r .fold-show}
aid_ingo <- 
  ds_survey %>%
  select(starts_with('ingo_'), -ends_with('text'), -ingo, -ingo_count) %>% 
  colnames() %>% 
  print()
```

```
 [1] "ingo_ac"        "ingo_acf"       "ingo_acted"     "ingo_alima"     "ingo_cadena"   
 [6] "ingo_care"      "ingo_drc"       "ingo_dtl"       "ingo_echo"      "ingo_erc"      
[11] "ingo_fao"       "ingo_fca"       "ingo_fcdo"      "ingo_gc"        "ingo_ger3"     
[16] "ingo_giz"       "ingo_goal"      "ingo_hi"        "ingo_hia"       "ingo_hias"     
[21] "ingo_ifrc"      "ingo_imc"       "ingo_impact"    "ingo_intersos"  "ingo_iom"      
[26] "ingo_irc"       "ingo_loop"      "ingo_lwf"       "ingo_mdm"       "ingo_medair"   
[31] "ingo_msf"       "ingo_nrc"       "ingo_osce"      "ingo_pin"       "ingo_pui"      
[36] "ingo_r2p"       "ingo_react"     "ingo_reach"     "ingo_si"        "ingo_tri"      
[41] "ingo_tsf"       "ingo_unwomen"   "ingo_undp"      "ingo_unfpa"     "ingo_unhcr"    
[46] "ingo_unicef"    "ingo_wfp"       "ingo_who"       "ingo_wck"       "ingo_red_cross"
[51] "ingo_ulead"     "ingo_usaid"     "ingo_caritas"   "ingo_adra"      "ingo_other"    
```

```{.r .fold-show}
aid_countries <- 
  ds_survey %>% 
  select(starts_with('countries'), -ends_with('text'), -countries, -countries_count) %>% 
  colnames() %>% 
  print()
```

```
 [1] "countries_australia"          "countries_austria"            "countries_azerbaijan"        
 [4] "countries_albania"            "countries_andorra"            "countries_belgium"           
 [7] "countries_bulgaria"           "countries_bosnia_herzegovina" "countries_vatican"           
[10] "countries_uk"                 "countries_armenia"            "countries_greece"            
[13] "countries_georgia"            "countries_denmark"            "countries_estonia"           
[16] "countries_ireland"            "countries_iceland"            "countries_spain"             
[19] "countries_italy"              "countries_canada"             "countries_cyprus"            
[22] "countries_latvia"             "countries_lithuania"          "countries_liechtenstein"     
[25] "countries_luxembourg"         "countries_malta"              "countries_moldova"           
[28] "countries_monaco"             "countries_netherlands"        "countries_germany"           
[31] "countries_new zealand"        "countries_norway"             "countries_northern_macedonia"
[34] "countries_poland"             "countries_portugal"           "countries_romania"           
[37] "countries_san marino"         "countries_serbia"             "countries_slovakia"          
[40] "countries_slovenia"           "countries_usa"                "countries_turkey"            
[43] "countries_hungary"            "countries_ukraine"            "countries_finland"           
[46] "countries_france"             "countries_croatia"            "countries_czech republic"    
[49] "countries_montenegro"         "countries_switzerland"        "countries_sweden"            
[52] "countries_japan"              "countries_other"             
```

```{.r .fold-show}
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

<summary>

meta data

</summary>


```{.r .fold-show}
# meta_survey %>% glimpse()
# 
# meta_survey %>% 
#   filter(type %in% c("begin_group","end_group")) %>% 
#   select(1:5) %>% 
#   print_all()
# 
# 
# meta_survey %>% glimpse()
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
    income_own_per_capita       = income_own_2021         / total_population_2022
    ,income_total_per_capita     = income_total_2021       / total_population_2022
    ,income_tranfert_per_capita  = income_transfert_2021   / total_population_2022
    ,idp_share                   = idp_number / total_population_2022
    ,idp_number = ifelse(idp_number == 0, NA, idp_number)
    ,log_income_total_2021 = log(income_total_2021)
    ,log_income_own_2021= log(income_own_2021)
    ,income_total_per_capita = income_total_2021/total_population_2022
    ,income_own_per_capita= income_own_2021/total_population_2022
    ,income_own_pct = income_own_2021/income_total_2021
    ,military_action = factor(military_action, labels = c("No", "Yes, Feb-March", "Yes, currently"))
    ,occupation = factor(occupation, labels = c("No", "Yes, was occupied", "Yes, currently occupied"))
    ,admin_services_stopped = recode(admin_services_stopped, "Так" = 1, "Ні" = 0)
  ) 


d_volunteers <- ds0 %>% 
  mutate(
    volunteers_per_capita = volunteers_number/total_population_2022 * 100
  ) %>% 
  filter(hromada_name %ni% c("Боярська міська громада", "Городищенська сільська громада")) 


d_garbage <- ds0 %>% 
  mutate(
    garbage_interruptions = case_when(
      garbage_interruptions == "Важко сказати" ~ NA_integer_
      ,garbage_interruptions == "Так" ~ 1L
      ,garbage_interruptions == "Ні"~ 0L)
  )
```

To make our analysis more nimble we create four alternative versions of `ds1` with Invasion Preparedness questions

<details>

<summary>

show transformations

</summary>



</details>

<details>

<summary>

examine the versions

</summary>



</details>

# Variable List

The following variables are present in the processed data table of survey responses:


```{.r .fold-hide}
ds0 %>% explore::describe_all() %>%neat_DT()
```

```{=html}
<div id="htmlwidget-a57eba2a21c695bdb776" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a57eba2a21c695bdb776">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"477\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"100\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"477\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-187.96\" data-max=\"296521627\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"307315069.53\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"322941928\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280"],["oblast","raion","hromada_name","military_action","occupation","occupation_duration","income_change_march","income_change_april","income_change_may","biggest_drop_type","biggest_drop_type_other","budget_deficit_estimate","budget_cut","budget_cut_pct","budget_cut_sphere","budget_cut_sphere.no_cuts","budget_cut_sphere.state_functions","budget_cut_sphere.public_safety","budget_cut_sphere.economic","budget_cut_sphere.environment","budget_cut_sphere.gkh","budget_cut_sphere.healthcare","budget_cut_sphere.phys_development","budget_cut_sphere.education","budget_cut_sphere.social","budget_cut_sphere.other","budget_cut_sphere.other_text","budget_cut_type","budget_cut_type.no_cuts","budget_cut_type.wages","budget_cut_type.utilities","budget_cut_type.social_payments","budget_cut_type.subsidies","budget_cut_type.capital","budget_cut_type.other","budget_cut_type.other_text","financial_support","population_change","combatant_families","combatant_families_number","idp_pct","idp_number","admin_services_stopped","admin_services_resumed","admin_services_time","mining_area","garbage_interruptions","garbage_functional","doctors_change","volunteers_number","humanitarian_hubs","ingo","ingo_ac","ingo_acf","ingo_acted","ingo_alima","ingo_cadena","ingo_care","ingo_drc","ingo_dtl","ingo_echo","ingo_erc","ingo_fao","ingo_fca","ingo_fcdo","ingo_gc","ingo_ger3","ingo_giz","ingo_goal","ingo_hi","ingo_hia","ingo_hias","ingo_ifrc","ingo_imc","ingo_impact","ingo_intersos","ingo_iom","ingo_irc","ingo_loop","ingo_lwf","ingo_mdm","ingo_medair","ingo_msf","ingo_nrc","ingo_osce","ingo_pin","ingo_pui","ingo_r2p","ingo_react","ingo_reach","ingo_si","ingo_tri","ingo_tsf","ingo_unwomen","ingo_undp","ingo_unfpa","ingo_unhcr","ingo_unicef","ingo_wfp","ingo_who","ingo_wck","ingo_red_cross","ingo_ulead","ingo_usaid","ingo_caritas","ingo_adra","ingo_other","ingo_other_text","foreign_aid","countries","countries_australia","countries_austria","countries_azerbaijan","countries_albania","countries_andorra","countries_belgium","countries_bulgaria","countries_bosnia_herzegovina","countries_vatican","countries_uk","countries_armenia","countries_greece","countries_georgia","countries_denmark","countries_estonia","countries_ireland","countries_iceland","countries_spain","countries_italy","countries_canada","countries_cyprus","countries_latvia","countries_lithuania","countries_liechtenstein","countries_luxembourg","countries_malta","countries_moldova","countries_monaco","countries_netherlands","countries_germany","countries_new zealand","countries_norway","countries_northern_macedonia","countries_poland","countries_portugal","countries_romania","countries_san marino","countries_serbia","countries_slovakia","countries_slovenia","countries_usa","countries_turkey","countries_hungary","countries_ukraine","countries_finland","countries_france","countries_croatia","countries_czech republic","countries_montenegro","countries_switzerland","countries_sweden","countries_japan","countries_other","countries_other_text","aid_received","aid_received_volume","aid_sent","aid_sent_volume","enterprises_relocated","new_projects","__version__","_id","_uuid","_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","index","key","hromada_code","type","oblast_center","hromada_center_code","hromada_center","lat_center","lon_center","travel_time","n_settlements","area","occipied_before_2022","total_population_2022","urban_popultaion_2022","urban_pct","budget_code","budget_name","oblast_name_en","region_en","region_code_en","income_total_2021","income_transfert_2021","income_military_2021","income_pdfo_2021","income_unified_tax_2021","income_property_tax_2021","income_excise_duty_2021","income_own_2021","own_income_prop_2021","transfert_prop_2021","military_tax_prop_2021","pdfo_prop_2021","unified_tax_prop_2021","property_tax_prop_2021","excise_duty_prop_2021","own_income_change","own_prop_change","total_income_change","income_own_2022","income_total_2022","income_transfert_2022","dfrr_executed","turnout_2020","sex_head","age_head","education_head","incumbent","rda","not_from_here","party","enterpreuner","unemployed","priv_work","polit_work","communal_work","ngo_work","party_national_winner","no_party","male","high_educ","sum_osbb_2020","edem_total","edem_petitions","edem_consultations","edem_participatory_budget","edem_open_hromada","youth_councils","youth_centers","business_support_centers","creation_date","creation_year","time_before_24th","voluntary","war_zone_27_04_2022","war_zone_20_06_2022","war_zone_23_08_2022","war_zone_10_10_2022","passangers_2021","train_station","budget_cut_sphere_count","budget_cut_type_count","ingo_count","countries_count","population_categories","age_categories","income_categories","turnout_categories","travel_time_categories","rk_type","rk_population_categories","rk_war_zone_10_10_2022","rk_voluntary","rk_income_categories","income_own_per_capita","income_total_per_capita","income_tranfert_per_capita","idp_share","log_income_total_2021","log_income_own_2021","income_own_pct"],["chr","chr","chr","fct","fct","dbl","chr","chr","chr","chr","chr","chr","chr","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dbl","chr","dbl","dbl","chr","dbl","dbl","chr","chr","chr","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","chr","dbl","dbl","chr","chr","dbl","chr","dat","lgl","lgl","chr","lgl","lgl","dbl","chr","chr","chr","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dat","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl"],[0,0,0,0,0,433,0,0,0,0,423,0,0,231,0,0,0,0,0,0,0,0,0,0,0,0,442,7,7,7,7,7,7,7,7,444,0,0,0,135,0,9,0,287,287,0,0,380,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,215,0,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,227,470,0,114,0,174,0,0,0,0,0,0,477,477,0,477,477,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,155,5,2,3,2,2,2,2,2,0,0,0,0,0,0,2,2,2,2,207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,343,0,0,7,0,227,0,3,0,5,0,0,0,0,0,0,0,0,0,7,0,0,0],[0,0,0,0,0,90.8,0,0,0,0,88.7,0,0,48.4,0,0,0,0,0,0,0,0,0,0,0,0,92.7,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,93.1,0,0,0,28.3,0,1.9,0,60.2,60.2,0,0,79.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,45.1,0,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,47.6,98.5,0,23.9,0,36.5,0,0,0,0,0,0,100,100,0,100,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32.5,1,0.4,0.6,0.4,0.4,0.4,0.4,0.4,0,0,0,0,0,0,0.4,0.4,0.4,0.4,43.4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,71.9,0,0,1.5,0,47.6,0,0.6,0,1,0,0,0,0,0,0,0,0,0,1.5,0,0,0],[24,106,471,3,3,35,7,7,7,7,49,5,2,39,108,2,2,2,2,2,2,2,2,2,2,2,31,33,3,3,3,3,3,3,3,34,4,6,2,48,5,249,2,4,64,44,3,3,4,58,17,156,1,1,2,1,1,2,2,1,2,2,2,1,1,1,1,2,1,1,2,1,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,211,2,160,3,3,3,2,2,3,3,2,3,3,2,3,3,3,3,3,2,3,3,3,2,3,3,2,3,3,2,2,3,3,2,3,3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3,8,2,95,2,82,17,257,1,477,477,476,1,1,1,1,1,477,477,477,3,2,477,464,477,477,397,71,463,1,472,308,299,477,471,24,5,5,477,477,312,477,477,477,477,477,77,77,18,54,16,40,17,142,74,101,477,477,477,321,472,3,46,3,3,3,3,38,2,2,2,2,2,2,3,3,3,3,80,5,2,2,2,2,3,8,15,27,6,27,2,2,2,2,2,134,2,10,7,12,16,5,4,1,4,3,3,5,2,2,1,477,477,477,470,477,477,477],[null,null,null,null,null,-163,null,null,null,null,null,null,null,0,null,0,0,0,0,0,0,0,0,0,0,0,null,null,0,0,0,0,0,0,0,null,null,null,null,-35,null,3,0,null,0,0,null,null,null,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,null,1,null,0,0,null,null,296521627,null,null,null,null,null,null,null,1,null,null,null,0,null,null,45.35,22.27,0,1,2.9,0,1814,0,0,null,null,null,null,null,8384.53,3809.38,-187.96,1350.59,216.38,224.03,4.34,2848.11,0.14,0.07,0,0.09,0.01,0,0,-0.85,-0.46,-0.68,1935.08,7385.63,4179.65,0,0.22,null,26,null,0,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,2015,556.92,0,0,0,0,0,127,0,0,0,0,1,null,null,null,null,null,null,null,0,0,null,0.53,2.56,0.62,0,9.03,7.95,0.14],[null,null,null,null,null,48.66,null,null,null,null,null,null,null,17.92,null,0.36,0.23,0.03,0.17,0.09,0.29,0.06,0.18,0.44,0.11,0.09,null,null,0.33,0.49,0.11,0.07,0.04,0.5,0.07,null,null,null,null,16.04,null,3183.83,0.4,null,42.81,165495.15,null,null,null,32.8,2.18,null,0,0,0.03,0,0,0.01,0.02,0,0,0,0.1,0,0,0,0,0.12,0,0,0,0,0.01,0,0,0,0.11,0.01,0,0,0,0.01,0.09,0.03,0.01,0.04,0,0.07,0,0,0,0,0,0.02,0.12,0.02,0.09,0.3,0.16,0.01,0.1,0.1,0.05,0.03,0.06,0.02,0.15,null,null,null,0.02,0.05,0,0,0,0.03,0.03,0,0.01,0.11,0,0.03,0.04,0.03,0.03,0.02,0,0.12,0.22,0.06,0,0.06,0.11,0,0,0,0,0,0.06,0.38,0,0.02,0,0.75,0.04,0.14,0,0,0.06,0.01,0.15,0.04,0.06,0.14,0.01,0.13,0.01,0.14,0,0.03,0.02,0.01,0.02,null,null,112.28,null,976.04,0.92,null,null,307315069.53,null,null,null,null,null,null,null,267.44,null,null,null,0.02,null,null,49.25,30.09,88.18,20.58,381.55,0,26835.47,18247.34,0.35,null,null,null,null,null,121623.72,40597.15,3251.92,47205.45,8953.7,12850.13,5067.82,81026.57,0.54,0.46,0.02,0.29,0.06,0.1,0.03,0.09,0.02,0.02,90602.9,128171.09,37568.18,42167.14,0.42,null,51.2,null,0.54,0.04,0.08,null,0.04,0.04,0.08,0.81,0.03,0.01,0.16,0.43,0.79,0.95,43.17,0.49,0.15,0.11,0.14,0.09,0.1,0.24,1.1,null,2018.06,1264.63,0.61,0.08,0.13,0.14,0.14,36706.04,0.28,1.68,1.28,1.91,3.19,null,null,null,null,null,null,null,0.14,0.61,null,2.64,4.56,1.92,0.13,10.98,10.3,0.54],[null,null,null,null,null,159,null,null,null,null,null,null,null,100,null,1,1,1,1,1,1,1,1,1,1,1,null,null,1,1,1,1,1,1,1,null,null,null,null,2000,null,157448,1,null,158,78354000,null,null,null,2000,30,null,0,0,1,0,0,1,1,0,1,1,1,0,0,0,0,1,0,0,1,0,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,null,null,null,1,1,1,0,0,1,1,0,1,1,0,1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,0,0,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,null,null,8000,null,200000,100,null,null,322941928,null,null,null,null,null,null,null,543,null,null,null,1,null,null,52.26,38.95,288,97,1779.6,0,1010537,1010537,1,null,null,null,null,null,4909714.68,937642.55,158495.77,2352286.75,569569.48,629526.24,271457.01,3972072.13,0.93,0.86,0.75,0.81,0.16,0.58,0.27,8.19,0.42,7.06,4108717.28,5038251.69,929534.41,711259.58,0.85,null,74,null,1,1,1,null,1,1,1,1,1,1,1,1,1,1,1327,4,1,1,1,1,2,7,254,null,2020,2383.92,1,1,1,1,1,1612102,1,9,5,11,16,null,null,null,null,null,null,null,1,1,null,25.92,27.88,4.29,1.85,15.41,15.19,0.93]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>type<\/th>\n      <th>na<\/th>\n      <th>na_pct<\/th>\n      <th>unique<\/th>\n      <th>min<\/th>\n      <th>mean<\/th>\n      <th>max<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"autoWidth":false,"columnDefs":[{"className":"dt-right","targets":[3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```

# 0. Introduction

<mark>0.1</mark> What is the goal of this report?

> This report overviews the responses to the survey conducted by \_\_\_\_\_ in Ukraine during 2022

# 1. General Information

<mark>1.1</mark> **How many hromadas contributed responses to so far?**

> As of 2023-01-26, 477 hromadas contributed valid response to the survey

<mark>1.2</mark> **What oblasts are represented in this sample\>?**


```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en, oblast_name_en) %>% 
  summarize(
    hormada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  left_join(
    ds_general %>% 
      group_by(region_en=region_en,  oblast_name_en=oblast_name_en) %>% 
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
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> 18.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Khmelnitsk </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 26.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Kirovograd </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> 32.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Poltava </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> 48.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> Vinnytsia </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> 39.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Donetks </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:left;"> 32.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Driproptrovska </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:left;"> 62.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Kharkiv </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> 23.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Luhansk </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> 11.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> Zaporizka </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:left;"> 20.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Chernigiv </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> 47.4% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Kyiv-oblast </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:left;"> 43.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Sumska </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:left;"> 33.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> Zhytomir </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> 40.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Kherson </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> 2.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Mykolayiv </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> 30.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> Odesa </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:left;"> 27.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Cherniveska </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> 17.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Ivano-Frankivsk </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> 41.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Lviv </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:left;"> 34.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Rivenska </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:left;"> 29.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Ternopilska </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> 34.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Vonyn </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> 37.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> Zakarpatska </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:left;"> 29.7% </td>
  </tr>
</tbody>
</table>

<mark>1.3</mark> **What type of hromadas are represented in the sample?**


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("type"))+
  labs(
    title = "What types of hromadas repsonded to the survey?"
    ,subtitle = "Data were collected during June-July of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/unnamed-chunk-4-1.png)<!-- -->

<mark>1.4</mark> **What hromadas experienced military occupation or military actions?**


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("military_action"))+
  labs(
    title = "How many respondent hromadas have experienced military action at the time of the interview?"
    ,subtitle = "Data were collected during June-July of 2022"
  ) +
  ylab(label = NULL)
```

![](figure-png-iso/unnamed-chunk-5-1.png)<!-- -->

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("occupation"))+
  labs(
    title = "How many respondent hromadas have experienced occupation at the time of the interview?"
    ,subtitle = "Data were collected during June-July of 2022"
    ,y=NULL
  )
```

![](figure-png-iso/unnamed-chunk-5-2.png)<!-- -->


<mark>1.5</mark> **Comparison between the survey sample and total population of hromadas based on the list of predictors**


```{.r .fold-hide}
library(tableone)

predictors_all <- 
  ds_general %>% 
  select(-c("hromada_code", "hromada_name","raion_code", "raion_name","oblast_code",
            "oblast_name","hromada_full_name","hromada_center_code","hromada_center",           
            "lat_center","lon_center", "budget_name", "creation_date", "budget_code", "party", "region_code_en")) %>% 
  colnames()


ds_survey_codes <- 
  ds0 %>% 
  filter(!is.na(hromada_code)) %>% 
  distinct(hromada_code) %>% 
  pull(hromada_code)

ds_surveyed <- 
  ds_general %>% 
  filter(hromada_code %in% ds_survey_codes) %>% 
  mutate(survey_participant = "surveyed" )

ds_comparison <- 
  ds_general  %>% 
  mutate(survey_participant = "all") 

#add to all gromadas (including surveyed) surveyed hromadas one more time to make a comparison
ds_comparison <- rbind(ds_surveyed, ds_comparison) %>% 
  filter(oblast_name_en %ni% c("Luhansk", "Kherson"))

table_pre <- CreateTableOne(vars=predictors_all
                            ,factorVars = c("sex_head", "education_head", "incumbent", "rda")
                            , strata = "survey_participant"
                            , data=ds_comparison)

p <-print(table_pre, smd=T, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, format = "html")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> all </th>
   <th style="text-align:left;"> surveyed </th>
   <th style="text-align:left;"> p </th>
   <th style="text-align:left;"> test </th>
   <th style="text-align:left;"> SMD </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> n </td>
   <td style="text-align:left;"> 1394 </td>
   <td style="text-align:left;"> 473 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> type (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.084 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> міська </td>
   <td style="text-align:left;"> 391 (28.0) </td>
   <td style="text-align:left;"> 147 (31.1) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> селищна </td>
   <td style="text-align:left;"> 406 (29.1) </td>
   <td style="text-align:left;"> 151 (31.9) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> сільська </td>
   <td style="text-align:left;"> 597 (42.8) </td>
   <td style="text-align:left;"> 175 (37.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> oblast_center (mean (SD)) </td>
   <td style="text-align:left;"> 0.02 (0.12) </td>
   <td style="text-align:left;"> 0.01 (0.12) </td>
   <td style="text-align:left;"> 0.967 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> travel_time (mean (SD)) </td>
   <td style="text-align:left;"> 90.71 (54.82) </td>
   <td style="text-align:left;"> 88.52 (50.49) </td>
   <td style="text-align:left;"> 0.444 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.042 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> n_settlements (mean (SD)) </td>
   <td style="text-align:left;"> 19.51 (15.95) </td>
   <td style="text-align:left;"> 20.61 (17.12) </td>
   <td style="text-align:left;"> 0.203 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> area (mean (SD)) </td>
   <td style="text-align:left;"> 372.72 (284.03) </td>
   <td style="text-align:left;"> 379.37 (289.27) </td>
   <td style="text-align:left;"> 0.662 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.023 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> occipied_before_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 0.02 (0.15) </td>
   <td style="text-align:left;"> 0.00 (0.00) </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_population_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 26235.84 (77018.29) </td>
   <td style="text-align:left;"> 26678.23 (71989.92) </td>
   <td style="text-align:left;"> 0.913 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> urban_popultaion_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 17692.90 (77138.10) </td>
   <td style="text-align:left;"> 18084.45 (72423.69) </td>
   <td style="text-align:left;"> 0.923 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> urban_pct (mean (SD)) </td>
   <td style="text-align:left;"> 0.31 (0.33) </td>
   <td style="text-align:left;"> 0.34 (0.32) </td>
   <td style="text-align:left;"> 0.094 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> oblast_name_en (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.031 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.317 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cherkassy </td>
   <td style="text-align:left;"> 66 (4.8) </td>
   <td style="text-align:left;"> 12 (2.5) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chernigiv </td>
   <td style="text-align:left;"> 57 (4.2) </td>
   <td style="text-align:left;"> 27 (5.7) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cherniveska </td>
   <td style="text-align:left;"> 52 (3.8) </td>
   <td style="text-align:left;"> 9 (1.9) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Donetks </td>
   <td style="text-align:left;"> 46 (3.4) </td>
   <td style="text-align:left;"> 15 (3.2) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Driproptrovska </td>
   <td style="text-align:left;"> 86 (6.3) </td>
   <td style="text-align:left;"> 54 (11.4) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ivano-Frankivsk </td>
   <td style="text-align:left;"> 62 (4.5) </td>
   <td style="text-align:left;"> 26 (5.5) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kharkiv </td>
   <td style="text-align:left;"> 56 (4.1) </td>
   <td style="text-align:left;"> 13 (2.7) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Khmelnitsk </td>
   <td style="text-align:left;"> 60 (4.4) </td>
   <td style="text-align:left;"> 16 (3.4) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kirovograd </td>
   <td style="text-align:left;"> 49 (3.6) </td>
   <td style="text-align:left;"> 16 (3.4) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kyiv-oblast </td>
   <td style="text-align:left;"> 69 (5.1) </td>
   <td style="text-align:left;"> 30 (6.3) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lviv </td>
   <td style="text-align:left;"> 73 (5.4) </td>
   <td style="text-align:left;"> 25 (5.3) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mykolayiv </td>
   <td style="text-align:left;"> 52 (3.8) </td>
   <td style="text-align:left;"> 16 (3.4) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Odesa </td>
   <td style="text-align:left;"> 91 (6.7) </td>
   <td style="text-align:left;"> 25 (5.3) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poltava </td>
   <td style="text-align:left;"> 60 (4.4) </td>
   <td style="text-align:left;"> 29 (6.1) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rivenska </td>
   <td style="text-align:left;"> 64 (4.7) </td>
   <td style="text-align:left;"> 19 (4.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sumska </td>
   <td style="text-align:left;"> 51 (3.7) </td>
   <td style="text-align:left;"> 17 (3.6) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ternopilska </td>
   <td style="text-align:left;"> 55 (4.0) </td>
   <td style="text-align:left;"> 19 (4.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vinnytsia </td>
   <td style="text-align:left;"> 63 (4.6) </td>
   <td style="text-align:left;"> 25 (5.3) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vonyn </td>
   <td style="text-align:left;"> 54 (4.0) </td>
   <td style="text-align:left;"> 20 (4.2) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Zakarpatska </td>
   <td style="text-align:left;"> 64 (4.7) </td>
   <td style="text-align:left;"> 19 (4.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Zaporizka </td>
   <td style="text-align:left;"> 67 (4.9) </td>
   <td style="text-align:left;"> 14 (3.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Zhytomir </td>
   <td style="text-align:left;"> 66 (4.8) </td>
   <td style="text-align:left;"> 27 (5.7) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> region_en (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.320 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Center </td>
   <td style="text-align:left;"> 298 (21.9) </td>
   <td style="text-align:left;"> 98 (20.7) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> East </td>
   <td style="text-align:left;"> 255 (18.7) </td>
   <td style="text-align:left;"> 96 (20.3) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North </td>
   <td style="text-align:left;"> 243 (17.8) </td>
   <td style="text-align:left;"> 101 (21.4) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> 143 (10.5) </td>
   <td style="text-align:left;"> 41 (8.7) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> 424 (31.1) </td>
   <td style="text-align:left;"> 137 (29.0) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_total_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 109233.12 (378056.95) </td>
   <td style="text-align:left;"> 120805.33 (354288.08) </td>
   <td style="text-align:left;"> 0.560 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.032 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_transfert_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 37751.26 (73199.20) </td>
   <td style="text-align:left;"> 40407.06 (71883.69) </td>
   <td style="text-align:left;"> 0.495 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.037 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_military_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 2601.10 (12187.84) </td>
   <td style="text-align:left;"> 3134.21 (13199.45) </td>
   <td style="text-align:left;"> 0.423 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.042 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_pdfo_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 41542.77 (180721.53) </td>
   <td style="text-align:left;"> 46778.57 (170985.49) </td>
   <td style="text-align:left;"> 0.582 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_unified_tax_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 8452.84 (44931.00) </td>
   <td style="text-align:left;"> 8931.83 (35535.86) </td>
   <td style="text-align:left;"> 0.834 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_property_tax_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 10852.72 (48120.17) </td>
   <td style="text-align:left;"> 12746.09 (45738.52) </td>
   <td style="text-align:left;"> 0.455 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_excise_duty_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 4522.56 (17889.30) </td>
   <td style="text-align:left;"> 5032.44 (18134.86) </td>
   <td style="text-align:left;"> 0.595 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.028 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_own_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 71481.86 (307348.03) </td>
   <td style="text-align:left;"> 80398.27 (284806.45) </td>
   <td style="text-align:left;"> 0.580 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> own_income_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.51 (0.16) </td>
   <td style="text-align:left;"> 0.54 (0.17) </td>
   <td style="text-align:left;"> 0.008 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.140 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> transfert_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.49 (0.16) </td>
   <td style="text-align:left;"> 0.46 (0.17) </td>
   <td style="text-align:left;"> 0.008 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.140 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> military_tax_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.01 (0.04) </td>
   <td style="text-align:left;"> 0.02 (0.05) </td>
   <td style="text-align:left;"> 0.391 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.045 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pdfo_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.28 (0.12) </td>
   <td style="text-align:left;"> 0.29 (0.12) </td>
   <td style="text-align:left;"> 0.007 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.142 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unified_tax_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.06 (0.02) </td>
   <td style="text-align:left;"> 0.06 (0.02) </td>
   <td style="text-align:left;"> 0.916 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> property_tax_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.10 (0.07) </td>
   <td style="text-align:left;"> 0.10 (0.07) </td>
   <td style="text-align:left;"> 0.665 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.023 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> excise_duty_prop_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 0.03 (0.04) </td>
   <td style="text-align:left;"> 0.03 (0.03) </td>
   <td style="text-align:left;"> 0.316 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> own_income_change (mean (SD)) </td>
   <td style="text-align:left;"> 0.04 (0.60) </td>
   <td style="text-align:left;"> 0.10 (0.69) </td>
   <td style="text-align:left;"> 0.091 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.087 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> own_prop_change (mean (SD)) </td>
   <td style="text-align:left;"> 0.00 (0.10) </td>
   <td style="text-align:left;"> 0.02 (0.08) </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.186 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_income_change (mean (SD)) </td>
   <td style="text-align:left;"> -0.01 (0.37) </td>
   <td style="text-align:left;"> 0.02 (0.46) </td>
   <td style="text-align:left;"> 0.163 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.070 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_own_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 76089.40 (316874.28) </td>
   <td style="text-align:left;"> 90271.62 (315272.63) </td>
   <td style="text-align:left;"> 0.401 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.045 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_total_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 111248.31 (385051.98) </td>
   <td style="text-align:left;"> 127699.84 (381909.79) </td>
   <td style="text-align:left;"> 0.422 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.043 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income_transfert_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 35158.91 (70702.96) </td>
   <td style="text-align:left;"> 37428.21 (69247.02) </td>
   <td style="text-align:left;"> 0.546 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.032 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dfrr_executed (mean (SD)) </td>
   <td style="text-align:left;"> 39130.23 (104429.18) </td>
   <td style="text-align:left;"> 40030.87 (90618.66) </td>
   <td style="text-align:left;"> 0.892 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> turnout_2020 (mean (SD)) </td>
   <td style="text-align:left;"> 0.43 (0.09) </td>
   <td style="text-align:left;"> 0.42 (0.09) </td>
   <td style="text-align:left;"> 0.715 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.020 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex_head = male (%) </td>
   <td style="text-align:left;"> 1131 (83.6) </td>
   <td style="text-align:left;"> 374 (79.2) </td>
   <td style="text-align:left;"> 0.038 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.112 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age_head (mean (SD)) </td>
   <td style="text-align:left;"> 50.99 (9.47) </td>
   <td style="text-align:left;"> 51.21 (9.31) </td>
   <td style="text-align:left;"> 0.663 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.023 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> education_head = non-higher (%) </td>
   <td style="text-align:left;"> 97 (7.2) </td>
   <td style="text-align:left;"> 22 (4.7) </td>
   <td style="text-align:left;"> 0.073 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.106 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> incumbent = 1 (%) </td>
   <td style="text-align:left;"> 754 (55.7) </td>
   <td style="text-align:left;"> 256 (54.2) </td>
   <td style="text-align:left;"> 0.612 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rda = 1 (%) </td>
   <td style="text-align:left;"> 54 (4.0) </td>
   <td style="text-align:left;"> 17 (3.6) </td>
   <td style="text-align:left;"> 0.811 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.020 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> not_from_here (mean (SD)) </td>
   <td style="text-align:left;"> 0.08 (0.28) </td>
   <td style="text-align:left;"> 0.08 (0.27) </td>
   <td style="text-align:left;"> 0.764 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.016 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enterpreuner (mean (SD)) </td>
   <td style="text-align:left;"> 0.03 (0.17) </td>
   <td style="text-align:left;"> 0.04 (0.19) </td>
   <td style="text-align:left;"> 0.429 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.041 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unemployed (mean (SD)) </td>
   <td style="text-align:left;"> 0.04 (0.20) </td>
   <td style="text-align:left;"> 0.04 (0.19) </td>
   <td style="text-align:left;"> 0.502 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.036 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> priv_work (mean (SD)) </td>
   <td style="text-align:left;"> 0.09 (0.28) </td>
   <td style="text-align:left;"> 0.08 (0.27) </td>
   <td style="text-align:left;"> 0.441 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.042 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polit_work (mean (SD)) </td>
   <td style="text-align:left;"> 0.75 (0.43) </td>
   <td style="text-align:left;"> 0.81 (0.39) </td>
   <td style="text-align:left;"> 0.017 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.130 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> communal_work (mean (SD)) </td>
   <td style="text-align:left;"> 0.04 (0.19) </td>
   <td style="text-align:left;"> 0.03 (0.16) </td>
   <td style="text-align:left;"> 0.174 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.076 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ngo_work (mean (SD)) </td>
   <td style="text-align:left;"> 0.01 (0.09) </td>
   <td style="text-align:left;"> 0.01 (0.09) </td>
   <td style="text-align:left;"> 0.905 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> party_national_winner (mean (SD)) </td>
   <td style="text-align:left;"> 0.16 (0.37) </td>
   <td style="text-align:left;"> 0.16 (0.37) </td>
   <td style="text-align:left;"> 0.829 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.011 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> no_party (mean (SD)) </td>
   <td style="text-align:left;"> 0.47 (0.50) </td>
   <td style="text-align:left;"> 0.42 (0.49) </td>
   <td style="text-align:left;"> 0.116 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.084 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male (mean (SD)) </td>
   <td style="text-align:left;"> 0.84 (0.37) </td>
   <td style="text-align:left;"> 0.79 (0.41) </td>
   <td style="text-align:left;"> 0.032 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.112 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> high_educ (mean (SD)) </td>
   <td style="text-align:left;"> 0.93 (0.26) </td>
   <td style="text-align:left;"> 0.95 (0.21) </td>
   <td style="text-align:left;"> 0.057 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.106 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sum_osbb_2020 (mean (SD)) </td>
   <td style="text-align:left;"> 38.70 (123.93) </td>
   <td style="text-align:left;"> 43.21 (133.60) </td>
   <td style="text-align:left;"> 0.616 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.035 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edem_total (mean (SD)) </td>
   <td style="text-align:left;"> 0.41 (0.89) </td>
   <td style="text-align:left;"> 0.49 (0.97) </td>
   <td style="text-align:left;"> 0.126 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.080 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edem_petitions (mean (SD)) </td>
   <td style="text-align:left;"> 0.14 (0.35) </td>
   <td style="text-align:left;"> 0.15 (0.36) </td>
   <td style="text-align:left;"> 0.560 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.031 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edem_consultations (mean (SD)) </td>
   <td style="text-align:left;"> 0.08 (0.27) </td>
   <td style="text-align:left;"> 0.11 (0.31) </td>
   <td style="text-align:left;"> 0.085 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.089 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edem_participatory_budget (mean (SD)) </td>
   <td style="text-align:left;"> 0.13 (0.33) </td>
   <td style="text-align:left;"> 0.14 (0.34) </td>
   <td style="text-align:left;"> 0.505 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.035 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edem_open_hromada (mean (SD)) </td>
   <td style="text-align:left;"> 0.06 (0.24) </td>
   <td style="text-align:left;"> 0.09 (0.28) </td>
   <td style="text-align:left;"> 0.058 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.097 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> youth_councils (mean (SD)) </td>
   <td style="text-align:left;"> 0.08 (0.29) </td>
   <td style="text-align:left;"> 0.10 (0.31) </td>
   <td style="text-align:left;"> 0.370 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.047 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> youth_centers (mean (SD)) </td>
   <td style="text-align:left;"> 0.21 (0.69) </td>
   <td style="text-align:left;"> 0.23 (0.64) </td>
   <td style="text-align:left;"> 0.587 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.029 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> business_support_centers (mean (SD)) </td>
   <td style="text-align:left;"> 0.71 (7.12) </td>
   <td style="text-align:left;"> 1.10 (11.86) </td>
   <td style="text-align:left;"> 0.394 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creation_year (mean (SD)) </td>
   <td style="text-align:left;"> 2018.19 (1.81) </td>
   <td style="text-align:left;"> 2018.06 (1.84) </td>
   <td style="text-align:left;"> 0.158 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.075 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> time_before_24th (mean (SD)) </td>
   <td style="text-align:left;"> 1220.13 (661.57) </td>
   <td style="text-align:left;"> 1267.64 (669.80) </td>
   <td style="text-align:left;"> 0.179 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.071 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> voluntary (mean (SD)) </td>
   <td style="text-align:left;"> 0.59 (0.49) </td>
   <td style="text-align:left;"> 0.62 (0.49) </td>
   <td style="text-align:left;"> 0.302 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.055 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> war_zone_27_04_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 0.13 (0.34) </td>
   <td style="text-align:left;"> 0.07 (0.26) </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.189 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> war_zone_20_06_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 0.16 (0.37) </td>
   <td style="text-align:left;"> 0.12 (0.33) </td>
   <td style="text-align:left;"> 0.027 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> war_zone_23_08_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 0.18 (0.38) </td>
   <td style="text-align:left;"> 0.13 (0.34) </td>
   <td style="text-align:left;"> 0.013 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.137 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> war_zone_10_10_2022 (mean (SD)) </td>
   <td style="text-align:left;"> 0.18 (0.38) </td>
   <td style="text-align:left;"> 0.13 (0.34) </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.140 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> passangers_2021 (mean (SD)) </td>
   <td style="text-align:left;"> 72563.30 (424587.76) </td>
   <td style="text-align:left;"> 36706.04 (146308.97) </td>
   <td style="text-align:left;"> 0.341 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.113 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> train_station (mean (SD)) </td>
   <td style="text-align:left;"> 0.23 (0.42) </td>
   <td style="text-align:left;"> 0.28 (0.45) </td>
   <td style="text-align:left;"> 0.014 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.128 </td>
  </tr>
</tbody>
</table>



# 2. Budget

<mark>2.1</mark> What questions were asked about preparations hromadas made?


```{.r .fold-hide}
ds0 %>%

  ggplot(aes(x = budget_cut_sphere_count)) +

  geom_histogram()
```

![](figure-png-iso/unnamed-chunk-7-1.png)<!-- -->

```{.r .fold-hide}
hist(ds0$budget_cut_sphere_count)
```

![](figure-png-iso/unnamed-chunk-7-2.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("population_change"))+

  labs(

    title = "How many respondent hromadas have experienced military action at the time of the interview?"

    ,subtitle = "Data were collected during October-November of 2022"

    ,y = NULL

  )
```

![](figure-png-iso/preparation-summary-1-1.png)<!-- -->

# 3. Population changes/IDPs

<mark>3.1</mark> **How hromadas population has changed since February 24th**


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("population_change"))+
  labs(
    title = "Changes in popultion since February 24th"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/population-change-1-1.png)<!-- -->

<mark>3.2</mark> **Share of IDPs among total population by region**


```{.r .fold-hide}
ds0 %>% 
  count(region_en, idp_pct) %>% 
  group_by(region_en) %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(idp_pct = factor(idp_pct, levels = c(
    "Не прибували, кількість населення в громаді зменшилася"
    ,"До 5% від населення громади"
    ,"5-10% від населення громади"
    ,"11-20% від населення громади"
    ,"Понад 20% від населення громади"
  ))) %>% 
  ggplot(aes(x = region_en, y = pct, fill = idp_pct)) +
  geom_col() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "% of IDPs",
       fill = NULL) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_brewer(palette="PuBu")
```

![](figure-png-iso/population-change-2-1.png)<!-- -->

<mark>3.4</mark> **Relation between number of passengers in 2021 and number of IDps**

#TO_DO: check passengers numbers \# `{r population-change-passangers, fig.height=5, fig.width=12,class.source="fold-hide"} # ds0 %>%  #   filter(passangers_2021 < 500000) %>%  #   ggplot(aes(x = passangers_2021, y = idp_number)) + #   geom_point() + #   geom_smooth(method = "lm", se=F) + #   theme_bw()   #`

<mark>3.4</mark> **Modelling of relation between number of IDPs and predictors**



# 4. Situation with services

<mark>4.1</mark> **How much time provision of all admin services were suspended**


```{.r .fold-hide}
ds0 %>% 
  count(admin_services_stopped, admin_services_resumed) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = admin_services_stopped , y = pct, fill = admin_services_resumed)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = 'Suspension and resumption of services') +
  xlab("Services stopped") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Status of service resumption"
                   ,values = c("#093058", "#EC4544", "#1DB5C4", "grey"))
```

![](figure-png-iso/services-admin-1.png)<!-- -->



```{.r .fold-hide}
ds0 %>% 
  group_by(military_action) %>% 
  count(admin_services_stopped) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = military_action , y = pct, fill = factor(admin_services_stopped))) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = 'Suspension of services per military actions status') +
  xlab("Military actions status") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Whether provision of services was stopped"
                    ,labels =c("No", "Yes")
                   ,values = c("#1DB5C4", "#EC4544"))
```

![](figure-png-iso/unnamed-chunk-8-1.png)<!-- -->



<mark>4.2</mark> **Interruption of garbage collection**


```{.r .fold-hide}
ds0 %>% 
  count(garbage_interruptions, garbage_functional) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = garbage_interruptions , y = pct, fill = garbage_functional)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  xlab("Hromada had interruption with garbage collection") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Current status of garbage collection"
                   ,values = c("#EC4544", "#1DB5C4"))
```

![](figure-png-iso/services-garbage-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  group_by(military_action) %>% 
  count(garbage_interruptions) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = military_action , y = pct, fill = factor(garbage_interruptions))) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = 'Suspension of garbage collection per military actions status') +
  xlab("Military actions status") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Whether garbage collection was stopped"
                    ,labels =c("Hard to say", "No", "Yes")
                   ,values = c("grey90", "#1DB5C4", "#EC4544"))
```

![](figure-png-iso/unnamed-chunk-9-1.png)<!-- -->


<mark>4.3</mark> **Modeling of relation between suspension time of admin services and different predictors**



<mark>4.4</mark> **Modeling of relation between garbage collection interruptions and different predictors**



# 5. Volunteers/humanitarian hubs

<mark>5.1</mark> **Relation between number of hubs and total population**


```{.r .fold-hide}
#TO CHANGE
d_volunteers %>% 
  ggplot(aes(x = total_population_2022, y = humanitarian_hubs)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between volunteers per capita and humanitarian hubs') +
  xlab("% of volunteers out of total population")
```

![](figure-png-iso/hubs-1.png)<!-- -->

<mark>5.2</mark> **Relation between % of volunteers and number of hubs**


```{.r .fold-hide}
d_volunteers %>% 
  ggplot(aes(x = volunteers_per_capita, y = humanitarian_hubs)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between volunteers per capita and humanitarian hubs') +
  xlab("% of volunteers out of total population")
```

![](figure-png-iso/unnamed-chunk-10-1.png)<!-- -->

<mark>5.3</mark> **Relation between % of volunteers and total population**


```{.r .fold-hide}
#TO CHANGE
d_volunteers %>% 
  filter(total_population_2022 <200000) %>% 
  ggplot(aes(x = log(total_population_2022) , y = volunteers_number, color = type)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between volunteers per capita and humanitarian hubs') +
  xlab("% of volunteers out of total population")
```

![](figure-png-iso/unnamed-chunk-11-1.png)<!-- -->

# 6. Aid providers

<mark>6.1</mark> **Share of hromadas in each region which received aid from other countries**


```{.r .fold-hide}
ds0 %>% 
  count(region_en, foreign_aid) %>% 
  group_by(region_en) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = region_en, y = pct, fill = foreign_aid)) +
  geom_col() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Aid from other countries per regions')
```

![](figure-png-iso/countries-regions-1.png)<!-- -->

<mark>6.2</mark> **Relation between number of countries as aid providers and number of international organizations**


```{.r .fold-hide}
ds0 %>% 
  ggplot(aes(x = ingo_count, y = countries_count)) +
  geom_jitter() +
  geom_smooth(method = "lm", se=F) +
  theme_bw()
```

![](figure-png-iso/countries-ingos-1.png)<!-- -->

<mark>6.3</mark> **Average number of countries and INGOs provided aid by region and type**


```{.r .fold-hide}
ds0 %>% 
  select(hromada_name, region_en, type, countries_australia:countries_japan) %>% 
  pivot_longer(-c(hromada_name, region_en, type), names_to = "country", values_to = "received") %>% 
  mutate(
    received = replace(received, is.na(received), 0)
  ) %>% 
  group_by(hromada_name, region_en) %>% 
  mutate(n = sum(received)) %>% 
  ungroup() %>% 
  distinct(hromada_name, region_en, type, n) %>% 
  group_by(region_en, type)  %>% 
  summarise(mean = mean(n), .groups = "drop", var = "country") %>% 
  rbind(
    ds0 %>% 
      select(hromada_name, region_en, type, starts_with("ingo_"), -ingo_other, -ingo_other_text, -ingo_count) %>% 
      pivot_longer(-c(hromada_name, region_en, type), names_to = "ingo", values_to = "received") %>% 
      group_by(hromada_name, region_en) %>% 
      mutate(n = sum(received)) %>% 
      ungroup() %>% 
      distinct(hromada_name, region_en, type, n) %>% 
      group_by(region_en, type)  %>% 
      summarise(mean = mean(n), .groups = "drop", var = "ingo") 
  ) %>% 
  ggplot(aes(x = var, y = mean, fill = var)) +
  geom_bar(stat = "identity") +
  facet_wrap(~region_en + type, ncol = 3) +
  geom_text(aes(label=round(mean, digits = 2)), vjust = -0.2, color="black", size=3.5) +
  ylim(0, 4.5)
```

![](figure-png-iso/countries-ingos-2-1.png)<!-- -->


<mark>6.3</mark> **How many hromadas covered by aid from each country ?**


```{.r .fold-hide}
ds0 %>% 
  filter(foreign_aid == "Так") %>% 
  select(hromada_name, region_en, countries_australia:countries_japan) %>% 
  pivot_longer(-c(hromada_name, region_en), names_to = "country", values_to = "received") %>% 
  mutate(
    country = str_to_title(str_remove(country, "countries_"))
  ) %>% 
  group_by(country) %>% 
  summarise(n = sum(received), .groups = "drop") %>% 
  filter(n>0)  %>% 
  ggplot(aes(x = n, y = fct_reorder(country, n))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label=n), hjust = -0.2, color="black", size=3.5) +
  theme(axis.text.y = element_text(size = 11),
        axis.title.y = element_blank()) 
```

![](figure-png-iso/countries-rating-1.png)<!-- -->

<mark>6.3.1</mark> **Maps of hromadas covered by each country**


```{.r .fold-hide}
library(tmap)

top_countries <- 
  ds0 %>% 
  filter(foreign_aid == "Так") %>% 
  select(hromada_name, region_en, countries_australia:countries_japan) %>% 
  pivot_longer(-c(hromada_name, region_en), names_to = "country", values_to = "received") %>% 
  mutate(
    country = str_to_title(str_remove(country, "countries_"))
  ) %>% 
  group_by(country) %>% 
  summarise(n = sum(received), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  head(11) %>% 
  pull(country)
  
  
ds0 %>% 
  filter(foreign_aid == "Так") %>% 
  select(hromada_name, region_en, countries_australia:countries_japan) %>% 
  pivot_longer(-c(hromada_name, region_en), names_to = "country", values_to = "received") %>% 
  mutate(
    country = str_to_title(str_remove(country, "countries_"))
  ) %>% 
  filter(country %in% top_countries)
```

```
# A tibble: 2,750 × 4
   hromada_name                  region_en country   received
   <chr>                         <chr>     <chr>        <dbl>
 1 Новомосковська міська громада East      Uk               0
 2 Новомосковська міська громада East      Spain            0
 3 Новомосковська міська громада East      Italy            0
 4 Новомосковська міська громада East      Lithuania        0
 5 Новомосковська міська громада East      Germany          1
 6 Новомосковська міська громада East      Poland           1
 7 Новомосковська міська громада East      Romania          0
 8 Новомосковська міська громада East      Usa              0
 9 Новомосковська міська громада East      Ukraine          0
10 Новомосковська міська громада East      France           0
# … with 2,740 more rows
```


<mark>6.4</mark> **How many hromadas covered by aid from each aid provider?**


```{.r .fold-hide}
ds0 %>% 
  select(hromada_name, region_en, starts_with("ingo_"), -ingo_other, -ingo_other_text, -ingo_count) %>% 
  pivot_longer(-c(hromada_name, region_en), names_to = "ingo", values_to = "received") %>% 
  mutate(
    ingo = str_to_upper(str_remove(ingo, "ingo_"))
  ) %>% 
  group_by(ingo) %>% 
  summarise(n = sum(received), .groups = "drop") %>% 
  filter(n>1)  %>% 
  ggplot(aes(x = n, y = fct_reorder(ingo, n))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label=n), hjust = -0.2, color="black", size=3.5) +
  theme(axis.text.y = element_text(size = 11),
        axis.title.y = element_blank()) 
```

![](figure-png-iso/ingos-rating-1.png)<!-- -->

<mark>6.5</mark> **Modelling of relation between number of countries provided aid and different predictors**



<mark>6.6</mark> **Modelling of relation between number of aid providers and different predictors**



# 7. Aid received

<mark>7.1</mark> **Modelling of relation between volume of received aid and different predictors**



# 8. Relocation and new projects

<mark>8.1</mark> **Modelling of relation between relocated enterprises and different predictors**



# Session Information {#session-info}

For the sake of documentation and reproducibility, the current report was rendered in the following environment. Click the line below to expand.

<details>

<summary>

Environment

</summary>


```
─ Session info ───────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.0 (2022-04-22)
 os       macOS Big Sur/Monterey 10.16
 system   x86_64, darwin17.0
 ui       X11
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Kiev
 date     2023-01-26
 pandoc   2.17.1.1 @ /Applications/RStudio.app/Contents/MacOS/quarto/bin/ (via rmarkdown)

─ Packages ───────────────────────────────────────────────────────────────────────────────────────
 package      * version   date (UTC) lib source
 abind          1.4-5     2016-07-21 [1] CRAN (R 4.2.0)
 archive        1.1.5     2022-05-06 [1] CRAN (R 4.2.0)
 assertthat     0.2.1     2019-03-21 [1] CRAN (R 4.2.0)
 backports      1.4.1     2021-12-13 [1] CRAN (R 4.2.0)
 base64enc      0.1-3     2015-07-28 [1] CRAN (R 4.2.0)
 bit            4.0.4     2020-08-04 [1] CRAN (R 4.2.0)
 bit64          4.0.5     2020-08-30 [1] CRAN (R 4.2.0)
 broom          0.8.0     2022-04-13 [1] CRAN (R 4.2.0)
 bslib          0.3.1     2021-10-06 [1] CRAN (R 4.2.0)
 cachem         1.0.6     2021-08-19 [1] CRAN (R 4.2.0)
 callr          3.7.0     2021-04-20 [1] CRAN (R 4.2.0)
 cellranger     1.1.0     2016-07-27 [1] CRAN (R 4.2.0)
 class          7.3-20    2022-01-16 [1] CRAN (R 4.2.0)
 classInt       0.4-7     2022-06-10 [1] CRAN (R 4.2.0)
 cli            3.5.0     2022-12-20 [1] CRAN (R 4.2.0)
 codetools      0.2-18    2020-11-04 [1] CRAN (R 4.2.0)
 colorspace     2.0-3     2022-02-21 [1] CRAN (R 4.2.0)
 crayon         1.5.1     2022-03-26 [1] CRAN (R 4.2.0)
 crosstalk      1.2.0     2021-11-04 [1] CRAN (R 4.2.0)
 DBI            1.1.3     2022-06-18 [1] CRAN (R 4.2.0)
 dbplyr         2.2.1     2022-06-27 [1] CRAN (R 4.2.0)
 devtools       2.4.3     2021-11-30 [1] CRAN (R 4.2.0)
 dichromat    * 2.0-0.1   2022-05-02 [1] CRAN (R 4.2.0)
 digest         0.6.29    2021-12-01 [1] CRAN (R 4.2.0)
 dplyr        * 1.0.9     2022-04-28 [1] CRAN (R 4.2.0)
 DT             0.24      2022-08-09 [1] CRAN (R 4.2.0)
 e1071          1.7-11    2022-06-07 [1] CRAN (R 4.2.0)
 ellipsis       0.3.2     2021-04-29 [1] CRAN (R 4.2.0)
 evaluate       0.15      2022-02-18 [1] CRAN (R 4.2.0)
 explore        0.8.0     2022-01-30 [1] CRAN (R 4.2.0)
 fansi          1.0.3     2022-03-24 [1] CRAN (R 4.2.0)
 farver         2.1.1     2022-07-06 [1] CRAN (R 4.2.0)
 fastDummies  * 1.6.3     2020-11-29 [1] CRAN (R 4.2.0)
 fastmap        1.1.0     2021-01-25 [1] CRAN (R 4.2.0)
 forcats      * 0.5.1     2021-01-27 [1] CRAN (R 4.2.0)
 fs             1.5.2     2021-12-08 [1] CRAN (R 4.2.0)
 generics       0.1.2     2022-01-31 [1] CRAN (R 4.2.0)
 ggplot2      * 3.4.0     2022-11-04 [1] CRAN (R 4.2.0)
 glue           1.6.2     2022-02-24 [1] CRAN (R 4.2.0)
 gridExtra      2.3       2017-09-09 [1] CRAN (R 4.2.0)
 gtable         0.3.1     2022-09-01 [1] CRAN (R 4.2.0)
 haven          2.5.0     2022-04-15 [1] CRAN (R 4.2.0)
 highr          0.9       2021-04-16 [1] CRAN (R 4.2.0)
 hms            1.1.1     2021-09-26 [1] CRAN (R 4.2.0)
 htmltools      0.5.2     2021-08-25 [1] CRAN (R 4.2.0)
 htmlwidgets    1.5.4     2021-09-08 [1] CRAN (R 4.2.0)
 httpuv         1.6.5     2022-01-05 [1] CRAN (R 4.2.0)
 httr           1.4.3     2022-05-04 [1] CRAN (R 4.2.0)
 janitor        2.1.0     2021-01-05 [1] CRAN (R 4.2.0)
 jquerylib      0.1.4     2021-04-26 [1] CRAN (R 4.2.0)
 jsonlite       1.8.0     2022-02-22 [1] CRAN (R 4.2.0)
 kableExtra     1.3.4     2021-02-20 [1] CRAN (R 4.2.0)
 KernSmooth     2.23-20   2021-05-03 [1] CRAN (R 4.2.0)
 knitr        * 1.39      2022-04-26 [1] CRAN (R 4.2.0)
 labeling       0.4.2     2020-10-20 [1] CRAN (R 4.2.0)
 labelled     * 2.9.1     2022-05-05 [1] CRAN (R 4.2.0)
 later          1.3.0     2021-08-18 [1] CRAN (R 4.2.0)
 lattice        0.20-45   2021-09-22 [1] CRAN (R 4.2.0)
 leafem         0.2.0     2022-04-16 [1] CRAN (R 4.2.0)
 leaflet        2.1.1     2022-03-23 [1] CRAN (R 4.2.0)
 leafsync       0.1.0     2019-03-05 [1] CRAN (R 4.2.0)
 lifecycle      1.0.3     2022-10-07 [1] CRAN (R 4.2.0)
 lubridate    * 1.8.0     2021-10-07 [1] CRAN (R 4.2.0)
 lwgeom         0.2-8     2021-10-06 [1] CRAN (R 4.2.0)
 magrittr       2.0.3     2022-03-30 [1] CRAN (R 4.2.0)
 MASS           7.3-58.1  2022-08-03 [1] CRAN (R 4.2.0)
 Matrix       * 1.5-1     2022-09-13 [1] CRAN (R 4.2.0)
 memoise        2.0.1     2021-11-26 [1] CRAN (R 4.2.0)
 mgcv           1.8-40    2022-03-29 [1] CRAN (R 4.2.0)
 mime           0.12      2021-09-28 [1] CRAN (R 4.2.0)
 mitools        2.4       2019-04-26 [1] CRAN (R 4.2.0)
 modelr         0.1.8     2020-05-19 [1] CRAN (R 4.2.0)
 munsell        0.5.0     2018-06-12 [1] CRAN (R 4.2.0)
 nlme           3.1-157   2022-03-25 [1] CRAN (R 4.2.0)
 pacman         0.5.1     2019-03-11 [1] CRAN (R 4.2.0)
 pillar         1.8.1     2022-08-19 [1] CRAN (R 4.2.0)
 pkgbuild       1.3.1     2021-12-20 [1] CRAN (R 4.2.0)
 pkgconfig      2.0.3     2019-09-22 [1] CRAN (R 4.2.0)
 pkgload        1.3.0     2022-06-27 [1] CRAN (R 4.2.0)
 png            0.1-7     2013-12-03 [1] CRAN (R 4.2.0)
 prettyunits    1.1.1     2020-01-24 [1] CRAN (R 4.2.0)
 processx       3.5.3     2022-03-25 [1] CRAN (R 4.2.0)
 promises       1.2.0.1   2021-02-11 [1] CRAN (R 4.2.0)
 proxy          0.4-27    2022-06-09 [1] CRAN (R 4.2.0)
 ps             1.7.0     2022-04-23 [1] CRAN (R 4.2.0)
 purrr        * 0.3.4     2020-04-17 [1] CRAN (R 4.2.0)
 R6             2.5.1     2021-08-19 [1] CRAN (R 4.2.0)
 raster         3.5-21    2022-06-27 [1] CRAN (R 4.2.0)
 RColorBrewer * 1.1-3     2022-04-03 [1] CRAN (R 4.2.0)
 Rcpp           1.0.9     2022-07-08 [1] CRAN (R 4.2.0)
 readr        * 2.1.2     2022-01-30 [1] CRAN (R 4.2.0)
 readxl       * 1.4.0     2022-03-28 [1] CRAN (R 4.2.0)
 remotes        2.4.2     2021-11-30 [1] CRAN (R 4.2.0)
 reprex         2.0.1     2021-08-05 [1] CRAN (R 4.2.0)
 rlang          1.0.6     2022-09-24 [1] CRAN (R 4.2.0)
 rmarkdown      2.14      2022-04-25 [1] CRAN (R 4.2.0)
 rstudioapi     0.13      2020-11-12 [1] CRAN (R 4.2.0)
 rvest          1.0.2     2021-10-16 [1] CRAN (R 4.2.0)
 sass           0.4.1     2022-03-23 [1] CRAN (R 4.2.0)
 scales         1.2.1     2022-08-20 [1] CRAN (R 4.2.0)
 sessioninfo    1.2.2     2021-12-06 [1] CRAN (R 4.2.0)
 sf             1.0-7     2022-03-07 [1] CRAN (R 4.2.0)
 shiny          1.7.2     2022-07-19 [1] CRAN (R 4.2.0)
 snakecase      0.11.0    2019-05-25 [1] CRAN (R 4.2.0)
 sp             1.5-0     2022-06-05 [1] CRAN (R 4.2.0)
 stargazer    * 5.2.3     2022-03-04 [1] CRAN (R 4.2.0)
 stars          0.5-5     2021-12-19 [1] CRAN (R 4.2.0)
 stringi        1.7.8     2022-07-11 [1] CRAN (R 4.2.0)
 stringr      * 1.5.0     2022-12-02 [1] CRAN (R 4.2.0)
 survey       * 4.1-1     2021-07-19 [1] CRAN (R 4.2.0)
 survival     * 3.3-1     2022-03-03 [1] CRAN (R 4.2.0)
 svglite        2.1.0     2022-02-03 [1] CRAN (R 4.2.0)
 systemfonts    1.0.4     2022-02-11 [1] CRAN (R 4.2.0)
 tableone     * 0.13.2    2022-04-15 [1] CRAN (R 4.2.0)
 terra          1.5-42    2022-06-15 [1] https://rspatial.r-universe.dev (R 4.2.0)
 tibble       * 3.1.8     2022-07-22 [1] CRAN (R 4.2.0)
 tidyr        * 1.2.0     2022-02-01 [1] CRAN (R 4.2.0)
 tidyselect     1.2.0     2022-10-10 [1] CRAN (R 4.2.0)
 tidyverse    * 1.3.1     2021-04-15 [1] CRAN (R 4.2.0)
 tmap         * 3.3-3     2022-03-02 [1] CRAN (R 4.2.0)
 tmaptools      3.1-1     2021-01-19 [1] CRAN (R 4.2.0)
 tzdb           0.3.0     2022-03-28 [1] CRAN (R 4.2.0)
 units          0.8-0     2022-02-05 [1] CRAN (R 4.2.0)
 usethis        2.1.6     2022-05-25 [1] CRAN (R 4.2.0)
 utf8           1.2.2     2021-07-24 [1] CRAN (R 4.2.0)
 vctrs          0.5.1     2022-11-16 [1] CRAN (R 4.2.0)
 viridisLite    0.4.1     2022-08-22 [1] CRAN (R 4.2.0)
 vroom          1.5.7     2021-11-30 [1] CRAN (R 4.2.0)
 webshot        0.5.3     2022-04-14 [1] CRAN (R 4.2.0)
 withr          2.5.0     2022-03-03 [1] CRAN (R 4.2.0)
 xfun           0.31      2022-05-10 [1] CRAN (R 4.2.0)
 XML            3.99-0.10 2022-06-09 [1] CRAN (R 4.2.0)
 xml2           1.3.3     2021-11-30 [1] CRAN (R 4.2.0)
 xtable         1.8-4     2019-04-21 [1] CRAN (R 4.2.0)
 yaml           2.3.5     2022-02-21 [1] CRAN (R 4.2.0)
 zoo            1.8-10    2022-04-15 [1] CRAN (R 4.2.0)

 [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library

──────────────────────────────────────────────────────────────────────────────────────────────────
```

</details>



Report rendered by serhii at 2023-01-26, 23:35 +0200 in 112 seconds.
