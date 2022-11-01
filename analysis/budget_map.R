#' ---
#' title: "Ellis Budget Change For Map"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-budget-hatsko.R") # run to knit, don't uncomment
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
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
path_admin <- "./data-private/derived/ua-admin-map.csv"
path_budget <- "./data-public/derived/hromada_budget_2020_2022.xlsx"

ds_admin_full <- readr::read_csv(path_admin)

types_budget <- c(rep('text', 13), rep('numeric', 222))

ds0 <- readxl::read_excel(path_budget, col_types = types_budget) %>% janitor::clean_names()

#+ tweak-data-1 ----------------------------------------------------------------
d_hromada_code <- 
  ds_admin_full %>% 
  distinct(hromada_code, hromada_name, budget_code, budget_code_old)

# Keep only valid hromadas (that existed after the end of the amalgamation process)
ds1 <- 
  ds0 %>% 
  filter(region_en != "Crimea") %>%
  filter(year %in% c(2021, 2022))

ds1 %>% glimpse()
ds1 %>% summarize(hromada_count = n_distinct(admin4_code, na.rm = T))

ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# ---- tweak-data-2 ------------------------------------------------------------

ds1_long <- 
  ds1 %>%
  pivot_longer(
    cols = -c("admin4_code","admin4_label","hromada_code","hromada_name", "raion_name",
              'raion_code', 'oblast_name', 'oblast_code', 'oblast_name_en', 'region_en',
              'region_code_en', 'year', 'month')
    , names_to = 'income_code'
    , values_to = 'income'
  )

ds1_long %>% glimpse()
ds1_long %>% count(income_code) 

#+ ----- compute share of personal income tax for military --------------------------------
tor_before_22 <- c("05561000000","05556000000","12538000000","05555000000","12534000000"
                   ,"05549000000","05557000000","05551000000","12539000000","05547000000","05548000000"
                   ,"05563000000","12537000000","12540000000","05560000000","12533000000","05552000000"
                   ,"05554000000","05564000000","12532000000","12541000000","05562000000","12535000000"
                   ,"05566000000","12531000000","05565000000","05559000000","05558000000","05550000000"
                   ,"12536000000","05553000000") 

d1 <- 
  ds1_long %>% 
  filter(!admin4_code %in% tor_before_22) %>% 
  mutate(
    date = paste0(year,"-",ifelse(
      nchar(month)==1L, paste0("0",month), month),  "-01"
    ) %>% as.Date()
    ,transfert = str_detect(income_code, "^x4.+")
    ,target_segment = month %in% c(3:7)
    ,military_tax = income_code %in% c('x11010200')
  )

d2 <- 
  d1 %>% 
  filter(target_segment) %>%  # we will compare Mar-Jul in 2021 and 2022
  filter(!transfert) %>%
  group_by(year, military_tax) %>% 
  summarize(
    income = sum(income, na.rm = T)) %>%
  group_by(year) %>%
  arrange(desc(military_tax)) %>%
  mutate(label_y = cumsum(income))

g2 <- 
  d2 %>%
  ggplot(aes(x = year, y = income, fill = military_tax))+
  geom_col(alpha = .3)+
  geom_text(aes(y = label_y, label = scales::unit_format(unit = "BN", scale = 1e-9)(income)), vjust = -0.5)+
  labs(
    title = "Year over year change in hromada's own revenue (for the period March-July)"
    ,subtitle = "Hromada's own revenue increased to a great extent due to income tax for military"
    ,x = NULL
    ,y = "Amount of tax revenue"
    ,caption = "Shown only own revenue, excluding transfert"
    ,fill = "Tax type"
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = "BN", scale = 1e-9))+
  scale_fill_discrete(labels = c("Other taxes", "Income tax \nfor military personnel"))
g2

g2 %>% quick_save("2-military-tax", w= 12, h = 7)

#+ ----- compute change of own income --------------------------------

ds2 <- 
  ds1_long %>% 
  filter(!admin4_code %in% tor_before_22) %>% 
  filter(income_code != 'x11010200') %>%
  mutate(
    date = paste0(year,"-",ifelse(
      nchar(month)==1L, paste0("0",month), month),  "-01"
    ) %>% as.Date()
    ,transfert = str_detect(income_code, "^x4.+")
    ,target_segment = month %in% c(3:7)
  )

ds2 %>% summarize(hromada_count = n_distinct(admin4_code))
ds2 %>% filter(income_code == 'x11010200')


ds3 <- 
  ds2 %>% 
  filter(target_segment) %>%  # we will compare Mar-Jul in 2021 and 2022
  group_by(admin4_code, year) %>% 
  summarize(
    income_total = sum(income, na.rm = T)
    ,income_transfert = sum(income*transfert, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(
    income_own = income_total - income_transfert
    ,own_prop = round(income_own/income_total,2)
    ,own_pct = scales::percent(own_prop)
  ) %>% 
  group_by(admin4_code) %>% 
  mutate(
    own_income_change = round((income_own / lag(income_own)) - 1,2)
    ,own_income_change_pct = scales::percent((income_own / lag(income_own)) - 1)
    ,own_prop_change = own_prop - lag(own_prop)
    ,own_prop_change_pct = scales::percent(own_prop - lag(own_prop))
  ) %>% 
  ungroup()

# mark oblast that were temp occupied since Feb 24

ds_tor <- 
  ds_admin_full %>% 
  distinct(oblast_code, oblast_name) %>% 
  mutate(
    oblast_tor = oblast_code %in% c(
      "UA65000000000030969"
      ,"UA63000000000041885"
      ,"UA59000000000057109"
      ,"UA14000000000091971"
      ,"UA23000000000064947"
      ,"UA48000000000039575"
      ,"UA32000000000030281"
      ,"UA12000000000090473"
      ,"UA44000000000018893"
      ,"UA74000000000025378"
      ,"UA18000000000041385"
    ) 
  ) %>% 
  arrange(oblast_tor)

v_tor <- ds_tor %>% filter(oblast_tor) %>% pull(oblast_code)

ds4 <- 
  ds3 %>% 
  # filter(year == 2022) %>% 
  mutate(
    outlier = own_income_change > quantile(own_income_change, na.rm = TRUE)[4] +
      1.5*IQR(own_income_change, na.rm = TRUE) | own_income_change < 
      quantile(own_income_change, na.rm = TRUE)[2] - 1.5*IQR(own_income_change, na.rm = TRUE)
    ,ntile = ntile(own_income_change,100)
  ) %>% 
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = paste0(budget_code,"0")) %>% 
      distinct(budget_code, hromada_name, hromada_code, oblast_name_display, map_position
               , region_ua, oblast_code)
    ,by = c("admin4_code"  = "budget_code")
  ) %>% 
  mutate(
    oblast_tor = oblast_code %in% v_tor
  ) %>%
  group_by(year) %>%
  # 2 rows duplicated cause budget code refers to 2 hromadas at once
  filter(!duplicated(admin4_code))

# 94 hromadas outliers
table(ds4$outlier)

ds4 %>% filter(outlier) %>% select(own_income_change) %>% view()

# ----- -----------------------------------------------------------------------

g1 <- 
  ds4 %>%
  filter(!outlier) %>%
  ggplot(aes(x = own_income_change, fill = oblast_tor))+
  geom_histogram(alpha = .3)+
  geom_vline(xintercept = 0, linetype = "solid", alpha = 0.1)+
  facet_wrap(facets = "oblast_name_display")+
  geom_vline(data = . %>% 
               group_by(oblast_name_display) %>% 
               summarise(line = mean(own_income_change, na.rm = T)), 
             mapping = aes(xintercept = line), linetype = "dotdash")+
  geom_vline(data = . %>% 
               group_by(oblast_name_display) %>% 
               summarise(line = median(own_income_change, na.rm = T)), 
             mapping = aes(xintercept = line), linetype = "dotted")+
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue")
  )+
labs(
  title = "Year over year change in hromada's own revenue (total - transfert)"
  ,subtitle = "Excluding Personal income tax on the financial support of military personnel\nIn percentage points, for the period March-July of each year"
  ,x = "Change in percent point"
  ,y = "Number of hromadas"
  ,caption = "Median value shown by dotted line\nMean values shown by dashed line\nHistograms show bottom 95% cases"
  ,fill = "Contains at least\none occupied\nhromada"
) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 15))

g1
g1 %>% quick_save("1-change-over-year", w= 12, h = 7)

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
ds4 %>% readr::write_csv("./data-private/derived/budget-change-for-map.csv")

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

