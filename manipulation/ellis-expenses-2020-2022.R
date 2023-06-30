#' ---
#' title: "Ellis Hromada Expenses 2020-2022"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-budget-2020-2022.R") # run to knit, don't uncomment
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
library(openxlsx)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# vector of hromadas that were occupied before 24th February
toh_before_22 <- c("05561000000","05556000000","12538000000","05555000000","12534000000"
                   ,"05549000000","05557000000","05551000000","12539000000","05547000000","05548000000"
                   ,"05563000000","12537000000","12540000000","05560000000","12533000000","05552000000"
                   ,"05554000000","05564000000","12532000000","12541000000","05562000000","12535000000"
                   ,"05566000000","12531000000","05565000000","05559000000","05558000000","05550000000"
                   ,"12536000000","05553000000") 


#+ declare-functions -----------------------------------------------------------

'%nin%' <- Negate('%in%') 

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

path_admin <- "./data-private/derived/ua-admin-map.csv"
path_econ <- "./data-private/raw/budget-data/expenses_econ_01.2021-12.2021.csv"
path_func <- "./data-private/raw/budget-data/expenses_func_01.2021-12.2021.csv"
path_prog <- "./data-private/raw/budget-data/expenses_prog_01.2021-12.2021.csv"

ds_admin_full <- readr::read_csv(path_admin)
ds_expenses_func <- readr::read_delim(path_func, delim = ";", 
                                      locale = locale(encoding = "WINDOWS-1251"))
ds_expenses_econ <- readr::read_delim(path_econ, delim = ";", 
                                      locale = locale(encoding = "WINDOWS-1251"))
ds_expenses_prog <- readr::read_delim(path_prog, delim = ";", 
                                      locale = locale(encoding = "WINDOWS-1251"))

#+ load-data, eval=eval_chunks -------------------------------------------------

#+ prepare-func-expenses, eval=eval_chunks -------------------------------------

ds1 <- 
  ds_expenses_func %>% 
  janitor::clean_names() %>%
  # select only local budgets 
  filter(grepl('^[0-9][0-9]5', admin3)) %>%
  filter(!is.na(func3)) %>%
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
    ,x2021_executed = case_when(is.na(x2021_executed) ~ 0,
                                .default = x2021_executed)
  ) %>% 
  filter(!(x2021_executed==0)) %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("admin4")) %>% 
  arrange(admin4_code)

ds2 <- 
  ds1 %>%
  mutate(func_code = paste0("func_", str_extract(func3, '[0-9]+'))) %>%
  select(-c(func1, func2, func3)) %>%
  pivot_longer(
    -c(starts_with('adm'), func_code)
    , names_to = 'year'
    , values_to = 'expenses'
  ) %>%
  mutate(
    year = str_extract(year, "(?<=x)....(?=_)")
    ) %>%
  pivot_wider(names_from = func_code, values_from = expenses) %>%
  select(admin4_code, admin4_label, year, sort(names(.))) %>%
  mutate(across(
    starts_with("func"), 
    ~case_when(is.na(.) ~ 0,
               .default = .)))

ds_func_fin <- ds2 %>%
  left_join(
    ds_admin_full %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("admin4_code"  = "budget_code")
  ) %>%
  relocate(hromada_name, hromada_code, admin4_label, admin4_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, 
           region_code_en, year) %>%
  rename(budget_name = admin4_label,
         budget_code = admin4_code) %>%
  select(-c(admin1, admin2, admin3)) %>% 
  # filtering out non-hromadas and hromadas occupied before 2022
  filter(!is.na(hromada_code) & budget_code %nin% toh_before_22)

#+ prepare-econ-expenses, eval=eval_chunks -------------------------------------

ds1 <- 
  ds_expenses_econ %>% 
  janitor::clean_names() %>%
  # select only local budgets 
  filter(grepl('^[0-9][0-9]5', admin3)) %>%
  filter(!is.na(econ3)) %>%
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
    ,x2021_executed = case_when(is.na(x2021_executed) ~ 0,
                                .default = x2021_executed)
  ) %>% 
  filter(!(x2021_executed==0)) %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("admin4")) %>% 
  arrange(admin4_code)

ds2 <- 
  ds1 %>%
  mutate(econ_code = paste0("econ_", str_extract(econ3, '[0-9]+'))) %>%
  select(-c(econ1, econ2, econ3)) %>%
  pivot_longer(
    -c(starts_with('adm'), econ_code)
    , names_to = 'year'
    , values_to = 'expenses'
  ) %>%
  mutate(
    year = str_extract(year, "(?<=x)....(?=_)")
  ) %>%
  pivot_wider(names_from = econ_code, values_from = expenses) %>%
  select(admin4_code, admin4_label, year, sort(names(.))) %>%
  mutate(across(
    starts_with("econ"), 
    ~case_when(is.na(.) ~ 0,
               .default = .)))

ds_econ_fin <- ds2 %>%
  left_join(
    ds_admin_full %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("admin4_code"  = "budget_code")
  ) %>%
  relocate(hromada_name, hromada_code, admin4_label, admin4_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, 
           region_code_en, year) %>%
  rename(budget_name = admin4_label,
         budget_code = admin4_code) %>%
  select(-c(admin1, admin2, admin3)) %>% 
  # filtering out non-hromadas and hromadas occupied before 2022
  filter(!is.na(hromada_code) & budget_code %nin% toh_before_22)

#+ prepare-prog-expenses, eval=eval_chunks -------------------------------------

ds1 <- 
  ds_expenses_prog %>% 
  janitor::clean_names() %>%
  # select only local budgets 
  filter(grepl('^[0-9][0-9]5', admin3)) %>%
  filter(!is.na(prog2)) %>%
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
    ,x2021_executed = case_when(is.na(x2021_executed) ~ 0,
                                .default = x2021_executed)
  ) %>% 
  filter(!(x2021_executed==0)) %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("admin4")) %>% 
  arrange(admin4_code)

ds2 <- 
  ds1 %>%
  mutate(prog_code = paste0("prog_", str_extract(prog2, '[0-9]+'))
         ) %>% 
  select(-c(prog1, prog2)) %>%
  # filter(prog_code %in% c("prog_1080", "prog_8130", "prog_5043"))
  pivot_longer(
    -c(starts_with('adm'), prog_code)
    , names_to = 'year'
    , values_to = 'expenses'
  ) %>%
  mutate(
    year = str_extract(year, "(?<=x)....(?=_)")
  ) %>%
  # some prog codes changed names so there are duplicates, sum it
  summarise(expenses = sum(expenses, na.rm = T),
            .by = c(admin4_code, admin4_label, year, prog_code)) %>% 
  pivot_wider(names_from = prog_code, values_from = expenses) %>%
  select(admin4_code, admin4_label, year, sort(names(.))) %>%
  mutate(across(
    starts_with("prog"), 
    ~case_when(is.na(.) ~ 0,
               .default = .)))

ds_prog_fin <- ds2 %>%
  left_join(
    ds_admin_full %>% 
      distinct(budget_code, hromada_name, hromada_code, raion_code, raion_name               
               , oblast_code, oblast_name, oblast_name_en, region_en, region_code_en)
    ,by = c("admin4_code"  = "budget_code")
  ) %>%
  relocate(hromada_name, hromada_code, admin4_label, admin4_code, raion_name, 
           raion_code, oblast_name, oblast_name_en, oblast_code, region_en, 
           region_code_en, year) %>%
  rename(budget_name = admin4_label,
         budget_code = admin4_code) %>%
  # filtering out non-hromadas and hromadas occupied before 2022
  filter(!is.na(hromada_code) & budget_code %nin% toh_before_22)

#+ merge-all, eval=eval_chunks -------------------------------------------------

ds_fin <- ds_prog_fin %>% 
  left_join(ds_econ_fin, suffix = c("", ".y"), 
            by = c("hromada_code", "year")) %>%
  select(-ends_with('.y')) %>% 
  left_join(ds_func_fin, suffix = c("", ".y"), 
            by = c("hromada_code", "year")) %>% 
  select(-ends_with('.y'))

#+ calculating shares of expenses ----------------------------------------------

ds_expenses_short <- ds_fin %>%
  mutate(total_expense = rowSums(across(starts_with("func")))
         ,func_0100_share = rowSums(across(starts_with("func_01"))) / total_expense
         ,func_0111_share = func_0111 / total_expense
         ,func_0400_share = rowSums(across(starts_with("func_04"))) / total_expense
         ,func_0700_share = rowSums(across(starts_with("func_07"))) / total_expense
         ,func_0900_share = rowSums(across(starts_with("func_09"))) / total_expense
         ,func_1000_share = rowSums(across(starts_with("func_10"))) / total_expense
         ,econ_2110_share = econ_2110 / total_expense
         ,econ_3000_share = rowSums(across(starts_with("econ_3"))) / total_expense
         ,association = case_when(prog_7680 > 0 ~ 1, .default = 0)
  ) %>% 
  select(hromada_code, total_expense, func_0100_share, func_0111_share, func_0400_share,
         func_0700_share, func_0900_share, func_1000_share, econ_2110_share, econ_3000_share,
         association)
# no func_0110 in 2021

#+ save, eval=eval_chunks -------------------------------------------------

openxlsx::write.xlsx(ds_fin, './data-private/derived/hromada_expenses_2021_2022.xlsx')
openxlsx::write.xlsx(ds_expenses_short, './data-private/derived/hromada_expenses_shares_2021.xlsx')

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















