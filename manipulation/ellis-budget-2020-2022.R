#' ---
#' title: "Ellis Hromada Budget 2020-2022"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
rmarkdown::render(input = "./manipulation/ellis-budget-2020-2022.R") # run to knit, don't uncomment
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

paths_budget <-  list.files("./data-private/raw/", pattern = "^boost_incomes_INCO4.+.xlsx$",full.names = T)

ls_import <- list()
for(i in seq_along(paths_budget)){
  
  ls_import[[i]] <- 
    readxl::read_xlsx(
      path = paths_budget[[i]]
    ) %>% 
    janitor::clean_names() %>%
    mutate_all(.funs = as.character)
}

lapply(ls_import, glimpse)

#+ tweak-data ------------------------------------------------------------------
# to join multiple files (slices) downloaded from the source
ds0 <- 
  bind_rows(ls_import,.id = "file_number") 

path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)
ds_admin_full %>% glimpse(70)


#+ inspect-data ----------------------------------------------------------------

#+ tweak-data-1 ----------------------------------------------------------------

# tidying up admin codes
ds1 <- 
  ds0 %>% 
  filter(admin4 != 'Обласні бюджети та бюджет АР Крим') %>%
  # deleting oblast budgets
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
  ) %>% 
  filter(grepl('^[0-9][0-9]5', admin4_code)) %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("file_number", "admin4")) %>% 
  arrange(admin4_code) %>%
  filter(!is.na(admin4_code)) %>%  # because it's summary for oblast
  arrange(admin4_code)
ds1 %>% glimpse()
ds1

ds1 %>% distinct(admin4_code)
# frame explaining hierarchy of incomes

ds_inco <- 
  ds1 %>% 
  distinct(inco4) %>% 
  arrange(inco4) %>%
  mutate(
    inco4_code = str_extract(inco4, "^(\\d)+") %>% as.integer()
    ,inco4_label = str_remove(inco4, "^(\\d)+") %>% str_trim("both")
  ) %>%  
  select(
    ends_with("_code"), ends_with("_label")
  ) %>% 
  arrange(inco4_code)

ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# ---- tweak-data-2 ------------------------------------------------------------
ds1 %>% glimpse()


ds2_long <- 
  ds1 %>%
  mutate(inc_code = str_extract(inco4, '[0-9]+')) %>%
  select(-c(inco4)) %>%
  pivot_longer(
    -c(starts_with('adm'), inc_code)
    , names_to = 'year_month'
    , values_to = 'income'
  ) %>%
  mutate(
    year = str_extract(year_month, "(?<=x)....(?=_)")
    ,month = str_extract(year_month, "(?<=_)[0-9]+(?=_)")
  ) %>%
  select(-c(year_month)) 

ds2_wide <- 
  ds2_long %>% 
  pivot_wider(names_from = inc_code, values_from = income) %>%
  select(admin4_code, admin4_label, year, month, sort( names(.)))

ds1 %>% glimpse()
ds2_long %>% glimpse()
ds2_wide %>% glimpse()

# ---- tweak-data-3 ------------------------------------------------------------

ds3 <- ds2_wide %>%
  left_join(
    ds_admin_full %>% 
      mutate(budget_code = paste0(budget_code,"0")) %>% 
      distinct(budget_code, hromada_name, hromada_code, oblast_name_display, map_position
               , region_ua, oblast_code)
    ,by = c("admin4_code"  = "budget_code")
  ) %>%
  relocate(admin4_code, admin4_label, hromada_name, hromada_code, oblast_name_display, map_position
           , region_ua, oblast_code)

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(ds3, "./data-public/derived/hromada_budget_2020_2022.csv")

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

