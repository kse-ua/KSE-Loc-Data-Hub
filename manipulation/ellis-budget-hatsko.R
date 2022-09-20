#' ---
#' title: "Ellis Economics"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
rmarkdown::render(input = "./manipulation/ellis-budget-hatsko.R") # run to knit, don't uncomment
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

paths_budget <-  list.files("./data-private/raw/", pattern = "revenues-\\d.xlsx$",full.names = T)
# names_budget <-  paths_budget %>% str_replace("^.+revenues-(\\d{1}).csv","\\1")

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
# %>% 
# select(1:10) # to help focusing during development
ds0 %>% glimpse(60)

path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)
ds_admin_full %>% glimpse(70)


#+ inspect-data ----------------------------------------------------------------
# Target case - Коростенська громада
# target_hromada_budget_code  <- "06563000000" # used in budget_code 
target_hromada_budget_code    <- "0656300000"  # used in ds_ua_admin

# ds_admin_full
# to establish connection with ds_admin
d <- 
  ds_admin_full %>% 
  filter(budget_code == target_hromada_budget_code)

d %>% glimpse()
# Let's find its UA hromada code
(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))
length(target_hromada_ua_code)==1L# should be a single value to assert one-to-one match

# hromada's admin coordinate
d %>% 
  arrange(rada_code) %>% 
  select(region_ua, oblast_name, raion_name, hromada_name, rada_name, settlement_name) %>%
  # select(reg•ion_ua, oblast_name, hromada_code, rada_code, settlement_code, budget_code_old) %>%
  print_all()
# GOAL: we need to link budget data to this frame


#+ tweak-data-1 ----------------------------------------------------------------


# let's understand how Open Budget organizes its data for download
ds0 %>% glimpse()
# file_number - download had multiple csv slices, this is the number of the slice
# admin1 - Only one value, drop it later or recode
# admin2 - name of the Oblast, which budget is observed
# admin3 - type of budget within Oblast (oblast, city, raion, hromada)
# admin4 - type of budget (settlement, rada, hromada) 
# inco1 - type of revenue (tax, non-tax, capital, transfer, target)
# inco2 - sub-type of revenue
# inco3 - sub-sub-type of revenue

ds0 %>% count(admin3) %>% print_all()
ds0 %>% count(admin4) %>% print_all()

# tidying up admin codes
ds1 <- 
  ds0 %>% 
  # because other rows contain summaries of most granular units
  # to see, view
  # IMPORTANT: copying admin3 code into admin4 when its oblast type budget
  # so it won't be dropped when dropping na's
  mutate(admin4 = case_when(
    admin4 == 'Обласні бюджети та бюджет АР Крим' ~ admin3
    , TRUE ~ admin4
  )) %>%
  mutate(
    admin3_code = as.character(str_extract(admin3, '[0-9]+'))
    ,admin3_label = str_remove(admin3, '[0-9]+ ')
    ,admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
  ) %>% 
  rename(oblast = admin2) %>% # because no need to use codes
  relocate(c("admin3_code", 'admin3_label'), .after = "admin3") %>%
  relocate(c("admin4_code", 'admin4_label'), .after = "admin4") %>%
  select(-c("file_number", "admin1", "admin3","admin4")) %>% 
  arrange(admin3_code, admin4_code) %>% 
  ### VERY BIG DEAL !!! make sure you see this!
  filter(!is.na(inco3)) %>%  # because inco3 is most granular, other rows are summaries
  filter(!is.na(admin4_code)) %>%  # because it's summary for oblast
  arrange(admin3_code, admin4_code)
ds1 %>% glimpse()
ds1


# frame explaining hierarchy of incomes

ds_inco <- 
  ds1 %>% # because already dropped is.na(inco3)
  distinct(inco1, inco2, inco3) %>% 
  arrange(inco1, inco2, inco3) %>%
  mutate(
    inco1_code = str_extract(inco1, "^(\\d)+") %>% as.integer()
    ,inco1_label = str_remove(inco1, "^(\\d)+") %>% str_trim("both")
    ,inco2_code = str_extract(inco2, "^(\\d)+") %>% as.integer()
    ,inco2_label = str_remove(inco2, "^(\\d)+") %>% str_trim("both")
    ,inco3_code = str_extract(inco3, "^(\\d)+") %>% as.integer() 
    ,inco3_label = str_remove(inco3, "^(\\d)+") %>% str_trim("both") 
  ) %>%  
  # select(inco3, inco3_code, inco3_label) %>% 
  select(
    ends_with("_code"), ends_with("_label")
  ) %>% 
  arrange(inco1_code, inco2_code, inco3_code)

ds_admin3_lkp <- 
  ds1 %>% 
  distinct(admin3_code, admin3_label)

ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# each admin unit will/may have these rows for each period of reporting

ds1 %>% 
  filter(admin4_code == paste0(target_hromada_budget_code,"0")) #%>% View()


# ---- tweak-data-2 ------------------------------------------------------------
ds1 %>% glimpse()


ds2_long <- 
  ds1 %>%
  mutate(inc_code = str_extract(inco3, '[0-9]+')) %>%
  select(-c(inco1, inco2, inco3, oblast, admin3_label, admin4_label, admin3_code)) %>%
  pivot_longer(
    -c(starts_with('adm'), inc_code)
    , names_to = 'year_quarter'
    , values_to = 'income'
  ) %>%
  mutate(
    year = str_extract(year_quarter, "(?<=x)....(?=_)")
    ,quarter = str_extract(year_quarter, "(?<=_).(?=_)")
    ,quarter_date = case_when(
      quarter == "1" ~ "01"
      ,quarter == "2" ~ "04"
      ,quarter == "3" ~ "07"
      ,quarter == "4" ~ "10"
    )
    ,date = as.Date(paste0(year,"-",quarter_date,"-","01"))
  ) %>%
  select(-c(year_quarter, quarter_date)) 

ds2_wide <- 
  ds2_long %>% 
  pivot_wider(names_from = inc_code, values_from = income) %>%
  select(admin4_code, year, quarter, date, sort( names(.)))

ds1 %>% glimpse()
ds2_long %>% glimpse()
ds2_wide %>% glimpse()

# ---- explore-single-hromada ------------------------------------------------------------

## to add higher level income - sum columns by code (as defined in inco_ds), 
## i.e. to get amount of all tax incomes you need to sum all codes starting with '1';
## to get all rent - sum all codes that start with '13'

## moreover, there I left only values for admin 4 - as we can get amount of income for 
## higher adm units by adding up 

target_hromada_budget_code
# to select reporting periods (quarters) BEFORE this settlement joined a hromada
d1 <- ds2_wide %>%
  filter(admin4_code == paste0(target_hromada_budget_code,"0")) %>% View()
# we can see that it joins hromada in 2021-1

# Sanity check - VERIFICATION OF ASSUMPTIONS ABOUT DATA
# The total revenue can be computed by summing the values in the columns right of `quarter`
# Let's verify this assertion 
ds2_wide %>% 
  filter(admin4_code == "06203100000") %>%  # not necessary, but use to remind the hierarchy
  # filter(admin3_code == "06200000000") %>%
  filter(year == "2018", quarter =="1") %>% 
  # select(-c(1:4)) %>% # less reproducible
  select(-c(admin4_code, year, quarter, date)) %>% # stronger code (resilient to changes in the order/quantity of columns) 
  pivot_longer(cols = everything(), values_to = "value", names_to ="code") %>% 
  summarize(total = sum(value, na.rm = T)) %>% 
  unlist() %>% 
  scales::comma()
# this number is identical to what we can derived from the original source:
ds0 %>% 
  filter(admin4 == "06203100000 Бюджет міста Коростеня") %>% 
  filter(is.na(inco1), is.na(inco2), is.na(inco3)) %>% 
  select(x2018_1_executed ) %>% 
  print()


target_hromada_ua_code
(target_hromada_budget_code) 
# Korosten hromada -  06563000000
# budget_code_hromada  <- "06563000000" # used in budget_code
# budget_code_hromada  <- "0656300000"  # used in ua_admin, old budget code
# TODO: please verify the difference between old and new budget codes

# ds_admin_full %>% glimpse()

# let us derive the codes for a SINGLE hromada
d_admin <- 
  ds_admin_full %>% 
  # filter(budget_code == budget_code_hromada) #%>% View()
  filter(budget_code == target_hromada_budget_code)
# budget_code - code used for statistics after 2020-
# budget_code_old - used prior, different by a single zero at the end

# hromada's admin coordinate
d %>% 
  arrange(rada_code) %>% 
  select(region_ua, oblast_name, raion_name, hromada_name, rada_name, settlement_name) %>%
  # select(reg•ion_ua, oblast_name, hromada_code, rada_code, settlement_code, budget_code_old) %>%
  print_all()

# NOTE: Korosten hromada is easy case also cause it has budget_old_code for all settlements
target_budget_codes_of_one_hromada <- 
  d_admin %>% pull(budget_code_old) %>% unique() 

target_hromada_budget_code # get all settlement code for this hromada

target_budget_codes_of_one_hromada <- 
  ds_admin_full %>% 
  filter(budget_code == target_hromada_budget_code) %>% 
  pull(budget_code_old) %>% 
  unique()


# ----- tranformation-stages -------------------------------


# 1 Process a simiple hromada (done) - Korosten
# 2 Process a more complex hromada 
# 3 Filter main dataframe to these two hromadas and practice general tranformation
# 4 Apply general tranformation (to all hromadas) and demostrate absence of errors

# ---- single-hromada-1 ----------------------------------------------------------

target_hromada_budget_code # get all settlement code for this hromada
target_budget_codes_of_one_hromada <- 
  ds_admin_full %>% 
  filter(budget_code == target_hromada_budget_code) %>% 
  pull(budget_code_old) %>% 
  unique()

#  Select the individual components of hromada BEFORE unification
# this table contains revenues for individuals RADAS/CITY before they joined the hromada
# NOTE: this is the simplest possible case: all radas joined this hromada
# at the same time, in 2021-1. TODO: find a more complex case to work through next

# TODO: clean up the code in this chunk and annotate the creation of the 
# single-hromada data

stem_names <- c("admin4_code", "year","quarter","date")
col_names <- setdiff(
  names(ds2_wide)
  , stem_names
)

# sum revenues by date before amalgamation
d_before_unification <- 
  ds2_wide %>% 
  filter(admin4_code %in% target_budget_codes_of_one_hromada) %>% # !!!
  # the line above selects MULTIPLE units (settlement) that eventual joined into a hromada
  # the code for hromada is absent in the table prior to this date, b/c it didn't exist
  select(stem_names, col_names) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  group_by(date) %>%
  # group_by(date, admin4_code) %>% uncomment to graph for individual radas
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
  ) %>% 
  ungroup()

# sum revenues by date after amalgamation
d_after_unification <- 
  ds2_wide %>% 
  filter(admin4_code == paste0(target_hromada_budget_code, "0")) %>% # !!!!
  # the line above selects a SINGLE unit (hromada) starting on the date it formed
  select(stem_names, col_names) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  group_by(date) %>%
  # group_by(date, admin4_code) %>% uncomment to graph for individual radas
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
  ) %>% 
  ungroup()

# join two frames 
d_joined <- 
  bind_rows(
    d_before_unification %>% filter(!total_revenue==0)
    ,d_after_unification %>% filter(!total_revenue==0)
  ) %>% 
  arrange(date)

# graph for aggregate revenue dynamic
g <- 
  d_joined %>% 
  ggplot(aes(x = date, y = total_revenue)) + 
  geom_line()+
  # geom_line(aes(color = admin4_code))+ uncomment to graph for individual radas
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g

# TODO: for next time: add lines of individual radas
# Result: most revenue before amalgamation gathered city hromada

# ---- single-hromada-2 ----------------------

target_hromada_budget_code <- "1954800000"

d <- 
  ds_admin_full %>% 
  filter(budget_code == target_hromada_budget_code)

(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))

(
  target_budget_codes_of_one_hromada <- 
    ds_admin_full %>% 
    filter(
      budget_code == target_hromada_budget_code
    ) %>%
    drop_na() %>% 
    pull(budget_code_old) %>% 
    unique()
)

stem_names <- c("admin4_code", "year","quarter","date")
col_names <- setdiff(
  names(ds2_wide)
  , stem_names
)

d_before_unification <- 
  ds2_wide %>% 
  filter(admin4_code %in% target_budget_codes_of_one_hromada) %>% # !!!
  # the line above selects MULTIPLE units (settlement) that eventual joined into a hromada
  # the code for hromada is absent in the table prior to this date, b/c it didn't exist
  select(stem_names, col_names) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  # group_by(admin4_code, date) %>% uncomment for individual radas
  group_by(date) %>%
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  ungroup()

d_after_unification <- 
  ds2_wide %>% 
  filter(admin4_code == paste0(target_hromada_budget_code, "0")) %>% # !!!!
  # the line above selects a SINGLE unit (hromada) starting on the date it formed
  select(stem_names, col_names) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  # group_by(admin4_code, date) %>% uncomment for individual radas
  group_by(date) %>%
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
  ) %>% 
  ungroup()

# join for aggregated dataset  
d_joined <-
  full_join(
    d_after_unification %>% rename(after = total_revenue)
    ,d_before_unification %>% rename(before = total_revenue)
  ) %>%
  group_by(date) %>%
  summarize(
    total_revenue = sum(before+ after)
  )

# join for individual radas and hromada dataset  
d_joined_ind <-
  bind_rows(
    d_before_unification
    ,d_after_unification
  ) %>% 
  arrange(date) %>%
  mutate(
    total_revenue = case_when(
    total_revenue == 0 ~ NA_real_
    ,TRUE ~ total_revenue
    ))

# alternative - explore solution via rbind to see if that's more flexible
# PROBLEM: in 2020 there are data both for hromada and radas

d_rada1 <- 
  ds2_wide %>% 
  filter(admin4_code %in% c(target_budget_codes_of_one_hromada, 
                            paste0(target_hromada_budget_code, "0"))) %>% 
  select(stem_names, col_names) %>% 
  mutate(
    total_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  select(-col_names, -year, -quarter) %>%
  mutate(
    total_revenue = case_when(
    total_revenue == 0 ~ NA_real_
    ,TRUE ~ total_revenue
    )) %>%
  arrange(admin4_code, date) %>%
  pivot_wider(names_from = admin4_code, values_from = total_revenue)

#+ tweak-data-4 ----------------------------------------------------------------

#+ tweak-data-5 ----------------------------------------------------------------

#+ table-1 ---------------------------------------------------------------------

#+ graph-1 ---------------------------------------------------------------------
g1 <- 
  d_joined %>%
  ggplot(aes(x = date, y = total_revenue)) + 
  # d_joined_ind %>% uncomment for individual radas
  # ggplot(aes(x = date, y = total_revenue, color = admin4_code)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1

# Result: it seems (from graph and look at d_rada1) that, at first, city rada (19203100000)
# was made into hromada at some time in 4th quarter of 2019, then - in 4th quarter of 2020
# all other radas was added to this hromada
# BUT what happened in 3rd qrt of 2022?


#+ graph-2 ---------------------------------------------------------------------
#+ save-to-disk, eval=eval_chunks-----------------------------------------------

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

