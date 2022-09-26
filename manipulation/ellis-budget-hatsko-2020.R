#' ---
#' title: "Ellis Budget"
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

#+ tweak-data ------------------------------------------------------------------
# to join multiple files (slices) downloaded from the source
# ds0 <-
#   bind_rows(ls_import,.id = "file_number") 
# %>% 
# select(1:10) # to help focusing during development

path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)
ds_admin_full %>% glimpse(70)
# path_budget <- "./data-private/raw/boost_incomes_12.08.2022.xlsx"
path_budget <- "./data-private/raw/boost_incomes_26.09.2022.csv"
# ds0 <- readxl::read_excel(path_budget, sheet = 1) %>% janitor::clean_names()
# ds0 <- readxl::read_xl(path_budget, sheet = 1) %>% janitor::clean_names()
# ds0 <- readr::read_csv(path_budget) %>% janitor::clean_names()
# ds0 %>% glimpse(60)



ds0 <- read_delim(
  file = "data-private/raw/boost_incomes_26.09.2022.csv"
  ,locale = locale(encoding = "WINDOWS-1251")
  ,delim = ";"
)  %>% janitor::clean_names()
ds0 %>% glimpse()

#+ inspect-data ----------------------------------------------------------------
d_hromada_code <- 
  ds_admin_full %>% 
  distinct(hromada_code, hromada_name, budget_code, budget_code_old)

#+ tweak-data-1 ----------------------------------------------------------------

# tidying up admin codes
ds1 <- 
  ds0 %>% 
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
  ) %>% #glimpse()
  select(starts_with("admin4_"), inco3, starts_with("x")) %>% 
  arrange(admin4_code) %>% #glimpse()
  ### VERY BIG DEAL !!! make sure you see this!
  filter(!is.na(inco3)) %>%  # because inco3 is most granular, other rows are summaries
  filter(!is.na(admin4_code)) %>%  # because it's summary for oblast 
  arrange(admin4_code)
ds1 %>% glimpse()
ds1

# Keep only valid hromadas (that existed after the end of the amalgamation process)
ds2 <- 
  ds1 %>% 
  inner_join(
    ds_admin_full %>%  distinct(hromada_code, hromada_name, budget_code) %>% 
      mutate(budget_code = paste0(budget_code, "0"))
    ,by = c("admin4_code" = "budget_code")
  )
ds2 %>% glimpse()
# frame explaining hierarchy of incomes

ds1 %>% summarize(hromada_count = n_distinct(admin4_code, na.rm = T))
ds2 %>% summarize(hromada_count = n_distinct(admin4_code, na.rm = T))

# we dropped codes for regions and city regions, but crimea stayed b/c in ds_admin
d <- 
  ds1 %>% 
  filter(
    !admin4_code %in% unique(ds2$admin4_code)
  )




ds_inco <- 
  ds2 %>% # because already dropped is.na(inco3)
  distinct(inco3) %>% 
  arrange(inco3) %>%
  mutate(
    # inco1_code = str_extract(inco1, "^(\\d)+") %>% as.integer()
    # ,inco1_label = str_remove(inco1, "^(\\d)+") %>% str_trim("both")
    # ,inco2_code = str_extract(inco2, "^(\\d)+") %>% as.integer()
    # ,inco2_label = str_remove(inco2, "^(\\d)+") %>% str_trim("both")
    inco3_code = str_extract(inco3, "^(\\d)+") %>% as.integer() 
    ,inco3_label = str_remove(inco3, "^(\\d)+") %>% str_trim("both") 
  ) %>%  
  # select(inco3, inco3_code, inco3_label) %>% 
  select(
    ends_with("_code"), ends_with("_label")
  ) %>% 
  arrange(inco3_code)

# ds_admin3_lkp <- 
#   ds1 %>% 
#   distinct(admin3_code, admin3_label)

ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# ---- tweak-data-2 ------------------------------------------------------------
# 
# ds2 <- 
#   ds1 %>% 
#   # compute the target index in this form before pivoting 
#   # difference between codes:
#   mutate(
#     target_measure = 
#   )



ds2 %>% glimpse()
ds2_long <- 
  ds2 %>% #select(inco3) %>% 
  mutate(
    inc_code = str_extract(inco3, '[0-9]+')
    # ,inc_label = str
  ) %>% #glimpse()
  # select(-c(admin4_label)) %>%
  pivot_longer(
    # -c(starts_with('adm'), inco3, inc_code)
    cols = -c("admin4_code","admin4_label", "inco3","hromada_code","hromada_name", "inc_code")
    , names_to = 'year_month'
    , values_to = 'income'
  ) %>% #glimpse()
  mutate(
     # year = str_extract(year_month, "x(\\d{4})_.+")
    # ,month = str_extract(year_month, "x\\d{4}_(d+).+")
    year = str_extract(year_month, "(?<=x)....(?=_)")
    ,month = str_extract(year_month, "(?<=_)[0-9]+(?=_)")
    ) %>%
  select(-c(year_month)) 

ds2_long %>% glimpse()
ds2_long %>% filter(admin4_code == "19548000000") %>% distinct() %>% View()

ds2_long %>% count(inc_code) 
# ds3_long <- 
#   ds2_long %>% 
#   filter(
#     inc_code == "11010000"
#   ) %>% 
#   distinct()

# ds3_long %>% filter(admin4_code == "19548000000") %>% distinct() %>% View()
# 
# ds2_long %>% glimpse()
# 
# ds2_long %>% 
#   filter(admin4_code == "01201100000") %>% 
#   filter(inc_code == "18010000") %>% 
#   View()
# 
# 
# code <- "01201100000"
# budget <- "18010000"
# 
# ds2_wide <- 
#   ds2_long %>% 
#   pivot_wider(names_from = inc_code, values_from = income, values_fn = length) %>%
#   select(admin4_code, year, month, sort( names(.)))
# 
# ds1 %>% glimpse()
# ds2_long %>% glimpse()
# ds2_wide %>% glimpse()



d <- 
  ds2_long %>% 
  # filter(admin4_code == "19548000000") %>% 
  filter(admin4_code %in% c("19548000000","08576000000")) %>%
  select(-admin4_label, -inco3) %>% 
  mutate(
    date = paste0(year,"-",ifelse(
      nchar(month)==1L, paste0("0",month), month),  "-01"
    ) %>% as.Date()
    ,transfert = str_detect(inc_code, "^41.+")
    # ,war_indicator = date >= as.Date("2022-03-01")
    ,target_segment = month %in% c(3:7)
  ) #%>% 
  # select(-year, -month)
d

d %>% 
  filter(target_segment) %>%  # we will compare Mar-Jul in 2021 and 2022
  group_by(admin4_code, year) %>% 
  summarize(
    income_total = sum(income, na.rm = T)
    ,income_transfert = sum(income*transfert, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    income_own = income_total - income_transfert
    ,own_prop = income_own /income_total
    ,won_pct = scales::percent(own_prop)
  )


ds2_long %>% 
  distinct(inc_code) %>% 
  left_join(
    ds_inco %>% mutate(inco3_code = as.character(inco3_code))
    ,by = c("inc_code"="inco3_code")
  ) %>% 
  mutate(
    transfert = str_detect(inc_code, "^41.+")
  ) %>% 
  relocate(transfert) %>% 
  print_all()

d2 <- 
  
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
  group_by(admin4_code, date) %>% #uncomment for individual radas
  # group_by(date) %>%
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
  group_by(admin4_code, date) %>% #uncomment for individual radas
  # group_by(date) %>%
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

g1 <- 
  # d_after_unification %>%
  # ggplot(aes(x = date, y = total_revenue)) + 
  d_joined_ind %>% #uncomment for individual radas
  ggplot(aes(x = date, y = total_revenue, color = admin4_code)) +
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1

# ---- single-hromada-3 ----------------------

# Trostyanetska hromada - there are only data after amalgamation for it,
# no old_budget_codes

target_hromada_budget_code <- "1351400000"

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
# NO budget_old_codes

stem_names <- c("admin4_code", "year","quarter","date")
col_names <- setdiff(
  names(ds2_wide)
  , stem_names
)

# there is only data after unification for this hromada
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

g1 <- 
  d_after_unification %>%
  ggplot(aes(x = date, y = total_revenue)) + 
  # d_joined_ind %>% uncomment for individual radas
  # ggplot(aes(x = date, y = total_revenue, color = admin4_code)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1

# ---- single-hromada-4 ----------------------

# Shiretska hromada - there are only data after amalgamation for it,
# budget_code is the same as old_budget_codes

target_hromada_budget_code <- "1352900000"

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

identical(paste0(target_hromada_budget_code, "0"), target_budget_codes_of_one_hromada)
# budget_code is the same as budget_old_codes

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

g1 <- 
  d_after_unification %>%
  ggplot(aes(x = date, y = total_revenue)) + 
  # d_joined_ind %>% uncomment for individual radas
  # ggplot(aes(x = date, y = total_revenue, color = admin4_code)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1

# ---- single-hromada-5 ----------------------

# Ralivska hromada - there are only part of old budget codes present,
target_hromada_budget_code <- "1357500000"

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
  # group_by(admin4_code, date) %>% #uncomment for individual radas
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
  # group_by(admin4_code, date) %>% #uncomment for individual radas
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

g1 <- 
  d_joined %>%
  ggplot(aes(x = date, y = total_revenue)) +
  # d_joined_ind %>% #uncomment for individual radas
  # ggplot(aes(x = date, y = total_revenue, color = admin4_code)) +
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1

#+ tweak-data-4 ----------------------------------------------------------------

#+ tweak-data-5 ----------------------------------------------------------------

#+ table-1 ---------------------------------------------------------------------

#+ graph-1 ---------------------------------------------------------------------
g1 <- 
  # d_joined %>%
  # ggplot(aes(x = date, y = total_revenue)) + 
  d_joined_ind %>% #uncomment for individual radas
  ggplot(aes(x = date, y = total_revenue, color = admin4_code)) +
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

