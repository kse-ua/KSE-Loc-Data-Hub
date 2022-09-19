#' ---
#' title: "Ellis Economics"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-economics.R") # run to knit, don't uncomment
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
  bind_rows(ls_import,.id = "file_number") %>% 
  select(1:10) # to help focusing during development
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
# Let's find its UA code
(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))
length(target_hromada_ua_code)==1L# should be a single value to assert one-to-one match
# hromada's admin coordinate
d %>% 
  arrange(rada_code) %>% 
  select(region_ua, oblast_name, raion_name, hromada_name, rada_name, settlement_name) %>%
  # select(reg•ion_ua, oblast_name, hromada_code, rada_code, settlement_code, budget_code_old) %>%
  print_all()
# goal: we need to link budget data to this frame


#+ tweak-data-1 ----------------------------------------------------------------


# let's understand how Open Budget organizes its data for download
ds0 %>% glimpse()
# file_number - download had multiple csv slices, this is the number of the slice
# admin1 - Only one value, drop it later or recode
# admin2 - name of the Oblast, which budget is observed
# admin3 - type of budget within Oblast (oblast, city, raion, hromada)
# admin4 - type of budget (settlement, rada, hromada) 
# incol1 - type of revenue (tax, non-tax, capital, transfer, target)
# incol2 - sub-type of revenue
# incol3 - sub-sub-type of revenue

# 
# ds0 %>% count(admin1)
# ds0 %>% count(admin2) %>% print_all()
ds0 %>% count(admin3) %>% print_all()
# ds0 %>% count(admin4) %>% print_all()
# ds0 %>% filter(admin2 == "Житомирська обл.") %>% count(admin3)
# ds0 %>% 
#   filter(admin2 == "Житомирська обл.") %>% 
#   filter(admin3 == "06200000000 Зведені бюджети міст обласного значення Житомирської області") %>% 
#   count(admin4)
# ds0 %>% filter(admin2 == "Житомирська обл.") %>% count(admin4)

# tidying up admin codes
ds1 <- 
  ds0 %>% 
  # because other rows contain summaries of most granular units
  # to see, view
  mutate(
    admin3_code = as.character(str_extract(admin3, '[0-9]+'))
    ,admin3_label = str_remove(admin3, '[0-9]+ ')
    
    ,admin4_code = as.character(str_extract(admin4, '[0-9]+'))
    ,admin4_label = str_remove(admin4, '[0-9]+ ')
    ,across(ends_with('executed'), as.numeric)
  ) %>% 
  rename(oblast = admin2) %>% # becase no need to use codes
  relocate(c("admin3_code"), .after = "admin3") %>%
  relocate(c("admin4_code"), .after = "admin4") %>%
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
  # ds0 %>% # differs by 1, strange, investigate
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
ds_inco %>% filter(!is.na(inco3_code)) %>%  print_all()


ds_admin3_lkp <- 
  ds1 %>% 
  distinct(admin3_code, admin3_label)

ds_admin4_lkp <- 
  ds1 %>% 
  distinct(admin4_code, admin4_label)

# each admin unit will/may have these rows for each period of reporting

ds1 %>% 
  filter(admin4_code == paste0(target_hromada_budget_code,"0")) %>% 
  View()


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
  # select(adin4_code, year, quarter, date, sort(everything(.)))
  select(admin4_code, year, quarter, date, sort( names(.)))

ds1 %>% glimpse()
ds2_long %>% glimpse()
ds2_wide %>% glimpse()

ds2 <- 
  ds1 %>%
  mutate(inc_code = str_extract(inco3, '[0-9]+')) %>%
  select(-c(file_number, inco1, inco2, inco3)) %>%
  pivot_longer(-c(starts_with('adm'), inc_code), names_to = 'year_quarter', values_to = 'income') %>%
  mutate(year = str_extract(year_quarter, "(?<=x)....(?=_)"),
         quarter = str_extract(year_quarter, "(?<=_).(?=_)")) %>%
  select(-year_quarter) %>%
  pivot_wider(names_from = inc_code, values_from = income) %>%
  select(admin1, admin2, admin3, admin4, year, quarter, sort(names(.)))

ds1 %>% glimpse()
ds2 %>% glimpse()

## to add higher level income - sum columns by code (as defined in inco_ds), 
## i.e. to get amount of all tax incomes you need to sum all codes starting with '1';
## to get all rent - sum all codes that start with '13'

## moreover, there I left only values for admin 4 - as we can get amount of income for 
## higher adm units by adding up 



## split admin4 into admin-code and admin-label
ds4 <- ds3 %>%
  mutate(admin4_code = as.character(str_extract(admin4, '[0-9]+')),
         admin4_label = str_remove(admin4, '[0-9]+ '),
         admin3_code = as.character(str_extract(admin3, '[0-9]+')),
         admin3_label = str_remove(admin3, '[0-9]+ ')) %>%
  select(starts_with('admin4'), starts_with('admin3'), everything()) %>%
  select(-c('admin1', 'admin2', 'admin3', 'admin4')) 

ds4 %>% glimpse()
ds4 %>% count(admin4_code, admin4_label)
ds4 %>% count(admin3_code, admin3_label)

# to select reporting periods (quarters) BEFORE this settlement joined a hromada
d1 <- ds4 %>%
  filter(admin4_code == "06203100000") #%>% View()
# we can see that it joins hormada in 2021-1

# The total revenue can be computed by summing the values in the columns right of `quarter`
# Let's verify this assertion 
ds4 %>% 
  filter(admin4_code == "06203100000") %>%  # not necessary, but use to remind the hierarchy
  filter(admin3_code == "06200000000") %>% 
  filter(year == "2018", quarter =="1") %>% 
  select(-c(1:6)) %>% 
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

# # Now the revenues recorded AFTER this settlement joined a hromada
# # 06563000000 - budget code of the hromada 
# d2 <- ds4 %>%
#   filter(admin4_code == "06563000000") #%>% View()
# 
# d3 <- ds4 %>%
#   filter(admin4_code %in% c("06563000000", "06203100000")) %>%
#   filter(!is.na(`11010000`))
#   
# d4 <- ds4 %>%
#   filter(admin3_code == '06200000000')
# 
# d5 <- ds_admin_full %>%
#   filter(budget_code == '0656300000') %>%
#   distinct(budget_code_old)
# 
# d6 <- ds4 %>%
#   filter(admin4_code %in% (d5%>%pull(budget_code_old)))

# Korosten hromada -  06563000000
budget_code_hromada  <- "06563000000" # used in budget_code
budget_code_hromada  <- "0656300000"  # old budget code
# TODO: please verify the difference between old and new budget codes

# ds_admin_full %>% glimpse()


# let us derive the codes for a SINGLE hromada
d_admin <- 
  ds_admin_full %>% 
  filter(budget_code == budget_code_hromada) #%>% View()
# budget_code - code used for statistics after 2020-
# budget_code_old - used prior, different by a single zero at the end

target_budget_codes_of_one_hromada <- 
  d_admin %>% pull(budget_code_old) %>% unique() 

ds4 %>% glimpse()

#  Select the individual components of hromada BEFORE unification
# this table contains revenues for individuals RADAS/CITY before they joined the hromada
# NOTE: this is the simplest possible case: all radas joined this hromada
# at the same time, in 2021-1. TODO: find a more complex case to work through next
ds4_one_hromada <-  # before unification
  ds4 %>% 
  filter(admin4_code %in% target_budget_codes_of_one_hromada)

# TODO: clean up the code in this chunk and annotate the creation of the 
# single-hromada data

ds4_one_hromada %>% count(admin3_code, admin3_label)
ds4_one_hromada %>% count(admin4_code, admin4_label)

ds4_one_hromada %>% filter(admin4_code == "06203100000" ) %>% View()

ds4_one_hromada %>% glimpse()



stem_names <- c("admin4_code", "admin4_label", "admin3_code", "admin3_label", "year","quarter")
col_names <- setdiff(
  names(ds4_one_hromada)
  , stem_names
)
# col_names <- col_names[1:2]


d_before_unification <- 
  ds4_one_hromada %>% 
  select(stem_names, col_names) %>% 
  select(-admin3_code, -admin3_label) %>% 
  mutate(
    quarter_date = case_when(
      quarter == "1" ~ "01"
      ,quarter == "2" ~ "04"
      ,quarter == "3" ~ "07"
      ,quarter == "4" ~ "10"
    )
    ,date = as.Date(paste0(year,"-",quarter_date,"-","01"))
  ) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  select(-col_names, -quarter_date) %>%
  group_by(date) %>%
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
  ) %>% 
  ungroup()



ds4_one_hromada_after <- 
  ds4 %>% 
  filter(admin4_code == "06563000000")

# col_names <- col_names[1:2]
d_after_unification <- 
  ds4_one_hromada_after %>% 
  select(stem_names, col_names) %>% 
  select(-admin3_code, -admin3_label) %>% 
  mutate(
    quarter_date = case_when(
      quarter == "1" ~ "01"
      ,quarter == "2" ~ "04"
      ,quarter == "3" ~ "07"
      ,quarter == "4" ~ "10"
    )
    ,date = as.Date(paste0(year,"-",quarter_date,"-","01"))
  ) %>% 
  mutate(
    row_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  select(-col_names, -quarter_date) %>%
  group_by(date) %>%
  summarize(
    total_revenue = sum(row_revenue, na.rm = T)
  ) %>% 
  ungroup()

d_joined <- 
  full_join(
    d_before_unification %>% rename(before = total_revenue)
    ,d_after_unification %>% rename(after = total_revenue)
  ) %>% 
  group_by(date) %>% 
  summarize(
    total_revenue = sum(before+ after)
  )
# alternative - explore solution via rbind to see if that's more flexible


g <- 
  d_joined %>% 
  ggplot(aes(x = date, y = total_revenue)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g

# TODO: for next time: add lines of individual radas
#+ tweak-data-1, eval=eval_chunks ------------------------------------------------

#+ tweak-data-2 ----------------------------------------------------------------

#+ tweak-data-3 ----------------------------------------------------------------

#+ table-1 ---------------------------------------------------------------------

#+ graph-1 ---------------------------------------------------------------------


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

