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
Sys.setlocale("LC_CTYPE", "russian")
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

ds0 <- bind_rows(ls_import,.id = "file_number")
ds0 %>% glimpse(80)


path_admin <- "./data-private/derived/ua-admin-map.csv"
ds_admin_full <- readr::read_csv(path_admin)

#+ inspect-data ----------------------------------------------------------------

# import_custom <- function(i){
#     readxl::read_xlsx(
#       path = paths_budget[[i]]
#     ) %>% 
#     janitor::clean_names() %>%
#     mutate_all(.funs = as.character)
# }
# 
# d <- import_custom(3)
ds0 %>% count(admin1)
ds0 %>% count(admin2)
ds0 %>% count(admin3)
ds0 %>% count(admin4) %>% arrange(admin4)



# Target case - Коростенська громада
ds0 %>% glimpse()

ds0 %>% count(admin1)
ds0 %>% count(admin2) %>% print_all()
ds0 %>% count(admin3) %>% print_all()


ds0 %>% filter(admin2 == "Житомирська обл.") %>% count(admin3)
ds0 %>% 
  filter(admin2 == "Житомирська обл.") %>% 
  filter(admin3 == "06200000000 Зведені бюджети міст обласного значення Житомирської області") %>% 
  count(admin4)
ds0 %>% filter(admin2 == "Житомирська обл.") %>% count(admin4)

ds0 %>% count(inco1)
ds0 %>% count(inco2) 
ds0 %>% count(inco3) %>% print_all()

# Brief dictinoary

# file_number = 
# admin1 - Only one value, drop it later or recode
# admin2 - name of the Oblast, which budget is observed
# admin3 - type of budget within Oblast (oblast, city, raion, hromada)
# admin4 - type of budget (settlement, rada, hromada) 
# incol1 - type of revenue (tax, non-tax, capital, transfer, target)
# incol2 - sub-type of revenue
# incol3 - sub-sub-type of revenue
#+ tweak-data-1 ----------------------------------------------------------------
ds1 <- ds0 %>%
  filter(!is.na(inco3))

# frame explaining hierarchy of incomes
inco_ds <- ds1 %>%
  distinct(inco1, inco2, inco3) %>% 
  arrange(inco1, inco2, inco3) %>%
  mutate(
    inco1_code = as.character(str_extract(inco1, '[0-9]+'))
    ,inco1_label = str_remove(inco1, '[0-9]+ ')
    ,inco2_code = as.character(str_extract(inco2, '[0-9]+'))
    ,inco2_label = str_remove(inco2, '[0-9]+ ')
    ,inco3_code = as.character(str_extract(inco3, '[0-9]+'))
    ,inco3_label = str_remove(inco3, '[0-9]+ ')
  ) %>%
  select(-c(inco1, inco2, inco3))

#+ tweak-data-2 ----------------------------------------------------------------

# 
ds2 <- ds1 %>% 
  mutate(
    across(ends_with('executed'), as.numeric)
    # , across(ends_with('executed'), round)
  )
ds1 %>% glimpse()
ds2 %>% glimpse()
ds2 %>%
  group_by(inco1, inco2, inco3) %>%
  summarise(sum = sum(x2018_1_executed, na.rm = T))
  
# to get summary for administrative unit
ds2 %>%
  group_by(admin2) %>%
  summarise(across(ends_with('executed'), ~ sum(.x, na.rm = TRUE)))

ds2 %>%
  group_by(admin3) %>%
  summarise(across(ends_with('executed'), ~ sum(.x, na.rm = TRUE)))

ds2 %>%
  group_by(admin4) %>%
  summarise(across(ends_with('executed'), ~ sum(.x, na.rm = TRUE)))

ds3 <- ds2 %>%
  mutate(inc_code = str_extract(inco3, '[0-9]+')) %>%
  select(-c(file_number, inco1, inco2, inco3)) %>%
  pivot_longer(-c(starts_with('adm'), inc_code), names_to = 'year_quarter', values_to = 'income') %>%
  mutate(year = str_extract(year_quarter, "(?<=x)....(?=_)"),
         quarter = str_extract(year_quarter, "(?<=_).(?=_)")) %>%
  select(-year_quarter) %>%
  pivot_wider(names_from = inc_code, values_from = income) %>%
  select(admin1, admin2, admin3, admin4, year, quarter, sort(names(.)))

ds2 %>% glimpse()
ds3 %>% glimpse()

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

d_hromada <- 
  full_join(
    d_before_unification %>% rename(before = total_revenue)
    ,d_after_unification %>% rename(after = total_revenue)
  ) %>% 
  group_by(date) %>% 
  summarize(
    total_revenue = sum(before+ after)
  ) %>% 
  mutate(admin4_label = "Бюджет Коростенської міської громади")
  # alternative - explore solution via rbind to see if that's more flexible



# TODO: for next time: add lines of individual radas

#+ tweak-data-3 ----------------------------------------------------------------
d_rada1 <- 
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
    total_revenue = rowSums(across(col_names),na.rm =T)
  ) %>% 
  select(-col_names, -admin4_code, -year, -quarter, -quarter_date) %>%
  filter(date < "2021-01-01")

d_rada_hromada <- rbind(d_hromada, d_rada1)

#+ tweak-data-4 ----------------------------------------------------------------
#adding grouping by taxes in Korostenska gromada

d_taxes_before <- 
  ds4_one_hromada %>% 
  select(year, quarter, col_names) %>% 
  mutate(
    quarter_date = case_when(
      quarter == "1" ~ "01"
      ,quarter == "2" ~ "04"
      ,quarter == "3" ~ "07"
      ,quarter == "4" ~ "10"
    )
    ,date = as.Date(paste0(year,"-",quarter_date,"-","01"))
  ) %>% 
  group_by(date) %>% 
  summarise_at(col_names, ~sum(., na.rm=T)) %>% 
  filter(date < "2021-01-01")

d_taxes_after <- 
  ds4_one_hromada_after %>% 
  select(year, quarter, col_names) %>% 
  mutate(
    quarter_date = case_when(
      quarter == "1" ~ "01"
      ,quarter == "2" ~ "04"
      ,quarter == "3" ~ "07"
      ,quarter == "4" ~ "10"
    )
    ,date = as.Date(paste0(year,"-",quarter_date,"-","01"))
  ) %>% 
  mutate_at(col_names, ~replace(., is.na(.),0)) %>% 
  select(-year, -quarter, -quarter_date) %>% 
  filter(date >= "2021-01-01")

d_taxes <- 
  rbind(d_taxes_before, d_taxes_after) %>% 
  pivot_longer(col_names,
    names_to = "tax"
    ,values_to = "revenue"
  )

#+ table-1 ---------------------------------------------------------------------



#+ graph-1 ---------------------------------------------------------------------
g1 <- 
  d_rada_hromada %>% 
  ggplot(aes(x = date, y = total_revenue, color = admin4_label)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))
g1


#+ graph-2 ---------------------------------------------------------------------
g2 <- 
  d_taxes %>% 
  left_join(
    inco_ds %>% select(inco3_code, inco2_code, inco2_label)
    ,by=c("tax"="inco3_code")
  ) %>% 
  group_by(date,inco2_code,inco2_label) %>% 
  summarise(revenue = sum(revenue)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = revenue, color = inco2_label)) + 
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.Date("2021-01-01"))

g2
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

