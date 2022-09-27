#' ---
#' title: "Ellis Budget"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
rmarkdown::render(input = "./manipulation/ellis-budget-2020.R") # run to knit, don't uncomment
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
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"
path_budget <- "./data-private/raw/boost_incomes_26.09.2022.csv"

ds_admin0 <- readr::read_csv(path_admin)
ds_admin0 %>% glimpse(70)

ds0 <- read_delim(
  file = path_budget
  ,locale = locale(encoding = "WINDOWS-1251")
  ,delim = ";"
)  %>% janitor::clean_names()
ds0 %>% glimpse()

#+ tweak-data ------------------------------------------------------------------

ds_admin <- 
  ds_admin0 %>% 
  mutate(
    budget_code = paste0(budget_code, "0")
  ) %>% 
  mutate(
    
  )


#+ inspect-data ----------------------------------------------------------------
target_hromadas <- c("19548000000","08576000000")
ds_admin %>% glimpse()

ds_hromada_code <- 
  ds_admin %>% #glimpse()
  mutate(
    budget_code = paste0(budget_code, "0")
  ) %>% 
  filter(budget_code %in% target_hromadas) %>% 
  distinct(hromada_code, hromada_name, budget_code)

#+ tweak-data-1 ----------------------------------------------------------------

# tidying up admin codes
ds1 <-
  ds0 %>%
  mutate(
    admin4_code = as.character(str_extract(admin4, '[0-9]+')) %>% str_trim("both")
    ,admin4_label = str_remove(admin4, '[0-9]+ ') %>% str_trim("both")
    ,across(ends_with('executed'), as.numeric)
    ,inco3_code = str_extract(inco3, "^(\\d)+") %>% as.integer()
    ,inco3_label = str_remove(inco3, "^(\\d)+") %>% str_trim("both")
  ) %>%
  # glimpse() %>%
  arrange(admin4_code) %>%
  # glimpse() %>%
  ### VERY BIG DEAL !!! make sure you see this!
  filter(!is.na(inco3)) %>%  # because inco3 is most granular, other rows are summaries
  filter(!is.na(admin4_code)) %>%  # because it's summary for oblast 
  select(starts_with("admin4_"),starts_with("inco3_"), starts_with("x")) %>%
  arrange(admin4_code)
ds1 %>% glimpse()
ds1


ds2 <- 
  ds1 %>% 
  filter(admin4_code %in% target_hromadas) %>% 
  select(1:6) %>% 
  rename(
    budget_code = admin4_code
    ,budget_label = admin4_label
    ,fin_code = inco3_code
    ,fin_label = inco3_label
  )
ds2 %>% glimpse()

ds_admin 



# 
# ds_admin %>% 
#   filter(region_en != "Crimea") %>% 
#   distinct(hromada_code, hromada_name, budget_code) 
# Keep only valid hromadas (that existed after the end of the amalgamation process)
ds2 <- 
  ds1 %>% 
  inner_join(
    ds_admin %>%  
      filter(region_en != "Crimea") %>% 
      distinct(hromada_code, hromada_name, budget_code) %>% 
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
ds2_long %>% filter(admin4_code == "19548000000") %>% distinct() #%>% View()

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



# ---- compute-target-small -------------------------------------
target_hromadas <- c("19548000000","08576000000")
# target_hromadas <- c("01211405000" ,"01303515000" ,"05556000000" )

# ds_admin %>% filter(budget_code_old %in% target_hromadas ) %>% View()
# ds_admin %>% filter(budget_code %in% target_hromadas )
# ds_admin %>% filter(budget_code %in% paste0(target_hromadas,"0"))

d_few0 <- 
  ds2_long %>% 
  # filter(admin4_code == "19548000000") %>% 
  filter(admin4_code %in% target_hromadas) %>%
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
d_few0

d_few1 <- 
  d_few0 %>% 
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
  ) %>% 
  group_by(admin4_code) %>% 
  mutate(
    own_income_change = (income_own / lag(income_own)) - 1
  )

# ----- compute-many --------------------------------
# target_hromadas <- c("01211405000" ,"01303515000" ,"05556000000" )
tor_before_22 <- c(
   "05561000000"
  ,"05556000000"
  ,"12538000000"
  ,"05555000000"
  ,"12534000000"
  ,"05549000000"
  ,"05557000000"
  ,"05551000000"
  ,"12539000000"
  ,"05547000000"
  ,"05548000000"
  ,"05563000000"
  ,"12537000000"
  ,"12540000000"
  ,"05560000000"
  ,"12533000000"
  ,"05552000000"
  ,"05554000000"
  ,"05564000000"
  ,"12532000000"
  ,"12541000000"
  ,"05562000000"
  ,"12535000000"
  ,"05566000000"
  ,"12531000000"
  ,"05565000000"
  ,"05559000000"
  ,"05558000000"
  ,"05550000000"
  ,"12536000000"
  ,"05553000000"
) 
ds3 <- 
  ds2_long %>% 
  # filter(admin4_code == "19548000000") %>% 
  # filter(admin4_code %in% c("19548000000","08576000000")) %>%
  select(-admin4_label, -inco3) %>% 
  filter(!admin4_code %in% tor_before_22) %>% 
  mutate(
    date = paste0(year,"-",ifelse(
      nchar(month)==1L, paste0("0",month), month),  "-01"
    ) %>% as.Date()
    ,transfert = str_detect(inc_code, "^41.+")
    # ,war_indicator = date >= as.Date("2022-03-01")
    ,target_segment = month %in% c(3:7)
  ) #%>% 
# select(-year, -month)
ds3
ds3 %>% summarize(hromada_count = n_distinct(admin4_code))

ds4 <- 
  ds3 %>% 
  # filter(admin4_code == "02501000000") %>%
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
    ,own_prop = income_own /income_total
    # ,won_pct = scales::percent(own_prop)
  ) %>% 
  group_by(admin4_code) %>% 
  mutate(
    own_income_change = (income_own / lag(income_own)) - 1
    # ,own_income_change = case_when(own_income_change==Inf ~ NA, TRUE~own_income_change)
  ) %>% 
  ungroup() 

ds4 %>% summarize(hromada_count = n_distinct(admin4_code))
ds4 %>% arrange(desc(own_income_change))

d_few1
ds4 %>% filter(admin4_code %in% target_hromadas)



# ---- ---------------


# mark oblast that were temp occupied since Feb 24

ds_tor <- 
  ds_admin %>% 
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

inside_tukey_fences <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

# ----- ------------------------
ds5 <- 
  ds4 %>% 
  # drop_na(own_income_change) %>% 
  mutate(
    # outlier = own_income_change >= .5
    ntile = ntile(own_income_change,100)
    ,outlier = inside_tukey_fences(own_income_change)
    
  ) %>% 
  # filter(ntile < 95) %>%
  left_join(
    ds_admin %>% 
      mutate(budget_code = paste0(budget_code,"0")) %>% 
      distinct(budget_code, hromada_name, oblast_name_display, map_position
               , region_ua, oblast_code)
    ,by = c("admin4_code"  = "budget_code")
  ) %>% 
  mutate(
    oblast_tor = oblast_code %in% v_tor
  ) %>% 
  group_by(oblast_name_display) %>% 
  mutate(
    median = median(own_income_change, na.rm =T )
    ,mean = mean(own_income_change, na.rm =T )
    ,median_display = median %>%  scales::comma(accuracy = .01)
    ,mean_display = mean(own_income_change, na.rm =T )%>%  scales::comma(accuracy = .01)
  )

# RemoveLeadingZero <- function( x ) {
#   #   g <- grep("\\A\\b(?<=0)(\\.\\d{1,})$", x, perl=TRUE, value=TRUE);
#   g <- gsub("\\b(0)(\\.\\d{1,})$", "\\2", x, perl=TRUE);
#   return( g )
# } 

ds5 %>% 
  filter(admin4_code %in% target_hromadas) %>% 
  select(hromada_name, oblast_code, oblast_name_display, oblast_tor,
         median, median_display, year) 
# ----- -----------------------------------------------------------------------
source("./scripts/operational-functions.R")

ds4 

ds6 <- 
  ds4 %>% 
  # select(-own_)
  filter(admin4_code == "02501000000") %>% 
  mutate(year = paste0('y',year)) %>% 
  pivot_longer(
    cols = -(c("admin4_code","year"))
    ,names_to = "measure"
    ,values_to = "value"
  ) %>% 
  pivot_wider(names_from = "year", values_from = "value") %>% 
  mutate(
    pct_change = (y2022 / y2021) - 1
  )
  
ds6 %>% filter(admin4_code == "02501000000")


g2 <-
  ds6 %>% #glimpse()
  select(admin4_code,income_own, year) %>% 
  # pivot_wider(names_from = year, values_from = own_prop)
  pivot_wider(value_from = c("income_own","own_pct"), names_from = "year")
  # filter(hromada_name %in% draw_random_id(idvar="hromada_name",100))
  g{
    ggplot(.,aes(nx=))
  }
  
# ----- -----------------------------------------------------------------------

g1 <- 
  ds5 %>% 
  drop_na(own_income_change) %>% 
  filter(ntile < 95) %>%
  {
   ggplot(., aes(x = own_income_change, fill = oblast_tor ))+
   geom_histogram(alpha = .3)+
   geom_vline(xintercept = 0, linetype = "dashed")+
   facet_wrap(facets = "oblast_name_display")+
   scale_fill_manual(
     values = c("TRUE" = "red", "FALSE" = "blue")
   )+
   geom_text(
     aes(
       x= -.2
       , y = Inf
       , label = median_display
      )
     # ,alpha = .7
     ,color = "grey30"
     , data = . %>% distinct()
     ,vjust = 1.1
   )+
   geom_text(
     aes(
       x=.4
       , y = Inf
       , label = mean_display
      )
     # ,alpha = .2
     ,color = "grey80"
     , data = . %>% distinct()
     ,vjust = 1.1
    )+
   labs(
     title = "Year over year change in hromada's own revenue (total - transfert)"
     ,subtitle = "In percertage points, for the period March-July of each year"
     ,x = "Change in percent point"
     ,y = "Number of hromadas"
     ,caption = "Median value shown in bold to the left of the dashed line\nMean values shown in faint to the right of the dashed line\nHistograms show bottom 95% cases"
     ,fill = "Contains at least\none occupied\nhromada"
   )
  }

g1
g1 %>% quick_save("1-change-over-year", w= 12, h = 7)


hist(ds4$own_income_change,breaks="FD")

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

# ds_admin %>% glimpse()

# let us derive the codes for a SINGLE hromada
d_admin <- 
  ds_admin %>% 
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
  ds_admin %>% 
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
  ds_admin %>% 
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
  ds_admin %>% 
  filter(budget_code == target_hromada_budget_code)

(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))

(
  target_budget_codes_of_one_hromada <- 
    ds_admin %>% 
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
  ds_admin %>% 
  filter(budget_code == target_hromada_budget_code)

(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))

(
  target_budget_codes_of_one_hromada <- 
    ds_admin %>% 
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
  ds_admin %>% 
  filter(budget_code == target_hromada_budget_code)

(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))

(
  target_budget_codes_of_one_hromada <- 
    ds_admin %>% 
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
  ds_admin %>% 
  filter(budget_code == target_hromada_budget_code)

(target_hromada_ua_code <- d %>% distinct(hromada_code) %>% pull(hromada_code))

(
  target_budget_codes_of_one_hromada <- 
    ds_admin %>% 
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

