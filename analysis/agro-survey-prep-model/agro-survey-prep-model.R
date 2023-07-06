rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# ---- load-packages -----------------------------------------------------------
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
library(glmnet)
library(stargazer)


# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling
source("./analysis/survey-prep-model/custom-model-functions.R")

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/agro-survey-prep-model/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
# ---- declare-functions -------------------------------------------------------
'%ni%' <- Negate(`%in%`)


make_corr_matrix <- function(d,na_action="remove", method="pearson"){
  
  item_names <- names(d)
  # browser()
  d1 <- 
    d %>% 
    select(all_of(item_names)) %>% 
    mutate(
      across(
        .cols = everything()
        ,.fns = ~as.numeric(.)
      )
    ) 
  
  if(na_action == "remove"){
    d2 <- d1 %>% drop_na()
  }
  
  if(na_action == "replace"){
    d2 <-
      d1 %>%
      mutate(
        across(
          .cols = everything()
          ,.fns = ~replace_na(.,0L)
        )
      )
  }
  cormat <- cor(d2,method = method)
  # row.names(cormat) <- item_names
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

# ---- load-data ---------------------------------------------------------------
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



# ---- inspect-data ------------------------------------------------------------
ds_survey %>% glimpse()

ds_survey %>% pull(hromada_code) %>% unique()

# ---- variable-groups -----------------------------------------------------------
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

budget_cut_types <- 
  ds_survey %>% 
  select(starts_with("budget_cut_type"), , -ends_with("text"), -budget_cut_type, -budget_cut_type_count) %>% 
  colnames() %>% 
  print()

aid_ingo <- 
  ds_survey %>%
  select(starts_with('ingo_'), -ends_with('text'), -ingo, -ingo_count) %>% 
  colnames() %>% 
  print()

aid_countries <- 
  ds_survey %>% 
  select(starts_with('countries'), -ends_with('text'), -countries, -countries_count) %>% 
  colnames() %>% 
  print()


# vector of income variables 
income <- 
  ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>%
  colnames() %>% 
  print()

# ---- meta-data-1 -------------------------------------------------------------
# meta_survey %>% glimpse()
# 
# meta_survey %>% 
#   filter(type %in% c("begin_group","end_group")) %>% 
#   select(1:5) %>% 
#   print_all()
# 
# 
# meta_survey %>% glimpse()

# ---- tweak-data-0 ----------------------

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
    ,occupation_b = ifelse(occupation == "No", 0, 1)
    ,budget_deficit_estimate = factor(budget_deficit_estimate, labels = c("0-5%", "11-20%", "21-30%", "6-10%", ">30%")) %>% 
      factor(levels = c("0-5%", "6-10%", "11-20%", "21-30%", ">30%"))
    ,budget_cut = recode(budget_cut,"Ні" = 0, "Так" = 1)
    ,financial_support = factor(financial_support, labels = c("Applied, didn't receive", 
                                                              "Applied, received",
                                                              "Didn't applie, didn't receive",
                                                              "Didn't applie, received"))
    ,financial_support_received = case_when(
      financial_support %in% c("Applied, received", "Didn't applie, received") ~ "Received"
      ,TRUE ~ "Didn't receive"
    )
    ,financial_support_applied= case_when(
      financial_support %in% c("Applied, received", "Applied, didn't receive") ~ "Applied"
      ,TRUE ~ "Didn't apply"
    )
    ,idp_pct = factor(idp_pct, labels = c("11-20%", "5-10%", "<5%", "None", ">20%")) %>% 
      factor(levels = c("None", "<5%", "5-10%", "11-20%", ">20%"))
    ,admin_services_stopped = recode(admin_services_stopped,"Ні" = 0, "Так" = 1)
    ,garbage_interruptions = case_when(
      garbage_interruptions == "Важко сказати" ~ NA_integer_
      ,garbage_interruptions == "Так" ~ 1L
      ,garbage_interruptions == "Ні"~ 0L)
    ,volunteers_share = case_when(
      hromada_name %ni% c("Боярська міська громада", "Городищенська сільська громада") ~ volunteers_number/total_population_2022 * 100
      ,TRUE ~ NA_real_
    )
    ,countries_count = replace(countries_count, is.na(countries_count), 0)
  )



#+ ----- vectors with predictors and outcomes ----------------------------------

outcomes_vars <- c(
  "budget_deficit_estimate" # дефіцит доходів бюджету станом на червень-липень 2022
  ,"budget_cut" # чи були скорочені видатки бюджету громади
  ,"budget_cut_pct" # обсяг скорочення бюджету відносно річного плану
  ,"financial_support" # чи подавалися та чи отримували фінансову допомогу
  ,"financial_support_received" # чи отримували фінасову допомогу
  ,"financial_support_applied" # чи подавалися на фінасову допомогу
  ,"idp_number" # к-сть прийнятих переселенців
  ,'idp_pct' # % прийнятих переселенців
  ,'admin_services_stopped' # громада втрачала можливість надання адмін послуг
  ,'garbage_interruptions' # чи була зупинка вивозу сміття
  ,'volunteers_share' # частка волонтерів від усього населення громади
  # ,'humanitarian_hubs'
  ,"ingo_count" # кількість міжнародних організацій, що надавали допомогу громаді
  ,"countries_count" #кількість країн, що надавали допомогу громаді
  ,"enterprises_relocated" #кількість релокованих підприємств
)


predictor_vars_continuous <- c(
  "income_own_per_capita"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita" # що надходить від держави, 
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,'dfrr_executed' # сума всіх проектів (скільки дали на розвиток громади - спец прокти), виграні інвестиційні проекти, на скільки г. залучила інвест кошти в рамках програми
  # ,'passangers_2021'
  ,"dfrr_executed_20_21" #сума проектів ДФРР в 2020-2021 роках
  ,"expenses_local_government_2021" #відсоток видатків на місцеве самоврядування
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ - не можна використовувати через неправильні заміни на 0 в сільських
  ,"turnout_2020" # явка
  ,'area' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,"distance_to_russia_belarus"
  ,"distance_to_russia"
  ,"distance_to_eu"
  ,"hromadas_30km_russia_belarus"
  ,"hromadas_30km_from_border"
  ,"buffer_nat_15km"
  ,"buffer_int_15km"
  ,"working_age_pct_declarations"
  ,"youth_pct_declarations"
  ,"n_agreements" #кількість договогорів про співробітництво за весь час, укладених до 24.02.2022
  ,"n_agreements_active" #кількість активних договогорів про співробітництво станом на 24.02.2022
  ,"n_agreements_hromadas" #кільсть унікальних громад, з якими були заключені договори про співробітництво за весь час, укладені до 24.02.2022
  ,"n_agreements_hromadas_active" #кільсть унікальних громад, з якими були заключені договори про співробітництво, активні станом на 24.02.2022
)

predictor_vars_continuous_new <- c(
  "income_own_per_capita"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita" # що надходить від держави,  
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,'dfrr_executed_k_zeros' # сума всіх проектів (скільки дали на розвиток громади - спец прокти), виграні інвестиційні проекти, на скільки г. залучила інвест кошти в рамках програми
  # ,'passangers_2021'
  ,"dfrr_executed_20_21_k_zeros"
  ,"dfrr_executed_per_capita_zeros"
  ,"dfrr_executed_20_21_per_capita_zeros"
  ,"expenses_local_government_2021" #відсоток видатків на місцеве самоврядування
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time_h" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ - не можна використовувати через неправильні заміни на 0 в сільських
  ,"turnout_2020" # явка
  ,'area_log' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th_years" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,"distance_to_russia_belarus_100"
  ,"distance_to_russia_100"
  ,"distance_to_eu_100"
  ,"hromadas_30km_russia_belarus"
  ,"hromadas_30km_from_border"
  ,"buffer_nat_15km"
  ,"buffer_int_15km"
  ,"working_age_pct_declarations"
  ,"youth_pct_declarations"
  ,"n_agreements" #кількість договогорів про співробітництво за весь час, укладених до 24.02.2022
  ,"n_agreements_active" #кількість активних договогорів про співробітництво станом на 24.02.2022
  ,"n_agreements_hromadas" #кільсть унікальних громад, з якими були заключені договори про співробітництво за весь час, укладені до 24.02.2022
  ,"n_agreements_hromadas_active" #кільсть унікальних громад, з якими були заключені договори про співробітництво, активні станом на 24.02.2022
  )


# Categorical - for color
predictor_vars_categorical <- c(
  "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
  ,"oblast_significance" #contains cities of oblast significance (before 2020)
  ,'incumbent'
  # ,'rda' # too small variation
  ,'not_from_here'
  # ,'enterpreuner' # too small variation
  # ,'unemployed' # too small variation
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_20_06_2022'
  ,'train_station'
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  # ,'city'
  ,'youth_centers' # наявність молодіжних центрів
  ,'youth_councils' # наявність молодіжних рад
  ,'business_support_centers' # наявність центру підтримки бізнесу
  ,"occupation_b"
  ,"creation_year"
  )

predictor_vars_categorical_new <- c(
  "sex_male"
  ,"education_higher"
  ,"type_city"
  ,"type_settlement"
  ,"type_village"
  ,"voluntary"
  ,"oblast_significance" #contains cities of oblast significance (before 2020)
  ,"region_en_Center"
  ,"region_en_West"
  ,"region_en_East"
  ,"region_en_North"
  ,"region_en_South"
  ,'incumbent'
  # ,'rda' # too small variation
  ,'not_from_here'
  # ,'enterpreuner' # too small variation
  # ,'unemployed' # too small variation
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_20_06_2022'
  ,'train_station'
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  ,'youth_centers_b' # наявність молодіжних центрів
  ,'youth_councils_b' # наявність молодіжних рад
  ,'business_support_centers_b' # наявність центру підтримки бізнесу
  ,"occupation_b"
  ,"pioneer_hromadas"
  ,"dfrr_binary"
  ,"dfrr_20_21_binary"
)

predictor_vars <- c(
  predictor_vars_continuous
  ,predictor_vars_categorical
)


#+ - tweak-data-2 --------------------------------------------------------------


ds1 <- 
  ds0 %>% 
  select(hromada_code, all_of(predictor_vars), oblast_name_en, hromada_name, 
         all_of(outcomes_vars)
  ) %>% 
  # scaling 
  mutate(
    income_own_per_capita_k = income_own_per_capita/1000
    ,income_total_per_capita_k = income_total_per_capita/1000
    ,income_tranfert_per_capita_k = income_tranfert_per_capita/1000
    ,time_before_24th_years = time_before_24th/365
    ,dfrr_executed_k = dfrr_executed/1000
    ,dfrr_executed_per_capita = dfrr_executed/total_population_2022
    ,dfrr_executed_20_21_k = dfrr_executed_20_21/1000
    ,dfrr_executed_20_21_per_capita = dfrr_executed_20_21/total_population_2022
    # ,relocated_companies_number = as.numeric(relocated_companies_text)
    # ,international_projects_number = as.numeric(international_projects)
    # ,no_school_days_number = as.numeric(no_school_days_coded)
    ,urban_perc_100 = urban_pct * 100
    ,idp_number_log = log(idp_number)
    ,idp_prop = idp_number/total_population_2022 * 100
    # ,dftg_creation_time = as.numeric(floor(difftime(dftg_creation_date, "2021-12-29", unit = "day")))
    # ,dftg_creation_time_na = ifelse(dftg_creation_time < 0, NA_integer_, dftg_creation_time)
    ,pioneer_hromadas = ifelse(creation_year %in% c(2015, 2016), 1, 0)
    ,travel_time_h = travel_time/60
    ,distance_to_russia_belarus_100 = distance_to_russia_belarus/100
    ,distance_to_russia_100 = distance_to_russia/100
    ,distance_to_eu_100 = distance_to_eu/100
    ,area_log = log(area)
  )  %>% 
  # zero filling NAs
  mutate(
    dfrr_executed_k_zeros = replace_na(dfrr_executed_k, 0)
    ,dfrr_executed_20_21_k_zeros = replace_na(dfrr_executed_20_21_k, 0)
    ,dfrr_executed_per_capita_zeros = replace_na(dfrr_executed_per_capita, 0)
    ,dfrr_executed_20_21_per_capita_zeros = replace_na(dfrr_executed_20_21_per_capita, 0)
    # ,passengers_2021_zeros = replace_na(passangers_2021, 0)
    # ,sum_osbb_2020_zeros = replace_na(sum_osbb_2020, 0)
    ,dfrr_binary = ifelse(is.na(dfrr_executed) | dfrr_executed == 0, 0, 1)
    ,dfrr_20_21_binary = ifelse(is.na(dfrr_executed_20_21) | dfrr_executed_20_21 == 0, 0, 1)
  ) %>%
  # making binary vars where not enough variation
  mutate(
    business_support_centers_b = ifelse(business_support_centers == 0, 0, 1)
    ,youth_centers_b = ifelse(youth_centers == 0, 0, 1)
    ,youth_councils_b =ifelse(youth_councils == 0, 0, 1)
    ,city = factor(ifelse(type == 'міська', 1, 0))
    ,financial_support_applied = ifelse(financial_support_applied == 'Applied', 1, 0)
    ,financial_support_received = ifelse(financial_support_received == 'Received', 1, 0)
    ,enterprises_relocated_b = ifelse(enterprises_relocated == 0, 0, 1)
    # ,hromada_exp_b = ifelse(hromada_exp == 'yes', 1, 0)
    # ,dftg_creation_b = ifelse(dftg_creation == 'yes', 1, 0)
  ) %>%
  #making dummy variables
  mutate(
    sex_male = ifelse(sex_head == "male", 1, 0)
    ,education_higher= ifelse(education_head == "higher", 1, 0)
    ,type = case_when(
      type == "міська" ~ "city"
      ,type == "селищна" ~ "settlement"
      ,type == "сільська" ~ "village"
    )
  ) %>% 
  dummy_cols(c("region_en", "type"), remove_selected_columns = T ) %>% 
  # mutate(
  #   across(
  #     .cols = all_of(predictor_vars_categorical_new)
  #     ,.fns = ~factor(.)
  #   )
  # ) %>% 
  mutate(country = "Ukraine")



g <- ggplot(ds1, aes(x = urban_pct, y = turnout_2020)) +
  geom_point() +
  geom_smooth(se=F) + 
  theme_bw()

g %>% quick_save("% urban vs turnout in 2020",w=16,h=9)

cor(ds1$urban_pct, ds1$turnout_2020, use = "complete.obs")
# turnoute rate decreses with increase in urbanization


#TO-DO - exclude hromadas with the higherst dispcrepancies between declarative % of 
#IDPs and actual percentage
ds2 <- ds1 %>% 
  select(hromada_name, total_population_2022, idp_number, idp_pct) %>%  
  mutate(
    idp_pct_calc = idp_number/total_population_2022 * 100
    ,idp_pct_group = cut(idp_pct_calc,
                         breaks = c(-1, 0, 5, 10, 20, 1000)
                         ,labels = c("None", "<5%", "5-10%", "11-20%", ">20%"))
    ,diff = ifelse(idp_pct == idp_pct_group, 0, 1)
    )

# ds1 %>% select(all_of(outcomes_vars)) %>% explore::describe_all() %>%neat_DT()
# 
# ds1 %>% select(all_of(outcomes_vars)) %>% GGally::ggpairs()
# 
# ds1 %>% sapply(outcomes_vars, FUN=plot)
# 
# par(mfrow = c(4, 3))
# 
# lapply(ds1[outcomes_vars], FUN=hist)
# 
# ggplot(reshape2::melt(ds1[outcomes_vars_new]),aes(x=value)) + geom_histogram() + facet_wrap(~variable, scales = 'free')
# 
# 


# ---- tableone------------------------------------------------------------
library(tableone)

cols <- colnames(ds_general) %>% setdiff(c("raion_code", "raion_name", "oblast_code", "oblast_name", "hromada_full_name"))


ds_table <- ds_survey %>% 
  select(cols) %>% 
  mutate(strata = "agro_survey") %>% 
  rbind(
    ds_general %>%
      select(-c("raion_code", "raion_name", "oblast_code", "oblast_name", "hromada_full_name")) %>% 
      mutate(strata = "all") %>% 
      filter(occipied_before_2022 == 0)
  ) %>% 
  select(-c("hromada_code", "hromada_name", "hromada_center_code", "hromada_center", "lat_center",
            "lon_center", "geometry", "budget_code","budget_name","oblast_name_en",                    
            "region_code_en", "party", "creation_date", "occipied_before_2022"))

table_pre <- CreateTableOne(vars= colnames(ds_table)
                            ,factorVars = c("sex_head", "education_head", "type", "creation_year","region_en")
                            , strata = "strata"
                            , data=ds_table)

p <- print(table_pre, smd=T, printToggle = FALSE, noSpaces = TRUE)
kable(p, format = "html")

stargazer(p, type = "html",  out = 'table1.html')




# ---- military-actions------------------------------------------------------------

cor_mat <- 
  cor(ds1 %>% 
        select(predictor_vars_continuous_new, predictor_vars_categorical_new)
      ,use = "complete.obs")

p <- corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 0.5, addCoef.col = "black", number.cex=0.5, order = "FPC")

p %>% quick_save("cor_predictors", w= 12, h = 7)

dev.off()


# ---- descriptive-statistics------------------------------------------------------------

ds1 %>% select(admin_services_stopped, garbage_interruptions) %>% summary()
 

predictor_vars_final <- 
  ds1 %>% 
  select(predictor_vars_continuous_new, predictor_vars_categorical_new) %>% 
  select(-c(income_tranfert_per_capita, transfert_prop_2021, war_zone_20_06_2022, sum_osbb_2020,
            n_settlements, time_before_24th_years, not_from_here, voluntary, buffer_int_15km, buffer_nat_15km,
            train_station, occupation_b, pioneer_hromadas), 
         -contains("dfrr"), -contains("edem")) %>% 
  colnames()


predictors_desc <- ds1 %>% select(predictor_vars_final) %>%
  skimr::skim() %>% 
  select(skim_variable, n_missing, numeric.mean, numeric.sd) %>% 
  mutate(n_missing = 477-n_missing, numeric.mean = round(numeric.mean, 2), numeric.sd = round(numeric.sd, 2)) %>% 
  rename(obs = n_missing)

openxlsx::write.xlsx(predictors_desc, "./analysis/agro_predictors_descriptive.xlsx")

ds1 %>% select(area) %>%   skimr::skim() %>% 
  mutate(numeric.mean = round(numeric.mean, 4), numeric.sd = round(numeric.sd, 4)) %>% View()



#--- suspension-admin------------------------------------------------------

# grid <- seq(from = log(0.0001), to = log(1), length.out = 100)
# grid <- exp(grid)
grid <- 10^seq(10, -2, length = 100)


###suspension of admin services

set.seed(1)
x <- ds1 %>% select(predictor_vars_continuous_new, admin_services_stopped, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(-admin_services_stopped) %>% 
  # mutate_all(~as.numeric(.)) %>% 
  as.matrix()

y <- ds1 %>% 
  select(predictor_vars_continuous_new, admin_services_stopped, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(admin_services_stopped) %>% 
  pull()

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

#made model based on the train data
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)

#generate min lambda 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, family="binomial")
bestlam <- cv.out$lambda.min

#predict values of the test data based on the train model and min lambda
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)


#create new model based on the full data
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
nonzero_coef <- which(lasso.coef != 0, arr.ind=TRUE)
nonzero_coef_matrix <- cbind(nonzero_coef, lasso.coef[nonzero_coef])[,3]


d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'admin_services_stopped'
    # ,depdist = "gaussian"
    # ,depdist = "poisson"
    ,depdist = "binomial"
    ,explantory_continous = predictor_vars_continuous_new
    ,confounder = c("type_city", "war_zone_20_06_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
d %>% plot_complex_scan()


m_admin <- glm(data = ds1, 
               admin_services_stopped ~ voluntary + type_city + war_zone_20_06_2022, family = "binomial")

summary(m_admin)


plot(fitted(m_admin), resid(m_admin))



library(purrr)
library(broom)
library(lmtest)
library(sandwich)

gen_model_admin <- function(var) {
  form = paste("admin_services_stopped ~ type_city + war_zone_20_06_2022 +", var)
  tidy(glm(as.formula(form), data = ds1, family = "binomial")) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}

gen_model_admin_robust <- function(var) {
  form = paste("admin_services_stopped ~ type_city + war_zone_20_06_2022 +", var)
  mod <- glm(as.formula(form), data = ds1, family = "binomial")
  tidy(coeftest(mod, vcov = vcovHC(mod, type = 'HC3'))) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}

admin_results <- map(c(predictor_vars_categorical_new, predictor_vars_continuous_new), gen_model_admin_robust) %>% 
  purrr::reduce(rbind) %>% 
  filter(!(term %in% c("(Intercept)", "type_city", "war_zone_20_06_2022")))


m1_admin <- glm(data = ds1, admin_services_stopped ~  type_city + war_zone_20_06_2022 + occupation_b, family = "binomial")
m2_admin <- glm(data = ds1, admin_services_stopped ~ region_en_North + type_city + war_zone_20_06_2022, family = "binomial")
m3_admin <- glm(data = ds1, admin_services_stopped ~ hromadas_30km_russia_belarus + type_city + war_zone_20_06_2022, family = "binomial")
m4_admin <- glm(data = ds1, admin_services_stopped ~ distance_to_russia_belarus + type_city + war_zone_20_06_2022, family = "binomial")
m5_admin <- glm(data = ds1, admin_services_stopped ~ n_agreements_hromadas + type_city + war_zone_20_06_2022, family = "binomial")
m6_admin <- glm(data = ds1, admin_services_stopped ~ n_agreements_hromadas_active + type_city + war_zone_20_06_2022, family = "binomial")


plot(fitted(m2_admin), resid(m2_admin))

plot(m6_admin)
summary(m1_admin)
coeftest(m1_admin, vcov = vcovHC(m1_admin, type = 'HC3'))

#m5 has some decrease in z value with robust standard errors, but m6 has an increase


#occupation status in addition to  war zone
gen_model_admin_robust_v2 <- function(var) {
  form = paste("admin_services_stopped ~ type_city + war_zone_20_06_2022 + occupation_b +", var)
  mod <- glm(as.formula(form), data = ds1, family = "binomial")
  tidy(coeftest(mod, vcov = vcovHC(mod, type = 'HC3'))) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}

admin_results_2 <- map(c(predictor_vars_categorical_new, predictor_vars_continuous_new), gen_model_admin_robust_v2) %>% 
  purrr::reduce(rbind) %>% 
  filter(!(term %in% c("(Intercept)", "type_city", "war_zone_20_06_2022", "occupation_b")))



# glm(data = ds1, admin_services_stopped ~ edem_petitions + type_city + war_zone_20_06_2022, family = "binomial")



#--- suspension-garbage------------------------------------------------------

set.seed(1)
x <- ds1 %>% select(predictor_vars_continuous_new, garbage_interruptions, 
                    predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(-garbage_interruptions) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix()

y <- ds1 %>% 
  select(predictor_vars_continuous_new, garbage_interruptions, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(garbage_interruptions) %>% 
  pull()

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

#made model based on the train data
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)

plot(lasso.mod)

#generate min lambda 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, family="binomial")
plot(cv.out)
bestlam <- cv.out$lambda.min

#predict values of the test data based on the train model and min lambda
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)


#create new model based on the full data
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
nonzero_coef <- which(lasso.coef != 0, arr.ind=TRUE)
nonzero_coef_matrix <- cbind(nonzero_coef, lasso.coef[nonzero_coef])[,3]

d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'garbage_interruptions'
    # ,depdist = "gaussian"
    # ,depdist = "poisson"
    ,depdist = "binomial"
    ,explantory_continous = predictor_vars_continuous_new
    ,confounder = c("type_city", "war_zone_20_06_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
d %>% plot_complex_scan()



gen_model_garbage <- function(var) {
  form = paste("garbage_interruptions ~ type_city + war_zone_20_06_2022 +", var)
  tidy(glm(as.formula(form), data = ds1, family = "binomial")) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}


gen_model_garbage_robust <- function(var) {
  form = paste("garbage_interruptions ~ type_city + war_zone_20_06_2022 +", var)
  mod <- glm(as.formula(form), data = ds1, family = "binomial")
  tidy(coeftest(mod, vcov = vcovHC(mod, type = 'HC3'))) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}


garbage_results <- map(c(predictor_vars_categorical_new, predictor_vars_continuous_new), gen_model_garbage_robust) %>% 
  purrr::reduce(rbind) %>% 
  filter(!(term %in% c("(Intercept)", "type_city", "war_zone_20_06_2022")))

m1_garbage <- glm(data = ds1, garbage_interruptions ~  type_city + war_zone_20_06_2022 + occupation_b, family = "binomial")
m2_garbage  <- glm(data = ds1, garbage_interruptions ~ region_en_North + type_city + war_zone_20_06_2022, family = "binomial")
m3_garbage  <- glm(data = ds1, garbage_interruptions ~ hromadas_30km_russia_belarus + type_city + war_zone_20_06_2022, family = "binomial")
m4_garbage  <- glm(data = ds1, garbage_interruptions ~ distance_to_russia_belarus + type_city + war_zone_20_06_2022, family = "binomial")

summary(m1_garbage)
coeftest(m1_garbage, vcov = vcovHC(m1_garbage, type = 'HC3'))

#add occupation status
gen_model_garbage_robust_v2 <- function(var) {
  form = paste("garbage_interruptions ~ type_city + war_zone_20_06_2022 + occupation_b +", var)
  mod <- glm(as.formula(form), data = ds1, family = "binomial")
  tidy(coeftest(mod, vcov = vcovHC(mod, type = 'HC3'))) %>% 
    mutate_at(vars(p.value, std.error, estimate), ~round(., 3))
}

garbage_results_2 <- map(c(predictor_vars_categorical_new, predictor_vars_continuous_new), gen_model_garbage_robust_v2) %>% 
  purrr::reduce(rbind) %>% 
  filter(!(term %in% c("(Intercept)", "type_city", "war_zone_20_06_2022", "occupation_b")))


#--- financial-support-applied-----------------------------------------------------

set.seed(1)
x <- ds1 %>% select(predictor_vars_continuous_new, predictor_vars_categorical_new, financial_support_applied) %>% 
  na.omit %>% 
  dplyr::select(-financial_support_applied) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix()

y <- ds1 %>% 
  select(predictor_vars_continuous_new, financial_support_applied, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(financial_support_applied) %>% 
  pull()

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

#made model based on the train data
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)

#generate min lambda 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, family="binomial")
bestlam <- cv.out$lambda.min

#predict values of the test data based on the train model and min lambda
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)


#create new model based on the full data
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
nonzero_coef <- which(lasso.coef != 0, arr.ind=TRUE)
cbind(nonzero_coef, lasso.coef[nonzero_coef])[,3]

d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'financial_support_applied'
    # ,depdist = "gaussian"
    # ,depdist = "poisson"
    ,depdist = "binomial"
    ,explantory_continous = predictor_vars_continuous_new
    ,confounder = c("type_city", "war_zone_20_06_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
d %>% plot_complex_scan()


m1_finsup <- glm(data = ds1, financial_support_applied ~ no_party + 
                   type_city + war_zone_20_06_2022, family = "binomial")

m2_finsup <- glm(data = ds1, financial_support_applied ~ voluntary +
                   type_city + war_zone_20_06_2022, family = "binomial")

m3_finsup <- glm(data = ds1, financial_support_applied ~ time_before_24th_years +
                   type_city + war_zone_20_06_2022, family = "binomial")

stargazer(m1_finsup, m2_finsup, m3_finsup, single.row = T, type = "text")



#---relocated-enterprises-----------------------------------------------------

set.seed(1)
x <- ds1 %>% select(predictor_vars_continuous_new, enterprises_relocated_b, 
                    predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(-enterprises_relocated_b) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix()

y <- ds1 %>% 
  select(predictor_vars_continuous_new, enterprises_relocated_b, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(enterprises_relocated_b) %>% 
  pull()

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

#made model based on the train data
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)


#generate min lambda 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, family="binomial")
bestlam <- cv.out$lambda.min

#predict values of the test data based on the train model and min lambda
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)


#create new model based on the full data
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
nonzero_coef <- which(lasso.coef != 0, arr.ind=TRUE)
cbind(nonzero_coef, lasso.coef[nonzero_coef])[,3]


d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'enterprises_relocated_b'
    # ,depdist = "gaussian"
    # ,depdist = "poisson"
    ,depdist = "binomial"
    ,explantory_continous = predictor_vars_continuous_new
    ,confounder = c("type_city", "war_zone_20_06_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
d %>% plot_complex_scan()


m1_relocated <- glm(data = ds1, enterprises_relocated_b ~ voluntary + war_zone_20_06_2022 * type_city,
               family = "binomial") 

m2_relocated <- glm(data = ds1, enterprises_relocated_b ~ time_before_24th_years + war_zone_20_06_2022 * type_city,
                    family = "binomial") 

stargazer(m1_relocated, m2_relocated, single.row = T, type = "text")




#---international-organisations-----------------------------------------------------

hist(ds1$ingo_count)

set.seed(1)
x <- ds1 %>% select(predictor_vars_continuous_new, ingo_count, 
                    predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(-ingo_count) %>% 
  mutate_all(~as.numeric(.)) %>% 
  as.matrix()

y <- ds1 %>% 
  select(predictor_vars_continuous_new, ingo_count, predictor_vars_categorical_new) %>% 
  na.omit %>% 
  dplyr::select(ingo_count) %>% 
  pull()

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

#made model based on the train data
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)

plot(lasso.mod)

#generate min lambda 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, family="poisson")
plot(cv.out)
bestlam <- cv.out$lambda.min

#predict values of the test data based on the train model and min lambda
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)


#create new model based on the full data
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
nonzero_coef <- which(lasso.coef != 0, arr.ind=TRUE)
cbind(nonzero_coef, lasso.coef[nonzero_coef])[,3]

d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'ingo_count'
    # ,depdist = "gaussian"
    ,depdist = "poisson"
    # ,depdist = "binomial"
    ,explantory_continous = predictor_vars_continuous_new
    ,confounder = c("type_city", "war_zone_20_06_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
d %>% plot_complex_scan()


#gaussian and neg binomial models
m1_ingo <- glm(data = ds1, enterprises_relocated_b ~ voluntary + 
                 war_zone_20_06_2022 + type_city, family = "binomial") 

m2_ingo <- glm(data = ds1, enterprises_relocated_b ~ time_before_24th_years + 
                 war_zone_20_06_2022 + type_city, family = "binomial") 

m3_ingo <- glm(data = ds1, enterprises_relocated_b ~ edem_total + 
                 war_zone_20_06_2022 + type_city, family = "binomial") 


stargazer(m1_ingo, m2_ingo, m3_ingo, single.row = T, type = "text")


#IDPs
idp_outliers <- ds1 %>%
  mutate(
    idp_prop = idp_number/total_population_2022 * 100
    ,lower_bound = quantile(idp_prop, probs = 0.25, na.rm = T) - 1.5 * IQR(idp_prop, na.rm = T)
    ,upper_bound = quantile(idp_prop, probs = 0.75, na.rm = T) + 1.5 * IQR(idp_prop, na.rm = T)
  ) %>% 
  filter(idp_prop < lower_bound | idp_prop > upper_bound) %>% 
  select(hromada_code,hromada_name, oblast_name_en, idp_prop) 


qqnorm(log(ds1$idp_number))
qqline(log(ds1$idp_number))
shapiro.test(log(ds1$idp_number))





#+ - plot-linear-models-1 ------------------------------------------------------

d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'countries_count'
    # ,depdist = "binomial"
    # ,depdist = "nbinom"
    ,depdist = "gaussian"
    ,explantory_continous = predictor_vars_continuous_scaled_wo_na
    ,confounder = c("war_zone_20_06_2022", "total_population_2022")
    # ,confounder = c("voluntary")
    , explanatory_categorical = predictor_vars_categorical_new
  )
# d %>% neat_DT()
 d %>% plot_complex_scan()

p %>% quick_save("idp number with control of total population",w=10,h=9)


# What hromada is outlier on total population?
outliers_total_population_2022 <- 
  ds1 %>%
  filter(total_population_2022 > 200000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_sum_osbb_2020 <- 
  ds1 %>%
  filter(sum_osbb_2020 > 190) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)
outliers_passangers_2021 <- 
  ds1 %>%
  filter(passangers_2021 >60000) %>% 
  # select(hromada_code,hromada_name, oblast_name_en) %>% 
  pull(hromada_code)

outliers <- 
  c(outliers_total_population_2022
    ,outliers_sum_osbb_2020
    ,outliers_passangers_2021) %>% 
  unique() 
outliers %>% length()


#+ ---- lasso --------------------------------------------------------------




# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/survey-prep-model/survey-prep-model.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
