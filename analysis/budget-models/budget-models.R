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

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R")             # basics
base::source("./scripts/graphing/graph-presets.R")       # font size, colors etc
base::source("./scripts/operational-functions.R")        # quick specific functions
base::source("./scripts/binary-categorical-functions.R") # graphing and modeling
source("./analysis/survey-prep-model/custom-model-functions.R")

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/budget-models/prints")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

data_cache_folder <- prints_folder # to sink modeling steps
# ---- declare-functions -------------------------------------------------------
'%ni%' <- Negate(`%in%`)

# ---- load-data ---------------------------------------------------------------
# the product of ./manipulation/ellis-general.R
d <- readr::read_csv("./data-private/derived/full_dataset.csv")
d <- d %>% 
  rename("YoY_mar_may" = "own_income_no_mil_change_YoY_mar_may_2021",
         "YoY_jan_feb" = "own_income_no_mil_change_YoY_jan_feb_2021",
         "YoY_mar_apr" = "own_income_no_mil_change_YoY_mar_apr_2021",
         "YoY_jun_aug" = "own_income_no_mil_change_YoY_jun_aug_2021",
         "YoY_adapt" = "own_income_no_mil_change_YoY_adapt_2021")

d <- d %>% mutate(income_own_2021_per_capita = income_own_2021/total_population_2022,
                  dfrr_executed_per_capita = dfrr_executed / total_population_2022,
                  dfrr_executed_corr = ifelse(is.na(dfrr_executed), 0, dfrr_executed),
                  dfrr_executed_corr_per_capita = dfrr_executed_corr / total_population_2022)

cor_mat_full <- 
  cor(d %>% 
        select(YoY_jun_aug,
               YoY_mar_apr ,
               
               
               total_population_2022 ,
               urban_pct , 
               area ,
               travel_time ,
               voluntary ,
               time_before_24th ,
               n_settlements ,
               
               distance_to_russia_belarus ,
               war_zone_20_06_2022 ,
               
               own_income_prop_2021 ,
               income_own_2021_per_capita,
               income_own_2021 ,
               dfrr_executed_per_capita,
               dfrr_executed ,    # a lot of NA
               dfrr_executed_corr_per_capita,
               train_station ,
               working_age_pct_declarations , #  a lot of NA
               
               youth_councils ,
               youth_centers ,
               business_support_centers ,
               age_head ,
               incumbent ,
               polit_work ,
               rda ,
               turnout_2020 ,
               edem_total ,
               n_agreements_hromadas_active)
      ,use = "complete.obs")

cor_mat <- 
  cor(d %>% 
        select(area,
               total_population_2022,
               urban_pct,n_settlements,
               own_income_prop_2021,
               YoY_jan_feb,
               YoY_mar_apr,
               YoY_mar_may,
               YoY_jun_aug,
               dfrr_executed,
               turnout_2020,
               incumbent
              )
      ,use = "complete.obs")
corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1, addCoef.col = "black", number.cex=1, order = "FPC")

d %>% select(train_station) %>% summary() 
a <- d %>% select(YoY_jun_aug,
                  YoY_mar_apr ,
                    
                    
                    total_population_2022 ,
                    urban_pct , 
                    area ,
                    travel_time ,
                    voluntary ,
                    time_before_24th ,
                    n_settlements ,
                    
                    region_en ,
                    distance_to_russia_belarus ,
                    war_zone_20_06_2022 ,
                    
                    own_income_prop_2021 ,
                    income_own_2021 ,
                    dfrr_executed ,    # a lot of NA
                    train_station ,
                    working_age_pct_declarations , #  a lot of NA
                    
                    youth_councils ,
                    youth_centers ,
                    business_support_centers ,
                    sex_head ,
                    age_head ,
                    education_head , 
                    incumbent ,
                    polit_work ,
                    rda ,
                    turnout_2020 ,
                    edem_total ,
                    n_agreements_hromadas_active) %>%
  summarise_all(funs(sum(is.na(.))))

ols_1 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              log(total_population_2022) +
              urban_pct + 
              area + 
              travel_time +
              voluntary +
              time_before_24th +
              n_settlements + 
              
              region_en + 
              distance_to_russia_belarus +

              own_income_prop_2021 +
              log(income_own_2021) +
              log(dfrr_executed + 1) +  #a lot of NA
              train_station+
              working_age_pct_declarations + # a lot of NA
              
              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
                          )

ols_2 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              log(total_population_2022) +
              urban_pct + 
              area + 
              travel_time +
              voluntary +
              time_before_24th +
              n_settlements + 
              
              region_en + 
              distance_to_russia_belarus +

              own_income_prop_2021 +
              log(income_own_2021) +
              train_station+

              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
)

ols_3 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              urban_pct + 
              area + 
              travel_time +
              time_before_24th +

              region_en + 
              distance_to_russia_belarus +
              
              log(income_own_2021_per_capita) +
              train_station+
              
              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
)

stargazer::stargazer(ols_1, ols_2, ols_3, single.row = T, type = 'html', out = './analysis/budget-models/revenue_fall.html')


ols_4 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              log(total_population_2022) +
              urban_pct + 
              area + 
              travel_time +
              voluntary +
              time_before_24th +
              n_settlements + 
              
              region_en + 
              distance_to_russia_belarus +
              
              own_income_prop_2021 +
              log(income_own_2021) +
              dfrr_executed_corr_per_capita +  #a lot of NA
              train_station+
              working_age_pct_declarations + # a lot of NA
              
              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
)

ols_5 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              log(total_population_2022) +
              urban_pct + 
              area + 
              travel_time +
              voluntary +
              time_before_24th +
              n_settlements + 
              
              region_en + 
              distance_to_russia_belarus +
              
              own_income_prop_2021 +
              log(income_own_2021) +
              dfrr_executed_corr_per_capita +
              train_station+
              
              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
)

ols_6 <- lm(data = d,
            YoY_jun_aug ~ YoY_mar_apr +
              
              
              urban_pct + 
              area + 
              travel_time +
              time_before_24th +
              
              region_en + 
              distance_to_russia_belarus +
              
              log(income_own_2021_per_capita) +
              dfrr_executed_corr_per_capita +
              train_station+
              
              youth_councils + 
              youth_centers + 
              business_support_centers +
              sex_head +
              age_head +
              education_head + 
              incumbent +
              polit_work +
              rda +
              turnout_2020 +
              edem_total +
              n_agreements_hromadas_active 
)

stargazer::stargazer(ols_4, ols_5, ols_6, single.row = T, type = 'html', out = './analysis/budget-models/revenue_fall.html')
