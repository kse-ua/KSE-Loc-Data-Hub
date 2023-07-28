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
library(MASS)
library(modelsummary)

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
  rename("YoY_mar_may" = "own_income_no_mil_change_YoY_mar_may",
         "YoY_jan_feb" = "own_income_no_mil_change_YoY_jan_feb",
         "YoY_mar_apr" = "own_income_no_mil_change_YoY_mar_apr",
         "YoY_jun_aug" = "own_income_no_mil_change_YoY_jun_aug",
         "YoY_jul_sep" = "own_income_no_mil_change_YoY_jul_sep",
         "YoY_oct_jan" = "own_income_no_mil_change_YoY_oct_jan",
         "YoY_oct_dec" = "own_income_no_mil_change_YoY_oct_dec",
         "YoY_may_feb" = "own_income_no_mil_change_YoY_may_feb",
         "YoY_adapt" = "own_income_no_mil_change_YoY_adapt",
         "income_own_2021" = "income_own_2021",
         "income_own_full_year_2021" = "income_own_full_year_2021")

options("scipen"=100, "digits"=4)

d <- d %>% mutate(income_own_2021_per_capita = income_own_full_year_2021*1000/total_population_2022,
                  dfrr_executed_per_capita = dfrr_executed / total_population_2022,
                  dfrr_executed_corr = ifelse(is.na(dfrr_executed), 0, dfrr_executed),
                  dfrr_executed_corr_per_capita = dfrr_executed_corr / total_population_2022,
                  creation_year = as.factor(creation_year),
                  pioneer = case_when(creation_year == "2015" | creation_year == "2016" ~ 1, TRUE ~ 0),
                  dfrr_executed_20_21_cat = case_when(dfrr_executed_20_21 > 0 ~ 1,
                                                      dfrr_executed_20_21 == 0 ~ 0,
                                                      is.na(dfrr_executed_20_21) ~ 0,
                                                      TRUE ~ 0),
                  sum_osbb_2020_corr = ifelse(is.na(sum_osbb_2020), 0, sum_osbb_2020),
                  Status_war_sept_ext = as.factor(Status_war_sept_ext),
                  Status_war_sept_ext = relevel(Status_war_sept_ext, ref = "not occupied"),
                  Status_war_sept = as.factor(Status_war_sept_ext),
                  Status_war_sept = relevel(Status_war_sept, ref = "not occupied"),
                  recovery_score = case_when(recovery_month_distance == 0 ~ 13,
                                             recovery_month_distance == 1~ 12,
                                             recovery_month_distance == 2~ 11,
                                             recovery_month_distance == 3~ 10,
                                             recovery_month_distance == 4~ 9,
                                             recovery_month_distance == 5~ 8,
                                             recovery_month_distance == 6~ 7,
                                             recovery_month_distance == 7~ 6,
                                             recovery_month_distance == 8~ 5,
                                             recovery_month_distance == 9~ 4,
                                             recovery_month_distance == 10~ 3,
                                             recovery_month_distance == 11~ 2,
                                             recovery_month_distance == 12~ 1,
                                             recovery_month_distance >= 13 |
                                               is.na (recovery_month_distance) ~ 0
                  ),
                  recovery_score_factor =  factor(recovery_score),
                  sex_head_0_1 = case_when(sex_head == "male" ~ 0,
                                           sex_head == "female" ~ 1),
                  recovery_count_score = case_when(
                                                   count_recovery == 1~ 1,
                                                   count_recovery == 2~ 2,
                                                   count_recovery == 3~ 3,
                                                   count_recovery == 4~ 4,
                                                   count_recovery == 5~ 5,
                                                   count_recovery == 6~ 6,
                                                   count_recovery == 7~ 7,
                                                   count_recovery == 8~ 8,
                                                   count_recovery == 9~ 9,
                                                   count_recovery == 0 |
                                               is.na (recovery_month_distance) ~ 0
                  ),
                  recovery_count_score_factor =  factor(recovery_count_score),
                  
                  recovery_distance_2_factor =  factor(recovery_month_distance_2),
                  recovery_distance_2_factor_alt = factor(case_when(is.na(recovery_month_distance_2) ~ 0,
                                                                !is.na(recovery_month_distance_2) ~ 1)))
levels(d$recovery_score_factor) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")
levels(d$recovery_count_score_factor) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
levels(d$recovery_distance_2_factor) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

fast_recovery <- subset(d, recovery_month_distance <= 2)
mid_recovery <- subset(d, recovery_month_distance <= 6 & recovery_month_distance > 2)
long_recovery <- subset(d, recovery_month_distance <= 12 & recovery_month_distance > 6)
no_recovery <- subset(d, recovery_month_distance >= 13 |
                        is.na (recovery_month_distance))

toh_before_22 <- c("0556100000","0555600000","1253800000","0555500000","1253400000"
                   ,"0554900000","0555700000","0555100000","1253900000","0554700000","0554800000"
                   ,"0556300000","1253700000","1254000000","0556000000","1253300000","0555200000"
                   ,"0555400000","0556400000","1253200000","1254100000","0556200000","1253500000"
                   ,"0556600000","1253100000","0556500000","0555900000","0555800000","0555000000"
                   ,"1253600000","0555300000") 
'%nin%' <- Negate('%in%') 
non_occupied_2021 <- d %>% filter(budget_code %nin% toh_before_22)

summary(non_occupied_2021$base_subsidy_prop_2021)
nrow(subset(non_occupied_2021, base_subsidy_prop_2021 <= 0.05))/nrow(non_occupied_2021)

nrow(subset(non_occupied_2021, base_subsidy_prop_2021 > 0.05 & 
              base_subsidy_prop_2021 <= 0.1))/nrow(non_occupied_2021)

nrow(subset(non_occupied_2021, base_subsidy_prop_2021 > 0.1 & 
              base_subsidy_prop_2021 <= 0.15))/nrow(non_occupied_2021)

nrow(subset(non_occupied_2021, base_subsidy_prop_2021 > 0.15 & 
              base_subsidy_prop_2021 <= 0.2))/nrow(non_occupied_2021)

nrow(subset(non_occupied_2021, base_subsidy_prop_2021 > 0.2 & 
              base_subsidy_prop_2021 <= 0.25))/nrow(non_occupied_2021)

nrow(subset(non_occupied_2021, base_subsidy_prop_2021 > 0.25))/nrow(non_occupied_2021)

non_occupied_2021 %>% filter(is.na(base_subsidy_prop_2021))

recovery_types <- d %>% mutate(recovery_type = as.factor(case_when(recovery_month_distance <= 2 ~ "fast_recovery",
                                                                   recovery_month_distance <= 6 & recovery_month_distance > 2 ~ "mid_recovery",
                                                                   recovery_month_distance <= 12 & recovery_month_distance > 6 ~ "long_recovery",
                                                                   recovery_month_distance >= 13 |
                                                                     is.na (recovery_month_distance) ~ "no_recovery"))) %>%
  group_by(recovery_type) %>% 
  summarise(YoY_jul_sep = mean(YoY_jul_sep, na.rm = TRUE),
            YoY_mar_apr = mean(YoY_mar_apr, na.rm = TRUE),
            
            
            total_population_2022 = mean(total_population_2022, na.rm = TRUE),
            urban_pct = mean(urban_pct, na.rm = TRUE),
            area = mean(area, na.rm = TRUE),
            travel_time = mean(travel_time, na.rm = TRUE),
            voluntary = mean(voluntary, na.rm = TRUE),
            pioneer = mean(pioneer, na.rm = TRUE),
            time_before_24th = mean(time_before_24th, na.rm = TRUE),
            n_settlements = mean(n_settlements, na.rm = TRUE),
            
            distance_to_russia_belarus = mean(distance_to_russia_belarus, na.rm = TRUE),
            
            own_income_prop_full_year = mean(own_income_prop_full_year, na.rm = TRUE),
            income_own_2021 = mean(income_own_2021, na.rm = TRUE),
            train_station = mean(train_station, na.rm = TRUE),
            diversification_income_score = mean(diversification_income_score, na.rm = TRUE),
            
            youth_councils = mean(youth_councils, na.rm = TRUE),
            youth_centers = mean(youth_centers, na.rm = TRUE),
            business_support_centers = mean(business_support_centers, na.rm = TRUE),
            age_head = mean(age_head, na.rm = TRUE),
            incumbent = mean(incumbent, na.rm = TRUE),
            polit_work = mean(polit_work, na.rm = TRUE),
            rda = mean(rda, na.rm = TRUE),
            turnout_2020 = mean(turnout_2020, na.rm = TRUE),
            edem_total = mean(edem_total, na.rm = TRUE),
            n_agreements_hromadas_active = mean(n_agreements_hromadas_active, na.rm = TRUE))



cor_mat_full <- 
  cor(d %>% 
        dplyr::select(YoY_jun_aug,
               YoY_mar_apr ,
               YoY_oct_jan,
               YoY_oct_dec,
               YoY_may_feb,
               
               recovery_month_distance,
               count_recovery,
               
               pioneer,
               total_population_2022 ,
               urban_pct , 
               area ,
               travel_time ,
               voluntary ,
               time_before_24th ,
               n_settlements ,
               
               distance_to_russia_belarus ,
               war_zone_20_06_2022 ,
               
               diversification_income_score,
               own_income_prop_full_year ,
               income_own_2021_per_capita,
               income_own_full_year_2021 ,
               dfrr_executed_per_capita,
               dfrr_executed ,    # a lot of NA
               dfrr_executed_corr_per_capita,
               dfrr_executed_20_21_cat,
               train_station ,
               working_age_pct_declarations , #  a lot of NA
               expenses_capital_2021,
               expenses_local_government_2021,
               expenses_state_functions_2021,
               
               youth_councils ,
               youth_centers ,
               business_support_centers ,
               age_head ,
               sex_head_0_1, 
               incumbent ,
               polit_work ,
               rda ,
               enterpreuner ,
               turnout_2020 ,
               edem_total ,
               n_agreements_hromadas_active,
               sum_osbb_2020_corr)
      ,use = "complete.obs")

openxlsx::write.xlsx(as.data.frame(cor_mat_full), './analysis/budget-models/cor_mat_full_budget_model.xlsx')

cor_mat <- 
  cor(d %>% dplyr::
        select(area,
               total_population_2022,
               urban_pct,n_settlements,
               own_income_prop_full_year,
               recovery_month_distance,
               diversification_income_score,
               YoY_jan_feb,
               YoY_mar_apr,
               YoY_mar_may,
               YoY_jun_aug,
               YoY_jul_sep,
               dfrr_executed,
               turnout_2020,
               incumbent,
               pioneer
              )
      ,use = "complete.obs")
corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1, addCoef.col = "black", number.cex=1, order = "FPC")

cor_mat_1 <-   cor(d %>% filter(Status_war_sept_ext != "occupied") %>% dplyr::
        select(diversification_income_score,
               pdfo_own_prop_2021,
               unified_tax_own_prop_2021,
               rent_own_prop_2021,
               corporate_tax_own_prop_2021,
               property_tax_own_prop_2021,
               parking_fee_own_prop_2021,
               tourist_fee_own_prop_2021,
               eco_tax_own_prop_2021,
               non_tax_own_prop_2021,
               capital_proceedings_own_prop_2021,
               special_funds_own_prop_2021,
               excise_duty_own_prop_2021,
               income_own_full_year_2021,
               income_own_2021_per_capita,
               own_income_prop_full_year,
               pdfo_prop_2021
        )
      ,use = "complete.obs")
corrplot::corrplot(cor_mat_1, tl.col = "black",tl.cex = 1,  number.cex=1, order = "FPC")


a <- d %>% dplyr::select(YoY_jun_aug,
                  YoY_mar_apr ,
                  YoY_jul_sep,
                  YoY_oct_jan,
                  YoY_oct_dec,
                  YoY_may_feb,
                  recovery_month_distance,
                  recovery_score,
                  recovery_score_factor,
                  count_recovery,
                  recovery_distance_2_factor,
                    
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
                    
                  diversification_income_score,
                  own_income_prop_full_year ,
                  income_own_full_year_2021 ,
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


indep_vars <- "YoY_mar_apr +
                 
                 log(total_population_2022) + 
                 
                 urban_pct + 
                 area + 
                 travel_time +
                 pioneer +
                 
                 Status_war_sept_ext +
                 
                 log(income_own_full_year_2021) +
                 train_station+
                 log(diversification_income_score) +
                 expenses_local_government_2021+
                 expenses_capital_2021+
                 
                 youth_centers + 
                 turnout_2020 +
                 edem_total +
                 n_agreements_hromadas +
                 dfrr_executed_20_21_cat"

YOY_5_2 <- lm(data = subset(d, Status_war_sept_ext != "occupied"),
              as.formula(paste("YoY_may_feb", "~", indep_vars)) 
                 
)
stargazer::stargazer(YOY_5_2,
                     single.row = T, type = 'html', out = './analysis/budget-models/budget_models_DRAFT_FINAL.html')


Ordinal_recovery_distance <- polr(data = subset(d, Status_war_sept_ext != "occupied"), Hess = TRUE ,
                         as.formula(paste("as.factor(recovery_month_distance)", "~", indep_vars)))
summary(Ordinal_recovery_distance)
modelsummary(Ordinal_recovery_distance, stars = TRUE)
ctable <- round(coef(summary(Ordinal_recovery_distance)),4)
p <- round((pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2),4)
## combined table
results_Ordinal_recovery_distance <- (ctable <- cbind(ctable, "p value" = p))

Ordinal_recovery_distance_2 <- polr(data = subset(d, Status_war_sept_ext != "occupied"), Hess = TRUE ,
                                    as.formula(paste("recovery_distance_2_factor", "~", indep_vars)))
summary(Ordinal_recovery_distance_2)
modelsummary(Ordinal_recovery_distance_2, stars = TRUE)
ctable <- round(coef(summary(Ordinal_recovery_distance_2)),4)
p <- round((pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2),4)
## combined table
results_Ordinal_recovery_distance_2 <- (ctable <- cbind(ctable, "p value" = p))
stargazer::stargazer(results_Ordinal_recovery_distance,
                     results_Ordinal_recovery_distance_2,
                     single.row = T, type = 'html', out = './analysis/budget-models/budget_models_RECOVERY_Ordinal.html')

# check distribution
x <- d %>% filter(Status_war_sept_ext != "occupied") %>%
  dplyr::select(recovery_count_score) %>% filter(!is.na(recovery_count_score)) %>% pull()
pois_dist <- fitdistrplus::fitdist(x, distr = "pois")
plot(pois_dist)
nbin_dist <- fitdistrplus::fitdist(x, distr = "nbinom")
plot(nbin_dist)

###### Poisson
fit1_poisson <- 
  glm(
    formula = as.formula(paste("recovery_count_score", "~", indep_vars)) 
    
    ,data = subset(d, Status_war_sept_ext != "occupied")
    ,family = "poisson"
  )
performance::check_overdispersion(fit1_poisson)
# overdispersed data - so negative binomial
performance::check_overdispersion(fit1_poisson)
###### Neg binom
fit1_nbinom <- 
  MASS::glm.nb(
    formula = as.formula(paste("recovery_count_score", "~", indep_vars)) 
    
    ,data = subset(d, Status_war_sept_ext != "occupied")
  )

summary(fit1_nbinom, digits=3)
modelsummary(fit1_nbinom, stars = TRUE)
