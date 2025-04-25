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
source("./analysis/survey-prep-model/custom-model-functions.R") # plots
# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/survey-prep-model/prints-building/")
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
ds0 <- readr::read_rds("./data-private/derived/survey-prep-model-ds2_prep.rds")


# ---- inspect-data ------------------------------------------------------------
ds0 %>% glimpse()
# ---- inspect-data-0 ------------------------------------------------------------

# ---- tweak-data-1 ------------------------------------------------------------
ds1 <- ds0
# ---- tweak-data-2 ------------------------------------------------------------
ds2 <- ds1
ds2 %>% glimpse ()
# ----- predictor-groups ---------------
# Support

support_vars <- c(
  "hromada_name"
)

# Outcomes

outcomes_vars <- c(
  "prep_score_feb"
  ,"prep_score_oct"
  ,"prep_score_combo"
  ,"idp_registration_number" # внутрішньо переміщені особи - кількість ВПО
  ,"idp_registration_share" # внутрішньо переміщені особи - кількість ВПО \ загальне населення до вторгнення
  ,"idp_real_number" # corrected index above 
  ,"idp_real_share" # corrected index above 
  ,"idp_child_education" # кількість ВПО дітей
  ,"idp_child_share" # відсоток ВПО дітей від популяціі громади до вторгнення
  ,'created_jobs' # ordinal
  ,'relocated_companies_text' # к-сть релокованих компаній
  ,'international_projects' # к-сть проєктів з міжнародними донорами
  ,'finance_school_shelters'
  ,'no_school_days_coded'
  ,'hromada_exp'
)

# Continuous - good for spreading out # Valentyn, please add relevant predictors here
predictor_vars_continuous <- c(
  "income_own_per_capita"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita" # що надходить від держави, 
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,'dfrr_executed' # сума всіх проектів (скільки дали на розвиток громади - спец прокти), виграні інвестиційні проекти, на скільки г. залучила інвест кошти в рамках програми
  ,'passangers_2021'
  ,'business_support_centers' # кількість центрів
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ 
  ,"turnout_2020" # явка
  ,'square' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers'
  ,'youth_councils'
 )

predictor_vars_continuous_scaled <- c(
  "income_own_per_capita_k"     # весь дохід з податків (без видатків з держви) - заможність громади
  ,"income_total_per_capita_k"  # свій доход + дотації, суммарний дохід
  ,"income_tranfert_per_capita_k" # що надходить від держави, 
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  ,'passangers_2021'
  ,'business_support_centers' # кількість центрів
  ,"n_settlements" #кількість населенних пунктів у громаді
  ,"travel_time" # відстань до обласного центру
  ,"urban_pct"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,"sum_osbb_2020" # кількість ОСББ 
  ,"turnout_2020" # явка
  ,'square' # площа громади у кв.км
  ,"age_head" # вік голови громади
  ,"time_before_24th_years" # коли сформувалась громада
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers'
  ,'youth_councils'
)

# Continuous variables with filled by 0 NAs
predictor_vars_continuous_scaled_wo_na <- c(
  "income_own_per_capita_k"           
  ,"income_total_per_capita_k"         
  ,"income_tranfert_per_capita_k"
  ,'own_income_prop_2021' # відсоток власних доходів у загальному доході
  ,'transfert_prop_2021' # відсоток трансфертів у загальному доході
  # ,"idp_registration_share" # more of an outcome
  # ,"idp_real_share" # more of an outcome
  # ,"idp_child_share" # more of an outcome
  ,'dfrr_executed_k_zeros'
  ,'passengers_2021_zeros'
  ,'business_support_centers'
  ,"travel_time"
  ,"sum_osbb_2020_zeros"
  ,"turnout_2020"
  ,"age_head"
  ,'square'
  ,"n_settlements"
  ,"urban_pct"
  ,"time_before_24th_years"
  ,"total_population_2022"
  ,"urban_population_2022"
  ,'edem_total' # cкільки інструментів електрон.демографіі залучену у громаді
  ,'youth_centers' # к-сть молодіжних центрів
  ,'youth_councils' # к-сть молодіжних рад
)

# Categorical - for color # Valentyn, please add relevant predictors here
predictor_vars_categorical <- c(
  "sex_head"
  ,"education_head"
  ,"type"
  ,"voluntary"
  ,"region_en"
  ,'incumbent'
  # ,'rda' # too small variation
  ,'not_from_here'
  # ,'enterpreuner' # too small variation
  # ,'unemployed' # too small variation
  ,'polit_work'
  ,'party_national_winner'
  ,'no_party'
  ,'war_zone_27_04_2022'
  ,'train_station'
  ,'edem_petitions' # binary from above
  ,'edem_consultations'# binary from above
  ,'edem_participatory_budget'# binary from above
  ,'edem_open_hromada' # binary from above
  # ,'city'
)
predictor_vars <- c(
  predictor_vars_continuous
  ,predictor_vars_categorical
)






# PREVIOUS explorations -------------------
# ---- one-model -----------------------
fit1_gaussian <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + sex_head
    ,data = ds2
    ,family = "gaussian"
  )
fit1_poisson <- 
  glm(
    formula = idp_registration_number ~ sum_osbb_2020_zeros
    ,data = ds1
    ,family = "poisson"
  )

fit2_poisson <- 
  glm(
    formula = idp_registration_number ~ sum_osbb_2020_zeros + city + sum_osbb_2020_zeros*city
    ,data = ds1
    ,family = "poisson"
  )

fit3_poisson <- 
  glm(
    formula = idp_registration_number ~ sum_osbb_2020_zeros + region_en + sum_osbb_2020_zeros*region_en
    ,data = ds1
    ,family = "poisson"
  )

fit1_gaussian %>% summary()

fit1_gaussian %>% broom::glance() # model properties
fit1_gaussian %>% broom::tidy() # coefficients
fit1_gaussian %>% broom::augment() # add predicted values

fit1_poisson %>% broom::glance() # model properties
fit1_poisson %>% broom::tidy() # coefficients
fit2_poisson %>% broom::tidy() # coefficients
fit3_poisson %>% broom::tidy() # coefficients

fit1_poisson %>% broom::augment() # add predicted values

fit1_gaussian %>% jtools::plot_summs()
fit1_poisson %>% jtools::plot_summs()

jtools::plot_summs(fit1_poisson, fit3_poisson)

car::vif(fit2_poisson)
car::vif(fit3_poisson)

# Resources for handling modeling objects
# jtools vignette - https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# broom vignette - https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# GGally - https://ggobi.github.io/ggally/

fit2_gaussian <- 
  glm(
    formula = prep_score_feb ~ sex_head
    ,data = ds2
    ,family = "gaussian"
  )
fit2_poisson <- 
  glm(
    formula = prep_score_feb ~ sex_head
    ,data = ds2
    ,family = "poisson"
  )

fit2_gaussian %>% broom::tidy() # coefficients
fit2_poisson %>% broom::tidy() # coefficients


jtools::plot_summs(fit2_gaussian, fit2_poisson)


vars_density <- c( 
"prep_score"
, 'square'
,"n_settlements"
,"urban_pct"
# ,"time_before_24th_years"
,"total_population_2022"
# ,"urban_population_2022"
  )
ds2 %>% 
  filter(total_population_2022 < 200000) %>% #select(hromada_name, total_population_2022)
  select(vars_density) %>% GGally::ggpairs()


fit3_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type
    ,data = ds2
    ,family = "gaussian"
  )
fit3_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k 
    ,data = ds2
    ,family = "poisson"
  )
anova(fit3_full, fit3_reduced,test = "Chisq")

jtools::plot_summs(model.names = c("Full","Reduced"),fit3_full, fit3_reduced)
fit3_full %>% broom::tidy()

d <- 
  fit3_full %>% 
  broom::augment()

g <- 
  d %>% 
  # ggplot(aes(y = prep_score_feb, x = income_own_per_capita_k, fill=type, color=type))+
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  # geom_smooth(method = "lm", se=F)+
  # facet_wrap("type")+
  geom_point()
g

ds2 %>% 
  ggplot(aes(x=type, y = prep_score_feb))+
  geom_boxplot()+
  geom_jitter()



# ----- fit4 ---------------------

fit4_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type +income_own_per_capita_k*type 
    ,data = ds2
    ,family = "poisson"
  )
fit4_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type 
    ,data = ds2
    ,family = "poisson"
  )
anova(fit4_full, fit4_reduced,test = "Chisq")
fit4_full %>% broom::tidy()
# jtools::plot_summs(model.names = c("Full","Reduced"),fit4_full, fit4_reduced)

g <- 
  fit4_full %>% 
  broom::augment() %>% 
  # ggplot(aes(y = prep_score_feb, x = income_own_per_capita_k, fill=type, color=type))+
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  # geom_smooth(method = "lm", se=F)+
  # facet_wrap("type")+
  geom_point()
g
# 
# ds2 %>% 
#   ggplot(aes(x=type, y = prep_score_feb))+
#   geom_boxplot()+
#   geom_jitter()


# ---- fit5 ----------

d_model <- 
  ds2 %>% 
  mutate(
    urban = case_when(
      type %in% c("селищна","сільска") ~ TRUE
      ,TRUE ~ FALSE
    )
  ) %>% 
  select(prep_score_feb, income_own_per_capita_k, type) %>% glimpse()


fit5_full <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type + income_own_per_capita_k*type 
    ,data = ds2
    ,family = "poisson"
  )
fit5_reduced <- 
  glm(
    formula = prep_score_feb ~ income_own_per_capita_k + type 
    ,data = ds2
    ,family = "poisson"
  )
anova( fit5_reduced,fit5_full,test = "Chisq")
fit5_full %>% broom::tidy()

g <- 
  fit5_full %>% 
  broom::augment() %>% 
  ggplot(aes(y = .fitted, x = income_own_per_capita_k, fill=type, color=type))+
  geom_point()
g

ds2 %>% 
  ggplot(aes(x=type, y = prep_score_feb))+
  geom_boxplot()+
  geom_jitter()


ds2 %>% select(income_own_per_capita, income_total_per_capita, income_tranfert_per_capita) %>% 
  GGally::ggpairs()

# ----- fit6 ---------------------------------
d_model <- 
  ds2 %>% 
  mutate(
    urban = case_when(
      type %in% c("селищна","сільска") ~ TRUE
      ,TRUE ~ FALSE
    )
  ) %>% 
  # select(prep_score_feb, income_total_per_capita_k, urban)
  select(prep_score_feb, income_own_per_capita_k, urban)

fit5_full <- 
  glm(
    formula = prep_score_feb ~ (.)^2 
    ,data = d_model
    ,family = "poisson"
  )
fit5_reduced <- 
  glm(
    formula = prep_score_feb ~  (.)^2
    ,data = d_model
    ,family = "poisson"
  )
anova(fit5_full, fit5_reduced,test = "Chisq")
fit5_full %>% broom::tidy()

g <- 
  fit5_full %>% 
  broom::augment() %>% 
  ggplot(aes(y = .fitted, x = income_total_per_capita_k, fill=urban, color=urban))+
  geom_point()
g

# ---- --------

vars_density <- c( 
   'square'
  ,"n_settlements"
  ,"total_population_2022"
)


ds2 %>% make_bi_freq_graph("type")
ds2 %>% make_bi_freq_graph("type", "voluntary")



# ----- print-many-models -------------
# To execution multiple scenarios
for(i in predictor_vars_categorical){
  
  for(ii in outcomes_vars){
    
    g <- 
      d %>% 
      make_plot_prepvs(
        xvar    = "item_value"
        ,yvar     = ii
        ,fillvar = i
      )   
      file_name <- paste0(ii,"/",i)
      g %>% quick_save(paste0("/bivar-cont/",file_name),w=16,h=9)
      
      g <- 
        d %>% 
        filter(hromada_code %ni% outliers) %>% 
        make_plot_prepvs(
          xvar    = "item_value"
          ,yvar     = ii
          ,fillvar = i
        )   
      file_name <- paste0(ii,"/",i)
      g %>% quick_save(paste0("/bivar-cont/",file_name,"-no_out"),w=16,h=9)
  }
}



# ---- simple-model-scan --------------------
# 1 - Distributions of outcomes and predictors (cont + cat)
# 2 - Rsq of a simple model: outcome ~  p1 (+p(2...N)), where p2...N is an optional confounder
# 3 -  Faceted scatter (x = p1...N, y = outcome, color = p1...N)
# 3a - Faceted scatter (x = cont.predictor, y =outcome, color=cat.prdictor)
# 3b - Faceted boxplot (x = cat.predictor, y =outcome, color=con.prdictor)



# ---- modeling-simple-case -----------------------
# Let us review the anatomy of glm models using a simple case to see how we can scale it up
# we want to run many simple  models (only one or two predictors)

d_model <- 
  ds2 %>% 
  select(
    prep_score_oct, age_head, sex_head, incumbent, income_own_per_capita, idp_child_share , travel_time
  )

d_model %>% GGally::ggpairs()

fit1  <- 
  glm(
    f
  )

# ---- modeling-demonstration -----------------------

model <- 
  stats::glm(
    
  )

model <- 
  stats::glm(
    
  )


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
