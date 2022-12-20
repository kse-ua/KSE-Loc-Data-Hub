
source("./analysis/survey-prep-model/custom-model-functions.R")
# ----- sinlge-model-glm ---------------
# run a single model, output = glm object
m <-
  ds1 %>%
  run_simple_model(
    dependent     = 'idp_registration_number'
    ,explanatory  = "transfert_prop_2021" # enter only one
    ,depdist      = "poisson"
    # ,confounder   = "sex_head"   # no confounders by default
  )
m %>% broom::glance()
m %>% broom::tidy()
m %>% get_model_fit()


# ----- single-model-tibble ---------------
# run a single model, output = tibble with model summary
d <-
  ds1 %>%
  run_simple_scan(
    dependent    = 'idp_registration_number'
    ,explanatory = c("transfert_prop_2021")
    ,depdist     = "poisson"
    # ,confounder  = NA
  )
d


# ----- many-models-graph ---------------
# run a many models, output a tibble with key indices
# run multiple models, separating continuous and categorical predictors
# source("./analysis/survey-prep-model/custom-model-functions.R") # for testing adjustments
d <-
  # ds1 %>%
  ds2_prep %>%
  run_complex_scan(
    dependent = 'prep_score_feb'
    # ,depdist = "poisson"
    ,depdist = "gaussian"
    ,explantory_continous = predictor_vars_continuous_scaled_wo_na
    ,confounder = c("voluntary")
    # ,confounder = c("urban_pct")
    , explanatory_categorical = predictor_vars_categorical
  )
d
d %>% plot_complex_scan()


d <-
  ds1 %>%
  run_complex_scan(
    dependent = 'idp_registration_number'
    ,depdist = "poisson"
    ,explantory_continous = c("sum_osbb_2020","dfrr_executed","passangers_2021")
    ,confounder = c("urban_pct")
    # ,confounder = c("voluntary")
  )
d
d %>% plot_complex_scan()

# ----- one-model-diagnose ----------------
lt <- 
  ds2_prep %>% 
  diagnose_one_model(
    dependent = 'prep_score_feb'
    ,explanatory = "sum_osbb_2020"
    # ,explanatory = "sum_osbb_2020"
    # ,confounder = ""
    ,depdist = "poisson"
    # ,confounder = c("voluntary")
  )
lt$model
lt$graph



