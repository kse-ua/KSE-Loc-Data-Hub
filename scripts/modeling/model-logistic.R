run_logistic_binary <- function(
  d
  ,dependent   # Y, the criterion of the system
  ,explanatory # first chosen as focal, rest are control
){
  # browser()
  # d <- ds1
  # dependent   <- "mort_5yr" # var2
  # explanatory  = c("sex","ulcer", "status","t_stage")
  # browser()
  # default colors to represent effect direction and significance
  pal_direction_significance <-  c(
    "Increase (99%)"   = "#2b8cbe"
    ,"Increase (95%)"  = "#7bccc4"
    ,"Increase (90%)"  = "#bae4bc"
    ,"Not Significant" = "NA"
    ,"Decrease (90%)"  = "#fdcc8a"
    ,"Decrease (95%)"  = "#fc8d59"
    ,"Decrease (99%)"  = "#d7301f"
  )
  # Direction of Effect (Significance Level at %)
  # To help with meaningful colors of effect interpretation

  # 1 - FORMULA
  eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )

  # 2 - MODEL
  model <- stats::glm(
    formula = eq_formula
    # ,family = "binomial"       # alternative way
    ,family = binomial(link=logit) # alternative way
    ,data   = d
  )

  # 3 - PREDICTED
  # generate model prediction for all unique combos of values on all predictors
  d_predicted_unique <- d %>%
    group_by(.dots = all_of(explanatory)) %>%
    summarize(count = n(),.groups="keep") %>%
    ungroup() %>%
    dplyr::mutate(
      probability = predict(object = model, newdata = .,type = "response")
      # In other way, to verify the steps
      ,log_odds = predict(object = model, newdata = .)
      ,probability2 = plogis(log_odds) # same thing, double checks
    )



  # 4 - compute ROC and AUC model performance index
  d_pred <- model %>%
    broom::augment() %>%
    mutate(
      probability = predict(object = model, newdata = .,type = "response")
    ) %>%
    select(all_of(c(dependent,"probability"))) # to minimize size
  # browser()
  roc_obj <- pROC::roc(d_pred[[dependent]], d_pred[["probability"]])
  rm(d_pred)
  auc_value <- pROC::auc(roc_obj) %>% as.numeric()
  g_roc <-  roc_obj %>%
    ggroc(color = "red")+
    # geom_abline(slope = 1, intercept = 1, color = "black")+
    geom_abline(slope = 1, intercept = 1, color = binary_colors["FALSE"])+
    geom_text(
      aes(
        label = paste0("AUC = ",auc_value %>% numformat(3))
        , x = .25
        , y = .25
      )
      ,color =  binary_colors["TRUE"]
    )

  # 5 - format and augment the table of coefficients
  # to separate variable name from variable value in the summary(model) table
  pattern_starts_with_explanatory <- paste0("^",explanatory, collapse = "|")
  # EXTRACT ESTIMATES TABLE FROM MODEL OBJECT (glm)
  d_estimates  <-   model %>%
    broom::tidy(
      conf.int = TRUE
      ,exp     = TRUE # converts log-odds into odds-ratios (i.e. =exp(estimate))
    ) %>%
    mutate(
      conv_odds    = (estimate-1) # careful, this relies on broom::tidy(exp=TRUE)
      # otherwise: exp(estimate) - 1
      ,var_name    = stringr::str_extract(term, pattern_starts_with_explanatory)
      ,value_level = stringr::str_remove( term, pattern_starts_with_explanatory)
    ) %>%
    # enhance with variables instrumental for graphing
    mutate(
      sign = cut(
        x = p.value,
        breaks = c(-Inf,.01, .05, .10, Inf),
        # labels = c("***", "**", "*", ".", " "), #These need to coordinate with the color specs.
        # labels = c("(***)", "(**)", "(*)", "(.)", " "), #These need to coordinate with the color specs.
        # labels = c("(Extremely)", "(Very)", "(Significant)", "(Potentially)", " "), #These need to coordinate with the color specs.
        labels = c("(99%)", "(95%)", "(90%)", "Not Significant"), #These need to coordinate with the color specs.
        right = TRUE, ordered_result = TRUE
      )
      # )
      ,sign_ = cut(
        x =p.value,
        breaks = c(-Inf, .01, .05, .10, Inf),
        # labels = c("<=.001", "<=.01", "<=.05", "<=.10", "> .10"), #These need to coordinate with the color specs.
        labels = c("(<=.01)", "(<=.05)", "(<=.10)", "(> .10)"), #These need to coordinate with the color specs.
        right = TRUE, ordered_result = TRUE
      )
      ,direction = ifelse(estimate>=1, "Increase", "Decrease")
      ,sign_direction = paste0(direction, " ", sign)
      ,sign_direction = ifelse(sign == "Not Significant", "Not Significant", sign_direction)
      ,sign_direction = ifelse(sign_direction %in% c("Increase  ", "Decrease  "), " ", sign_direction)
      ,sign_direction = fct_relevel(sign_direction,names(pal_direction_significance))
    )

  # 6 - MODEL FIT
  model_fit <- model %>% get_model_fit(print=F)

  # LASTLY - assemble everything into a list object
  ls_out <- list() # create the shell for the object
  ls_out[["equation"]]  <- list(
    "formula"      = eq_formula
    ,"dependent"   = dependent
    ,"explanatory" = explanatory
  )
  ls_out[["model"]]     <- model
  ls_out[["predicted"]] <- d_predicted_unique
  ls_out[["roc"]]       <- list(
    "roc"        = roc_obj
    ,"roc_plot"  = g_roc
    ,"auc"       = auc_value
  )
  ls_out[["estimates"]] <- d_estimates
  ls_out[["model_fit"]] <- model_fit
  ls_out[["focal_factor"]] <- list(
    "levels" = levels(d[[explanatory[1]]])
  )


  return(ls_out)
}
# How to use
# dependent   <- "has_ea" # var2
# focal       <- "age_group" # var1
# control     <- c("sex")
# ls <- ds2 %>%
#   select(all_of(c(dependent, focal, control)))%>%
#   run_logistic(dependent, focal, control)
# ls$equation #%>% print(environment=F)
# ls$model
# # ls$model %>% summary() # to get a traditional view
# ls$model %>% gtsummary::tbl_regression(exponentiate =F)
# ls$model %>% gtsummary::tbl_regression(exponentiate =T)
# ls$model %>% tbl_regression(exponentiate = TRUE)
# ls$predicted # model prediction for all unique combination of predictors
# ls$estimates
# ls$roc$roc
# ls$roc$auc
# ls$model_fit

run_logistic_binary_model_comparison <- function(
  d # data tibble
  ,dependent #= "has_ea"
  ,explanatory #= c("age_group",'sex', "employment_state")
  # the first will be used as focal
){
  # browser()
  full_model_spec    <- explanatory
  reduced_model_spec <- explanatory[2:length(explanatory)]
  model_comparison   <- list("full" =full_model_spec , "reduced" = reduced_model_spec)
  ls_out <- list()
  ls_model <- list(
    "reduced"  = d %>% run_logistic_binary(dependent, reduced_model_spec)
    ,"full"    = d %>% run_logistic_binary(dependent, full_model_spec)
  )

  # Model comparison test
  chi_square_diff <- ls_model$full$model_fit$chisquare - ls_model$reduced$model_fit$chisquare
  df_diff         <- ls_model$full$model_fit$df - ls_model$reduced$model_fit$df
  pvalue          <- pchisq(chi_square_diff, df_diff, lower.tail = FALSE)
  pvalue_pretty   <- paste0("<= ", numformat(pvalue, decimal_count = 3))
  r_squared_full  <- ls_model$full$model_fit$rsquare
  r_squared_diff  <- (r_squared_full - ls_model$reduced$model_fit$rsquare)
  chi_squared_diff_test <- paste0(
    "Improvement over Reduced Model: "
    ,"Chi-Square (df = ",df_diff,") = ",scales::comma(chi_square_diff, accuracy = .1)
    # ,", df = ", df_diff,
    ,", p-value ", pvalue_pretty
    ,", R-Square  = ", numformat(r_squared_full, 3),", gained ",
    numformat(r_squared_diff,3)
  )

  # Make model comparison table
  t_reduced <- ls_model$reduced$model %>% gtsummary::tbl_regression(exponentiate=T)
  t_full    <- ls_model$full$model %>% tbl_regression(exponentiate=T)
  t_out <-
    gtsummary::tbl_merge(
      # tbls = list(t_reduced, t_full)
      tbls = list(t_full, t_reduced)
      ,tab_spanner = c("Full", "Reduced")
    ) %>%
    gtsummary::modify_caption(chi_squared_diff_test)
  ls_model[["compare"]] <- list(
    "test" = chi_squared_diff_test
    ,"table" = t_out
  )

  return(ls_model)
}
# How to use
# ls_model <- run_logistic_binary_model_comparison(
#   ds2
#   ,"has_ea"
#   ,c("age_group",'sex', "employment_state")
# )
# components
# the identical structure  for `ls_model$reduced`
# ls_model$full # all from Reduced + 1 focal (exploratory[1])
# ls_model$full$equation
# ls_model$full$model
# ls_model$full$model %>% summary()
# ls_model$full$model %>% broom::tidy(exp=T)
# ls_model$full$predicted # for all unique combos on explanatory vars
# ls_model$full$roc
# ls_model$full$estimates
# ls_model$full$estimates %>% glimpse()
# ls_model$full$model_fit
# ls_model$full$focal_factor
# ls_model$full$model_fit
# Now the element that compares models
# ls_model$compare$test
# ls_model$compare$table
