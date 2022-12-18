run_simple_scan <- function(
    d_in
    ,dependent   # Y, the criterion of the system
    ,explanatory # first chosen as focal, rest are control
    ,depdist = "gaussian"
){
  # browser()
  # d_in
  # d_in <- ds2_prep
  # to help with levels of the factor that indicates statistical significance
  pal_direction_significance <-  c( # turn on when absent from environment
    "Increase (99%)"   = "#2b8cbe"
    ,"Increase (95%)"  = "#7bccc4"
    ,"Increase (90%)"  = "#bae4bc"
    ,"Not Significant" = "NA"
    ,"Decrease (90%)"  = "#fdcc8a"
    ,"Decrease (95%)"  = "#fc8d59"
    ,"Decrease (99%)"  = "#d7301f"
  )
  # 1 - FORMULA
  eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )
  
  # 2 - MODEL
  model <- stats::glm(
    formula = eq_formula
    ,family = depdist     # alternative way
    ,data   = d_in
  )

  # 3 - compare the addition of the first term to the rest of the model
  if( length(explanatory)>1L){
    explanatory_reduced <- explanatory[2:length(explanatory)]
  }
  if( length(explanatory)==1L){
    explanatory_reduced <- " 1 "
  }
  # browser()
  
  #  explanatory_reduced = case_when(
  #   length(explanatory)>1L ~ explanatory[-1]
  #   ,length(explanatory)==1L ~ " 1 "
  # )
  # # 
  # a <- explanatory[1]
  eq_formula_reduced <- as.formula(paste0(dependent," ~ ", paste(explanatory_reduced, collapse = " + ") ) )
  
  model_reduced <- stats::glm(
    formula = eq_formula_reduced
    ,family = depdist      # alternative way
    ,data   = model %>% 
      # otherwise, it's fit to a differnt data bc of missingness on x
      broom::augment() %>%
      select(
        any_of(c(dependent,explanatory))
      )
  )
  # browser()
  # model %>% broom::augment()
  # model_reduced %>% broom::augment()
  
  # browser()
  # 4 - format and augment the table of coefficients
  # to separate variable name from variable value in the summary(model) table
  pattern_starts_with_explanatory <- paste0("^",explanatory, collapse = "|")
  # EXTRACT ESTIMATES TABLE FROM MODEL OBJECT (glm)
    
    if(depdist=="binomial"){
      
      d_estimates0  <-   
        model %>%
        broom::tidy(
          conf.int = TRUE
          ,exp     = FALSE # converts log-odds into odds-ratios (i.e. =exp(estimate))
        ) %>%
        mutate(
          conv_odds    = (estimate-1) # careful, this relies on broom::tidy(exp=TRUE)
          # otherwise: exp(estimate) - 1
        )
    }
    
    if( depdist %in% c("gaussian","poisson") ){
      d_estimates0  <-   
        model %>%
        broom::tidy(
          conf.int = TRUE
        ) 
    }
    
  suppressWarnings(
    d_estimates  <-   
     d_estimates0 %>% 
      mutate(
        var_name     = stringr::str_extract(term, pattern_starts_with_explanatory)
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
  )
  
  
  # 6 - MODEL FIT
  model_fit <- model %>% get_model_fit(print=F)
  
  model_imrovement <- 
    ((model %>% get_model_fit(print=F))$rsquare) -
    ((model_reduced %>% get_model_fit(print=F))$rsquare) 
  # browser()
  # LASTLY - assemble everything into a list object
  # ls_out <- list() # create the shell for the object
  # ls_out[["equation"]]  <- list(
  #   "formula"      = eq_formula
  #   ,"outcome"   = dependent
  #   ,"predictor"   = explanatory[1]
  #   ,"confounder"     = setdiff(explanatory,explanatory[1])
  # )
  # ls_out[["model"]]     <- model
  # ls_out[["estimates"]] <- d_estimates
  # ls_out[["model_fit"]] <- model_fit
  # ls_out[["anova"]] <- (anova(model_reduced, model, test = "Chisq"))[,2] %>% pull(`Pr(>Chi)`)
  # ls_out[["rsq_change"]] <- model_imrovement
  # ls_out[["nobs"]] <- model %>% broom::glance() %>% pull(nobs)
  # ls_out[["depdist"]] <- depdist
  # # 
  # # Final product
  # 
  # ls_out[["equation"]]  <- list(
  #   "formula"      = eq_formula
  #   ,"outcome"   = dependent
  #   ,"predictor"   = explanatory[1]
  #   ,"confounder"     = setdiff(explanatory,explanatory[1])
  # )
  # 
  d_out <- 
    list(
    "outcome"       = dependent
    ,"predictor"    = explanatory[1]
    ,"confounder"   = setdiff(explanatory,explanatory[1]) %>% paste0(collapse = " + ")
    ,"rsq"          = model_fit$rsquare # explanatory capacity of the full model
    ,"model_pval"   = model_fit$pvalue
    ,"rsq_change"   = model_imrovement # gains in explanatory capacity due to predictor
    ,"model_change_pval"   = (anova(model_reduced, model, test = "Chisq"))$`Pr(>Chi)`[2]
    ,"nobs"         = model %>% broom::glance() %>% pull(nobs)
    ,"distribution" = depdist
  ) %>% 
    as_tibble()
  
  return(d_out)
}

# Function study:
# m1 <-
#   ds2_prep %>%
#   run_simple_scan(
#     dependent    = "idp_registration_share"
#     ,explanatory = c("age_head")
#     ,depdist     = "poisson"
# )
# to help un derstand the anatomy
# m1$equation$formula %>% deparse()
# m1$model # the full fitted object
# m1$model %>% summary()
# m1$estimates
# m1$model_fit
# m1$anova
# m1$rsq_change
# m1$nobs
# # m1 <- 
#   ds2_prep %>% 
#   run_simple_scan(
#     dependent    = "sex_head"
#     ,explanatory = c("age_head")
#     ,depdist     = "binomial"
#   )
# m1$estimates 
# 
# m2 <- 
#   ds2_prep %>% 
#   run_simple_scan(
#     dependent    = "prep_score_feb"
#     ,explanatory = c("age_head")
#     ,depdist     = "gaussian"
#   )
# m2$estimates 
# 


# a wrapper round `run_simple_scan` that differentiates countinous and categorical predictors
run_complex_scan <- function(
    d_in
    ,dependent
    ,depdist
    ,confounder = NA
    ,explantory_continous
    ,explanatory_categorical
    
){
  # browser()
  ls_temp <- list()
  for(i in explantory_continous){
    # for(i in predictor_vars_categorical){
    ls_temp[[i]] <- 
      ds2_prep %>% 
      run_simple_scan(
        dependent    = dependent
        ,explanatory = c(i,confounder) %>% na.omit() %>% as.character()
        ,depdist     = depdist # distribution of the dependent variable
      )
    ls_temp[[i]][["data_type"]] <- "continuous"
  }
  # We can use the same ls_temp object to put more things in:
  
  for(i in predictor_vars_categorical){
    ls_temp[[i]] <- 
      ds2_prep %>% 
      run_simple_scan(
        dependent    = dependent
        ,explanatory = c(i,confounder) %>% na.omit() %>% as.character()
        ,depdist     = depdist # distribution of the dependent variable
      )
    ls_temp[[i]][["data_type"]] <- "categorical"
  }
  d_out <-  ls_temp %>% bind_rows()
  return(d_out)
}
# Function study:
# d <- 
#   ds2_prep %>% 
#   run_complex_model(
#     dependent = "voluntary"
#     ,depdist = "binomial"
#     ,confounder = NA
#     ,explantory_continous = setdiff(predictor_vars_continuous_scaled,"time_before_24th_years")
#     ,explanatory_categorical = predictor_vars_categorical
#   )
# 
# d