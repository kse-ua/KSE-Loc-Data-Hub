run_simple_scan <- function(
    d_in
    ,dependent   # Y, the criterion of the system
    ,explanatory # first chosen as focal, rest are control
    ,depdist = "gaussian"
){
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
  
  if( length(explanatory)>1L){
    explanatory_reduced <- explanatory[2:length(explanatory)]
  }
  if( length(explanatory)==1L){
    explanatory_reduced <- " 1 "
  }
  eq_formula_reduced <- as.formula(paste0(dependent," ~ ", paste(explanatory_reduced, collapse = " + ") ) )
  
  # browser()
  # 2 - MODEL
  model <- stats::glm(
    formula = eq_formula
    ,family = depdist     # alternative way
    ,data   = d_in
  )
  # compare the addition of the first term to the rest of the model
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
  
  # 3 - MODEL FIT
  model_fit         <- model %>% get_model_fit(print=F)
  model_reduced_fit <- model_reduced %>% get_model_fit(print=F)
  model_improvement <- model_fit$rsquare - model_reduced_fit$rsquare
  model_improvement_pval <- (anova(model_reduced, model, test = "Chisq"))$`Pr(>Chi)`[2]
 
  model_reduced_pval <- ifelse(
    model_reduced_fit$pvalue %in% c(0L),NA,model_reduced_fit$pvalue
  )
  # 4 - assemble the output
  d_out <- 
    list(
    "outcome"       = dependent
    ,"predictor"    = explanatory[1]
    ,"confounder"   = setdiff(explanatory,explanatory[1]) %>% paste0(collapse = " + ")
    ,"rsq"          = model_fit$rsquare # explanatory capacity of the full model
    ,"model_pval"   = model_fit$pvalue
    ,"rsq_change"   = model_improvement # gains in explanatory capacity due to predictor
    ,"model_change_pval"   = model_improvement_pval # Full vs Reduced test
    ,"model_reduced_pval" = model_reduced_pval
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
    ,dependent # the name of the criterion variable
    ,depdist # the ditribution of the depended var will be moded as this
    ,confounder = NA # list one or more predictors to adjust for
    ,explantory_continous # list of continous predictors
    ,explanatory_categorical # list of categorical predictors
    
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

plot_complex_scan <- function(d_in){
  
  confounder_text <- unique(d_in$confounder)
  outcome_text <- unique(d_in$outcome)
  distribution_text <- str_to_title(unique(d_in$distribution))
  point_size <- 2
  # browser()
  g_out <- 
    {d_in %>% 
        mutate(
          model_sign_05 = model_pval <= .05
          ,model_reduced_sign_05 = model_reduced_pval <= .05
        ) 
    } %>% 
    
    ggplot(
      .
      ,aes(
        y= fct_reorder(predictor,rsq)
        # ,x = rsq
        # ,color = model_sign_05, fill= model_sign_05
        # ,shape = model_sign_05
      ))+
    
    # REDUCED - predictive capacity of the REDUCED model (outcome ~ confounder(s))
    geom_point(
      aes(
        x = rsq-rsq_change
        ,shape = model_reduced_sign_05
      )
      ,size = point_size
      # ,fill = "grey80"
      # , color = "grey80"
    )+
    geom_segment(
      aes(
        x     = 0
        ,xend = rsq-rsq_change,yend=predictor
      )
      ,linetype = "dotted"
    )+
    # FULL - predictive capacity of the FULL model  (outcome ~ explanatory + confounder)
    geom_segment(
      aes(
        x     = rsq - rsq_change
        ,xend = rsq
        ,yend = predictor
      )
      ,linetype="solid"
      ,linewidth=2
      ,alpha =.2
    )+
    geom_point(
      aes(
        x      = rsq
        ,shape = model_sign_05
      )
      ,size = point_size
    )+
    # adjustment
    scale_shape_manual(values = c("TRUE"=16,"FALSE"=21))+
    scale_x_continuous(
      labels = scales::percent_format()
      # ,breaks = seq(0,100,2), minor_breaks = seq(0,100,1)
      )+
    labs(
      subtitle = paste0("After adjusting for [", confounder_text,"]")
      ,title = paste0("Modeling  [", outcome_text, "]  from  [ ... ] ",
                      " with a ", distribution_text, " distribution"
      )
      
      ,x = "Percent of variabilty in the outcome explained by predictor(s)"
      ,shape = "Model\nsignificant\nat .05 level"
    )+
    theme(
      axis.text.y = element_text(size =10)
    )
  return(g_out)
}
# function study
# d %>% plot_complex_scan()

