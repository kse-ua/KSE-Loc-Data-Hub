# supporting function ( slightly differnt than in ./scripts/modeling)
get_model_fit <- function(m, print=F){
  d_test <-
    m %>% 
    broom::glance() %>% 
    dplyr::mutate(
      # we round(6) to avoid floating decimal point problem, which occasionally
      # reulsts in values extremely closely to zero, but still not a zero
      # In these cases, because df_delta = 0, it will result in pvalue = 0
      deviance_delta = (null.deviance - deviance) %>% round(6) # this is the value of Chi-Square
      ,df_delta = (df.null - df.residual) %>% round(6)
      ,pvalue = pchisq(deviance_delta, df_delta, lower.tail = FALSE)
      ,rsquared = (1 - (deviance/null.deviance)) %>% round(6)
    )
  if(print){
    cat("MODEL FIT",
        "\nChi-Square = ",d_test %>% pull(deviance_delta),
        "\ndf = "        ,d_test %>% pull(df_delta),
        "\np-value = "   ,d_test %>% pull(pvalue),"\n"
    )
  }
  return(d_test)
}

# d <- ds1 # to study the function examples
# outputs a model object to scrutinize manually
run_simple_model <- function(
    d_in
    ,dependent   # Y, the criterion of the system
    ,explanatory # first chosen as focal, rest are control
    ,depdist = "gaussian"
    ,confounder = NA
){
  if(length(confounder)==1L & sum(is.na(confounder))==1L){
    explanatory_reduced <- " 1 "
  }else{
    explanatory_reduced <- confounder
    explanatory <- c(explanatory, confounder)
  }
  
  # browser()
  eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )
  eq_formula_reduced <- as.formula(paste0(dependent," ~ ", paste(explanatory_reduced, collapse = " + ") ) )
  
  model_out <- stats::glm(
    formula = eq_formula
    ,family = depdist     # alternative way
    ,data   = d_in
  )
  return(model_out)
  
}
# function study:
# m <-
#   ds1 %>%
#   run_simple_model(
#     dependent     = 'idp_registration_number'
#     ,explanatory  = "transfert_prop_2021" # enter only one
#     ,depdist      = "poisson"
#     # ,confounder   = "sex_head"   # no confounders by default
#   )
# m %>% broom::glance()
# m %>% broom::tidy()
# m %>% get_model_fit()



# outputs a tibble with a single row 
# as opposed to `run_simple_model()` this functions returs a tibble, not a model object
run_simple_scan <- function(
    d_in
    ,dependent   # Y, the criterion of the system
    ,explanatory # first chosen as focal, rest are control
    ,depdist = "gaussian"
    ,confounder = NA
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
  # 1 - adjust the equations based on the presence of confounders
  if(length(confounder)==1L & sum(is.na(confounder))==1L){
    explanatory_reduced <- " 1 "
  }else{
    explanatory_reduced <- confounder
    explanatory <- c(explanatory, confounder)
  }
  eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )
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
  # compute relevant indices
  (rsq_full    <- get_model_fit(model)$rsquared)
  (rsq_reduced <- get_model_fit(model_reduced)$rsquared)
  
  (pval_full    <- get_model_fit(model)$pvalue)
  (pval_reduced <- get_model_fit(model_reduced)$pvalue)
  
  (rsq_improvement <- rsq_full - rsq_reduced)
  (pval_improvement <- (anova(model_reduced, model, test = "Chisq"))$`Pr(>Chi)`[2])
  
  # the effective sample size will be the same because we forced it (see above)
  (nobs <- model %>% broom::glance() %>% pull(nobs))
  
  # 4 - assemble the output
  d_out <- 
    list(
    "outcome"             = dependent
    ,"distribution"       = depdist
    ,"predictor"          = explanatory[1]
    ,"confounder"         = setdiff(explanatory,explanatory[1]) %>% paste0(collapse = " + ")
    
    ,"rsq_full"           = rsq_full # variance explained by the FULL model
    ,"pval_full"          = pval_full # significance of the REDUCED model
    
    ,"rsq_reduced"        = rsq_reduced # significance of the FULL model
    ,"pval_reduced"      = pval_reduced # significance of the REDUCED model
    
    ,"rsq_improvement"    = rsq_improvement # gains of FULL model over REDUCED
    ,"pval_improvement"   = pval_improvement # significance of added terms to the REDUCED model
    
    ,"nobs"               = nobs
  ) %>%
    as_tibble()
  
  return(d_out)
}
# Function study:
# d <-
#   ds1 %>%
#   run_simple_scan(
#     dependent    = 'idp_registration_number'
#     ,explanatory = c("transfert_prop_2021")
#     ,depdist     = "poisson"
#     # ,confounder  = NA
#   )
# d





# a wrapper round `run_simple_scan` that differentiates countinous and categorical predictors
# outputs a tibble with multile rows (as many as predictors passed to it)
run_complex_scan <- function(
    d_in
    ,dependent # the name of the criterion variable
    ,depdist # the ditribution of the depended var will be moded as this
    ,explantory_continous =NULL # list of continous predictors
    ,explanatory_categorical = NULL # list of categorical predictors
    ,confounder = NA # list one or more predictors to adjust for
    
){
  # browser()
  ls_temp <- list()
  
  if( is.null(explantory_continous) & is.null(explanatory_categorical) ){
    stop("Select at least one explanatory variable")
  }
  
  if(!is.null(explantory_continous)){
    for(i in explantory_continous){
      # for(i in predictor_vars_categorical){
      ls_temp[[i]] <- 
        d_in %>% 
        run_simple_scan(
          dependent    = dependent
          # ,explanatory = c(i,confounder) %>% na.omit() %>% as.character()
          ,explanatory = i
          ,depdist     = depdist # distribution of the dependent variable
          ,confounder  = confounder
        )
      ls_temp[[i]][["data_type"]] <- "continuous"
    }
  }
  # We can use the same ls_temp object to put more things in:
  if(!is.null(explanatory_categorical)){
    for(i in explanatory_categorical){
      ls_temp[[i]] <- 
        d_in %>% 
        run_simple_scan(
          dependent    = dependent
          # ,explanatory = c(i,confounder) %>% na.omit() %>% as.character()
          ,explanatory = i 
          ,depdist     = depdist # distribution of the dependent variable
          ,confounder  = confounder
        )
      ls_temp[[i]][["data_type"]] <- "categorical"
    }
  }
  # browser()
  d_out <-  ls_temp %>% bind_rows()
  return(d_out)
}
# Function study:
# d <-
#   ds1 %>%
#   run_complex_scan(
#     dependent = 'idp_registration_number'
#     ,depdist = "poisson"
#     ,explantory_continous = c("transfert_prop_2021","total_population_2022")
#     ,explanatory_categorical = c("sex_head")
#     ,confounder = c("voluntary")
#   )
# d




# takes the product of `run_complex_scan` and turns it into a graph
plot_complex_scan <- function(d_in){
  # browser()
  confounder_text <- unique(d_in$confounder)
  outcome_text <- unique(d_in$outcome)
  distribution_text <- str_to_title(unique(d_in$distribution))
  point_size <- 2
  
  # browser()
  g_out <- 
    {d_in %>% 
        mutate(
          model_sign_05 = pval_full <= .05
          ,model_reduced_sign_05 = pval_reduced <= .05
          ,model_improvement_sign_05 = pval_improvement <= .05
        ) 
    } %>% 
    
    ggplot(
      .
      ,aes(
        y= fct_reorder(predictor,rsq_full)
      ))+
    
    # REDUCED - predictive capacity of the REDUCED model (outcome ~ confounder(s))
    geom_point(
      aes(
        x = rsq_full-rsq_improvement
        ,shape = model_reduced_sign_05
      )
      ,size = point_size
    )+
    geom_segment(
      aes(
        x     = 0
        ,xend = rsq_full-rsq_improvement,yend=predictor
      )
      ,linetype = "dotted"
    )+
    # FULL - predictive capacity of the FULL model  (outcome ~ explanatory + confounder)
    geom_segment(
      aes(
        x     = rsq_full - rsq_improvement
        ,xend = rsq_full
        ,yend = predictor
      )
      ,linetype="solid"
      ,linewidth=2
      ,alpha =.2
    )+
    geom_point(
      aes(
        x      = rsq_full
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
## Function study
# d <-
#   # ds1  %>%
#   ds2_prep  %>%
#   run_complex_scan(
#     dependent = 'prep_score_feb'
#     ,depdist = "poisson"
#     # ,explantory_continous = predictor_vars_continuous
#     ,explantory_continous = c("urban_pct", "sum_osbb_2020")
#     ,confounder = c("voluntary")
#     ,explanatory_categorical = c("sex_head", "type")
#   )
# d %>% plot_complex_scan()
# 
# d


# outputs a model object and a graph
# designed to help to zoom in onto a single model
diagnose_one_model <- function(
    d_in
    ,dependent
    ,explanatory
    ,depdist = "gaussian"
    ,confounder = NA
){
  
  if(length(confounder)==1L & sum(is.na(confounder))==1L){
    explanatory_reduced <- " 1 "
  }else{
    explanatory_reduced <- confounder
    explanatory <- c(explanatory, confounder)
  }
  
  # browser()
  eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )
  
  
  # browser()
  # 2 - MODEL
  model_out <- stats::glm(
    formula = eq_formula
    ,family = depdist     # alternative way
    ,data   = d_in
  )
  
  g <- 
    d_in %>% 
    ggplot(aes(x=!!rlang::sym(explanatory[1]), y = !!rlang::sym(dependent)))+
    geom_point(shape=21)+
    geom_smooth(method = "lm")+
    ggpmisc::stat_poly_eq(
      formula = y ~ + x
      ,aes(
        label = paste(..eq.label.., ..rr.label.., ..n.label..,sep = "~~~")
      )
      ,parse = TRUE
    )
  list_out <- list(
    "model" = model_out
    ,"graph" = g
  )
}
# function study

