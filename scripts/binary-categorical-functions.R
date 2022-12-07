# see https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
# for details on using `.data[[]]` pronoun


# ---- contingency-table-views ----------------
# function to produce a frequency table between (upto) TWO categorical variables
make_bi_freq_table <- function(
    d
    , var1                # 1st categorical variable
    , var2=var1           # 2nd categorical variable
){
  # browser()
  # d <- ds1 # load the data set before testing the function
  # var1 = "sex"
  # var2 = "t_stage"
  # to ensure only one variable is passed if univariate
  ls_var_names <- list(var1, var2)
  if( ls_var_names[1][[1]] == ls_var_names[2][[1]] ){
    ls_var_names <- ls_var_names[-2]
  }

  d1 <- d %>%
    # dplyr::group_by(.dots = unique(c(var1, var2)))%>% # simpler but depricated
    dplyr::group_by(!!!rlang::syms(ls_var_names)) %>%
    dplyr::summarize(
      row_count = n()
      ,id_count = n_distinct(person_oid) # counts UNIUQE persons
      ,.groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      id_count_total = sum(id_count, na.rm =T)
    ) %>%
    # dplyr::group_by(.dots = var1) %>%
    dplyr::group_by(!!rlang::sym(var1)) %>%
    dplyr::mutate(
      var1_count = sum(id_count, na.rm = T)

      ,var1_prop = (var1_count/id_count_total)
      ,var12_prop = (id_count/var1_count)
      #
      ,var1_pct = scales::label_percent(accuracy = .1)(var1_prop)
      ,var12_pct = scales::label_percent(accuracy=.1)(var12_prop)
    ) %>%
    ungroup()

  # ,pct_1 = scales::label_percent()(total_1/total)
  # ,pct_12 = scales::label_percent()(n_people/total_1)
  #
  n_total <-  d1 %>% pull(id_count_total) %>% unique()
  return(d1)
}
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state", var2="sex")
# ds2 %>% make_bi_freq_table(var1="employment_state")
# ds2 %>% make_bi_freq_table(var1="gender", var2="employment_state")
# ds2 %>% make_bi_freq_table("sex","employment_state")
# View relationship between two categorical variables as a set of bar graphs
make_bi_freq_graph <- function(
    d
    ,var1                # 1st categorical variable
    ,var2     = var1     # 2nd categorical variable
    ,voption  ="plasma"  # a palette from {viridis} package
    ,n_breaks = 10       # number of labeled values on horizontal axis
    ,rev      = FALSE    # reverse the color gradient
){
  # var1 = "gender"
  # var1 = "is_episode_count"
  # var2 = var1
  # voption="plasma"
  # n_breaks = 10
  # d <- ds2
  # browser()
  d1 <- d  %>%
    make_bi_freq_table(var1, var2) %>%
    mutate_at(
      .vars = all_of(var1)
      ,.funs = ~factor(.data[[var1]])
    ) %>%
    mutate_at(
      .vars = all_of(var2)
      ,.funs = ~factor(.data[[var2]])
    )
  if(rev){
    d1 <- d1 %>%
      mutate_at(
        .vars = all_of(var1)
        ,.funs = ~fct_rev(.data[[var1]])

      )
  }

  d2 <- d1 %>% filter(!is.na(.data[[var1]])) # drop empty factors

  n_total <-  d2 %>% pull(id_count) %>% unique()

  g1 <- d2 %>%
    ggplot2::ggplot(ggplot2::aes_string(x =var1, y = "id_count", fill = var2 ))+
    ggplot2::geom_col(position = ggplot2::position_dodge(), alpha = .7, color = "black")+
    # ggplot2::geom_text(ggplot2::aes(label = n_people),position = ggplot2::position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
    ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = voption
                                  ,guide= guide_legend(reverse=T)
    )+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(
      expand=ggplot2::expansion(mult = c(0,.1))
      , labels = scales::comma_format()
      # ,breaks =pretty_breaks()
      ,breaks = scales::breaks_pretty(n=n_breaks)

    )
  g1
  if(var1 == var2){
    g1 <- g1 +
      ggplot2::geom_text(ggplot2::aes_string(label = "var1_pct"),position = ggplot2::position_dodge(.9), hjust = -0.1, color = "black", size = 4)+
      ggplot2::labs(y = "Count", title = paste0("Frequency distribution of (",var1,")"))
  }else{
    g1 <- g1 +
      ggplot2::geom_text(ggplot2::aes_string(label = "var12_pct"),position = ggplot2::position_dodge(.9)
                         # , vjust = -.5
                         ,hjust = -0.1
                         , color = "black", size = 4)
    g1 <- g1 + ggplot2::labs(y = "Number of respondents", title = paste0("Association between (",var1,") and (",var2,")"))
  }


  return(g1)
}
# How to use
# ds2 %>% make_bi_freq_graph(var1="sex", var2="employment_state")
# ds2 %>% make_bi_freq_graph(var1="employment_state", var2="sex")


# Input = two categorical variables, var2 - used as dependent
run_contingency <- function(d,var1, var2){
  # browser()
  # d <- ds1
  # var1 <- "sex"
  # var2 <- "ulcer"

  # d1 <- d %>% select(.data[[var1]], .data[[var2]]) %>% na.omit()
  # v1 <- d1 %>% select(.data[[var1]]) %>% mutate_all(., as.character) %>% pull()
  # v2 <- d1 %>% select(.data[[var2]]) %>% mutate_all(., as.character) %>% pull()
  #
  d1 <- d %>%
    # select(.data[[var1]], .data[[var2]]) %>%
    select(all_of(c(var1,var2))) %>%
    na.omit() %>%
    mutate_all(., as.character) # to ensure factors and integers are treated as factors

  # (crosstab <- table(v1,v2))
  (crosstab <- table(d1[[var1]], d1[[var2]]))

  # Interpretations (min, max)
  # contingency_c   - (0,1) - Misfit adjusted for sample size
  # cramer_v        - (0,1) - Strength of association (none to perfect)
  # uncertainty_c   - (0,1) - Entropy/ given Y, what fraction of the bits of X can we predict?
  # kruskal_lambda  - (0,1) - Proportional Reduction in Error

  chisq.test(crosstab) %>% print()

  # Contingency Coefficient Corrected - rescaled to be 0 to 1 (C_max is not bound by 1 and depends on dims(crosstab))
  # measures the degree of association (from none to perfect),
  contingency_coef_corrected <- DescTools::ContCoef(crosstab, correct = TRUE)
  # Cramer's V Unbiased - (bias = lower N tends to overestimate effect)
  cramer_v_unbiased <- rcompanion::cramerV(crosstab, bias.correct = TRUE)
  # Uncertainty Coefficient - Measure of Entropy
  # uncertainty_c  <- DescTools::UncertCoef(crosstab, direction = "column", conf.level = NA, p.zero.correction = 1/(sum(crosstab)^2) )
  uncertainty_c  <- DescTools::UncertCoef(crosstab, direction = "column" )
  # Proportional reduction in error - Goodman-Kruskal Lambda
  kruskal_lambda <- DescTools::Lambda(crosstab, direction = "column") # Count( var1) is independed. X(var1) predicts Y(var2)
  # for verification see https://rcompanion.org/handbook/H_10.html

  # crosstab %>% DescTools::Assocs() # view a useful set of indices

  lto <- list(
    "varnames"        = c("var1"=var1, "var2"=var2)
    ,"crosstab"       = crosstab
    ,"test_values" = list(
      "contingency_c"   = contingency_coef_corrected
      ,"cramer_v"       = cramer_v_unbiased
      ,"uncertainty_c"  = uncertainty_c
      ,"kruskal_lambda" = kruskal_lambda
    )

  )
  return(lto)
}
# ls_crosstab <- ds2 %>% run_contingency("sex", "employment_state")
# ls_crosstab$varnames      # names of the two dimension used to create crosstab
# ls_crosstab$crosstab      # cross-tabulation of counts (rows by default)
# ls_crosstab$test_values   # curated set of indicies obtained from crosstab

# ----- modeling-functions ---------------
get_rsquared <- function(m){
  cat("R-Squared, Proportion of Variance Explained = ",
      scales::percent((1 - (summary(m)$deviance/summary(m)$null.deviance)),accuracy = .01)
      , "\n")
}

get_model_fit <- function(m, print=T){
  mfit <- list(
    "chisquare" =  with(m, null.deviance - deviance)
    ,"df"       = with(m, df.null - df.residual)
    ,"pvalue"   = with(m, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    ,"aic"      = m$aic
    ,"rsquare"  = (1 - (summary(m)$deviance/summary(m)$null.deviance)) # variance explained
  )
  if(print){
    cat("MODEL FIT",
        "\nChi-Square = ", mfit$chisquare,
        "\ndf = ", mfit$df,
        "\np-value = ", mfit$pvalue,"\n"
    )
  }

  return(mfit)
}

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
  pal_direction_significance <-  c( # turn on when absent from environment
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
  # browser()
  # 3 - PREDICTED
  # generate model prediction for all unique combos of values on all predictors
  d_predicted_unique <- d %>%
    group_by(!!!rlang::syms(all_of(explanatory))) %>%
    summarize(count = n(),.groups="keep") %>%
    ungroup() %>%
    dplyr::mutate(
      probability = predict(object = model, newdata = .,type = "response")
      # In other way, to verify the steps
      ,log_odds = predict(object = model, newdata = .)
      ,probability2 = plogis(log_odds) # same thing, double checke
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
    pROC::ggroc(color = "red")+
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
  t_full    <- ls_model$full$model %>% gtsummary::tbl_regression(exponentiate=T)
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



# ----- graphing ----------------

# Compare Odds Ratios (OR) across models (Full, Reduced, Null)
# make_odds_ratios_graph <- function(ls_model, var1, var2){
make_odds_ratios_graph <- function(
    ls_model
){
  # browser()

  pal_direction_significance <-  c( # turn on, when absent in environment
    "Increase (99%)"   = "#2b8cbe"
    ,"Increase (95%)"  = "#7bccc4"
    ,"Increase (90%)"  = "#bae4bc"
    ,"Not Significant" = "NA"
    ,"Decrease (90%)"  = "#fdcc8a"
    ,"Decrease (95%)"  = "#fc8d59"
    ,"Decrease (99%)"  = "#d7301f"
  )
  # Identify components
  l_model <- ls_model$full
  var_focal <- l_model$equation$explanatory[1]
  d_est     <- l_model$estimates

  g <- d_est %>%

    filter(var_name==var_focal) %>%
    mutate(
      value_level = factor(value_level, levels = l_model$focal_factor$levels) %>%
        fct_rev()
    ) %>%
    ggplot(aes(x=conv_odds, y = value_level, fill = sign_direction ))+
    # ggplot(aes(x=or_prop, y = predictor, fill = sign_direction))+
    geom_col( color = "black", size = .3)+
    # coord_flip()+
    scale_fill_manual(values = pal_direction_significance)+
    geom_vline(xintercept = 0,  color = "black")+
    geom_text(aes(label = numformat(conv_odds,2), x = conv_odds,
                  hjust = ifelse(conv_odds>=0,-.5,1.1)), color = "grey40", size = 3
    )+
    scale_x_continuous(expand = expansion(mult=c(.3,.3)), breaks = seq(-10,10,1),
                       minor_breaks = seq(-10,10, .2))+
    scale_y_discrete(position = "right", drop = F)+
    #
    # facet_grid(predictor_group~outcome, scales = "free_y", space = "free")+
    # labs(
    #   title = "Factors affecting the outcome of the Income Support program"
    #   ,subtitle = "Measured at 3 months after the exit from the program"
    #   ,caption = "Interpretation example: Married clients are 51% less likely to be in School or Training"
    #   ,y = ""
    #   ,x = "Change in Odds Ratio relative to baseline"
    #   ,fill = "Effect on likelihood of outcome\n(significant at probability level)"
    # )+
    theme(
      plot.caption = element_text(hjust = -0, size = 8, color = "grey40")
      # ,legend.position = "none"
      ,axis.title.y = element_blank()
    )
  g
  return(g)
}
# How to use
# ls_model <- run_logistic_binary_model_comparison(ds2,"has_ea",c("age_group", "sex"))
# g <- ls_model %>%
#   make_odds_ratios_graph()
# g
# View relationship between two categorical variables as a set of bar graphs
make_bi_contingency_graph_components <- function(
    d
    ,var1                # 1st categorical variable - predictor
    ,var2     = var1     # 2nd categorical variable - outcome
    ,voption  ="plasma"  # a palette from {viridis} package
    ,n_breaks = 5       # number of labeled values on horizontal axis
    ,rev      = TRUE    # reverse the color gradient
){
  # var1 = "sex"
  # var2 = "ulcer"
  # # var2 = var1
  # voption="plasma"
  # n_breaks = 10
  # rev = FALSE
  # d <- ds2


  d1 <- d  %>%
    make_bi_freq_table(var1, var2) %>%
    mutate_at(
      .vars = all_of(var1) # factorize regardless of type
      ,.funs = ~factor(.data[[var1]])
    ) %>%
    mutate_at(
      .vars = all_of(var2) # factorize regardless of type
      ,.funs = ~factor(.data[[var2]])
    )
  if(rev){
    d1 <- d1 %>%
      mutate_at(
        .vars = all_of(var1)
        ,.funs = ~fct_rev(.data[[var1]])
      )
  }
  # browser()
  d2 <- d1 %>% na.omit()
  n_total <-  d2 %>% pull(id_count) %>% sum()
  # browser()
  g0 <- d2 %>%
    ggplot2::ggplot(ggplot2::aes_string(x =var1, y = "id_count", fill = var2 ))+
    # ggplot2::geom_col(position = ggplot2::position_stack(), alpha = .7, color = "black")+
    # ggplot2::geom_col(position = ggplot2::position_fill(), alpha = .7, color = "black")+
    ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = voption
                                  ,guide= guide_legend(reverse=T)
    )+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(
      # expand=ggplot2::expansion(mult = c(0,.1))
      labels = scales::comma_format()
      # ,breaks =pretty_breaks()
      ,breaks = scales::breaks_pretty(n=n_breaks)
    )
  # g1
  g1 <- g0 +
    ggplot2::geom_col(position = ggplot2::position_stack(), alpha = .7, color = "black")+
    theme(
      # axis.title.y = element_blank()
      # axis.text.y = element_blank()
      # ,axis.ticks.y = element_blank()
      # ,legend.position = "none"
    )+
    labs(
      y = "Count"
    )
  # g2
  g2 <- g0 +
    ggplot2::geom_col(position = ggplot2::position_fill(), alpha = .7, color = "black")+
    theme(
      # axis.title.y = element_blank()
      # ,axis.text.y = element_text(hjust = -0)
      # ,axis.ticks.y = element_blank()
      # ,legend.position = "none"
    )+
    labs(
      y = "Percent"
    )
  # g3
  t1 <- d %>% run_contingency(var1, var2) # X(var1) predicts
  t1$crosstab %>% DescTools::Assocs()
  v_chitest <- t1$crosstab %>% chisq.test() %>% broom::tidy()
  chi_test_result <- paste0(
    "Chi-Squared = ", scales::comma(v_chitest$statistic, accuracy = .1),
    ", df = ", v_chitest$parameter,
    ", p-value < ", scales::comma(v_chitest$p.value, accuracy = .0001)
  )
  # t1
  index_name <- c(
    "contingency_c"     = "Contingency\nCoefficient"
    ,"cramer_v"         = "Cramer's\n V"
    ,"kruskal_lambda"   = "Kruskal\n Lambda"
    ,"uncertainty_c"    = "Uncertainty\n/ Entropy"
  )
  t1$test_values
  d_test <- t1$test_values %>%
    as_tibble() %>%
    pivot_longer(names_to = "measure_name", values_to = "value", cols = names(.)) %>%
    mutate(
      measure_name = factor(
        measure_name
        ,levels = names(index_name)
        ,labels = index_name
      )
    )

  g3 <- d_test %>%
    ggplot(aes(x=measure_name, y = value, fill = measure_name))+
    geom_col(alpha = .7, color = "black")+
    geom_text(
      aes(label = numformat(value,decimal_count = 3))
      ,vjust = -.5
      # ,size = 8
    )+
    scale_y_continuous(

      # limits = c(0,.5)
      # breaks = seq(0,1,.1)
      # ,minor_breaks = seq(0,1,.05)
      expand=ggplot2::expansion(mult = c(0,.3))
      ,labels = RemoveLeadingZero

    )+
    # scale_x_discrete(
    #   position="top"
    # )+
    scale_fill_brewer(
      # RColorBrewer::brewer.pal.info
      type = "qual", palette = 2, direction = 1
    )+
    theme(
      legend.position = "none"
      ,axis.ticks.x = element_blank()
      ,panel.grid.major.x = element_blank()
    )+
    labs(y = NULL, x = NULL
         # ,title = paste0("Measures of Association between (",var1,") and (",var2,")")
         ,caption = paste0(
           "Direction (C|R): (",var1,") predicts (",var2,")\n"
           ,chi_test_result
         )
    )
  g3

  return(list( "stack"=g1, "fill" = g2, "measures" = g3))
}
# How to use
# var1 <- "age_group"
# var2 <- "has_ea"
# ls_bi_graph <- d %>% make_bi_contingency_graph_components(var1,var2)
# ls_bi_graph$stack
# ls_bi_graph$fill
# ls_bi_graph$measures


make_bi_contingency_graph <- function(
    d
    , var1
    , var2
){
  # d <- ds1
  # var1 = "age_group"
  # var2 = "sex"
  # Assemble the final display
  # ds3 %>% make_bi_contingency_graph("has_ea")



  ls_graph <- d %>% make_bi_contingency_graph_components(var1,var2)

  p1 <- ls_graph$stack
  p2 <- ls_graph$fill
  p3 <- ls_graph$measures

  # store the legend to share later
  legend <- cowplot::get_legend(
    p2 +
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  combined_plot <- cowplot::plot_grid(
    p1 + theme(legend.position = "left" )+scale_x_discrete(position="bottom")
    ,p2 + theme(legend.position = "none")
    ,p3 + theme(legend.position = "none")
    , labels = c('', '')
    , label_size = 12
    ,nrow =1
    ,rel_widths = c(.35,.35, .3)
  )
  # Title to share
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Assossiation between two categorical variables",
      fontface = 'bold',
      x = 0,
      hjust = 0,
      size = 12,
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )

  plot_row <- cowplot::plot_grid(
    title
    ,combined_plot
    # ,legend
    ,ncol = 1
    # rel_heights values control vertical title margins
    ,rel_heights = c(0.1, 1)

  )+
    theme(
      plot.background =element_rect(fill = "white", color = "white")
    )
  plot_row
  # plot_row %>% quick_save("0-combined-1.jpg",width = 10, height = 3)
}
# How to use
# ds2 %>%
#   make_bi_contingency_graph("age_group","has_ea") %>%
#   # make_bi_contingency_graph("has_ea","age_group") %>%  # keep "outcome" binary for now
#   quick_save("0-combined-1",width = 10, height = 3)


# ---- To Rule Them All ---------------

get_bi_test_objects <- function(
    d
    ,dependent
    ,explanatory # the first will be treated as focal
    ,clear_cache = FALSE
    ,get_cache = TRUE # load file from disk by default
){
  # d <- ds2
  # dependent = "has_ea"
  # explanatory = c("age_group","sex")
  # browser()
  # Designate variable roles
  var1 <- explanatory[1]
  var2 <- dependent
  # to save the model file to cache for easier graphing later
  model_name <- paste0(c(dependent, explanatory), collapse = "-")
  model_path <- paste0(data_cache_folder,model_name,".rds")

  # delete cache file if exisits
  if(clear_cache==TRUE){
    # testit::assert("File for this model does not exist", file.exists(model_path))
    if(file.exists(model_path)){file.remove(model_path)}
  }
  # read the cache file if exists
  if(get_cache==FALSE){ # if TRUE reads the cached object, if FALSE computes it
    # ls_model    <- d %>% run_logistic_binary_model_comparison("has_ea", c("age_group","sex"))
    ls_model    <- d %>% run_logistic_binary_model_comparison(var2, explanatory)
    # ls_crosstab <- d %>% run_contingency("has_ea","age_group")
    ls_crosstab <- d %>% run_contingency(var2,var1)
    # ls_bi_graph <- d %>%  make_bi_contingency_graph_components(var1          , var2=)
    ls_bi_graph <- d %>%  make_bi_contingency_graph_components(var1=var1, var2=var2)
    dto <- list(
      "ls_model" = ls_model
      ,"ls_crosstab" = ls_crosstab
      ,"ls_bi_graph" = ls_bi_graph
      ,"model_name" = model_name
    )
    # readr::write_rds(dto, model_path, compress = "xz")
    readr::write_rds(dto, model_path)
  }else{
    testit::assert("File for this model does not exist", file.exists(model_path))
    dto <- readr::read_rds(model_path)

  }
  return(dto)
  # Components we'll need for display A
  # p1 <- dto$ls_bi_graph$stack
  # p2 <- dto$ls_bi_graph$fill
  # p3 <- dto$ls_model %>% make_odds_ratios_graph()
  # p4 <- dto$ls_bi_graph$ls_graph$measures
  # p5 <- ls_model$full$roc$roc_plot
}
# data objects
# How to use
# dto <- get_bi_test_objects(
#   d
#   ,dependent    = "has_ea"
#   ,explanatory = c("age_group","sex")
#   ,cache = T
# )
# Produces
# dto$

# to split the title into 2 lines
multi_line2 <- function(x){
  paste0(
    x %>% substr(1,round(nchar(x)/2))
    ,"\n"
    ,x %>% substr(round(nchar(x)/2),nchar(x))
  )
}

# split a formula object into two lines
multi_line2_formula <- function(x){
  fobject <- x %>% as.character()
  fstring <- paste0(fobject[2]," ~ ", fobject[3])

  paste0(
    fstring %>% substr(1,round(nchar(fstring)/2))
    ,"\n"
    ,fstring %>% substr(round(nchar(fstring)/2),nchar(fstring))
  )
}
# dto$ls_model$full$equation$formula  %>% multi_line2_formula()


make_bi_test_output_A <- function(
    dto # made by get_bi_test_object
    ,d
    ,...
){
  # d <- ds2
  #Identify components used in display
  p1 <- dto$ls_bi_graph$stack # horiz bi bar counts
  p2 <- dto$ls_bi_graph$fill  # horiz bi bar percent
  p3 <- dto$ls_model %>% make_odds_ratios_graph() # Converted Odds
  p4 <- dto$ls_bi_graph$measures # Measures of association
  p5 <- dto$ls_model$full$roc$roc_plot # ROC plot
  # p6 <- dto$ls_model$compare$estimate_plot

  var1 <- dto$ls_crosstab$varnames[["var1"]]
  var2 <- dto$ls_crosstab$varnames[["var2"]]
  p7 <- d %>% make_bi_freq_graph(var1, var2,n_breaks = 4)
  p8 <- d %>% make_bi_freq_graph(var2, var1,n_breaks = 4)
  # browser()
  # store the legend to share later
  legend <- cowplot::get_legend(
    p1 +
      # labs(fill="Dependend/Outcome Variable: ")+
      guides(color = guide_legend(nrow = 1)) +
      theme(
        legend.position = "top"
        ,legend.title = element_text(size = 16)

      )
  )

  ### - First row of Display A : three horizontal bar-plots
  ### - Counts + Percent + Change in Odds
  top_row_effects <- cowplot::plot_grid(
    p1 +
      labs(title="Person Count", y = NULL)+
      theme(
        legend.position = "none"
        ,axis.title.y = element_blank()
        ,plot.title.position = "panel"
        ,plot.title = element_text(hjust = .5)
      )

    ,p2 +
      labs(title="Group Percent", y = NULL)+
      theme(
        legend.position = "none"
        ,axis.text.y = element_blank()
        ,axis.ticks.y = element_blank()
        ,plot.title = element_text(hjust = .5)
      )
    ,p3 +
      labs(title="Change in Odds", x = NULL)+
      theme(
        legend.position = "none"
        ,plot.title = element_text(hjust = .5)
      )
    # , labels = c('xx', 'yy','dd')
    , label_size = 12
    ,nrow =1
    ,rel_widths = c(.35,.35, .3)
  )
  # top_row_effects



  second_row_performance <- cowplot::plot_grid(
    plot.new()
    ,p4 +
      labs(
        y = "Measures\nof\nAssociation"
      )+
      theme(
        axis.title.y = element_text(angle = 0,vjust = .5)
      )
    ,p5
    ,nrow = 1
    ,rel_widths = c(.2,.6,.4)
  )

  third_row_bi_graphs <- cowplot::plot_grid(
    p7
    ,p8
    ,nrow=1
    ,rel_widths = c(.5, .5)
  )
  # browser()
  title_final <- (cowplot::ggdraw()+cowplot::draw_label(dto$ls_model$full$equation))
  caption_final <- cowplot::ggdraw()+cowplot::draw_label(dto$ls_model$compare$test)
  final_plot <- cowplot::plot_grid(
    title_final
    ,legend
    ,top_row_effects
    ,second_row_performance
    ,caption_final
    ,third_row_bi_graphs
    ,ncol = 1
    # rel_heights values control vertical title margins
    ,rel_heights = c(.1, .1, .5, .6,.2, .7)

  )+
    theme(
      plot.background =element_rect(fill = "white", color = "white")
    )
  # final_plot
  # final_plot %>% quick_save(dto$model_name,width = 10, height = 5)
  ggplot2::ggsave(
    filename = paste0(dto$model_name,".jpg"),
    plot     = final_plot,
    device   = "jpg",
    path     = prints_folder,
    width    = 10,
    height   = 8,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )

  return(final_plot)



}
# How to use
# gA <- dto %>% make_bi_test_output_A()

make_bi_test_output_B <- function(dto){
  # browser()
  table_plot_path <- paste0(prints_folder,dto$model_name,".png" )
  g_out <-
    dto$ls_model$compare$table %>%    # build gtsummary table
    gtsummary::as_gt()              # convert to gt table
  g_out %>% gt::gtsave(filename = table_plot_path)             # save table as image
  return(g_out)
}
# How to use
# dto %>% make_bi_test_output_B()



