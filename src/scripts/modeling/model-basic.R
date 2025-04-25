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
