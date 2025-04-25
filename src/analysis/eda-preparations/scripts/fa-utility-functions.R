
compute_CFI <- function(chi_null,df_null,chi_model,df_model){
  d_null <- chi_null - df_null
  d_model <- chi_model - df_model
  (cfi <- (d_null - d_model) / d_null)
  return(cfi)
}

compute_TLI <- function(chi_null,df_null,chi_model,df_model){
  (r_null <- chi_null/df_null)
  (r_model <- chi_model/df_model)
  (tli <- (r_null - r_model)/((r_null)-1))
  return(tli)
}

solve_factors <- function(corrmat,min,max,sample_size,fm="minres",rotation="none"){
  # corrmat = R0
  # maxfactors = 10
  # sample_size = 643
  # rotation = "bifactor"
  # rotation = "none"
  ls_fit <- list()
  for(i in c(min:max) ){
    # i <- 2
    ls_fit[[paste(i)]] <- psych::fa(
      r = corrmat,
      nfactors = i,
      n.obs = sample_size,
      fm = fm,
      rotate = rotation
    )
  }
  return(ls_fit)
  
} 
# ls_solution <- solve_factors(R0,1,15,643)

get_indices <- function(ls){
  # ls <- ls_solution
  # i <- 1
  indices <- list()
  for(i in seq_along(names(ls)) ){
    indices[["n_factors"]][[i]] <- names(ls)[i] %>% as.numeric()
    indices[["chisq_null"]][[i]] <- ls[[i]]$null.chisq
    indices[["df_null"]][[i]] <- ls[[i]]$null.dof
    indices[["chisq"]][[i]] <- ls[[i]]$chi
    indices[["df"]][[i]] <- ls[[i]]$dof
    indices[["RMSEA"]][[i]] <- ls[[i]]$RMSEA["RMSEA"]
    indices[["RMSEA_low"]][[i]] <- ls[[i]]$RMSEA["lower"]
    indices[["RMSEA_high"]][[i]] <- ls[[i]]$RMSEA["upper"]
    indices[["TLI2"]][[i]] <- ls[[i]]$TLI
  }
  d <- as.data.frame(indices) %>% 
    dplyr::mutate(
      CFI = ((chisq_null-df_null) - (chisq-df))/(chisq_null-df_null),
      # TLI = ((chisq_null/df_null) - (chisq-df))/((chisq_null/df_null)) 
      TLI = ((chisq_null/df_null) - (chisq/df))/((chisq_null/df_null)-1)
    ) %>% 
    dplyr::select(
      n_factors, 
      chisq_null, df_null,
      chisq, df, CFI, TLI
      # , TLI2
      # , RMSEA, RMSEA_low, RMSEA_high
    )
  
  return(d)
}
# ds_index <- get_indices(ls_solution)
plot_fit_indices <- function(x){
  x <- ds_index
  d <- x %>% 
    tidyr::gather_(key="index",value = "value",c("CFI","TLI"))   
  d %>% 
    ggplot2::ggplot(aes(x=n_factors, y=value,shape=index))+
    geom_abline(intercept=.95,size=1,alpha=.5, slope=0,linetype="dotted", color="black")+
    geom_point(size = 3)+
    scale_shape_manual(values = c("TLI"=22, "CFI"=24))+
    geom_line(aes(group=index, linetype=index))+
    scale_x_continuous(breaks=c(1:15))+
    scale_y_continuous(breaks=seq(0,1,.1), limits=c(.6,1.1))+
    main_theme 
}
# ----- fitting-functions ---------------------

fit_rotate <- function(
  R,             # correlation matrix
  k,             # number of factors 
  sample_size,   # number of observations
  rotation,      # method
  save_file = F, # save the factor pattern matrix?
  folder = NULL  # location for saving files
){
  # Values for testing and development
  # R = R0
  # k = 6
  # sample_size = 643
  # rotation = "quartimin"
  # 
  A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
  L <- A$loadings
  if(rotation=="oblimin"  ){rotation_string <- "(L, Tmat=diag(ncol(L)), gam=0,               normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="quartimin"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="targetT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="targetQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="pstT"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="pstQ"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="oblimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="entropy"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="quartimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="Varimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="simplimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),           k=nrow(L), normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="bentlerT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="bentlerQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="tandemI"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="tandemII" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="geominT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="geominQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="cfT"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="cfQ"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="infomaxT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="infomaxQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="mccammon" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="bifactorT"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation=="bifactorQ"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  
  rotated_solution <- base::eval(base::parse(text=paste0(rotation,rotation_string)))  
  return(rotated_solution)
}
# Usage
# solution <- fit_rotate(
#   R           = R0,          # correlation matrix
#   k           = 6,          # number of factors
#   sample_size = 643,        # number of observations
#   rotation    = "oblimin",
#   save_file   = F,          # save the factor pattern matrix?
#   folder      = NULL        # location for saving files
# )
# str(solution)


fit_rotate_best <- function(
  R,             # correlation matrix
  k,             # number of factors 
  sample_size,   # number of observations
  rotation,      # method
  random.starts = 15, # replications in FindBestPattern()
  maxit         = 1000, # maximu iteration for factanal()
  num.digits    = 3, # option for print.FLS
  cutoff        = 0.30, # option for print.FLS
  promax.m      = 3 # option for promax
  # sort          = FALSE # order rows in FixPattern()
){
  # Values for testing and development
  # R = R0
  # k = 6
  # sample_size = 643
  # rotation = "quartimin"
  # random.starts = 15 # replications in FindBestPattern()
  # maxit         = 1000 # maximu iteration for factanal()
  # num.digits    = 3
  # cutoff        = 0.30
  # promax.m      = 3 # option for promax
  # sort_          = TRUE # order rows in FixPattern()
  # 
  # get the matrix of factor loadings (factor pattern matrix)
  solution <- factanal(covmat=R,n.obs=sample_size,factors=k,maxit=maxit,rotation="none")
  p <- dim(solution$loadings)[1]
  m <- dim(solution$loadings)[2]
  A <- solution$loadings[1:p,]
  
  factor.labels <- paste("F",1:m,sep="")
  # plot_factor_pattern(A, factor_width = 8) %>% quick_save("01-unrotated")
  
  if(rotation == "Varimax"){
    # now we rotate the initial matrix of factor patterns
    A.varimax <- varimax(A)$loadings[1:p,]
    # examine the rotated matrix
    # plot_factor_pattern(A.varimax, factor_width = 8) %>% quick_save("02-varimax-a")
    # Variamx
    res <- list(Lh=A.varimax,orthogonal=TRUE)
    res <- FixPattern(res, sort=FALSE)
    A.varimax <- list(F=res$Lh)
    A <- A.varimax
    # plot_factor_pattern(A.varimax$F, factor_width = 8) %>% quick_save("02-varimax-b")
  }
  
  if(rotation == "promax"){
    res <- GPromax(A,pow=promax.m)
    # plot_factor_pattern(res$Lh, factor_width = 8) %>% quick_save("03-promax-a")
    res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
    res <- FixPattern(res, sort=FALSE)
    Phi.promax <- res$Phi
    A.promax <- list(F=res$Lh,Phi=Phi.promax,orthogonal=FALSE)
    A <- A.promax
    # plot_factor_pattern(A.promax$F, factor_width = 8) %>% quick_save("03-promax-b")
    # cat(".")
  }
  
  if(rotation == "quartiminQ"){
    res <- FindBestPattern(A,"quartimin",reps=random.starts,is.oblique=TRUE)
    # plot_factor_pattern(res$Lh, factor_width = 8) %>% quick_save("04-quartimin-a")
    res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
    res <- FixPattern(res, sort=FALSE)
    Phi.quartimin <- res$Phi
    A.quartimin <- list(F=res$Lh,Phi = Phi.quartimin)
    A <- A.quartimin
    # plot_factor_pattern(A.quartimin$F, factor_width = 8) %>% quick_save("04-quartimin-b")
    # cat(".")
  }
  
  if(rotation == "bifactorT"){
    # Bifactor
    res <- FindBestPattern(A,"bifactor",reps=random.starts)
    # plot_factor_pattern(res$Lh, factor_width = 8) %>% quick_save("05-bifactorT-a")
    orthogonal=TRUE
    res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=orthogonal)
    res <- FixPattern(res, sort=FALSE)
    Phi=NULL
    A.bifactor <- list(F=res$Lh,Phi=Phi)
    A <- A.bifactor
    # plot_factor_pattern(A.bifactor $F, factor_width = 8) %>% quick_save("05-bifactorT-b")
    # cat(".")
  }
  
  if(rotation == "bifactorQ"){
    # Bifactor Oblique
    res <- FindBestPattern(A,"bifactor",reps=random.starts,is.oblique=TRUE)
    res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
    res <- FixPattern(res, sort=FALSE)
    # cat(".")
    Phi.bifactor.oblique <- res$Phi
    A.bifactor.oblique <- list(F=res$Lh,Phi=Phi.bifactor.oblique)
    A <- A.bifactor.oblique
    # cat(".")
  }
  class(A)="FLS"
  return(A)
  
}

#Usage:
# solution <- fit_rotate_best(
#   R = R0,             # correlation matrix
#   k = 6,             # number of factors 
#   sample_size = 643,   # number of observations
#   rotation = "Varimax",      # method
#   random.starts = 15, # replications in FindBestPattern()
#   maxit         = 1000, # maximu iteration for factanal()
#   num.digits    = 3,
#   cutoff        = 0.30,
#   promax.m      = 3 # option for promax
#   # sort_         = FALSE # order rows in FixPattern()
# )
# print(solution,sort=F)
# solution$F %>% plot_factor_pattern()


# ----- inspecting-functions ----------------------
fa_model_summary <- function(model_object){
  cat("\nRMSEA: \n")
  print(RMSEA(model_object))
  cat("\nLL:",logLik(model_object))
  cat("\nAIC:",AIC(model_object))
  cat("\nAICc:",AICc(model_object))
  cat("\nBIC:",BIC(model_object))
  cat("\n")
  GetPrettyPattern(model_object, cutoff = 0.05,sort=FALSE)
}

numformat2 <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }
numformat3 <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }
numformat4 <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.4f", val)) }

sem_model_summary <- function(model_object){
  # model_object <- fit_sem_0_bifactor
  modsum <- summary(model_object,fit.indices = c("GFI","AGFI","RMSEA","AIC","AICc","BIC","CFI","NNFI")) 
  
  chisq    <- modsum$chisq 
  df_model <- modsum$df 
  df_null <-  modsum$dfNull 
  GFI <- modsum$GFI 
  AGFI <- modsum$AGFI 
  CFI <- modsum$CFI 
  NNFI <- modsum$NNFI
  AIC <- modsum$AIC 
  AICc <- modsum$AICc 
  BIC <- modsum$BIC
  RMSEA <- RMSEA(model_object) 

  # str(modsum) 
   
  cat("\nModel Chiquare = ",chisq, " | df model = ",df_model," | df null = ",df_null) 
  cat("\nGoodness-of-fit index = ", GFI) 
  cat("\nAdjusted Goodness-of-fit index = ", AGFI)
  cat("\nRMSEA index = ",numformat4(RMSEA$Point.Estimate),"         ",
      paste0(
        RMSEA$Confidence.Level*100,"% CI: (",
        numformat3(RMSEA$Lower.Limit),",",
        numformat3(RMSEA$Upper.Limit),")"
        ))
  cat("\nComparitive Fit Index (CFI =", CFI)
  cat("\nTucker Lewis Index (TLI/NNFI) = ", NNFI)
  cat("\nAkaike Information Criterion (AIC) =", AIC)
  cat("\nBayesian Information Criterion (BIC) =", BIC)
}
  


# ----- general-functions -----------------------
make_cor <- function(ds,metaData,items){
  # items <- items_phase_0
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d <- ds %>% dplyr::select_(.dots=items)
  d <- sapply(d, as.numeric)
  cormat <- cor(d)
  # str(cormat)
  names <- attr(cormat,"dimnames")[[1]]
  names(names) <- names
  for(i in names){
    domain <- as.character(metaData[metaData$name_new==i,"domain"])
    label_graph <- as.character(metaData[metaData$name_new==i,"label_graph"])
    names[i] <- paste0(gsub("foc_","",names[i]),"_", domain,"_",label_graph)
  }
  names_new <- as.character(names)
  attr(cormat,"dimnames")[[1]] <- names_new
  attr(cormat,"dimnames")[[2]] <- names_new
  # str(cormat)
  return(cormat)
}

# this funtion may need to be brought into the script for specific tweaking
quick_save <- function(g,name,folder){
  # g <- g
  # name = "0_bifactorT_1"
  # folder  = "./reports/temp_image/"
  ggplot2::ggsave(
    filename= paste0(name,".png"), 
    plot=g,
    device = png,
    path = folder, #"./reports/MLFA-study/images/",
    width = 500,
    height = 960,
    # units = "cm",
    dpi = 400,
    limitsize = FALSE
  )

}

save_fp_dummy <- function(R,n.factors,path){
  # R <- R0
  # n.factors = 6
  # path = "./analysis/fp_dummy.csv"
  varnames <- row.names(R)
  m <- as.data.frame(matrix(nrow=length(varnames),ncol=n.factors))
  # row.names(m) <- varnames
  colnames(m) <- paste0("Factor",1:n.factors)
  
  d <- data.frame(cbind(varnames,m))
  readr::write_csv(d,path)
}

input_factor_pattern <- function(path,sheet){
  # path = "./analysis/phase0_cfa_patterns.xlsx"
  # sheet = "A"
  
  model <- readxl::read_excel(path, sheet = sheet)
  item_names <- model$varnames
  factor_names <- colnames(model %>% dplyr::select(-varnames))
  m <- as.matrix(model %>% dplyr::select(-varnames))
  row.names(m) <- item_names
  colnames(m) <- factor_names
  str(m)
  x <- list(F=m)
  return(x)
}

# ---- graphing-functions ------------------------
# factor analysis stats (ggplot version)
FA.StatsGG <- function(Correlation.Matrix, n.obs, n.factors, conf=.90, maxit=1000, RMSEA.cutoff=NULL, main="RMSEA Plot", sub=NULL) {
  #This function is a ggplot2 adaption for the function written by James H. Steiger (2013): Advanced Factor Functions V1.05  2013/03/20
  runs <- length(n.factors)  
  R <- Correlation.Matrix
  maxfac <- max(n.factors)
  res <- matrix(NA, runs,8)
  roots <- eigen(R)$values
  for( i in 1:runs ) {
    output <- factanal(covmat=R, n.obs=n.obs, factors=n.factors[i], maxit=maxit)
    X2 <- output$STATISTIC
    df <- output$dof
    ci <- rmsea.ci(X2, df ,n.obs,conf)
    pvar <- sum(roots[1:n.factors[i]])
    v <- c(n.factors[i], pvar, X2, df, 1-pchisq(X2,df), ci$Point.Estimate, ci$Lower.Limit, ci$Upper.Limit)      
    res[i, ] <- v
  }
  colnames(res)=c("Factors","Cum.Eigen","Chi-Square","Df","p.value", "RMSEA.Pt","RMSEA.Lo","RMSEA.Hi")
  ds <- data.frame(FactorID=n.factors, Rmsea=res[,6], Lower=res[,7], Upper=res[,8])
  g <- ggplot(ds, aes(x=FactorID, y=Rmsea, ymin=Lower, ymax=Upper)) +
    annotate("rect", ymax=RMSEA.cutoff, ymin=-Inf, xmin=-Inf, xmax=Inf, fill="#F4A58255") +
    geom_line(size=1.5, color="#0571B0", na.rm = TRUE) +
    geom_errorbar(width=0.05, size=1.5, color="#92C5DE", na.rm = TRUE) +
    scale_x_continuous(breaks=n.factors) +
    scale_y_continuous(expand=c(0,0)) + 
    labs(title=main, x="Number of Factors", y="RMSEA") +
    theme_bw() +
    theme(panel.grid.minor=element_blank()) + 
    theme(plot.title=element_text(color="gray30", size=30)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
    theme(axis.title=element_text(color="gray30", size=18)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
    theme(axis.text.x=element_text(color="gray50", size=18, vjust=1.3)) + #(eg, V1, V2,...)
    theme(axis.text.y=element_text(color="gray50", size=18))  #(eg, 0.5, 1.0)
  
  print(g)
  
  return(res)
}

# scree plot (ggplot version)
Scree.PlotGG <- function(R, main="Scree Plot", sub=NULL){
  #This function is a ggplot2 adaption for the function written by James H. Steiger (2013): Advanced Factor Functions V1.05  2013/03/20
  roots <- eigen(R)$values
  x <- 1:dim(R)[1]    
  ds <- data.frame(x=x, roots=roots)
  g <- ggplot2::ggplot(ds, aes(x=x, y=roots)) +
    annotate("rect", ymax=1, ymin=-Inf, xmin=-Inf, xmax=Inf, fill="#F4A58255") +#rgb(1, 0, 0, alpha=.1,maxColorValue=1)) +
    geom_line(size=1.5, color="#0571B0", na.rm = TRUE) +
    geom_point(size=5, color="#92C5DE", na.rm = TRUE)+
    scale_x_continuous(breaks=x) +
    scale_y_continuous(expand=c(0,0)) +
    labs(title=main, x="Component Number", y="Eigenvalue") +
    theme_bw() +
    theme(panel.grid.minor=element_blank()) + 
    theme(plot.title=element_text(color="gray30", size=30)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
    theme(axis.title=element_text(color="gray30", size=18)) + #The labels (eg, 'Eigenvalue' & 'Component Number') 
    theme(axis.text.x=element_text(color="gray50", size=18, vjust=1.3)) + #(eg, V1, V2,...)
    theme(axis.text.y=element_text(color="gray50", size=18))  #(eg, 0.5, 1.0)
  
  print(g)
}


# print factor pattern 
# a modern version of fpmFunction()
plot_factor_pattern <- function(
  fpm,   # matrix of factor loadings, solution$loadings, Factor Pattern Matrix (fpm)
  factor_width = 10, # number of columns to show, width of canvass
  save = F,
  filename = NULL,
  mainTitle=NULL
) {
  # Values for testing & development
  # fpm <- solution$loadings
  # factor_width = 10
  
  p <- dim(fpm)[1]
  k <- dim(fpm)[2]
  
  # FPM <- solution$loadings # FPM - Factor Pattern Matrix
  fpm <- cbind(fpm, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
  colnames(fpm) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
  
  keep_factors <- paste0("F",1:factor_width) # number of columns to show, width of canvass
  show_factors <- paste0("F",1:k) # number of factors to print, fill of canvass
  
  fpm <- fpm[,1:factor_width]
  # prepare data for plotting
  dsFORp <- reshape2::melt(fpm, id.vars=rownames(fpm))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
  dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
  # browser()
  dsFORp$Positive <- ifelse(dsFORp$Loading >= 0, "Positive", "Negative") #Or see Recipe 10.8
  dsFORp$LoadingAbs <- abs(dsFORp$Loading) # Long form
  # dsFORp$LoadingPretty <- round(dsFORp$Loading, roundingDigits) # Long form
  numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }
  dsFORp$LoadingPretty <- numformat(dsFORp$Loading) # Long form
  # dsFORp$LoadingPretty <- paste0(ifelse(dsFORp$Loading>=0, "+", "-"), dsFORp$LoadingPretty)
  dsFORp$VariablePretty <- gsub(pattern=" ", replacement="\n", x=dsFORp$Variable) # seems unnecessary
  # temp <- dsFORp
  
  
  # set options 
  roundingDigits     <- 2 # Let the caller define in the future (especially with observed values that aren't close to 1.0).
  stripSize          <- 11# Let the caller define in the future?
  valuelabelSize     <- 4 # 7 # the values of the factor loadings
  axisTitleFontSize  <- 12
  axisTextFontSize   <- 12
  legendTextFontSize <- 6 # 
  titleSize          <- 14 # 
  
  # Colors for fill and font
  {
    # positive Green, negative Purple
    colorsFill <- c("Positive"="#A6DBA0" ,"Negative"="#C2A5CF") # The colors for negative and positve values of factor loadings for ggplot
    colorFont <- c("Positive"="#008837" ,"Negative"="#7B3294") # The colors for negative and positve values of factor loadings for ggplot
    
    #   # positive Organge,negative Purple
    #   colorsFill <- c("Positive"="#FDB863" ,"Negative"="#B2ABD2") # The colors for negative and positve values of factor loadings for ggplot
    #   colorFont <- c("Positive"="#E66101" ,"Negative"="#5E3C99") # The colors for negative and positve values of factor loadings for ggplot
    
    #   # Positive Teal,Negative Brown
    #   colorsFill <- c("Positive"="#80CDC1" ,"Negative"="#DFC27D") # The colors for negative and positve values of factor loadings for ggplot
    #   colorFont <- c("Positive"="#018571" ,"Negative"="#A6611A") # The colors for negative and positve values of factor loadings for ggplot
    
    #   # Positive Teal,Negative Brown
    #   colorsFill <- c("Positive"="#80CDC1" ,"Negative"="#DFC27D") # The colors for negative and positve values of factor loadings for ggplot
    #   colorFont <- c("Positive"="#018571" ,"Negative"="#A6611A") # The colors for negative and positve values of factor loadings for ggplot
    
    #   # Positive Blue, Negative Red
    #   colorsFill <- c("Positive"="#92C5DE" ,"Negative"="#F4A582") # The colors for negative and positve values of factor loadings for ggplot
    #   colorFont <- c("Positive"="#0571B0" ,"Negative"="#CA0020") # The colors for negative and positve values of factor loadings for ggplot
    
    
  } # close color theme selection
  
  # browser()
  # keep_factors <- paste0("F",1:10)
  # show_factors <- paste0("F",1:k)
  # vjust_ <- 2.2#1.3
  # stripSize <- 12 #24
  # valuelabelSize <- 4 #  7
  dsFORp <- dsFORp %>% dplyr::filter(Factor %in% keep_factors)# %>% dplyr::slice(1:20)
  temp <- dsFORp
  # Graph definition
  g <- ggplot2::ggplot(
    dsFORp,
    ggplot2::aes(
      x     = Factor,
      y     = LoadingAbs,
      fill  = Positive,
      color = Positive,
      label = LoadingPretty
    )
  ) +
    geom_bar(stat="identity", na.rm=T) +
    geom_text(y=0, vjust=-.1,size=valuelabelSize, na.rm=T) +
    scale_color_manual(values=colorFont, guide="none") +
    scale_fill_manual(values=colorsFill) +
    scale_y_continuous(limits=c(0,1.1), breaks=c(0), expand=c(0,0)) +
    facet_grid(VariablePretty ~ .) +
    labs(title=mainTitle, x="Weights", y="Loadings (Absolute)", fill=NULL) + 
    theme_minimal() +
    # theme_tufte()+
    theme(panel.grid.minor=element_blank()) + 
    theme(axis.title=element_text(color="gray30", size=axisTitleFontSize)) + #The labels (eg, 'Weights' & 'Loadings') 
    theme(axis.text.x=element_text(color="gray50", size=axisTextFontSize, vjust=1.2)) + #(eg, V1, V2,...)
    # theme(axis.text.y=element_text(color="gray50", size=axisTextFontSize)) + #(eg, 0.5, 1.0)
    theme(axis.text.y=element_blank()) + #(eg, 0.5, 1.0)
    theme(strip.text.y=element_text(angle=0, size=stripSize,hjust = 0, vjust=1)) + 
    theme(plot.title = element_text(size=titleSize, hjust=1, vjust=1)) +
    theme(legend.position="blank")
  # g
  # if(save){
  #   savePlot <- function(myPlot) {
  #     folder <- "./reports/temp_image/"
  #     png("myPlot.png")
  #     print(myPlot)
  #     dev.off()
  #   }
  #   savePlot(g)
  # }
  
  return( g )
}

# examine the relative contribution of items
# input = vector of items' Rsqures
dot_plot <- function(x, sorted = T){
  d <- data.frame(
    item = attr(x,"names"),
    value = x
  )
  if(sorted){
    g <- ggplot(d, aes(x=value, y=reorder(item, value))) 
  }
  if(!sorted){
    g <- ggplot(d, aes(x=value, y=item)) 
  }
  g <- g +
    geom_point(size=3) + # Use a larger dot
    geom_segment(aes(yend=item), xend=0, colour="grey50") +
    theme(panel.grid.major.y = element_blank()) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
    labs(title = "Relative contribution of items",
         y = "Item name", x = "R-squared")
  g
}

