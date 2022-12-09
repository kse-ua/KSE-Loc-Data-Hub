# a copy of this script has been created on 2016-07-30 and called ./scripts/factor_pattern_plot.R
# TODO: point to the script creating the object FPM.matrix, the source object
# fpmFunction is used to create output$ objects in server.R
fpmFunction <- function( FPM.matrix, mainTitle=NULL ) {
  roundingDigits <- 3 #Let the caller define in the future (especially with observed values that aren't close to 1.0).
  stripSize <- 11  #Let the caller define in the future?
  valuelabelSize <- 4 #7 # the values of the factor loadings
  axisTitleFontSize <- 12
  axisTextFontSize <- 12
  legendTextFontSize <- 6 #
  keep_factors <- paste0("F",1:3) # number of columns to show
  show_factors <- paste0("F",1:k) # number of factors to print
  titleSize <- 14 #
  # Data prep for ggplot
  dsFORp <- reshape2::melt(FPM.matrix, id.vars=rownames(FPM.matrix))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
  dsFORp <- plyr::rename(dsFORp, replace=c(Var1="Variable", Var2="Factor", value="Loading"))
 # browser()
  dsFORp$Positive <- ifelse(dsFORp$Loading >= 0, "Positive", "Negative") #Or see Recipe 10.8
  dsFORp$LoadingAbs <- abs(dsFORp$Loading) # Long form
#   dsFORp$LoadingPretty <- round(abs(dsFORp$Loading), roundingDigits) # Long form
  dsFORp$LoadingPretty <- round(dsFORp$Loading, roundingDigits) # Long form

  dsFORp$LoadingPretty <- paste0(ifelse(dsFORp$Loading>=0, "+", "-"), dsFORp$LoadingPretty)
  #   dsFORp$VariablePretty <- gsub(pattern="_", replacement="\n", x=dsFORp$Variable)
  dsFORp$VariablePretty <- gsub(pattern=" ", replacement="\n", x=dsFORp$Variable)
  # colors <- c("FALSE"="darksalmon" ,"TRUE"="lightskyblue") # The colors for negative and positve values of factor loadings for ggplot

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
  dsFORp_ <- dsFORp %>% dplyr::filter(Factor %in% keep_factors)# %>% dplyr::slice(1:20)
  # Graph definition
  pp <- ggplot(dsFORp_,aes(x=Factor,y=LoadingAbs,fill=Positive,color=Positive,label=LoadingPretty)) +
    geom_bar(stat="identity", na.rm=T) +
    geom_text(y=0, vjust=-.1,size=valuelabelSize, na.rm=T) +
    scale_color_manual(values=colorFont, guide="none") +
    scale_fill_manual(values=colorsFill) +
    #   scale_fill_discrete(h=c(0,360)+15, c=100, l=65, h.start=0, direction=1, na.value="grey50") + #http://docs.ggplot2.org/0.9.3/scale_hue.html
    # scale_y_continuous(limits=c(0,1.1), breaks=c(.5,1), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,1.1), breaks=c(0), expand=c(0,0)) +
    # scale_x_discrete(breaks = show_factors)+
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
  pp
#     theme(legend.text=element_text(size=legendTextFontSize))

#   if( k < p ) { #If there's space in the lower right corner of the plot area, then use it.
#     pp <- pp + theme(legend.position=c(1, 0), legend.justification=c(1, 0))
#     pp <- pp + theme(legend.background=element_rect(fill="gray70"))
#   }
#   else { #Otherwise, put it outside the plot area.
#     pp <- pp + theme(legend.position="left")
#   }

  return( pp )
}

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


# ---- print-factor-pattern ---------------------------
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


# ----- general-fitting-function ---------------------
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
    ###### Promax ###############
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
    ###### Quartimin ###################
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
