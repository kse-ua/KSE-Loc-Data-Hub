### Advanced Factor Functions V1.05  2013/03/20
### 2013 James H. Steiger
packageLoaded <- function(name) 0 != length(grep(paste("^package:", 
                                                       name, "$", sep=""), search()))

if(!packageLoaded("plotrix")) library(plotrix)
if(!packageLoaded("sem")) library(sem)

# This code block includes R code for 
#
# Gradient Projection Algorithms and Software for Arbitrary
# Rotation Criteria in Factor Analysis.
#
# by:
#
# Coen A. Bernaards and Robert I. Jennrich.
#
# Website: http://www.stat.ucla.edu/research
# 

# GPForth is the main GP algorithm for orthogonal rotation.
# GPFoblq is the main GP algorithm for oblique rotation.
# For both algorithms is required: a loadings matrix A. Optional  
# a initial rotation matrix Tmat. By default this is the identity matrix.
# Optional: the rotation method to be used. Between quation marks have to
# be the last part of the name of the vgQ function, e.g. for vgQ.varimax
# the argument is "varimax". Identical arguments can be used for oblique
# rotation. Some rotation criteria (including simplimax, pst, procrustes, 
# cf,...) require one or more additional arguments. For example, simplimax
# needs the number of 'close to zero loadings'. This is given included as
# the extra argument k=27. Check out the rotation methods for details. 
# When a new rotation method is implemented, and it needs an additional
# argument then this is the easiest way to pass it to the function. 
#
# New rotation methods need to be programmed as vgQ.newmethod. The only
# inputs are the matrix L, and potential additional arguments. The
# output consists of the value f  of the criterion, its gradient Gq at L,
# and the name of the method.

GPForth <- function(A,Tmat=diag(ncol(A)),method="varimax",...){
 al <- 1
  L  <- A %*% Tmat
  VgQ <- get(paste("vgQ",method,sep="."))(L,...)
  G <- crossprod(A,VgQ$Gq)
  f <- VgQ$f
  Table <- NULL
  for (iter in 0:500){
    M <- crossprod(Tmat,G)
    S <- (M+t(M))/2
    Gp <- G - Tmat %*% S
    s <- sqrt(sum(diag(crossprod(Gp))))
    Table <- rbind(Table,c(iter,f,log10(s),al))
    if (s < 1e-5) 
      break
    al <- 2*al
    for (i in 0:10){
      X <- Tmat - al * Gp
      UDV <- svd(X)
      Tmatt <- UDV$u %*% t(UDV$v)
      L <- A %*% Tmatt
      VgQt <- get(paste("vgQ",method,sep="."))(L,...)
      if (VgQt$f < (f-.5*s^2*al))
        break
      al <- al/2
    }
    Tmat <- Tmatt
    f <- VgQt$f
    G <- crossprod(A,VgQt$Gq)
  }
  Th <- Tmat
  Lh <- L
  method <- VgQ$Method
  orthogonal <- T
  ## Added by Steiger
  colnames(Table) <- c("Iteration", "Function", "Log10(s)","Alpha")
  ##
  return(list(Lh=Lh,Th=Th,Table=Table,method=method,orthogonal=orthogonal))
}

GPFoblq <- function(A,Tmat=diag(ncol(A)),method="quartimin",...){
  al <- 1
  L <- A %*% t(solve(Tmat))
  VgQ <- get(paste("vgQ",method,sep="."))(L,...)
  G <- -t(t(L) %*% VgQ$Gq %*% solve(Tmat))
  f <- VgQ$f
  Table <- NULL
  for (iter in 0:500){
    Gp <- G-Tmat %*% diag(apply(Tmat*G,2,sum))
    s <- sqrt(sum(diag(crossprod(Gp))))
    Table <- rbind(Table,c(iter,f,log10(s),al))
    if (s < 1e-5)
      break
    al <- 2*al
    for (i in 0:10){
      X <- Tmat-al*Gp
      v <- 1/sqrt(apply(X^2,2,sum))
      Tmatt <- X %*% diag(v)
      L <- A %*% t(solve(Tmatt))
      VgQt <- get(paste("vgQ",method,sep="."))(L,...)
      if (VgQt$f < (f-.5*s^2*al))
        break
      al <- al/2
    }
    Tmat <- Tmatt
    f <- VgQt$f
    G <- -t(t(L) %*% VgQt$Gq %*% solve(Tmatt))
    
  }
  Th <- Tmat
  Lh <- L
  Phi <- t(Tmat) %*% Tmat
  method <- VgQ$Method
  orthogonal <- F
  return(list(Lh=Lh,Phi=Phi,Th=Th,Table=Table,method=method,orthogonal=orthogonal))
}


vgQ.quartimin <- function(L){
  Method="Quartimin"
  L2 <- L^2
  k <- ncol(L)
  M <- matrix(1,k,k)-diag(k)
  f <- sum(L2 * (L2 %*% M))/4
  Gq <- L * (L2 %*% M)
  return(list(Gq=Gq,f=f,Method=Method))
} 

vgQ.oblimin <- function(L,gam=0){
  Method <- paste("Oblimin g=",gam,sep="")
  if (gam == 0) Method <- "Oblimin Quartimin"
  if (gam == .5) Method <- "Oblimin Biquartimin"
  if (gam == 1) Method <- "Oblimin Covarimin"
  k <- ncol(L)
  p <- nrow(L)
  N <- matrix(1,k,k)-diag(k)
  f <- sum(L^2 * (diag(p)-gam*matrix(1/p,p,p)) %*% L^2 %*% N)/4
  Gq <- L * ((diag(p)-gam*matrix(1/p,p,p)) %*% L^2 %*% N)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.target <- function(L,Target){
  Method <- "Target rotation"
  # Needs Target matrix, e.g.  Target <- matrix(c(rep(9,4),rep(0,8),rep(9,4)),8) 
  f <- sum((L-Target)^2)
  Gq <- 2*(L-Target)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.pst <- function(L,W,Target){
  Method <- "Partially specified target"
  # Needs weight matrix W with 1's at specified values, 0 otherwise
  # e.g. W = matrix(c(rep(1,4),rep(0,8),rep(1,4)),8). 
  # When W has only 1's this is procrustes rotation
  # Needs a Target matrix Target with hypothesized factor loadings.
  # e.g. Target = matrix(0,8,2)
  Btilde <- W * Target
  f <- sum((W*L-Btilde)^2)
  Gq <- 2*(W*L-Btilde)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.oblimax <- function(L){
  Method <- "Oblimax"
  f <- -(log(sum(L^4))-2*log(sum(L^2)))
  Gq <- -(4*L^3/(sum(L^4))-4*L/(sum(L^2)))
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.entropy <- function(L){
  Method <- "Minimum entropy" 
  f <- -sum(L^2 * log(L^2 + (L^2==0)))/2
  Gq <- -(L * log(L^2 + (L^2==0)) + L)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.quartimax <- function(L){
  Method <- "Quartimax"
  f <- -sum(diag(crossprod(L^2)))/4
  Gq <- -L^3
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.varimax <- function(L){
  Method <- "Varimax"
  QL <- sweep(L^2,2,apply(L^2,2,mean),"-")
  f <- -sqrt(sum(diag(crossprod(QL))))^2/4
  Gq <- -L * QL
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.simplimax <- function(L,k=nrow(L)){
  Method <- "Simplimax"
  # m: Number of close to zero loadings
  Imat <- sign(L^2 <= sort(L^2)[k])
  f <- sum(Imat*L^2)
  Gq <- 2*Imat*L
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.bentler <- function(L){
  Method <- "Bentler's criterion"
  L2 <- L^2
  M <- crossprod(L2)
  D <- diag(diag(M))
  f <- -(log(det(M))-log(det(D)))/4
  Gq <- -L * (L2 %*% (solve(M)-solve(D)))
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.tandemI <- function(L){  # Tandem Criterion, Comrey, 1967.
  Method <- "Tandem I"
  LL <- (L %*% t(L))
  LL2 <- LL^2
  f <- -sum(diag(crossprod(L^2, LL2 %*% L^2)))
  Gq1 <- 4 * L *(LL2 %*% L^2)
  Gq2 <- 4 * (LL * (L^2 %*% t(L^2))) %*% L
  Gq <- -Gq1 - Gq2 
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.tandemII <- function(L){  # Tandem Criterion, Comrey, 1967.
  Method <- "Tandem II"
  LL <- (L %*% t(L))
  LL2 <- LL^2
  f <- sum(diag(crossprod(L^2, (1-LL2) %*% L^2)))
  Gq1 <- 4 * L *((1-LL2) %*% L^2)
  Gq2 <- 4 * (LL * (L^2 %*% t(L^2))) %*% L
  Gq <- Gq1 - Gq2 
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.geomin <- function(L,eps=.01){
  Method <- "Geomin"
  k <- ncol(L)
  p <- nrow(L)
  L2 <- L^2+eps
  pro <- exp(apply(log(L2),1,sum)/k) 
  f <- sum(pro)
  Gq <- (2/k)*(L/L2)*matrix(rep(pro,k),p)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.cf <- function(L,kappa=0){
  k <- ncol(L)
  p <- nrow(L)
  # kappa <- 0 # Quartimax 
  # kappa <- 1/p # Varimax
  # kappa <- m/(2*p) # Equamax
  # kappa <- (m-1)/(p+m-2) # Parsimax
  # kappa <- 1 # Factor parsimony
  Method <- paste("Crawford-Ferguson:k=",kappa,sep="")
  N <- matrix(1,k,k)-diag(k)
  M <- matrix(1,p,p)-diag(p)
  L2 <- L^2
  f1 <- (1-kappa)*sum(diag(crossprod(L2,L2 %*% N)))/4
  f2 <- kappa*sum(diag(crossprod(L2,M %*% L2)))/4
  f <- f1 + f2
  Gq <- (1-kappa) * L * (L2 %*% N) + kappa * L * (M %*% L2)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.infomax <- function(L){
  Method <- "Infomax"
  k <- ncol(L)
  p <- nrow(L)
  S <- L^2
  s <- sum(S)
  s1 <- apply(S, 1, sum)
  s2 <- apply(S, 2, sum)
  E <- S/s
  e1 <- s1/s
  e2 <- s2/s
  Q0 <- sum(-E * log(E))
  Q1 <- sum(-e1 * log(e1))
  Q2 <- sum(-e2 * log(e2))
  f <- log(k) + Q0 - Q1 - Q2
  H <- -(log(E) + 1)
  alpha <- sum(S * H)/s^2
  G0 <- H/s - alpha * matrix(1, p, k)
  h1 <- -(log(e1) + 1)
  alpha1 <- s1 %*% h1/s^2
  G1 <- matrix(rep(h1,k), p)/s - as.vector(alpha1) * matrix(1, p, k)
  h2 <- -(log(e2) + 1)
  alpha2 <- h2 %*% s2/s^2
  G2 <- matrix(rep(h2,p), ncol=k, byrow=T)/s - as.vector(alpha2) * matrix(1, p, k)
  Gq <- 2 * L * (G0 - G1 - G2)
  return(list(Gq=Gq,f=f,Method=Method))
}

vgQ.mccammon <- function(L){
  Method <- "McCammon entropy"
  k <- ncol(L)
  p <- nrow(L)
  S <- L^2
  M <- matrix(1,p,p)
  s2 <- apply(S, 2, sum)
  P <- S / matrix(rep(s2,p),ncol=k,byrow=T)
  Q1 <- -sum(P * log(P))
  H <- -(log(P) + 1)
  R <- M %*% S
  G1 <- H/R - M %*% (S*H/R^2)
  s <- sum(S)
  p2 <- s2/s
  Q2 <- -sum(p2 * log(p2))
  h <- -(log(p2) + 1)
  alpha <- h %*% p2
  G2 <- rep(1,p) %*% t(h)/s - as.vector(alpha)*matrix(1,p,k)
  Gq <- 2*L*(G1/Q1 - G2/Q2)
  Q <- log(Q1) - log(Q2)
  return(list(Gq=Gq,f=Q,Method=Method))
}

#
# GPromax is a separate function!!!
# Call directly from command prompt.
# R code only.
# 
GPromax <- function(A,pow=3){
  method <- "Promax"
  # Initial rotation: Standardized Varimax
  #require(mva)
  xx <- promax(A,pow)
  Lh <- xx$loadings
  Th <- xx$rotmat
  orthogonal <- F
  Table <- NULL
  return(list(Lh=Lh,Th=Th,Table=NULL,method,orthogonal=orthogonal))
}


##
## UTILITY FUNCTIONS

##

# Kaisers standardized variables

# Can be used for standardization and de-standardization.

# For standardization: only provide 1 argument: the matrix A

# output: standardized A and weights W

# For destandardization: provide 2 arguments: matrix A

# and weights W.

# output: destandardized A and weights W.

Standardize <- function(A,W){
  
  if (nargs()==1)
    
    W <- sqrt(apply(A^2,1,sum))
  else
    
    W <- 1/W
  
  A <- sweep(A,1,W,"/")
  
  return(A,W)
}



# Random Start

# k: number of dimensions

# orthogonal random start? (Yes, by default).

Random.Start <- function(k,orthogonal=T){
  
  mat <- matrix(rnorm(k*k),k)
  
  if (orthogonal){
    ans <- qr.Q(qr(mat))
  }
  
  else{
    
    ans <- mat %*% diag(sqrt(1/diag(t(mat) %*% mat)))
    
  }
  
  ans
  
} 


########## Following routines added by Steiger

vgQ.bifactor <- function(L){
  Method <- "bifactor"
  k <- dim(L)[2]
  p <- dim(L)[1]
  L <- L[,2:k]
  Lsq <- L * L
  M <- matrix(1,k-1,k-1) - diag(k-1)
  f <- sum(Lsq * (Lsq %*% M))
  zero <- matrix(0,p,1)
  Gq <- 4*cbind(zero, L * (Lsq %*% M))
  return(list(Gq=Gq,f=f,Method=Method))
}


################  End of Augmented Jennrich-Bentler Functions ############


################ The following code block augments some service
################ functions for Exploratory Factor Analysis with R
##########################################################################



sx2 <- function(df, lambda)
{
  x <- 2 * df + 4 * lambda
  if(x<0)x<-0
  return(sqrt(x))
}

FindLambda <- function(X2Observed,Df,CumP)
{
  
##should get symbolic integer codes for droot_c
##compute rough approximation to the start value
  ErrorCondition <- FALSE
  if (pchisq(X2Observed, Df) < CumP)
  {
    print("No non-negative value of Lambda will produce the desired cumulative probability.")
    return(NA)
  }
  zgoal <- qnorm(CumP)   
  df <- Df
  lambda1 <- X2Observed - df
  if(lambda1 < 0) lambda1 <- 0
  s1 <- sx2(df, lambda1)
  lambda2 <- lambda1 - s1 * zgoal  ##temporary guess for lambda2
  s2 <- sx2(df, lambda2)
  s_est <- (s1 + s2) / 2
  lambda2 <- lambda1 - zgoal * s_est
  if(lambda2 < 0) lambda2 <- 0
  pp2<-pchisq(X2Observed, Df, lambda2)-CumP
  s1 <- sx2(df,lambda2)
  z <-( X2Observed - df - lambda2)/s1
  lambda3 <- lambda2 - s1 * (z-zgoal)  ##temporary guess for lambda2
  s2 <- sx2(df, lambda3)
  s_est <- (s1 + s2) / 2
  lambda3 <- lambda2 - (z-zgoal) * s_est
  if(lambda3 < 0)
  {
    lambda3 <- 0
  }
  pp3<-pchisq(X2Observed, Df, lambda3)-CumP
  ##   should be in a linear range by now
  if((pp2-pp3)!= 0)
  {
    xstart <- lambda3 - pp3 * ((lambda2-lambda3)/(pp2-pp3))
  }	
  n<-1
  maxit<-10000
  step<-1.e-4
  eps<-1.e-10
  
  
  xstart<-max(xstart,1)
  if(abs(pchisq(X2Observed, Df)-CumP) <= eps)
  {
    return(0)
  }
  xold<-0
  xc<-xstart
  fc<-pchisq(X2Observed, Df, xc)-CumP
  if(ErrorCondition)
  {
    return(NA)
  }
  hc<-max(step*xc,eps)
  xnew<-xc-hc
  fnew<-pchisq(X2Observed, Df, xnew)-CumP
  if(ErrorCondition)
  {
    return(NA)
  }
  ac<-(fc-fnew)/hc
  while ( abs(xnew-xold) > eps && n < maxit && abs(fc) > eps && abs(ac) >= 1.e-35)
  {
    n<-n+1
    xnew<-xc-fc/ac
    xold<-xc
    fold<-fc
    xc<-xnew
    fc<-pchisq(X2Observed, Df, xnew)-CumP
    ac<-(fold-fc)/(xold-xc)
    if(ErrorCondition)
    {
      return(NA)
    }
    
  }
  ##iteration complete
  if( (n >= maxit)||(abs(fc) > eps))	  
  {
    ErrorCondition <- TRUE
    print("Percentage point convergence failure.")
    return(NA)
  }
  else
  {
    return(xc)	
  }
}
ChiSquare.Lambda.CI <- function(X2Observed,Df,Confidence=.90){
  p <- (1. - Confidence)/2.
  if( pchisq(X2Observed, Df) <= p)
{
    Upper <- 0.
    Lower <- 0.
    return(c(Lower,Upper))
  }
     Upper <- FindLambda(X2Observed,Df,p)
     if(pchisq(X2Observed, Df) <= 1.-p)
     {
       Lower  <- 0
       return(c(Lower,Upper))
     }
     Lower <- FindLambda(X2Observed,Df,1-p)
     return(c(Lower,Upper))
}

rmsea.ci <- function(chisq,df,n,conf=.90){
  options(warn=-1)
  sample.rmsea <- sqrt(max(((chisq-df)/(n-1)),0)/df)
#  out <- conf.limits.nc.chisq(Chi.Square=chisq,conf.level=conf,df=df,Jumping.Prop=0.001)
  out<-ChiSquare.Lambda.CI(chisq,df,conf)
  lower <- if(is.na(out[1])) 0 else out[1]
  upper <- if(is.na(out[2])) 0 else out[2]
  lower <- sqrt(lower/(n-1)/df)
  upper <- sqrt(upper/(n-1)/df)
  results <- list(Lower.Limit= lower, 
                  Point.Estimate = sample.rmsea,
                  Upper.Limit = upper,
                  Confidence.Level = conf)
  return((results))
}

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

fa_stats <- function(Correlation.Matrix,n.obs,n.factors,conf=.90,
                     maxit=1000,RMSEA.cutoff=NULL,
                     main="RMSEA Plot",sub=NULL){
  # browser()
  runs <- length(n.factors)  
  R <- Correlation.Matrix
  maxfac <- max(n.factors)
  res <- matrix(NA,runs,10)
  roots <- eigen(R)$values
  for(i in 1:runs){
    # i <- 3
    # R <- R0
    # n.obs = 643
    # n.factors=3:5
    # maxit = 1000
    output <- factanal(covmat=R,n.obs=n.obs,factors=n.factors[i],maxit=maxit)
    fit <- psych::fa(R,nfactors=n.factors[i],n.obs=n.obs,fm="ml")
    X2 <- output$STATISTIC
    df <- output$dof
    ci <- rmsea.ci(X2,df,n.obs,conf)
    pvar <- sum(roots[1:n.factors[i]])
    # cfi  <- compute_CFI(fit$null.chisq,fit$null.dof,fit$chi, fit$dof)
    # tli  <- compute_TLI(fit$null.chisq,fit$null.dof,fit$chi, fit$dof)
    cfi  <- compute_CFI(fit$null.chisq,fit$null.dof,X2, df)
    tli  <- compute_TLI(fit$null.chisq,fit$null.dof,X2, df)
    v <- c(n.factors[i],pvar,X2,df,1-pchisq(X2,df),ci$Point.Estimate,
           ci$Lower.Limit,ci$Upper.Limit, cfi,tli)
    
    res[i,] <- v
  }
  colnames(res)=c("Factors","Cum.Eigen","Chi-Square","Df","p.value",
                  "RMSEA.Pt","RMSEA.Lo","RMSEA.Hi","CFI","TLI")
  plotCI(n.factors,res[,6],li = res[,7],ui=res[,8],
         ylab="RMSEA",xlab="Number of Factors",main=main,
         sub=sub)
  lines(n.factors,res[,6],col="blue")
  abline(h=0,col="black",lty=3)
  abline(h=RMSEA.cutoff,col="red",lty=2)
  
  return(res)
}

FA.Stats <- function(
  Correlation.Matrix,
  n.obs,
  n.factors,
  conf=.90,
  maxit=1000,
  RMSEA.cutoff=NULL,
  main="RMSEA Plot",
  sub=NULL
){
  # browser()
  runs <- length(n.factors)  
  R <- Correlation.Matrix
  maxfac <- max(n.factors)
  res <- matrix(NA,runs,8)
  roots <- eigen(R)$values
  for(i in 1:runs){
     output <- stats::factanal(
       covmat=R,
       n.obs=n.obs,
       factors=n.factors[i],
       maxit=maxit
     )
     X2 <- output$STATISTIC
     df <- output$dof
     ci <- rmsea.ci(X2,df,n.obs,conf)
     pvar <- sum(roots[1:n.factors[i]])
     v <- c(
       n.factors[i],
       pvar,
       X2,
       df,
       1-pchisq(X2,df),
       ci$Point.Estimate,
       ci$Lower.Limit,
       ci$Upper.Limit
     )
     res[i,] <- v
  }
  colnames(res)=c("Factors","Cum.Eigen","Chi-Square","Df","p.value",
                      "RMSEA.Pt","RMSEA.Lo","RMSEA.Hi")
  plotCI(n.factors,res[,6],li = res[,7],ui=res[,8],
         ylab="RMSEA",xlab="Number of Factors",main=main,
         sub=sub)
  lines(n.factors,res[,6],col="blue")
  abline(h=0,col="black",lty=3)
  abline(h=RMSEA.cutoff,col="red",lty=2)
  return(res)
}

Scree.Plot <- function(R,main="Scree Plot",sub=NULL){
  roots <- eigen(R)$values
  x <- 1:dim(R)[1]
  plot(x,roots,type="b",col='blue',ylab="Eigenvalue",
       xlab="Component Number",main=main,sub=sub) 
  abline(h=1,lty=2,col="red")
  
}
# Scree.PlotGG <- function(R, main="Scree Plot",sub=NULL){
#   roots <- eigen(R)$values
#   x <- 1:dim(R)[1]
# #   plot(x,roots,type="b",col='blue',ylab="Eigenvalue",
# #        xlab="Component Number",main=main,sub=sub) 
# #   abline(h=1,lty=2,col="red")
#   
#   ds <- data.frame(x=x, roots=roots)
#   g <- ggplot(ds, aes(x=x, y=roots)) +
#     geom_rect(ymax=1, ymin=-Inf, xmin=-Inf, xmax=Inf, fill="red") +
#     geom_line(size=1.5, color="blue", na.rm = TRUE) +
#     geom_point(size=5, color="darkblue", na.rm = TRUE) +
#     labs(title=main, x="Component Number", y="Eigenvalue") +
#     theme_bw()
#   
#   print(g)
# }

FindBifactor <- function(A,reps){
  warn=-1
  results <- rep( list(list()), reps ) 
  criterion <- rep(NA,reps)
  m <- dim(A)[2]
  for(i in 1:reps) {
    x <- GPForth(A,method="bifactor",Tmat = Random.Start(m))
    criterion[i] <- min(x$Table[,2])
    results[[i]] <- x
  }
  j <- order(criterion)[1]
  warn=1
  return(results[[j]])
}

FindBestPattern <- function(A,method,is.oblique=FALSE,reps=15){
  results <- rep( list(list()), reps ) 
  criterion <- rep(NA,reps)
  m <- dim(A)[2]
  for(i in 1:reps) {
    if(!is.oblique)x <- GPForth(A,method=method,Tmat = Random.Start(m))else
      x <- GPFoblq(A,method=method,Tmat=Random.Start(m))
    criterion[i] <- min(x$Table[,2])
    results[[i]] <- x
  }
  j <- order(criterion)[1]
  return(results[[j]])
}

FixPattern <- function(res,sort=TRUE){
  F <- res$Lh
  p <- dim(F)[1]
  m <- dim(F)[2]
  factor.names <- paste("Factor",1:m,sep="")
  one.prime <- matrix(1,1,p)
  F.sign <- as.vector(sign(one.prime %*% F))
  #  cat("\nLoadings:\n")
#  fx <- format(round(Lambda, digits))
#  names(fx) <- NULL
#  nc <- nchar(fx[1L], type="c")
#  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
#  print(fx, quote = FALSE, ...) 
#  F <- Lambda
#fix signs
  Lambda <- F
  Lambda <- Lambda %*% diag(F.sign)
#sort columns by SS loadings
  mx <- diag(t(Lambda)%*%Lambda)
  ind <- order(mx,decreasing=TRUE)
  Lambda <- Lambda[,ind]
#  browser()
#  print(res$orthogonal)
#  print(res$Phi)
  if(!res$orthogonal)
  {
#   fix up Phi
   Phi <- diag(F.sign) %*% res$Phi %*% diag(F.sign)
   Phi <- Phi[ind,]
   Phi <- Phi[,ind]
  }
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.4] <- m + 1
    Lambda <- Lambda[order(mx, 1L:p),]
  }
  colnames(Lambda) <- factor.names
  res$Lh <- Lambda
  if(!res$orthogonal)
  {
  colnames(Phi) <- factor.names
  rownames(Phi) <- factor.names
  res$Phi <- Phi
  }
  return(res)
}

# FindBifactorPattern is not used anywhere else in this code
FindBifactorPattern <- function(A,reps,digits=2) {
  options(warn=-1)
  p<-dim(A)[1]
  out <- FindBifactor(A,reps)
  F <- out$Lh
  one.prime <- matrix(1,1,p)
  F.sign <- as.vector(sign(one.prime %*% F))
  F <- F %*% diag(F.sign)
  options(warn=1)
  return(round(F,digits))
} 

GPromax <- function(A,pow=3){
  method <- "Promax"
  # Initial rotation: Standardized Varimax
  xx <- promax(A,pow)
  Lh <- xx$loadings
  Th <- xx$rotmat
  orthogonal <- F
  Table <- NULL
  Tinv <- solve(Th)
  Phi <- Tinv %*% t(Tinv)
  return(list(Lh=Lh,Phi=Phi,Th=Th,Table=NULL,method,orthogonal=orthogonal))
}

print.FLS <- function(x, sort=TRUE, num.digits=3, cutoff=.25, ...)
{
  Lambda <- unclass(x$F)
  
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p),]
  }
  fx <- format(round(Lambda, num.digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type="c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  print(fx, quote = FALSE, ...)
  vx <- colSums(Lambda^2)
  varex <- rbind("SS loadings" = vx)
  if(is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, "Proportion Var" = vx/p)
    if(factors > 1)
      varex <- rbind(varex, "Cumulative Var" = cumsum(vx/p))
  }
  cat("\n")
  print(round(varex, num.digits))
  
if(!is.null(x$Phi)){
  cat("\nFactor Intercorrelations\n")
  cat("------------------------\n")
  print(round(unclass(x$Phi),num.digits))  
  }
 invisible(x)
}

print.MLFA<-function(x,num.digits=3,cutoff=0.25,sort_=TRUE,...)
{
cat("\n## Unrotated\n------------------\n")
print.FLS(x$Unrotated,num.digits=num.digits,cutoff=cutoff,sort=sort_)
cat("\n## Varimax (T)\n------------------\n")
print.FLS(x$Varimax,num.digits=num.digits,cutoff=cutoff,sort=sort_)
cat("\n## Promax (Q)\n-----------------\n")
print.FLS(x$Promax,num.digits=num.digits,cutoff=cutoff,sort=sort_)
cat("\n## Quartimin (Q)\n-----------------\n")
print.FLS(x$Quartimin,num.digits=num.digits,cutoff=cutoff,sort=sort_)
cat("\n## Bifactor (T)Loadings\n---------------------------\n")
print.FLS(x$Bifactor,num.digits=num.digits,cutoff=cutoff,sort=sort_)
cat("\n## Bifactor (Q)\n-------------------------\n")
# print.FLS(x$BifactorOblique,num.digits=num.digits,cutoff=cutoff,sort=sort_)
# # the last element must be invisible for proper output in R
invisible(print.FLS(x$BifactorOblique,num.digits=num.digits,cutoff=cutoff,sort=sort_))


# # Crawford-Ferguson added by Koval
# cat("\nCF Loadings\n-------------------------\n")
# print.FLS(x$CF,num.digits=num.digits,cutoff=cutoff,sort=sort_)
# cat("\nCFQ Loadings\n-------------------------\n")
# # the last element must be invisible for proper output in R
# invisible(print.FLS(x$CFQ,num.digits=num.digits,cutoff=cutoff,sort=sort_))

}

MLFA <- function(Correlation.Matrix=NULL,
                 n.factors=NA,
                 n.obs=NA,
                 data=NULL,
                 Factor.Pattern=NULL,
                 random.starts=15,
                 maxit=1000,
                 num.digits=3,
                 cutoff=0.30,
                 promax.m=3,
                 sort = TRUE)
{
cat("This will take a moment.")
if(!is.null(Correlation.Matrix)&&is.null(data)){
        R <- Correlation.Matrix
        p <- dim(R)[1]
        A <- factanal(covmat=R,n.obs=n.obs,factors=n.factors,maxit=maxit,rotation="none")$loadings[1:p,]
  }
if(!is.null(data)){
        R <- cor(data)
    n.obs <- dim(data)[1]
        p <- dim(R)[1]
        A <- factanal(covmat=R,n.obs=n.obs,factors=n.factors,maxit=maxit,rotation="none")$loadings[1:p,]
}
if(is.null(data)&&is.null(Correlation.Matrix)){ 
        A <- Factor.Pattern
        p <- dim(A)[1]
}

m <- dim(A)[2]
factor.labels <- paste("Factor",1:m,sep="")
# browser()
# Variamx
A.varimax <- varimax(A)$loadings[1:p,]
res <- list(Lh=A.varimax,orthogonal=TRUE)
res <- FixPattern(res, sort = sort)
A.varimax <- list(F=res$Lh)
# Promax
res <- GPromax(A,pow=promax.m)
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
res <- FixPattern(res, sort = sort)
Phi.promax <- res$Phi
A.promax <- list(F=res$Lh,Phi=Phi.promax,orthogonal=FALSE)
cat(".")
# Quartimin
res <- FindBestPattern(A,"quartimin",reps=random.starts,is.oblique=TRUE)
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
res <- FixPattern(res, sort = sort)
Phi.quartimin <- res$Phi
A.quartimin <- list(F=res$Lh,Phi = Phi.quartimin)
cat(".")
# Bifactor
res <- FindBestPattern(A,"bifactor",reps=random.starts)
orthogonal=TRUE
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=orthogonal)
res <- FixPattern(res, sort = sort)
Phi=NULL
A.bifactor <- list(F=res$Lh,Phi=Phi)
cat(".")
# Bifactor Oblique
res <- FindBestPattern(A,"bifactor",reps=random.starts,is.oblique=TRUE)
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
res <- FixPattern(res, sort = sort)
cat(".")
Phi.bifactor.oblique <- res$Phi
A.bifactor.oblique <- list(F=res$Lh,Phi=Phi.bifactor.oblique)
cat(".")



#### ADDitional rotations 

# Geomin (orthogonal)
res <- FindBestPattern(A,"geomin",reps=random.starts)
orthogonal=TRUE
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=orthogonal)
res <- FixPattern(res, sort = sort)
Phi=NULL
A.geominT <- list(F=res$Lh,Phi=Phi)
cat(".")
# Geomin (Oblique)
res <- FindBestPattern(A,"geomin",reps=random.starts,is.oblique=TRUE)
res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
res <- FixPattern(res, sort = sort)
cat(".")
Phi.bifactor.oblique <- res$Phi
A.geominQ <- list(F=res$Lh,Phi=Phi.bifactor.oblique)
cat(".")

# # Oblimin
# res <- FindBestPattern(A,"oblimin",reps=random.starts,is.oblique=TRUE)
# res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
# res <- FixPattern(res, sort = sort)
# Phi.oblimin <- res$Phi
# A.oblimin <- list(F=res$Lh,Phi = Phi.oblimin)
# cat(".")

# # CF added by Koval
# # Crawford-Ferguson
# res <- FindBestPattern(A,"cf",reps=random.starts,is.oblique=FALSE)
# res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=TRUE)
# res <- FixPattern(res, sort = sort)
# Phi=NULL
# A.cf <- list(F=res$Lh,Phi=Phi)
# cat(".")
# # Crawford-Ferguson Oblique
# res <- FindBestPattern(A,"cf",reps=random.starts,is.oblique=TRUE)
# res <- list(Lh=res$Lh,Phi=res$Phi,orthogonal=FALSE)
# res <- FixPattern(res, sort = sort)
# cat(".")
# Phi.cfq <- res$Phi
# A.cfq <- list(F=res$Lh,Phi=Phi.cfq)
# cat(".")
# browser()
# reconstruct the unrotated solution
A <- list(F=A,Phi=NULL) # line moved from end of the BifactorOblique section
# cat(".")
#
class(A)="FLS"
class(A.varimax)="FLS"
class(A.promax)="FLS"
class(A.quartimin)="FLS"
cat(".exiting\n") # what is this for?
class(A.bifactor)="FLS"
class(A.bifactor.oblique)="FLS"
class(A.geominT) ="FLS"
class(A.geominQ) ="FLS"

# additional rotations
# class(A.cf)="FLS"
# class(A.cfq)="FLS"
# class(A.oblimin) = "FLS"

output<-list(
  Unrotated       = A, 
  Varimax         = A.varimax,
  GeominT         = A.geominT,
  Bifactor        = A.bifactor,
  Promax          = A.promax,
  Quartimin       = A.quartimin,
  GeominQ         = A.geominQ,
  BifactorOblique = A.bifactor.oblique 
  # Oblimin = A.oblimin
  # CF=A.cf,
  # CFQ=A.cfq
       )
class(output) = "MLFA"
return(output)
}
# end of MLFA function definition

Loadings <- function(x,num.digits=3,cutoff=.25){
  invisible(print.MLFA(x,cutoff=cutoff,num.digits=num.digits))
}

FAtoCFA <-  function(
  x,
  model.name           = "fit",
  cutoff               = 0.30,
  factor.names         = colnames(x$F),
  reference.indicators = FALSE,
  covs                 = paste(factor.names, collapse=",")
){
  # browser()
  F <- x$F
  fname <- paste(model.name,".r",sep="")
  file.create(fname)
  rn <- rownames(F)
  p <- dim(F)[1]
  m <- dim(F)[2]
  line <- rep(" ",m)
  for (j in 1:m) {
    prefix <- paste(factor.names[j],":",sep="")
    count <- sum(abs(F[,j]>cutoff))
    postfix <- rep(" ",count)
    i.postfix <- 0
    for(i in 1:p){
      if(abs(F[i,j])> cutoff){
        i.postfix <- i.postfix + 1
        if(i.postfix<count) postfix[i.postfix] <- paste(rn[i],", ",sep="")else postfix[i.postfix] <- paste(rn[i],"\n",sep="")
      }
    }
    postfix <- paste(postfix,collapse='')
    line[j] <- paste(prefix,postfix)  
    cat(line[j],file=fname,append=TRUE)
  }
  cat("\n",file=fname,append=TRUE)
  cstring <- paste(  c("cfa(reference.indicators=reference.indicators,covs=covs,
                       file=\"",fname,"\")"),collapse="")
  cfa.model <- eval(parse(text=cstring))
  return(cfa.model)
}

FAtoREF <-  function(x,R,model.name="fit",factor.names=colnames(x$F),
                     make.start.values=TRUE,cov.matrix=TRUE,num.digits=4){
  F <- round(x$F,num.digits)
  fname <- paste(model.name,".r",sep="")
  file.create(fname)
  Phi <- x$Phi
  rn <- rownames(F)
  p <- dim(F)[1]
  m <- dim(F)[2]
  #  cat(paste(model.name,"<- cfa(reference.indicators=FALSE)\n"))
  line <- rep(" ",p-(m-1))
  ref.position <- rep(NA,m)
  par.number <- 0
  for (j in 1:m)ref.position[j] <- order(F[,j])[p]
## Got reference positions -- if start values are needed, compute them
## Now, construct the transformation matris
## First, pull out F1
  F1 <- F[ref.position[1],]
  for (j in 2:m) F1 <- rbind(F1,F[ref.position[j],])
## Got F1, now compute T and then rotate F and compute Phi
  if(make.start.values){
  T <- solve(F1) %*% diag ( sqrt( diag(F1 %*% t(F1) ) ) )
  F <- F %*% T
  Tinv <- solve(T)
  Phi <- Tinv %*% t(Tinv)
  }
  for (j in 1:m) {
    prefix <- paste(factor.names[j]," -> ",sep="")
    for(i in 1:p){
      postfix <- rn[i]
      non.fixed=TRUE
      line.out=FALSE
      for(jj in 1:m)
      {
        if(i == ref.position[jj]&&jj==j)
        {
          non.fixed=TRUE
          line.out=TRUE}
        if(i == ref.position[jj]&&jj!=j)
          non.fixed=FALSE
      } 
      if(non.fixed){
        
        par.number <- par.number + 1
        par.name <- paste("Theta",as.character(par.number),sep="")
        if(make.start.values)
          sv<-paste(c(", ",as.character(round(F[i,j],num.digits)),"\n"),collapse="")else  sv <- ", NA\n"
        postfix <- paste(c(postfix,", ",par.name,sv),collapse="")
        line.out=TRUE
      }
      
      
      if(line.out){
        line <- paste(c(prefix,postfix),collapse="")  
        cat(line,file=fname,append=TRUE)
      }
    }
  }
  for(j in 1:m)
  {
    cat(paste(c(factor.names[j]," <-> ",factor.names[j],", NA, 1\n"),collapse=""),file=fname,append=TRUE)
  }
  if(cov.matrix && m>1 )  
  {
    ok <- !is.null(Phi)
    for(i in 2:m)
      for(j in 1:(i-1))
      { 
        par.number <- par.number + 1
        prefix <- paste(factor.names[i], "<-> ",sep="")
        postfix <- factor.names[j]
        if(make.start.values&&ok)
          sv<-paste(c(", ",as.character(round(Phi[i,j],num.digits)),"\n"),collapse="") else  sv <- ", NA\n"
        par.name <- paste("Theta",as.character(par.number),sep="")
        postfix <- paste(c(postfix,", ",par.name,sv),collapse="")
        cat(paste(c(prefix,postfix),collapse=""),file=fname,append=TRUE)  
      }
  }  
## add unique variances
  if(is.null(Phi))Phi <- diag(p)
  U2 <- diag(R - F %*% Phi %*% t(F))
  for(i in 1:p){
    par.number <- par.number+1
    prefix <- paste(c(rn[i]," <-> ",rn[i],", "),collapse="")
    par.name <- paste("Theta",as.character(par.number),sep="")
    if(make.start.values)
      sv<-paste(c(", ",as.character(round(U2[i],num.digits)),"\n"),collapse="") else  sv <- ", NA\n"
    cat(paste(c(prefix,par.name,sv),collapse=""),file=fname,append=TRUE)  
    }
    
  cat("\n",file=fname,append=TRUE)
  cstring <- paste(  c("specifyModel(file=\"",fname,"\")"),collapse="")
  sem.model <- eval(parse(text=cstring))
  return(sem.model)
}

FAtoSEM <-  function(
  x,
  model.name          = "fit",
  cutoff              = 0.30,
  factor.names        = colnames(x$F),
  make.start.values   = FALSE,
  cov.matrix          = FALSE,
  num.digits          = 4,
  par.prefix          = c("Theta","Theta"),
  reference.variables = FALSE
){
  F <- round(x$F,num.digits)
  fname <- paste(model.name,".r",sep="")
  file.create(fname)
  cat("\n")
  Phi <- x$Phi
  rn <- rownames(F)
  p <- dim(F)[1]
  m <- dim(F)[2]
  #  cat(paste(model.name,"<- cfa(reference.indicators=FALSE)\n"))
  line <- rep(" ",p-(m-1))
  par.number <- 0
  for (j in 1:m) {
    prefix <- paste(factor.names[j]," -> ",sep="")
    var.count <- 0
    for(i in 1:p){
      postfix <- rn[i]
      if(abs(F[i,j])>cutoff)
      { var.count <- var.count + 1
        if(var.count>1 || !reference.variables){
        par.number <- par.number + 1
        par.name <- paste(par.prefix[1],as.character(par.number),sep="")
        if(make.start.values)
        {sv<-paste(c(", ",as.character(F[i,j]),"\n"),collapse="")}else sv <- ", NA\n"
        postfix <- paste(c(postfix,", ",par.name,sv),collapse="")
        }else{
          postfix <- paste(postfix,",NA,1\n",sep="")
        }
        line <- paste(c(prefix,postfix),collapse="")  
        cat(line,file=fname,append=TRUE)
      } 
    }
  }
  for(j in 1:m)
  {
    if(!reference.variables){
      cat(paste(c(factor.names[j]," <-> ",factor.names[j],", NA, 1\n"),collapse=""),
           file=fname,append=TRUE)}
}
if(cov.matrix && m>1 )  
  {
    ok <- !is.null(Phi)
    for(i in 2:m)
      for(j in 1:(i-1))
      { 
        par.number <- par.number + 1
        prefix <- paste(factor.names[i], "<-> ",sep="")
        postfix <- factor.names[j]
        if(make.start.values&&ok)sv<-paste(c(", ",as.character(Phi[i,j]),"\n"),collapse="") else  sv <- ", NA\n"
        par.name <- paste(par.prefix[2],as.character(par.number),sep="")
        postfix <- paste(c(postfix,", ",par.name,sv),collapse="")
        cat(paste(c(prefix,postfix),collapse=""),file=fname,append=TRUE)  
      }
  }  
  cat("\n",file=fname,append=TRUE)
  cstring <- paste(  c("specifyModel(file=\"",fname,"\")"),collapse="")
  sem.model <- eval(parse(text=cstring))
  return(sem.model)
}

GetPattern <- function(sem.object){
  A <- sem.object$A
  P <- sem.object$P
  p <- sem.object$n
  m <- sem.object$m - p
  F <- A[1:p,(p+1):sem.object$m]
  Phi <- P[(p+1):sem.object$m,(p+1):sem.object$m]
  return(list(F=F,Phi=Phi))
}

SetupCFAPattern <- function(
  R,
  n.factors    = NA,
  factor.names = NULL
){
  p <- dim(R)[1]
  m <- n.factors
  Fp <- matrix(0,p,m)
  rownames(Fp) <- colnames(R)
  if(is.null(factor.names))factor.names <- paste("Factor",1:m,sep="")
  colnames(Fp) <- factor.names
  Fp <- edit(Fp)
  return(list(F=Fp))
} # return Factor pattern that was manually set up in the editor

UseMod <- function(sem.object,loadings.only=TRUE){
  options(warn=-1)
new.path <- CheckMod(sem.object,row.form=TRUE,loadings.only=loadings.only)$NewPath
old.model <- sem.object$semmod
R <- sem.object$S
n.obs <- sem.object$N
new.model <- rbind(old.model,new.path)
class(new.model) <- class(old.model)
fit.object <- sem(new.model,R,n.obs)
return(fit.object)
}

CFAModel <- function(
  R,
  model.name           = "fit",
  n.factors            = NULL,
  factor.names         = NULL,
  cutoff               = 0.30,
  covs                 = TRUE,
  reference.indicators = FALSE
){
 if(is.null(n.factors))n.factors=length(factor.names)
 x <- SetupCFAPattern(R,n.factors,factor.names)
 factor.names <- colnames(x$F)
 if(covs){
   covariances=paste(factor.names, collapse=",")
 }else{covariances=NULL}
 # browser()
model <- FAtoCFA(
  x                    = x,
  model.name           = model.name,
  cutoff               = cutoff,
  factor.names         = factor.names,
  reference.indicators = reference.indicators,
  covs                 = covariances
)
return(model)
}

QuickCFA <- function(
  R,
  n.factors            = NULL,
  n.obs,
  model.name           = "Model0",
  factor.names         = NULL,
  cutoff               =0.30,
  factor.correlations  = FALSE,
  reference.indicators = FALSE)
{
  if(is.null(factor.names))factor.names <- paste("Factor",1:n.factors,sep="")
  model <- CFAModel(
    R,
    model.name,
    n.factors,
    factor.names,
    cutoff,
    covs=factor.correlations,
    reference.indicators
  )
  fit.object <- sem(model,R,n.obs)
  return(fit.object)
}

CheckMod <- function(sem.object,row.form=FALSE,loadings.only=TRUE){
  options(warn=-1)
  mi <- modIndices(sem.object)
  A <- mi$mod.A
  P <- mi$mod.P
  p <- sem.object$n
  m <- sem.object$m - p
  F <- A[1:p,(p+1):sem.object$m]
  par.number <- sem.object$t + 1
  par.name <- paste("AddedTheta",par.number,sep="")
  largest.F <- max(F,na.rm=TRUE)
  largest.P <- 0
  largest.Q <- 0
  if(!loadings.only){
  Q <- P
  P <- P[(p+1):sem.object$m,(p+1):sem.object$m]
  Q <- Q[1:p,1:p]
  largest.P <- max(P,na.rm=TRUE)
  largest.Q <- max(Q,na.rm=TRUE)
  }
  maxes <- c(largest.F,largest.P,largest.Q)
  for(i in 1:3)if(maxes[i]==-Inf)maxes[i]<-0
  max.mod.pos <- order(maxes,decreasing=TRUE)[1]
  if(max.mod.pos == 1){
    arrow <- "->"
    F.pos <- which(F==largest.F,arr.ind=TRUE)
    if(length(F.pos[,1])>1)F.pos <- F.pos[1,]
    postfix <- rownames(F)[F.pos[1]]
    prefix <- colnames(F)[F.pos[2]]
    value <- F[F.pos[1],F.pos[2]]
  }
  if(max.mod.pos==2){
        arrow="<->"
        P.pos <- which(P==largest.P,arr.ind=TRUE)
        if(length(P.pos[,1])>1)P.pos <- P.pos[1,]
        prefix <- colnames(P)[P.pos[1]]
        postfix <- colnames(P)[P.pos[2]]
        value <- P[P.pos[1],P.pos[2]]
  }
  if(max.mod.pos==3){
    arrow="<->"
    Q.pos <- which(Q==largest.Q,arr.ind=TRUE)
    if(length(Q.pos[,1])>1)Q.pos <- Q.pos[1,]
    prefix <- colnames(Q)[Q.pos[1]]
    postfix <- colnames(Q)[Q.pos[2]]
    value <- Q[Q.pos[1],Q.pos[2]]
  }
  path <- paste(c(prefix,arrow,postfix),collapse="")
  if(!row.form)line <- paste(c(path,",",par.name,", NA"),collapse="")else line <- c(path,par.name,", NA")
  fname="added.r"
  file.create(fname)
  cat(line,file=fname)
  output  <- list(NewPath=line,ModIndex=value)
  return(output)  
}
CullModel<-function(fit.object,alpha,cull.Phi=FALSE){
  model <- fit.object$semmod
  zcrit <- qnorm(1-alpha/2)
  zval <- summary(fit.object)$coeff$'z value'
  sig <- abs(zval)>zcrit
  par.posn <- fit.object$par.posn
  new.model <- model
  model.length <- dim(model)[1]
  pos<-1
  to.cull <- 0
  is.ok <- rep(TRUE,model.length)
  if(!cull.Phi){
    do.not.cull <- grep("<->",model)
    for(i in 1:length(do.not.cull)){
    is.ok[do.not.cull[i]]<-FALSE
    }
  }
  for(i in 1:length(sig)){
    if(!sig[i]&&is.ok[par.posn[i]]){
      to.cull[pos] <- par.posn[i]
      pos <- pos+1
    }
  }
new.model <- new.model[-to.cull,]
class(new.model) <- class(model)
return(new.model)
}  

QuickJoreskog <- function(R,n.factors,n.obs,model.name="model.0",
                          alpha=0.05,use.promax=TRUE,promax.m=3){
  mlfa.object <- MLFA(R,n.factors,n.obs,promax.m=promax.m)
  if(use.promax)x <- mlfa.object$Promax else x <- mlfa.object$Varimax
  ref.model <- FAtoREF(x,R,model.name)
  ref.fit <- sem(ref.model,R,n.obs)
  culled.model <- CullModel(ref.fit,alpha)
  culled.fit <- sem(culled.model,R,n.obs)
  return(culled.fit)
}

GetPrettyPattern <- function(fit.object,cutoff=0.10){
  print.FLS(GetPattern(fit.object),cutoff=cutoff)}

RMSEA <- function(fit.object,conf=0.90){
  n.obs <- fit.object$N
  X2 <- (n.obs-1)*fit.object$criterion
  p <- fit.object$n
  t <- fit.object$t
  df <- p*(p+1)/2 - t
  rmsea.ci(X2,df,n.obs,conf)
}
QuickEFAtoCFA <- function(
  R,
  n.factors,
  n.obs,
  rotation="Varimax",
  model.name="model.0",
  cutoff=0.30,
  alpha=0.05,
  make.start.values=TRUE,
  cov.matrix=TRUE,
  num.digits=4,
  promax.m=3
){
  rotation.list <- c("Varimax","Promax","Quartimin","Bifactor", "BifactorOblique")
  rot <- 0
  for(i in 1:5)
    if(rotation==rotation.list[i]){rot<-i;break}  
  if(rot==0){print("Illegal Rotation Method");return()}
  x <- MLFA(R,n.factors,n.obs,cutoff=cutoff,num.digits=num.digits,promax.m=promax.m)   
  cstring <- paste(  c("rot.pattern <-x$",rotation),collapse="")
  eval(parse(text=cstring))
  model <- FAtoSEM(rot.pattern,model.name=model.name,cutoff=cutoff,
                   factor.names=colnames(rot.pattern$F),
  make.start.values=make.start.values,cov.matrix=cov.matrix,num.digits=num.digits)
  fit <- sem(model,R,n.obs)
  return(fit)
}
QuickSEM <- function(S = NULL,n.obs=NULL,max.latents = dim(S)[1]){
x <- data.frame(Latent.Variable.Names = rep("",max.latents))
x <- edit(x)
p <- dim(S)[1]
LV <- x$Latent.Variable.Names
LV <- as.character(LV[LV!=""])
n.LV <- length(LV)
F <- matrix(0,n.LV,n.LV)
Lx <-NULL
Ln <-NULL
rownames(F) <- LV
colnames(F) <- LV
F <- edit(F)
cs <- colSums(F)
for(i in 1:n.LV)if(cs[i]==0)F <- F[1:n.LV,-i]
rs <- rowSums(F)
nc <- dim(F)[2]
for(i in 1:n.LV)if(rs[i]==0){
  Lx <- c(Lx,rownames(F)[i])
  F <- F[-i,1:nc]
}
Ln <- rownames(F)
n.Ln <- length(Ln)
n.Lx <- length(Lx)
Lambda.x <- matrix(0,p,n.Lx)
rownames(Lambda.x) <- colnames(S)
colnames(Lambda.x) <- Lx
Lambda.x <- edit(Lambda.x)
cly <- colnames(S)
rs <- rowSums(Lambda.x)
rn <-NULL
for(i in 1:p)if(rs[i]==0)rn <- c(rn,cly[i])
rows.Lambda.y <- length(rn)
Lambda.y <- matrix(0,rows.Lambda.y,n.Ln)
rownames(Lambda.y) <- rn
colnames(Lambda.y) <- Ln
Lambda.y <- edit(Lambda.y)
q <- dim(Lambda.x)[2]
rs <- rowSums(Lambda.x)
LL<-NULL
RR<-NULL
RN<-rownames(Lambda.x)
for(i in 1:p)if(rs[i]>0){
  LL <- rbind(LL,Lambda.x[i,])
  RR <- c(RR,RN[i])
}
Lambda.x <- LL
colnames(Lambda.x)<- Lx
rownames(Lambda.x)<- RR
xxx <- list(F=Lambda.x)
yyy <- list(F=Lambda.y)
mm1 <- FAtoSEM(xxx,"xxx",par.prefix=c("Lamdax","Phi"))
mm2 <- FAtoSEM(yyy,"yyy",reference.variables=TRUE,par.prefix=c("Lambday","Delta"))
struc.model <- FtoSEM(F,Lx,Ln,"sss")
my.model <- rbind(mm1,mm2,struc.model)
class(my.model) <- class(mm1)
sem(my.model,S,n.obs)
}

FtoSEM <- function(F,Lx,Ln,model.name){
  fname <- paste(model.name,".r",sep="")
  file.create(fname)
  cat("\n")
#Get Betas 
  #Are there any?
  if(dim(F)[2]>length(Lx)){
  par.prefix <- "Beta"
  lnlist <- colnames(F)
  for(k in 1:length(Lx)){
    for(j in 1:length(lnlist))
      if(Lx[k]==lnlist[j])lnlist[k]<-" "}
  lns <- lnlist[lnlist!=" "]
  bbb=0
## List of prefix names for Betas is in lns
  for(j in 1:length(lns)){
    for(i in 1:length(Ln)){
#      browser()
      if(F[Ln[i],lns[j]]>0){bbb<-bbb+1
      prefix <- paste(c(lns[j],"->",Ln[i]),collapse="")
      par.name <- paste(c(",",par.prefix,as.character(bbb)),collapse="")
      postfix <- ",NA\n"
      }
    }
    line <- paste(c(prefix,par.name,postfix),collapse="")  
    cat(line,file=fname,append=TRUE)
  }
  }
#Get Gammas    
  gamma<-0
  par.prefix <- "Gamma"
  for(i in 1:length(Lx)){
    for(j in 1:length(Ln))
  {
      if(F[Ln[j],Lx[i]]>0){gamma<-gamma+1
      prefix <- paste(Lx[i],"->",sep="")
      prefix <- paste(prefix,Ln[j],sep="")
      par.name <- paste(c(",",par.prefix,as.character(gamma)),sep="")
      postfix <- ",NA\n"
    
        line <- paste(c(prefix,par.name,postfix),collapse="")  
        cat(line,file=fname,append=TRUE)
  }}}
  cat("\n",file=fname,append=TRUE)
  cstring <- paste(  c("specifyModel(file=\"",fname,"\")"),collapse="")
  sem.model <- eval(parse(text=cstring))
  return(sem.model)

  
}

EditSEM<- function(sem.object){
  old.model <- sem.object$semmod
  new.model <- edit(old.model)
  return(sem(new.model,sem.object$S,sem.object$N))
}