# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(ggplot2)
library(dplyr)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("readr")   # data input
requireNamespace("tidyr")   # data manipulation
requireNamespace("dplyr")   # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # For asserting conditions meet expected patterns.
requireNamespace("corrplot")  # For asserting conditions meet expected patterns.
# requireNamespace("car")     # For it's `recode()` function.
library(psych)
library(plotrix)
library(sem)
library(GPArotation)
library(corrplot)
library(corrgram)

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./analysis/eda-preparations/scripts/SteigerRLibraryFunctions.txt")
source("./analysis/eda-preparations/scripts/AdvancedFactorFunctions_CF.R")
source("./analysis/eda-preparations/scripts/fa-utility-functions.R") # to graph factor patterns
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
source("./scripts/common-functions.R") # used in multiple reports
baseSize = 8
# ---- declare-globals ---------------------------------------------------------

prints_folder <- paste0("./analysis/open-budget/efa-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

income_vars <- c(
   "total"            = "Total revenue amount" # sum of all tax codes     
  ,"own"              = "Revenue from OWN sources"# = income_total - income_transfert   
  ,"transfert"        = "Revenue from transferts"                                                  
  ,"pdfo"             = "Own: income tax"                                                 
  ,"military"         = "Own: military salaries"                           
  ,"unified"          = "Own: unified tax"                                                
  ,"property"         = "Own: property tax"                                               
  ,"excise"           = "Own: excise duty"                                                
  ,"other"            = "Own: other sources" # income_own - sum(military,pdfo,unified,property,excise)                                                
)


d_meta <- 
  tibble::tribble(
    ~item_name, ~item_number, ~label_en,
    "total"        ,1L , "Total revenue amount"    ,
    "own"          ,2L , "Revenue from OWN"        ,
    "transfert"    ,3L , "Revenue from transferts (1)" ,
    "pdfo"         ,4L , "Own: income tax         (2)"         ,
    "military"     ,5L , "Own: military salaries  (3)"  ,
    "unified"      ,6L , "Own: unified tax        (4)"        ,
    "property"     ,7L , "Own: property tax       (5)"       ,
    "excise"       ,8L , "Own: excise duty        (6)"        ,
    "other"        ,9L , "Own: other sources      (7)"          
  )

econ_vars <- d_meta %>% pull(item_name)

efa_vars <- econ_vars[3:9]
describe_item <- function(d, varname,dmeta = d_meta){
  # d <- ds %>% select(id, Q9)
  # d %>% glimpse()
  # varname <- "prep_first_aid_water"
  
  (variable_label <- dmeta %>% filter(item_name==varname) %>% pull(label_ua))
  (variable_keyword <- dmeta %>% filter(item_name==varname) %>% pull(keyword))
  d %>% 
    TabularManifest::histogram_discrete(varname)+
    labs(
      # title = paste0(variable_keyword,": ",variable_label)
      title = paste0("Name: [", varname,"] | Keyword: [", variable_keyword,"]")
      ,subtitle = variable_label
    )
}
# ds0 %>% describe_item("prep_backup")

make_corr_matrix <- function(d,metaData=d_meta,item_names,display_var="label_en", method="pearson"){
  # browser()
  # d <- ds0
  # metaData <- d_meta
  # item_names <- (d_meta %>% pull(item_name) %>% as.character() )[1:3]
  # add_short_label <- TRUE
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select(all_of(item_names))
  d2 <- d1[complete.cases(d1),]
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = !!rlang::sym(display_var))
  # rownames <- rownames[,"display_name"]
  # rownames <- rownames %>% as.list() %>% unlist() %>% as.character()
  rownames <- rownames %>% pull(display_name)
  d3 <- sapply(d2, as.numeric)
  # d3 %>% glimpse()
  cormat <- cor(d3,method = method)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
  return(cormat)
}


make_corr_plot <- function (
    corr,
    lower="number",
    upper="number",
    bg="white",
    addgrid.col="gray"
    ,title 
    , ...
){
  corrplot::corrplot(
    corr
    , add=F
    , type   = "lower"
    , method = lower
    , diag   = TRUE
    , tl.pos = "lt"
    , cl.pos = "n"
    # ,order = "hclust"
    # ,addrect = 3
    ,...
  )
  corrplot::corrplot(
    corr
    ,add=T
    , type="upper"
    , method=upper
    , diag=TRUE
    , tl.pos="n"
    # ,order = "hclust"
    # ,addrect = 3
    ,title = title  
    , ...
    )
  
}


# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_rds("./data-private/derived/study-pca-ds2.rds")

# ---- inspect-data -------------------------------------------------------------
ds0 %>% glimpse()
# ---- tweak-data --------------------------------------------------------------

ds1 <- 
  ds0 %>%
  dplyr::select( all_of(c("hromada_code",econ_vars)))  # no codes

ds1 %>% glimpse()


m_spearman <- 
  ds1 %>% 
  make_corr_matrix(item_names = efa_vars, method = "spearman")  
m_pearson <- 
  ds1 %>% 
  make_corr_matrix(item_names = efa_vars, method = "pearson")  
m_kendall <- 
  ds1 %>% 
  make_corr_matrix(item_names = efa_vars, method = "kendall")  


m_list <- list(
  "spearman" = m_spearman
  ,"pearson" = m_pearson
  ,"kendall" = m_kendall
)


# ---- inspect-correlations -------------------------------------


for(i in c("pearson","spearman","kendall")){
  
  title_i <- paste0("Correlation method: ", str_to_title(i) )
  path_i  <- paste0("1-",i,".png")
  
  png(paste0(prints_folder,path_i),width = 12,height = 12,units = "in",res = 100)  
  m_list[[i]] %>% make_corr_plot(upper = "pie", title = title_i)
  dev.off()
}


for(i in c("pearson","spearman","kendall")){
  
  title_i <- paste0("Correlation method: ", str_to_title(i) )
  path_i  <- paste0("2-",i,"-cluster.png")
  
  png(paste0(prints_folder,path_i),width = 12,height = 12,units = "in",res = 100)  
  m_list[[i]] %>% corrplot(order="hclust", addrect=3, title =title_i)
  dev.off()
}

# now with clusters 


# ---- diagnose-0a ------------------------------------------------------
# R0 <-  d_ordinal_complete %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
R0 <-  ds1 %>%  make_corr_matrix(item_names = efa_vars, method = "spearman")
# R0 <-  d_binary_complete %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
# R0 <-  d_binary_NAto0 %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")  
# Diagnosing number of factors
(n_obs <- nrow(ds1))
sample_size <- n_obs

Scree.Plot(R0)
#The first 15 eigen values
data.frame(
  eigen = c(1:nrow(R0)),
  value = eigen(R0)$values
) %>%
  dplyr::filter(eigen < 16) %>%
  print()
# ---- diagnose-0b -------------------------
# MAP
psych::nfactors(R0,n.obs = n_obs)
# ---- diagnose-0c -------------------------
pa_results <- psych::fa.parallel(R0,n_obs,fm = "ml",fa="fa")
ds_pa <- data.frame(
  observed_eigens = pa_results$fa.values,
  simulated_eigens = pa_results$fa.sim
) %>% head(15) %>% print()
# ---- diagnose-0d ------------------------------------------------------
ls_solution <- solve_factors(R0,min=1,max=2,sample_size = n_obs)
# ds_index <- get_indidces(ls_solution) # not working, investigate, possibly old packages
# ds_index %>% print()
# ---- diagnose-0e -------------------------
FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:3,RMSEA.cutoff = .08)

FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:3,RMSEA.cutoff = .05)
# ---- estimate-0 ---------------------------------
fit_efa_0 <- MLFA(
  Correlation.Matrix = R0,
  n.factors = 3,
  n.obs = n_obs,
  sort = FALSE
)
#Loadings from the EFA solution\n")
f_pattern <- fit_efa_0[['Varimax']]$F # fit_efa_0$
f_pattern %>% plot_factor_pattern(factor_width = 3)
# Loadings above threashold (.3) are masked to see the simpler structure
cat("\nLoadings above threashold (.3) are masked to see the simpler structure\n")
f_pattern[f_pattern<.30] <- NA
f_pattern %>% plot_factor_pattern(factor_width = 3)

# ----- confirm-0 ------------------------
# These values are translated into CFA model and used as starting values
model_0 <- FAtoSEM(
  x                 = fit_efa_0[["Varimax"]] ,
  cutoff            = 0.30,
  factor.names      = c("One","Two","Three"),
  make.start.values = TRUE,
  cov.matrix        = FALSE, # TRUE - oblique, FALSE - orthogonal
  num.digits        = 4
)
# the model is estimated using sem package
fit_0 <- sem::sem(model_0,R0,sample_size) # PROBLEM, not sure what's wrong, I think misfit is too great
# the pattern of the solution
m <- GetPattern(fit_0)$F
m[m==0] <- NA
m %>% plot_factor_pattern(factor_width=3)
# Summary of the fitted model
sem_model_summary(fit_0)
#Relative contribudion of items
sort(summary(fit_0)$Rsq) %>% dot_plot()


# ---- write-up-0 -----------------------
d <- ds1 %>% select(varname_n_scale)
alpha(d)
solution <- ls_solution




# ---- compare-conditions ------------------------------------------------------

display_solution <- function(R,k, sample_size,rotation_,mainTitle=NULL){
  A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
  L <- A$loadings
  if(rotation_=="oblimin"  ){rotation_string <- "(L, Tmat=diag(ncol(L)), gam=0,               normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="quartimin"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="targetT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="targetQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="pstT"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="pstQ"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="oblimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="entropy"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="quartimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="Varimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="simplimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),           k=nrow(L), normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bentlerT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bentlerQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="tandemI"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="tandemII" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="geominT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="geominQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="cfT"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="cfQ"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="infomaxT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="infomaxQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="mccammon" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bifactorT"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bifactorQ"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  
  rotated_solution <- eval(parse(text=paste0(rotation_,rotation_string)))
  p <- nrow(R)
  
  FPM <- rotated_solution$loadings # FPM - Factor Pattern Matrix
  FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
  colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
  FPM  # THE OUTPUT
  Phi <- rotated_solution$Phi # factor correlation matrix
  if( is.null(Phi)) {Phi <- diag(k)} else{Phi}
  colnames(Phi) <- paste0("F", 1:k)
  rownames(Phi) <- paste0("F", 1:k)
  Phi
  solution <- list("FPM"=FPM,"Phi"=Phi)
  # load the function to gread the graph, needs k value
  source("./analysis/eda-preparations/factor-pattern-plot.R") # to graph factor patterns
  g <- fpmFunction(FPM.matrix=solution$FPM, mainTitle=mainTitle) #Call/execute the function defined above.
  # print(g) #Print graph with factor pattern
  # file_name <- paste0("./data/shared/derived/FPM/",rotation_,"_",k,".csv")
  #browser()
  # save_file <- as.data.frame(FPM[,1:k])
  # readr::write_csv(save_file,file_name)
  
  return(g)
}

# R <- R0 # correlation matrix for items at phase 0

conditions <- c(
  "pearson"   = "Pearson"    # 0|1|2 - dropped NA
  ,"spearman" = "Spearman"    # 0|1 - dropped NA
  ,"kendal"   = "Kendall" # 0|1|2 - replaced NA with 0
)

for(condition_i in names(conditions)){
  
  title_string <- conditions[condition_i]
  file_string <- condition_i
  
  if(condition_i=="pearson"){
    d <- ds1 
    R <-  d %>%  make_corr_matrix(item_names = efa_vars, method = "pearson")
  }
  if(condition_i=="spearman"){
    d <- ds1
    R <-  d %>%  make_corr_matrix(item_names = efa_vars, method = "spearman")
  }
  if(condition_i=="kendall"){
    d <- ds1
    R <-  d %>%  make_corr_matrix(item_names = efa_vars, method = "kendall")
  }
 
  
  
  sample_size <- nrow(d)
  k <- 3
  nfactors_ <- k
  
  for(rotation_ in c(
    "oblimin"     # notable
    # ,"quartimin"
    # ,"oblimax"  
    # ,"entropy"  
    ,"quartimax"  # notable
    ,"Varimax"    # notable
    # ,"simplimax"
    # ,"bentlerT" 
    # ,"bentlerQ" 
    # ,"tandemI"  
    # ,"tandemII" 
    ,"geominT"    # notable
    ,"geominQ"    # notable
    # ,"cfT"      
    # ,"cfQ"      
    # ,"infomaxT" 
    # ,"infomaxQ" 
    # ,"mccammon" 
    ,"bifactorT" # notable
    ,"bifactorQ" # notable
  )){
    
    file_name <- paste0("solutions-3/",file_string,"-",rotation_)
    
    cat("\n\n")
    cat(paste0("## ",rotation_))
    # for(nfactors_ in c(4:10)){
    # for(nfactors_ in c(4:10)){
    mainTitle <- paste0(rotation_,": ", title_string, "(N = ",sample_size," )")
    cat("\n\n")
    # cat(paste0("### ",nfactors_));
    # cat("\n\n")
    solution <- 
      display_solution(R,k=nfactors_,sample_size,rotation_,mainTitle=mainTitle) %>%
      quick_save(name = file_name,w=8,h=12)
    cat("\n\n")
    
    # }
  }
  
}




# ---- print-solution --------------------
phase <- "0"
# R <- R0 # correlation matrix for items at phase 0
# R <-  d_ordinal_complete %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
# R <-  d_ordinal_NAto0 %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
R <-  d_binary_complete %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
# R <-  d_binary_NAto0 %>%  make_corr_matrix(item_names = econ_vars, method = "spearman")
#

sample_size <- n_obs
k <- 3
nfactors_ <- k

for(rotation_ in c(
  "Varimax"
  ,"geominT"
  ,"bifactorT"
  # ,"quartimax"
  # ,"oblimin"
  ,"geominQ"
  # ,"bifactorQ"
)){
  
  cat("\n\n")
  cat(paste0("## ",rotation_))
  # for(nfactors_ in c(4:10)){
  # for(nfactors_ in c(4:10)){
  mainTitle <- paste0(rotation_,", Binary - Comnplete Cases Only")
  cat("\n\n")
  # cat(paste0("### ",nfactors_));
  # cat("\n\n")
  solution <- display_solution(R,k=nfactors_,sample_size,rotation_,mainTitle=mainTitle) %>%
    quick_save(name = paste0("BCCO-",rotation_),w=8,h=12)
  cat("\n\n")
  
  
  # }
}

# ---- more-cor-1 ----------------------------

corrplotCustom <- function (corr, lower="number", upper="circle", tl.pos=c("d",
                                                                           "lt", "n"), diag=c("n", "l", "u"), bg="white", addgrid.col="gray", ...)  {
  
  diag <- match.arg(diag)
  tl.pos <- match.arg(tl.pos)
  n <- nrow(corr)
  corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
  corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
  if (diag == "n" & tl.pos != "d") {
    symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
  }
}


ds1 <- ds1 %>%
  dplyr::rename(
    new = score_n,
    chu = score_chu,
    warwick = score_warwick,
    old = score_e
  )

# item_names <- c(varname_n_scale,varname_e_scale, c("score_n","score_chu","score_warwick","score_e"))
item_names <- c(varname_n_scale, c("new","chu","warwick","old"))
ds_matrix <- ds1 %>% dplyr::select_(.dots=item_names)
# ds_matrix %>% glimpse(80)

rownames <- dto$metaData %>%
  dplyr::filter(item_name %in% item_names) %>%
  dplyr::mutate(display_name1 = paste0(item_name),
                display_name2 = paste0(item_name,"\n",item_label),
                display_name3 = paste0(item_name,"_",item_label)
  )

rownames <- rownames[,"display_name2"]
rownames <- c(rownames %>% as.list() %>% unlist() %>% as.character(),  c("new","chu","warwick","old"))

cormat <- cor(ds_matrix)
colnames(cormat) <- rownames; rownames(cormat) <- rownames

cormat %>% corrplotCustom(
  # order="AOE",
  lower="pie", upper="number",
  title="Correlation Among Observed Variables", line=-1,
  tl.col="black", addCoef.col="black", cl.cex=1.7)


# ----- publisher --------------------
path <- "./analysis/2-efa/2-efa.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)
