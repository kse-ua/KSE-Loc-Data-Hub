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

prints_folder <- paste0("./analysis/eda-preparations/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }


# d_meta <- tibble::tribble(
#   ~item_name, ~keyword, ~label_ua,
#   "prep_first_aid_water"            , "store_water"  ,"Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби)",
#   "prep_first_aid_fuel"             , "store_fuel"   ,"Сформовані запаси товарів першої необхідності (паливо)",
#   "prep_reaction_plan"              , "plan_react"   ,"Оновлено чи затверджено план реагування на надзвичайні ситуації",
#   "prep_evacuation_plan"            , "plan_evac"    ,"Складено спеціальний план евакуації населення при загрозі збройного конфлікту",
#   "prep_reaction_plan_oth_hromadas" , "plan_other"   ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад",
#   "prep_reaction_plan_oda"          , "plan_oda"     ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА",
#   "prep_dftg_creation"              , "create_tdf"   ,"Розпочато створення (добровольчого) формування територіальної громади",
#   "prep_national_resistance"        , "nat_resist"   ,"Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади",
#   "prep_starosta_meeting"           , "meet_heads"   ,"Проведена зустріч зі старостами з приводу дій у випадку вторгнення",
#   "prep_communal_meetiing"          , "meet_utility" ,"Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення",
#   "prep_online_map"                 , "shelter_map"  ,"Опублікована онлайн-мапа укриттів в громаді",
#   "prep_shelter_list"               , "shelter_list" ,"Опублікований перелік адрес укриттів в соцмережах або на сайті громади",
#   "prep_notification_check"         , "notify_test"  ,"Перевірено засоби оповіщення населення",
#   "prep_backup"                     , "backup_full"  ,"Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)",
#   "prep_partly_backup"              , "backup_part"  ,"Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)"
# )


d_meta <- 
  tibble::tribble(
    ~item_name, ~item_number, ~label_ua,  ~label_en, ~label_ua_full,
    "prep_first_aid_water"            ,1L , "Воду запасли - (1)"             , "Water stored (1)","Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби)",
    "prep_first_aid_fuel"             ,2L , "Паливо запасли - (2)"           , "Fuel stored (2)","Сформовані запаси товарів першої необхідності (паливо)",
    "prep_reaction_plan"              ,3L , "План реагування є - (3)"        , "Plan of response (3)","Оновлено чи затверджено план реагування на надзвичайні ситуації",
    "prep_evacuation_plan"            ,4L , "План евакуації є - (4)"         , "Plan of evacuation (4)","Складено спеціальний план евакуації населення при загрозі збройного конфлікту",
    "prep_reaction_plan_oth_hromadas" ,5L , "План узгоджен з інш.грм. - (5)" , "Plan coord w/ oth. Hs (5)","Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад",
    "prep_reaction_plan_oda"          ,6L , "План узгоджен з ОДА - (6)"      , "Plan coord w/ Oblast (6)","Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА",
    "prep_dftg_creation"              ,7L , "Створили ТРО - (7)"             , "Territorial Defense (7)","Розпочато створення (добровольчого) формування територіальної громади",
    "prep_national_resistance"        ,8L , "План спротиву є - (8)"          , "Plan of resistance (8)","Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади",
    "prep_starosta_meeting"           ,9L , "Зустрілись зі старостами - (9)" , "Meeting with heads (9)","Проведена зустріч зі старостами з приводу дій у випадку вторгнення",
    "prep_communal_meetiing"          ,10L , "Зустрілись із ЖЕКами - (10)"    , "Meeting with utilities (10)","Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення",
    "prep_online_map"                 ,11L , "Мапа укриттів онлайн є - (11)"  , "Shelter map online (11)","Опублікована онлайн-мапа укриттів в громаді",
    "prep_shelter_list"               ,12L , "Список укриттів онлан є - (12)" , "Shelter list online (12)","Опублікований перелік адрес укриттів в соцмережах або на сайті громади",
    "prep_notification_check"         ,13L , "Оповіщення працює! - (13)"      , "Communication tested (13)","Перевірено засоби оповіщення населення",
    "prep_backup"                     ,14L , "Дані зберегли повністю - (14)"  , "Data backup fully (14)","Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)",
    "prep_partly_backup"              ,15L , "Дані зберегли частково - (15)"  , "Data backed up partially (15)","Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)"
  )
# d_meta <- tibble::tribble(
#   ~item_name, ~keyword, ~label_ua,
#   "prep_first_aid_water"            , "store_water"  ,"Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби)",
#   "prep_first_aid_fuel"             , "store_fuel"   ,"Сформовані запаси товарів першої необхідності (паливо)",
#   "prep_reaction_plan"              , "plan_react"   ,"Оновлено чи затверджено план реагування на надзвичайні ситуації",
#   "prep_evacuation_plan"            , "plan_evac"    ,"Складено спеціальний план евакуації населення при загрозі збройного конфлікту",
#   "prep_reaction_plan_oth_hromadas" , "plan_other"   ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад",
#   "prep_reaction_plan_oda"          , "plan_oda"     ,"Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА",
#   "prep_dftg_creation"              , "create_tdf"   ,"Розпочато створення (добровольчого) формування територіальної громади",
#   "prep_national_resistance"        , "nat_resist"   ,"Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади",
#   "prep_starosta_meeting"           , "meet_heads"   ,"Проведена зустріч зі старостами з приводу дій у випадку вторгнення",
#   "prep_communal_meetiing"          , "meet_utility" ,"Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення",
#   "prep_online_map"                 , "shelter_map"  ,"Опублікована онлайн-мапа укриттів в громаді",
#   "prep_shelter_list"               , "shelter_list" ,"Опублікований перелік адрес укриттів в соцмережах або на сайті громади",
#   "prep_notification_check"         , "notify_test"  ,"Перевірено засоби оповіщення населення",
#   "prep_backup"                     , "backup_full"  ,"Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)",
#   "prep_partly_backup"              , "backup_part"  ,"Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру)"
# )

prep_vars <- d_meta %>% pull(item_name)

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
ds0 <- readr::read_rds("./analysis/eda-preparations/d3.rds")

# ---- inspect-data -------------------------------------------------------------
ds0 %>% glimpse(
  
)
# ---- tweak-data --------------------------------------------------------------

ds1 <- 
  ds0 %>%
  # dplyr::select( all_of(c("hromada_code",prep_vars)))  # no codes
  dplyr::select( all_of(c("_id",prep_vars)))  # no codes

ds1 %>% glimpse()

# ordinal, only complete cases
d_ordinal_complete <- ds1 %>% na.omit()
d_ordinal_complete %>% glimpse()
m_ordinal_complete <- 
  d_ordinal_complete %>% 
  make_corr_matrix(item_names = prep_vars, method = "spearman")  


# ordinal, with NA replaced with 0
d_ordinal_NAto0 <- 
  ds1 %>% 
  mutate(
    across(
      .cols = prep_vars
      ,.fns = ~tidyr:::replace_na(., 0)
    )
  )
d_ordinal_NAto0 %>% glimpse()
m_ordinal_NAto0 <- 
  d_ordinal_NAto0%>% 
  make_corr_matrix(item_names = prep_vars, method = "spearman")  


# Binary, only complete cases
d_binary_complete <- 
  ds1 %>% 
  na.omit() %>% 
  mutate(
    across(
      prep_vars
      , ~case_when(.==0L~0L,TRUE ~ 1L)
    )
  )
d_binary_complete %>% glimpse()
m_binary_complete <- 
  d_binary_complete %>% 
  make_corr_matrix(item_names = prep_vars, method = "spearman")  


# Binary. with NA replaced with 0
d_binary_NAto0 <- 
  ds1 %>% 
  mutate(
    across(
      prep_vars
      , ~case_when(.==0L~0L,TRUE ~ 1L)
    )
  )
d_binary_NAto0 %>% glimpse()
m_binary_NAto0 <- 
  d_binary_NAto0 %>% 
  make_corr_matrix(item_names = prep_vars, method = "spearman")  

# ---- inspect-correlations -------------------------------------

png(paste0(prints_folder,"1-OCCO-raw.png"),width = 12,height = 12,units = "in",res = 100)
m_ordinal_complete %>% make_corr_plot( title = "0|1|2 - Complete Cases Only")
dev.off()

png(paste0(prints_folder,"1-ONAZ-raw.png"),width = 12,height = 12,units = "in",res = 100)
m_ordinal_NAto0 %>% make_corr_plot(title =  "0|1|2 - NA replaced with 0")
dev.off()

png(paste0(prints_folder,"1-BCCO-raw.png"),width = 12,height = 12,units = "in",res = 100)
m_binary_complete %>% make_corr_plot(title = "0|1 - Complete Cases Only")
dev.off()

png(paste0(prints_folder,"1-BNAZ-raw.png"),width = 12,height = 12,units = "in",res = 100)
m_binary_NAto0 %>% make_corr_plot(title = "0|1 - NA replaced with 0")
dev.off()

# now with clusters 

png(paste0(prints_folder,"2-OCCO-hclust.png"),width = 12,height = 12,units = "in",res = 100)
m_ordinal_complete %>% corrplot(order="hclust", addrect=3, title = "0|1|2 - Complete Cases Only")
dev.off()

png(paste0(prints_folder,"2-ONAZ-hclust.png"),width = 12,height = 12,units = "in",res = 100)
m_ordinal_NAto0 %>% corrplot(order="hclust", addrect=3, title = "0|1|2 - NA replaced with 0")
dev.off()

png(paste0(prints_folder,"2-BCCO-hclust.png"),width = 12,height = 12,units = "in",res = 100)
m_binary_complete %>% corrplot(order="hclust", addrect=3, title = "0|1 - Complete Cases Only")
dev.off()

png(paste0(prints_folder,"2-BNAZ-hclust.png"),width = 12,height = 12,units = "in",res = 100)
m_binary_NAto0 %>% corrplot(order="hclust", addrect=3,title = "0|1 - NA replaced with 0")
dev.off()

# ---- diagnose-0a ------------------------------------------------------
# R0 <-  d_ordinal_complete %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
R0 <-  d_ordinal_NAto0 %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
# R0 <-  d_binary_complete %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
# R0 <-  d_binary_NAto0 %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")  
# Diagnosing number of factors
(obs <- nrow(d_ordinal_NAto0))
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
ds_index <- get_indidces(ls_solution) # not working, investigate, possibly old packages
ds_index %>% print()
# ---- diagnose-0e -------------------------
FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:6,RMSEA.cutoff = .08)

FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:6,RMSEA.cutoff = .05)
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
  "OCCO" = "Ordinal - Complete Cases Only"    # 0|1|2 - dropped NA
  ,"BCCO" = "Binary - Complete Cases Only"    # 0|1 - dropped NA
  ,"ONAZ" = "Ordinal - NA replaced with Zero" # 0|1|2 - replaced NA with 0
  ,"BNAZ" = "Binary - NA replaced with Zero"  # 0|1 - replaced NA with 0
)

for(condition_i in names(conditions)){
  
  title_string <- conditions[condition_i]
  file_string <- condition_i
  
  if(condition_i=="OCCO"){
    d <- d_ordinal_complete
    R <-  d %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
  }
  if(condition_i=="ONAZ"){
    d <- d_ordinal_NAto0
    R <-  d %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
  }
  if(condition_i=="BCCO"){
    d <- d_binary_complete
    R <-  d %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
  }
  if(condition_i=="BNAZ"){
    d <- d_binary_NAto0
    R <-  d %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
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
# R <-  d_ordinal_complete %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
# R <-  d_ordinal_NAto0 %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
R <-  d_binary_complete %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
# R <-  d_binary_NAto0 %>%  make_corr_matrix(item_names = prep_vars, method = "spearman")
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
