rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# working through the vignette 
# https://cran.r-project.org/web/packages/twang/vignettes/twang.pdf
# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(explore)   # for `describe_all()`
library(scales)    # formatting
library(labelled)  # labels - https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
library(rlang)     # tidy evaluations -  https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
library(twang)     # propensity score analysis
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
# prints_folder <- paste0("./analysis/.../prints/")
# if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

path_data_input <- "./data-private/derived/ellis-2021-11-26.rds"


pivot <- c(
  "treat"

)

covariates <- c(
   "age"
  ,"educ"
  ,"black"
  ,"hispan"
  ,"married"
  ,"nodegree"
  ,"re74"
  ,"re75"

)

outcomes <- c(
  "re78"
)

extra <- c(

)


# ---- declare-functions -------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/twang-vignette/prints/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

# ---- load-data ---------------------------------------------------------------
data(lalonde)
ds0 <- lalonde
ds0 %>% glimpse()


ds1 <-
  ds0 %>%
  select(
    all_of(
      c(
        pivot
        ,covariates
        ,outcomes
        ,extra
      )
    )
  )

ds1 %>% explore::describe_all()

# ---- inspect-data ------------------------------------------------------------
ds1 %>%
  group_by(
    treat
  ) %>%
  summarize(
    count_id = n()
  ) %>%
  arrange(treat) %>%
  group_by(treat)
da1 <- ds1


# Steps in the PSA analysis described in twang() vignette
# 1. Determine the estimand (ATT)
# 2. Determine the observed confounding factors to be balanced (covariate list)
# 3. Fit the propensity score model
# 3(a) Evaluate the convergence of the algorithm
# 3(b) Assess the balance of confounding factors before and after applying the propensity score weights
# 3(c) Rerun if needed tweaking algorithm features
# 4. Estimate the net effect

# ---- ps1-model ---------------------------------------------------------------

da1 %>% select(c(pivot))   %>% describe_all() %>% print(n=nrow(.))
da1 %>% select(covariates) %>% describe_all() %>% print(n=nrow(.))
da1 %>% select(outcomes)   %>% describe_all() %>% print(n=nrow(.))

dependent <- "treat"
explanatory <- covariates
eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )

# ps1 <-
#   ps(
#     formula            = eq_formula
#     ,data              = da1
#     ,n.trees           = 5000
#     ,interaction.depth = 2
#     ,shrinkage         = 0.01
#     ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
#     ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
#     ,n.minobsinnode    = 10
#     ,n.keep            = 1
#     ,n.grid            = 25
#     ,ks.exact          = NULL
#     ,verbose           = FALSE
#   )

# read description of the ls object here: https://www.rdocumentation.org/packages/twang/versions/1.6/topics/ps

# readr::write_rds(ps1,"./data-private/derived/ps1_15000.rds")
# readr::write_rds(ps1,"./data-private/derived/ps1_5000.rds")
# ps1 <- readr::read_rds("./data-private/derived/ps1_15000.rds")
ps1 <- readr::read_rds("./data-private/derived/ps1_5000.rds")
# ---- ps1-diagnostics-1 ---------------------------------------------------------

bt1 <- ps1 %>% bal.table() # table with balance evaluation
bt1[1:2] %>% purrr::map(head) # one for each stop.method + unweighted
ps1 %>% plot("histogram")
summary(ps1)

bt1$unw %>% head()
bt1$es.mean.ATT %>% head()

# we can use these tables to compute our own summary of standardized bias
summary(ps1) %>% head(1)
stop_method <- "unw"
tibble(
  max.es  = max( abs(bt1[[stop_method]]$std.eff.sz))
  ,mean.es = mean(abs(bt1[[stop_method]]$std.eff.sz))
  ,max.ks  = max( abs(bt1[[stop_method]]$ks))
  ,mean.ks = mean(abs(bt1[[stop_method]]$ks))
)

# ------ explore-summary-table ---------------
d <- bal.table(ps1)$es.mean.ATT
st <- summary(ps1) %>% round(4)
st[2,c(5:7,9)] # summary for es.mean.ATT
d %>% summarize(max.es = max(std.eff.sz))
d %>% summarize(mean.es = mean(abs(std.eff.sz)))
d %>% summarize(max.ks = max(ks))
d %>% summarize(mean.ks = mean(abs(ks)))

# ---- ps1-diagnostics-2 ----

ps1 %>% plot("optimize") # balance as  function of gbm.iteration
# this command plots the size of the imbalance vs. the number of iterations.  For
# es.max.ATT and ks.max.ATT, this is the maximum of the absolute value of std.eff.sz
# (standardized bias).  For ex.mean.ATT and ks.mean.ATT, this is the mean of the
# absolute value of std.eff.sz (standardized bias)

ps1 %>% plot("boxplot") # distribution of propensity scores
# we have common support when the propensity scores approximately lineup between treatment and control
# we lack common support if propensity score is generally low for control and high for treatment

ps1 %>% plot("es") # standardized effect size of pre-treatment variables
# each dot is a variable and the higher the dot, the greater the dissimilarity between treatment and control
# es test assumes normal distribution to compute standardized effects

ps1 %>% plot("t") # t-test p-values for weighted pre-treatment variables
# we learned that es.max.ATT optimizes the worst match and so we should use it
# if we are afraid that the poorest match is the most important variable to balance on
# we should use one of the other three methods if all variables to balance on are equally important

ps1 %>% plot("ks") # kolmogorov-smirnov p-values for weighted pre-treatment variables
# ks test is a nonparametric test that compares cumulative distributions of two datasets
# Ho: both groups are sampled from populations with identical distributions
# Ha: null hypothesis violated:  different medians, variances, or distributions.
bt1$es.max.ATT
# the higher the P-value, the closer the groups are to being balanced for each variable
# CAUTION:  this is actually not a good measure because it is sensitive to sample size
# we should be looking at standardized effect sizes rather than P-values.

ps1 %>% plot("histogram") # of weights for treatment and control
# we are unclear what weights represent.  We need to investigate further
# this is a distribution of $w
ps1$w %>%
  tidyr::pivot_longer(1:4) %>%
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(facets = "name", nrow=1)

d_rel_inf <-
  ps1$gbm.obj %>%
  summary(
    n.trees = ps1$desc$ks.max.ATT$n.trees, plot=FALSE
  )
d_rel_inf # relative influence of each variable for estimating the probability of
# treatment assignment (adds up to 100%)
bt1$unw

# ---- balance tables and statistical distances ----
da1_unw <- bt1$unw$std.eff.sz
da1_es_mean <- bt1$es.mean.ATT$std.eff.sz
da1_es_max <- bt1$es.max.ATT$std.eff.sz
da1_ks_mean <- bt1$ks.mean.ATT$std.eff.sz
da1_ks_max <- bt1$ks.max.ATT$std.eff.sz

unw_dist <- sqrt(sum(da1_unw*da1_unw))
es_mean_dist <- sqrt(sum(da1_es_mean*da1_es_mean))
es_max_dist <- sqrt(sum(da1_es_max*da1_es_max))
ks_mean_dist <- sqrt(sum(da1_ks_mean*da1_ks_mean))
ks_max_dist <- sqrt(sum(da1_ks_max*da1_ks_max))
# for this particular data set, ks.max yielded the best match:  lowest statistical distance
# all four estimators produced much lower statistical distance than un-weighted, which is 0.435
d <-
  tibble(unw_dist, es_mean_dist, es_max_dist, ks_mean_dist, ks_max_dist) %>%
  tidyr::pivot_longer(cols = 1:5) %>%
  arrange(value)
d
d %>% ggplot(
  aes(x = value, y = name)
) +
  scale_x_continuous(expand=expansion(mult = c(0, .15))) +
  geom_col(alpha = .3, fill = "salmon", color = "black") +
  geom_text(aes(label = scales::number(value, accuracy = .0001)), hjust= -.7)

# let us verify the values produced by the balance tables.
# is the un-weighted table consistent with the original data?
da1 %>%
  group_by(gender, tx) %>%
  count() %>%
  arrange(tx) %>%
  group_by(tx) %>%
  mutate(
    tx_total = sum(n)
    ,gedner_prop = n / tx_total
  )
bt1$unw # yes it is
# is the weighting ex-post?
# let us use ks.max because we found that to be the lowest statistical distance
bt1$ks.max.ATT # yes it is
# this confirms that ATT is the choice we need because only the comparison group and not the
# program group changes
# ATT = Average Treatment Effect on the Treated

# ----- tweak-data-2 --------------------------------
# different stopping methods produce different effective sample size
ps1 <- ps1
summary(ps1)

# view the description of ps object: https://rdrr.io/rforge/twang/man/ps.html
ps1$ps # estimated propensity scores
ps1$w # propensity score weights

# the ps() function creates two objects storing weights: ps and w
d <- ps1$data
dps <- bind_cols(d, ps1$ps); dps %>% glimpse()
dw <- bind_cols(d, ps1$w); dw %>% glimpse()
# they are not the same values
tibble(dps$es.mean.ATT, dw$es.mean.ATT) %>% head()
# get.weights() clearly returns the contents of $w object (and not $ps)
d1 <- get.weights(ps1,"es.mean","ATT",withSampW =TRUE)
tibble(dps$es.mean.ATT, dw$es.mean.ATT,d1) %>% head()

# to verify what "w" table contains
# Hypothesis: for ATT
# Treatment group = 1
# Control group = p/(1-p)
dps %>% glimpse()
dw %>% glimpse()
# to verify
d1 <-
  dplyr::bind_cols(
    dps %>% select(pivot, covariates, outcomes, extra)
    ,ps = dps$es.mean.ATT
    ,w = dw$es.mean.ATT
  ) %>%
  as_tibble()
d1 %>% glimpse()
#
d2 <-
  d1 %>%
  mutate(
    new_w = case_when(
      treat == 1 ~ 1
      ,treat == 0 ~ ps/(1-ps)
    )
    ,is_same = zapsmall(w - new_w)==0L
  )

d2 %>%
  group_by(treat) %>%
  summarize(
    sum_ps    = sum(ps, na.rm = T)
    ,sum_w     = sum(w, na.rm = T)
    ,sum_new_w = sum(new_w, na.rm = T)
  )
summary(ps1)



summary(ps1)
dps$es.mean.ATT %>% sum() # sample size of the treatment group
dw$es.mean.ATT %>% sum() # ??? but what is this
# moreover, the distri
d <-
  tibble(dps$es.mean.ATT, dw$es.mean.ATT,group=dps$treat) %>%
  pivot_longer(cols=1:2) %>%
  mutate(
    name =  name %>% str_remove("^d") %>% str_remove("\\$es.mean.ATT$")
  )
d
d %>%
  ggplot(aes(x=value, fill=name))+
  geom_histogram(alpha=.5,color="black",position = "identity",binwidth = .05)+
  # geom_density(alpha=.5)+
  facet_grid(group~name)+
  labs()
# the presence of 1 in estimates of $w give us a hint
# dw %>% filter(treat==1)
dw %>% filter(es.mean.ATT==1) %>% summarize(sum(es.mean.ATT))
dps %>% filter(es.mean.ATT==1) %>% summarize(sum(es.mean.ATT))

dw %>% as_tibble()
dw %>% group_by(treat) %>% summarize(sum = sum(es.mean.ATT))
dps %>% group_by(treat) %>% summarize(sum = sum(es.mean.ATT))
# dw %>% group_by(group,tx) %>% summarize(sum = sum(es.mean.ATT))
# dps %>% group_by(group,tx) %>% summarize(sum = sum(es.mean.ATT))

# ----- -----------------------------

da2 <- da1
da2$w <- get.weights(ps1, stop.method = "es.max")
da2$w <- get.weights(ps1, stop.method = "es.mean")
# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
