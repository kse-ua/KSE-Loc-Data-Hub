#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "ukr")
rm(list = ls())

#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(survey)
library(fastDummies)
library(lubridate)
#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/survey hromada analysis/prints
                        ")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

#+ declare-functions -----------------------------------------------------------
'%ni%' <- Negate(`%in%`)

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")

# Xls_form
survey_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
choices_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "choices")

#+ 

#mcq questions from kobofile
mcq<-survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(str_detect(type, "select_multiple"))%>%
  dplyr::select(name)%>%
  pull()

#vectors of mcq names
preparation <- ds_survey %>% select(starts_with("prep_")) %>% colnames()
comm_channels <- ds_survey %>% select(telegram:hotline) %>% colnames()
idp_help <- ds_survey %>% select(starts_with('idp_help/')) %>% colnames()
military_help <- ds_survey %>% select(starts_with('help_for_military/')) %>% colnames()
# only for occupied hromadas - few cases
hromada_cooperation <- d1 %>% select(starts_with('hromada_cooperation/')) %>% colnames()
prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')

#+ ANALYSIS

# correlations

#SPEARMEN RANK CORRELATION FOR Q on preparations (between 14 items + total score + financial metrics)
# ?financial metrics which represent preparation - income per capita, level of own income

cor_mat <- 
  cor(d3 %>% select(all_of(preparation), prep_count)
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, file="./analysis/prints/cor_preparation.png", type = "cairo")

corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1.5, addCoef.col = "black", number.cex=1.5, order = "FPC")

dev.off()

#+ --------model for preparation and income before war -------------------------
d3 %>%
  # filter(income_own_per_capita < 15000) %>% 
  ggplot(aes(income_own_per_capita, prep_count)) +
  geom_point(aes(color = region_en)) +
  geom_smooth(se=T, method = "lm")

lm(data = d3 
   # filter(income_own_per_capita < 15000)
   ,prep_count ~ income_own_per_capita
) %>% summary()

hist(d3$income_own_per_capita)

#+ COMPARISON OF SURVEYED HROMADAS WITH GENERAL POPULATION OF HROMADAS ---------
