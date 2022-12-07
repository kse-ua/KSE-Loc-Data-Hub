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
prints_folder <- paste0("./analysis/survey-hromada-analysis/prints")
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
preparation <- ds_survey %>% select(starts_with("prep_"), -prep_winter_count, -prep_count) %>% 
  colnames()
comm_channels <- ds_survey %>% select(telegram:hotline) %>% colnames()
idp_help <- ds_survey %>% select(starts_with('idp_help/'), -ends_with('number')) %>% colnames()
military_help <- ds_survey %>% select(starts_with('help_for_military/')) %>% colnames()
# only for occupied hromadas - few cases
hromada_cooperation <- ds_survey %>% select(starts_with('hromada_cooperation/')) %>% colnames()
prep_for_winter <- c('info_campaign', 'reserves', 'count_power_sources', 
                     'count_heaters_need', 'solid_fuel_boiler')

#+ recode variables ------------------------------------------------------------

ds_survey <- ds_survey %>% 
  mutate(income_own_per_capita = income_own_2021 / total_population_2022,
         income_total_per_capita = income_total_2021 / total_population_2022,
         income_tranfert_per_capita = income_transfert_2021 / total_population_2022)

#+ ANALYSIS

#+ correlations of preparation items with preparation index --------------------

cor_mat <- 
  cor(ds_survey %>% select(all_of(preparation), prep_count)
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, 
    file="./analysis/survey-hromada-analysis/prints/cor_preparation.png", 
    type = "cairo")

corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1.5, 
                   addCoef.col = "black", number.cex=1.5, order = "FPC")

dev.off()

#+ correlations of preparation index with economic indicators ------------------

income <- ds_survey %>%
  select(starts_with('income'), ends_with('prop_2021')) %>% colnames()

cor_mat <- 
  cor(ds_survey %>% select(all_of(income), prep_count, prep_winter_count)
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, 
    file="./analysis/survey-hromada-analysis/prints/cor_preparation_economics.png", 
    type = "cairo")

corrplot::corrplot(cor_mat[1:23,22:23, drop=F], tl.col = "black",tl.cex = 1.5, 
                   addCoef.col = "black", number.cex=1.5, cl.pos = 'n')

dev.off()

#+ vol/nonvol amalgamation and preparation index ---------------------------------

ds_survey %>%
  group_by(voluntary) %>%
  summarize(mean_prep = mean(prep_count)) %>%
  ggplot(aes(x = factor(voluntary), y = mean_prep)) +
  geom_text(aes(label = mean_prep)) +
  geom_col()

ds_survey %>%
  group_by(voluntary) %>%
  rstatix::get_summary_stats(prep_count, type = "mean_sd")

res.aov <- ds_survey %>% rstatix::anova_test(prep_count ~ voluntary)
# Result: doesn't differ

#+ --------model for preparation and income before war -------------------------

ds_survey %>%
  # filter(income_total_per_capita < 15000) %>%
  ggplot(aes(income_total_per_capita, prep_count)) +
  geom_point(aes(color = type)) +
  geom_smooth(se=T, method = "lm")

lm(data = ds_survey 
   # filter(income_own_per_capita < 15000)
   ,prep_count ~ income_total_per_capita
) %>% summary()

lm(data = ds_survey 
   # filter(income_own_per_capita < 15000)
   ,prep_count ~ income_own_per_capita
) %>% summary()

lm(data = ds_survey 
   %>%
   filter(income_tranfert_per_capita < 3000)
   ,prep_count ~ income_tranfert_per_capita
) %>% summary()

hist(ds_survey$income_tranfert_per_capita)

#+ administrative adaptation (percent of working) ------------------------------

hist(ds_survey$percent_working_march)
hist(ds_survey$percent_working_now)

ds_survey <- ds_survey %>%
  mutate(percent_working_march_cut = cut(percent_working_march, breaks = c(0,80,100), include.lowest = F),
         percent_working_now_cut = cut(percent_working_now, breaks = c(0,80,100), include.lowest = F))

plot(ds_survey$percent_working_march_cut)
plot(ds_survey$percent_working_now_cut)

ds_survey %>%
  group_by(percent_working_march_cut) %>%
  summarise(mean_prep = mean(prep_count, na.rm = T),
            sd_prep = sd(prep_count, na.rm = T)) %>%
ggplot(aes(x=percent_working_march_cut, y = mean_prep)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean_prep-sd_prep, ymax=mean_prep+sd_prep), width=.2,
                position=position_dodge(.9)) 

#+ COMPARISON OF SURVEYED HROMADAS WITH GENERAL POPULATION OF HROMADAS ---------
