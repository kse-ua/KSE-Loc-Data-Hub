#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "rus")
rm(list = ls())

#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(car)
library(broom)
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
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")
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
# vector of income variables 
income <- ds_survey %>%
  select(ends_with('capita'), ends_with('prop_2021')) %>% colnames()

#+ recode variables ------------------------------------------------------------

ds_survey <- ds_survey %>% 
  mutate(income_own_per_capita = income_own_2021 / total_population_2022,
         income_total_per_capita = income_total_2021 / total_population_2022,
         income_tranfert_per_capita = income_transfert_2021 / total_population_2022,
         idp_registration_share = idp_registration_number / total_population_2022,
         idp_real_share = idp_real_number / total_population_2022,
         idp_child_share = idp_child_education / idp_registration_number,
         sum_osbb_2020 = replace_na(sum_osbb_2020, 0)
  )

#+ ANALYSIS

#+ general info about survey ---------------------------------------------------

d1 <- ds_general %>%
  count(oblast_name_en)

d2 <-  ds_survey %>%
  count(oblast_name_en)

d3 <- d1 %>% left_join(d2, by = c('oblast_name_en'))

d4 <- d3 %>%
  filter(!is.na(oblast_name_en)) %>%
  rename(total_number_ATC = n.x,
         survey_number_ATC = n.y) %>%
  mutate(survey_number_ATC = replace_na(survey_number_ATC, 0),
         survey_coverage_atc = case_when(is.na(survey_number_ATC) ~ 0,
                                     TRUE ~ survey_number_ATC / total_number_ATC),
         survey_coverage_atc_perc = scales::percent(survey_coverage_atc, 1)
  ) %>%
  arrange(desc(survey_coverage_atc)) %>%
  select(-survey_coverage_atc)

d5 <- ds_general %>%
  group_by(oblast_name_en) %>%
  summarise(population = sum(total_population_2022))

d6 <- ds_survey %>%
  group_by(oblast_name_en) %>%
  summarise(survey_population = sum(total_population_2022))

d7 <- d5 %>% left_join(d6, by = c('oblast_name_en'))
  
  
d8 <- d7 %>%
  filter(!is.na(oblast_name_en)) %>%
  mutate(survey_population = replace_na(survey_population, 0),
         survey_coverage_pop = case_when(is.na(survey_population) ~ 0,
                                     TRUE ~ survey_population / population),
         survey_coverage_pop_perc = scales::percent(survey_coverage_pop, accuracy = 1)
  ) %>%
  arrange(desc(survey_coverage_pop)) %>%
  select(-survey_coverage_pop)

d9 <- d4 %>% left_join(d8 %>% select(oblast_name_en, survey_coverage_pop_perc), by = c('oblast_name_en'))

survey_cov_gttable1 <- gt::gt(d9)

d10 <- d2 %>% 
  mutate(pct = n / sum(n), perc = scales::percent(pct, 1)) %>% 
  arrange(desc(pct)) %>%
  select(-pct) %>%
  rename(ATCs = n, Share = perc)
  
survey_cov_gttable2 <- gt::gt(d10)

survey_cov_gttable1 <- survey_cov_gttable1 %>%
  gt::tab_header(
    title = "Survey Regional Coverage"
    ) %>%
  gt::cols_label(oblast_name_en = 'Oblast',
                 total_number_ATC = 'Total number \nof ATCs',
                 survey_number_ATC = 'ATCs in the survey',
                 survey_coverage_atc_perc = 'Survey Coverage of ATCs',
                 survey_coverage_pop_perc ='Survey Coverage of Population'
                 )
survey_cov_gttable1 %>% gt::gtsave("tab_1.png", expand = 10)
#+ social capital --------------------------------------------------------------
check <- c('youth_councils', 'youth_centers', 'sum_osbb_2020')

ds_survey %>%
  select(all_of(check)) %>%
  summarise_all(funs(sum(is.na(.)))) %>% t()

ds_survey %>%
  filter(is.na(sum_osbb_2020)) %>% arrange(type) %>% view()

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

#+ models of preparation index -------------------------------------------------
cor_mat <- 
  cor(ds_general %>% select(sum_osbb_2020, youth_councils, youth_centers, business_support_centers)
      ,use = "complete.obs"
      ,method = "spearman")

psych::KMO(cor_mat)
psych::cortest.bartlett(cor_mat)
ev <- eigen(cor_mat) # get eigenvalues
psych::scree(ds_general %>% select(sum_osbb_2020, youth_councils, youth_centers, business_support_centers), pc=FALSE)  # Use pc=FALSE for factor analysis

##
ds_models <- ds_survey %>% filter(!is.na(turnout_2020))

model_prep_count_1 <- lm(data = ds_models %>% filter(!is.na(prep_count)),
                         prep_count ~ log(income_total) + own_income_prop_2021 +
                           urban_pct + n_settlements + region_en +
                           occupation + military_action + voluntary)
model_prep_count_2 <- lm(data = ds_models %>% filter(!is.na(prep_count)),
                         prep_count ~ log(income_total) + own_income_prop_2021 +
                           urban_pct + n_settlements + region_en +
                           occupation + military_action + voluntary + turnout_2020 +
                           sex_head + age_head + education_head + incumbent)
model_prep_count_3 <- lm(data = ds_models %>% filter(!is.na(prep_count)) %>%
                           mutate(sum_osbb_2020 = replace_na(sum_osbb_2020, 0)),
                         prep_count ~ log(income_total) + own_income_prop_2021 + turnout_2020 +
                           urban_pct + n_settlements + region_en +
                           occupation + military_action + voluntary + sex_head + age_head +
                           education_head + incumbent + business_support_centers + 
                           youth_centers + sum_osbb_2020)
stargazer(model_prep_count_1, model_prep_count_2, model_prep_count_3, single.row = T, 
          dep.var.labels = 'Index of Preparation', type = 'html')

# model diagnostics
plot(model_prep_count_3)
anova(model_prep_count_1, model_prep_count_2, model_prep_count_3)
#+ vol/nonvol amalgamation and preparation index -------------------------------

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

#+ IDP correlation -------------------------------------------------------------

d1 <- ds_survey %>% 
  filter(!is.na(idp_accept)) %>%
  select(idp_registration_number, idp_registration_share, urban_pct,
         total_population_2022, all_of(income), type, region_en) %>%
  fastDummies::dummy_cols(select_columns = c('type', 'region_en'),
                          remove_selected_columns = TRUE)
  
# plot
cor_mat_idp <- 
  cor(d1
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, file="./analysis/survey-hromada-analysis/prints/cor_idp.png", type = "cairo")

corrplot::corrplot(cor_mat_idp[1:22, 1:2], tl.col = "black",tl.cex = 1.5, 
                   addCoef.col = "black", number.cex=1.5, cl.pos = 'n')

dev.off()

#+ Communication of head of hromada

unique(ds_survey$head_hromada_communication)
com_hromadas_levels <- c('2_3_times', 'once_a_day', 'few_times_a_week', 
                         'once_a_week', 'none')

ds_survey <- ds_survey %>%
  mutate(head_hromada_communication_num = 
           case_when(
             head_hromada_communication == '2_3_times' ~ 4,
             head_hromada_communication == 'once_a_day' ~ 3,
             head_hromada_communication == 'few_times_a_week' ~ 2,
             head_hromada_communication == 'once_a_week' ~ 1,
             head_hromada_communication == 'none' ~ 0))

d2 <- ds_survey %>%
  select(all_of(comm_channels), head_hromada_communication_num)

cor_mat_head_com <- 
  cor(d2
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, 
    file="./analysis/survey-hromada-analysis/prints/cor_head_com_and_com_channels.png", 
    type = "cairo")

corrplot::corrplot(cor_mat_head_com, tl.col = "black",tl.cex = 1.5, 
                   addCoef.col = "black", number.cex=1.5, cl.pos = 'n')

dev.off()

crossprod(!is.na(d2))
summary(d2)
