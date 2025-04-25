#' ---
#' title: "Ellis Budget Change For Map"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-budget-hatsko.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console
#+ echo=FALSE, results="asis" --------------------------------------------------
cat("Report's native working directory: `", getwd(),"`") # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "ukr")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/prints/general")

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
path_general <- "./data-private/derived/full_dataset.csv"
path_survey <- "./data-private/derived/survey-hromadas-cleaned.csv"

ds_general <- readr::read_csv(path_general)
ds_survey <- readr::read_csv(path_survey)

predictors_all <- 
  ds_general %>% 
  select(-c("hromada_code", "hromada_name","raion_code", "raion_name","oblast_code",
            "oblast_name","hromada_full_name","hromada_center_code","hromada_center",           
            "lat_center","lon_center", "budget_name", "budget_code", "party", "region_code_en")) %>% 
  colnames()


#TO-DO: add data from agro survey


#+ tweak-data-1 ----------------------------------------------------------------



#+ comparison-survey-general ----------------------------------------------------------------
library(tableone)
ds_survey_codes <- 
  ds_survey %>% 
  filter(!is.na(hromada_code)) %>% 
  distinct(hromada_code) %>% 
  pull(hromada_code)

# ds_general <- ds_general %>% 
#   mutate(survey_participant = ifelse(hromada_code %in% ds_survey_codes, "surveyed", "non-surveyed"))
#   
# table_pre <- CreateTableOne(vars=predictors_all, strata = "survey_participant", data=ds_general ,addOverall	= T)
# print(table_pre, smd=T)

ds_surveyed <- 
  ds_general %>% 
  filter(hromada_code %in% ds_survey_codes) %>% 
  mutate(survey_participant = "surveyed" )

ds_comparison <- 
  ds_general  %>% 
  mutate(survey_participant = "all")

#add to all gromadas (including surveyed) surveyed hromadas one more time to make a comparison
ds_comparison <- rbind(ds_surveyed, ds_comparison)

table_pre <- CreateTableOne(vars=predictors_all
                            ,factorVars = c("sex_head", "education_head", "incumbent", "rda")
                            , strata = "survey_participant"
                            , data=ds_comparison)

p <-print(table_pre, smd=T, printToggle = FALSE, noSpaces = TRUE)

kable(p, format = "html")


#+ propensity-score-matching ----------------------------------------------------------------
library(MatchIt)

ds1_general <- 
  ds_general %>% 
  mutate(
    survey_participant = ifelse(hromada_code %in% ds_survey_codes, 1, 0)
  ) %>% 
  filter(occipied_before_2022 != 1)

skimr::skim(ds1_general)


ds_matching <- matchit(survey_participant ~ type + travel_time + n_settlements +
                       area +  total_popultaion_2022 +  
                       urban_popultaion_2022 + urban_pct+ income_total_2021 +income_transfert_2021 +
                        income_military_2021 + income_pdfo_2021 + income_unified_tax_2021 + 
                        income_property_tax_2021 + income_excise_duty_2021 + income_own_2021 +
                        dfrr_executed + turnout_2020 + sex_head + age_head + education_head +
                        incumbent + rda, data = ds1_general, distance ="glm", link = "probit")

summary(ds_matching)
matched <- match.data(ds_matching)

table_matched <- CreateTableOne(vars=xvars, factorVars = fact_xvars, strata = "enrolled", data=matched)
print(table_matched, smd=T)


#+ model-preparation ----------------------------------------------------------------

m1_preparation <- lm(data = d1, prep_count ~ 
                       type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action)

m2_preparation <- lm(data = d1, prep_count ~ 
                       type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action +
                       #financial variables
                       income_total_2021 + income_transfert_2021 + income_military_2021 + 
                       income_pdfo_2021 + income_unified_tax_2021 + income_property_tax_2021 +
                       income_excise_duty_2021 + income_own_2021 + dfrr_executed)

m3_preparation <- lm(data = d1, prep_count ~ 
                       type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action +
                       #financial variables
                       income_total_2021 + income_transfert_2021 + income_military_2021 + 
                       income_pdfo_2021 + income_unified_tax_2021 + income_property_tax_2021 +
                       income_excise_duty_2021 + income_own_2021 + dfrr_executed +
                       #head variables
                       turnout_2020 + sex_head + age_head + education_head + incumbent *                 
                       rda + not_from_here)

m3_preaparation_v2 <- lm(data = d1, prep_count ~ 
                           type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action +
                           #head variables
                           turnout_2020 + sex_head + age_head + education_head + incumbent *               
                           rda + not_from_here)


m4_preparation <- lm(data = d1, prep_count ~ 
                       type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action +
                       #financial variables
                       income_total_2021 + income_transfert_2021 + income_military_2021 + 
                       income_pdfo_2021 + income_unified_tax_2021 + income_property_tax_2021 +
                       income_excise_duty_2021 + income_own_2021 + dfrr_executed +
                       #head variables
                       turnout_2020 + sex_head + age_head + education_head + incumbent +                
                       rda + not_from_here + 
                       #civil society - community competence variables
                       sum_osbb_2020 + edem_total + youth_centers + business_support_centers)



stargazer::stargazer(m1_preparation, m2_preparation, m3_preparation, m3_preaparation_v2, single.row = T, type = "text")

stargazer::stargazer(m3_preaparation_v2, single.row = T, type = "text")




#+ model-military-help ----------------------------------------------------------------

skimr::skim(d1 %>% select(predictors_all))


m1_military <- lm(data = d1, help_military_count ~ 
                       type + log(area) + log(total_popultaion_2022) + urban_pct + n_settlements + occupation + military_action)

m2_military <- lm(data = d1, help_military_count ~ 
                       type + log(area) + log(total_popultaion_2022) + urban_pct + n_settlements + occupation + military_action +
                       #financial variables
                       log(income_total_2021) + log(income_transfert_2021) + income_military_2021 + 
                       log(income_pdfo_2021) + log(income_unified_tax_2021) + log(income_property_tax_2021) +
                      log(income_excise_duty_2021) + log(income_own_2021) + dfrr_executed)

m3_military <- lm(data = d1, help_military_count ~ 
                       type + log(area) + log(total_popultaion_2022) + urban_pct + n_settlements + occupation + military_action +
                       #financial variables
                       log(income_total_2021) + log(income_transfert_2021) + income_military_2021 + 
                       log(income_pdfo_2021) + log(income_unified_tax_2021) + log(income_property_tax_2021) +
                       log(income_excise_duty_2021) + log(income_own_2021) + dfrr_executed +
                       #head variables
                       turnout_2020 + sex_head + age_head + education_head + incumbent *                 
                       rda + not_from_here)

m3_military_v2 <- lm(data = d1, prep_count ~ 
                           type + area + total_popultaion_2022 + urban_pct + n_settlements + occupation + military_action +
                           #head variables
                           turnout_2020 + sex_head + age_head + education_head + incumbent *               
                           rda + not_from_here)

stargazer::stargazer(m1_military, m2_military, m3_military, single.row = T, type = "text")


#+ model-Maryna ----------------------------------------------------------------


library(osmdata)
library(ggmap)
library(geosphere)
library(geojsonsf)

path_polygons <-  "/Users/serhii/Desktop/russia_border.geojson"

russia_border <- st_read(path_polygons)
russia_border_matrix <- st_as_sf(russia_border)

a <-  geojsonsf::geojson_sf(path_polygons)


center_matrix <- 
  as.matrix(
    d1 %>% select(lon_center, lat_center)
  )

distances <- dist2Line(p = center_matrix, russia_border_matrix, distfun=distGeo)

# russia_bb <- getbb("Russia", featuretype = "country")
# 
# russia_border <- russia_bb %>%
#   opq() %>%
#   add_osm_feature(key = "name:en", value = "Russia — Ukraine") %>%
#   osmdata_sf()
# 
# ?add_osm_feature
# 
# russia_border2 <- st_as_sf(russia_border)
# 
# mad_map <- get_map(getbb("Russia", featuretype = "country"), maptype = "toner-lite")
# 
# ggmap(mad_map)+
#   geom_sf(data = russia_border$osm_lines,
#           inherit.aes = FALSE)

#+ Maryna-hypotheses---------
# Щодо гіпотез - можна перевірити чи впливає на різні аспекти готовності громад 
# близькість до раніше окупованих територій. Також - на скільки економічний стан 
# (всі ці податкові показники) вплинув на інші три компоненти, зокрема, 
# на інфраструктуру та social cohesion



#+ Myroslava-hypotheses----------

d_cleaned <- readxl::read_excel("./data-private/derived/survey-hromadas-cleaned-coded.xlsx")

d1 <- 
  d_cleaned %>% 
  distinct(hromada_code, .keep_all = T) %>% 
  left_join(
    ds_general
    ,by = "hromada_code"
  ) %>%
  filter(!is.na(type)) %>% 
  mutate(
    dfrr_executed = replace(dfrr_executed, is.na(dfrr_executed), 0)
    ,russia_border = case_when(
      oblast %in% c("Chernigiv", "Kharkiv", "Kherson", "Sumska", "Donetska", "Luhanska") ~ 1
      ,TRUE ~ 0
    )
    ,percent_working_march = as.numeric(percent_working_march)
    ,percent_working_now = as.numeric(percent_working_now)
    ,telegram_before_war = case_when(telegram == 2 ~ 1, TRUE ~ 0)
    ,viber_before_war = case_when(viber == 2 ~ 1, TRUE ~ 0)
    ,facebook_before_war = case_when(facebook == 2 ~ 1, TRUE ~ 0)
    ,socmedia_before_war = rowSums(across(telegram_before_war:facebook_before_war))
    ,head_hromada_communication = case_when(
      head_hromada_communication == "none" ~ 0
      ,head_hromada_communication == "once_a_week" ~ 1
      ,head_hromada_communication == "few_times_a_week" ~ 2
      ,head_hromada_communication == "once_a_day" ~ 3
      ,head_hromada_communication == "2_3_times" ~ 4
    )
    ,transport_help_communal = as.numeric(transport_help_communal)
    ,transport_help_bought = as.numeric(transport_help_bought)
    ,income_military_per_capita = income_military_2021/total_popultaion_2022
    ,income_military_per_capita = case_when(
      income_military_per_capita == 0 ~ 1
      ,TRUE ~ income_military_per_capita
    )
    ,osbb_per_1000 = case_when(
      urban_pct > 0 ~ sum_osbb_2020/(total_popultaion_2022/1000)
      ,TRUE ~ NA_real_
    )
    # ,military_base = ifelse(income_military_2021 > 1000, 1, 0)
  )

# "Індекс підготовки" перевірити з
# 1. добровільним/недобровільним об'єднанням 
# 2. + до того, що писала Марина - близкість до кордонів з РФ 
# 3 (?) частка працівників, які лишилися працювати на 1 березня та сьогоднішній день (чим краще готові, тим більше лишилося працювати)


m5_preparation <- lm(data = d1, prep_count ~ 
                       voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m6_preparation <- lm(data = d1, prep_count ~ 
                       voluntary + russia_border + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m7_preparation <- lm(data = d1, prep_count ~ 
                       voluntary + russia_border + type + area + log(total_popultaion_2022) + urban_pct + n_settlements +
                       sex_head + age_head + education_head + incumbent)

stargazer::stargazer(m5_preparation, m6_preparation, m7_preparation, single.row = T, type = 'html', out = 'preparation.html')

#do not use percentages of working due to the low variability
mean(d1$percent_working_march)
mean(d1$percent_working_now)

plot(m5_preparation, 2)
shapiro.test(rstandard(m5_preparation))

plot(m6_preparation, 2)
shapiro.test(rstandard(m6_preparation))

# Frequency of head of hromada communication перевірити з
# 1. наявністю Viber, FB, TG до вторгнення 
# 2. близкість до кордону з рф чи білоруссю 
# 3. кількість населення (чим менше, тим менше можливо треба онлайн щось комунікувати) 
# 4. сільська/селищна/міська громада або кількість сільського населення

m1_comms <- lm(data = d1, head_hromada_communication ~ 
                 telegram_before_war + viber_before_war + facebook_before_war +
                 voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m2_comms <- lm(data = d1, head_hromada_communication ~ 
                 russia_border +
                 telegram_before_war + viber_before_war + facebook_before_war +
                 voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m3_comms <- lm(data = d1, head_hromada_communication ~ 
                 russia_border +
                 telegram_before_war + viber_before_war + facebook_before_war +
                 voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements +
                 sex_head + age_head + education_head + incumbent)

stargazer::stargazer(m1_comms, m2_comms, m3_comms, single.row = T, type = 'html', out = 'comms.html')


# Кількість придбаних машин громадою (більше 10) перевірити з: - ЗАМІНИЛИ НА ІНДЕКС ДОПОМОГИ ВІЙСЬКОВИМ
# 1. наявністю військових частин на території
# 2. близкістю до лінії бойових дій 
# 3. наявністю цих рад підприємництва, молодіжних центрів, органів підтримки підприємництва
# 4. кількістю осбб на 1 тис. тощо

hist(d1$transport_help_bought)
hist(d1$income_military_per_capita)
hist(d1$osbb_per_1000)

m4_military <- lm(data = d1, help_military_count ~ 
                   log(income_military_per_capita) +
                   russia_border +
                   voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m5_military <- lm(data = d1, help_military_count ~ 
                   log(income_military_per_capita) +
                   russia_border + 
                   youth_councils + youth_centers + business_support_centers +
                   voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m6_military <- lm(data = d1, help_military_count ~ 
                    log(income_military_per_capita) +
                    russia_border +
                    youth_councils + youth_centers + business_support_centers +
                    voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements +
                    sex_head + age_head + education_head + incumbent)

m7_military <- lm(data = d1, help_military_count ~ 
                    log(income_military_per_capita) +
                    russia_border +
                    youth_councils + youth_centers + business_support_centers +
                    osbb_per_1000 +
                    voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements)

m8_military <- lm(data = d1, help_military_count ~ 
                    log(income_military_per_capita) +
                    russia_border +
                    youth_councils + youth_centers + business_support_centers +
                    osbb_per_1000 +
                    voluntary + type + area + log(total_popultaion_2022) + urban_pct + n_settlements +
                    sex_head + age_head + education_head + incumbent)

stargazer::stargazer(m4_military, m5_military, m6_military, m7_military, m8_military, single.row = T, type = 'html', out = 'military.html')



#+ results="asis", echo=F ------------------------------------------------------
cat("\n# A. Session Information{#session-info}")
#+ results="show", echo=F ------------------------------------------------------
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)

