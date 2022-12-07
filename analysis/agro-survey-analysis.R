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
library(pollster)
library(stargazer)
#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/ellis-agro-survey-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "n/a", "NOt available", '<NA>')
#+ declare-functions -----------------------------------------------------------
'%ni%' <- Negate(`%in%`)


#+ Read data -------------------------------------------------------------------

d0 <- readr::read_csv("data-private/derived/agro-survey-full.csv") 

d1 <- d0 %>% 
  mutate(
    idp_number = ifelse(idp_number == 0, NA, idp_number)
    ,log_income_total_2021 = log(income_total_2021)
    ,log_income_own_2021= log(income_own_2021)
    ,income_total_per_capita = income_total_2021/total_popultaion_2022
    ,income_own_per_capita= income_own_2021/total_popultaion_2022
    ,income_own_pct = income_own_2021/income_total_2021
  )

svy.agro <- svydesign(ids = ~1 , data = d1, weights = ~weight)


# 20 або 21 питання: Кількість незареєстрованих внутрішньо переміщених осіб з 
# - економічними показниками 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ 

#plot % of IDPs out of total population
p1 <- d1 %>% 
  count(type, idp_pct) %>% 
  group_by(type) %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(idp_pct = factor(idp_pct, levels = c(
    "Не прибували, кількість населення в громаді зменшилася"
    ,"До 5% від населення громади"
    ,"5-10% від населення громади"
    ,"11-20% від населення громади"
    ,"Понад 20% від населення громади"
  ))) %>% 
  ggplot(aes(x = type, y = pct, fill = idp_pct)) +
  geom_col() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "% of IDPs",
       fill = NULL) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_brewer(palette="PuBu")

p1 %>% quick_save("1-hromada-preparation", w= 12, h = 7)

#plot relation between IDP number and popularion/total income per capita
ggplot(d1, aes(x = total_popultaion_2022, y = idp_number)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  facet_wrap(~region_en)
  
p2 <- ggplot(d1, aes(x = income_total_per_capita , y = idp_number)) +
  geom_point() +
  geom_smooth(se=F, method = "lm") +
  facet_wrap(~region_en) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "IDPs vs income per capita",
       fill = NULL)

p2 %>% quick_save("2-hromada-preparation", w= 12, h = 7)


cor_mat <- 
  cor(d1 %>% 
        select(area,total_popultaion_2022,urban_pct,n_settlements, idp_number, income_total_per_capita, income_own_per_capita)
      ,use = "complete.obs")

corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1, addCoef.col = "black", number.cex=1, order = "FPC")


m1_idps <-lm(data = d1 %>% filter(!is.na(idp_number)), 
       log(idp_number) ~ log_income_total_2021 + income_own_pct + 
         type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_idps <-lm(data = d1 %>% filter(!is.na(idp_number)), 
             log(idp_number) ~ log_income_total_2021 + income_own_pct + 
               turnout_2020 +
               type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m3_idps <-lm(data = d1 %>% filter(!is.na(idp_number)),
             log(idp_number) ~ log_income_total_2021 + income_own_pct +
               turnout_2020 +
               type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda)

m4_idps <-lm(data = d1 %>% filter(!is.na(idp_number)),
             log(idp_number) ~ log_income_total_2021 + income_own_pct +
               turnout_2020 +
               type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda +
               youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_idps, m2_idps, m3_idps, m4_idps, single.row = T, type = 'html', out = 'idps.html')



# Тривалість часу протягом якої буле відстунє надання адміністартивних послуг з: 
#   - близкість до російського/білоруського кордону 
# - економічні показники 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ 


p3 <- d1 %>% 
  count(admin_services_stopped, admin_services_resumed) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = admin_services_stopped , y = pct, fill = admin_services_resumed)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = 'Suspension and resumption of services', 
       xlab = "Services stopped") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Status of service resumption"
                   ,values = c("steelblue3", "indianred", "palegreen4", "grey"))

p3 %>% quick_save("3-suspension-services", w= 12, h = 7)

summary(d1$admin_services_time)

m1_services <-lm(data = d1, 
                 admin_services_time ~ log_income_total_2021 + income_own_pct + 
                 type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_services <-lm(data = d1,
                 admin_services_time ~ log_income_total_2021 + income_own_pct +
                  turnout_2020 +
                  type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                  sex_head + age_head + education_head + incumbent + rda)

m3_services <-lm(data = d1,
              admin_services_time  ~ log_income_total_2021 + income_own_pct +
               turnout_2020 +
               type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
               sex_head + age_head + education_head + incumbent + rda +
               youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_services, m2_services, m3_services, single.row = T, type = 'html', out = 'services.html')



# Кількість волонтерів, які регулярно працюють в громаді з 
# - економічні показники 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ 
# - вік голови 

d_volunteers <- d1 %>% 
  mutate(
    volunteers_per_capita = volunteers_number/total_popultaion_2022 * 100
  ) %>% 
  filter(hromada_name %ni% c("Боярська міська громада", "Городищенська сільська громада")) 
  
m1_volunteers <-lm(data = d_volunteers, 
                   volunteers_per_capita ~ log_income_total_2021 + income_own_pct + 
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_volunteers <-lm(data = d_volunteers,
                   volunteers_per_capita ~ log_income_total_2021 + income_own_pct +
                   turnout_2020 +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda)

m3_volunteers <-lm(data = d_volunteers,
                   volunteers_per_capita  ~ log_income_total_2021 + income_own_pct +
                   turnout_2020 +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda +
                   youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_volunteers, m2_volunteers, m3_volunteers, single.row = T, type = 'html', out = 'volunteers.html')


# Кількість штабів з
# - кількість волонтерів 
# - вік голови 
# - кількість сільського населення 
# - економічні показники 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ 

p4 <- 
  d_volunteers %>% 
  ggplot(aes(x = volunteers_per_capita, y = humanitarian_hubs)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between volunteers per capita and humanitarian hubs') +
  xlab("% of volunteers out of total population")

p4 %>% quick_save("4-volunteers", w= 12, h = 7)

m1_hubs<-lm(data = d_volunteers, 
                    humanitarian_hubs ~ volunteers_per_capita + log_income_total_2021 + income_own_pct + 
                     type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_hubs <-lm(data = d_volunteers,
                    humanitarian_hubs ~ volunteers_per_capita + log_income_total_2021 + income_own_pct + 
                    turnout_2020 +
                     type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                     sex_head + age_head + education_head + incumbent + rda)

m3_hubs <-lm(data = d_volunteers,
                    humanitarian_hubs ~ volunteers_per_capita + log_income_total_2021 + income_own_pct + 
                    turnout_2020 +
                     type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                     sex_head + age_head + education_head + incumbent + rda +
                     youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_hubs, m2_hubs, m3_hubs, single.row = T, type = 'html', out = 'hubs.html')


# Країни, від громад яких найчастіше отримували допомогу 
# (було б цікаво співставити місце розташування громади в України і її можливості до міжнародної взаємодії, 
# гіпотеза була б чим західніше громада або чи ближче вона до обласного центру, то більше в неї громад закородоном) 

hist(d1$ingo_count)
hist(d1$countries_count)

p5 <- ggplot(d1, aes(x = ingo_count, y = countries_count)) +
  geom_jitter() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between number of countries as aid providers and number of international organizations') 

p5 %>% quick_save("5-countries-ingo", w= 12, h = 7)

p6 <- d1 %>% 
  count(region_en, foreign_aid) %>% 
  group_by(region_en) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = region_en, y = pct, fill = foreign_aid)) +
  geom_col() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Aid from other countries per regions')
  
p6 %>% quick_save("6-countries-regions", w= 12, h = 7)


p7 <- d1 %>% 
  filter(foreign_aid == "Так") %>% 
  select(hromada_name, region_en, countries_australia:countries_japan) %>% 
  pivot_longer(-c(hromada_name, region_en), names_to = "country", values_to = "received") %>% 
  mutate(
    country = str_to_title(str_remove(country, "countries_"))
  ) %>% 
  group_by(country) %>% 
  summarise(n = sum(received), .groups = "drop") %>% 
  filter(n>0)  %>% 
  ggplot(aes(x = n, y = fct_reorder(country, n))) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw() 
  
p7 %>% quick_save("7-countries", w= 12, h = 7)


m1_countries <-lm(data = d1, 
              countries_count ~ log_income_total_2021 + income_own_pct + 
              type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_countries<-lm(data = d1, 
                 countries_count ~ log_income_total_2021 + income_own_pct + 
              type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
              sex_head + age_head + education_head + incumbent + rda)

m3_countries<-lm(data = d1, 
                 countries_count ~ log_income_total_2021 + income_own_pct + 
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda +
                   youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_countries, m2_countries, m3_countries, single.row = T, type = 'html', out = 'countries.html')


# Кількість гуманітарних вантажів було отримано вашою громадою? (орієнтовно, в тоннах)
# - кількість волонтерів 
# - вік голови 
# - кількість сільського населення 
# - економічні показники 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ 

summary(d1$aid_received_volume)

m1_aid_received <-lm(data = d1, 
                  aid_received_volume ~ log_income_total_2021 + income_own_pct + volunteers_number + humanitarian_hubs +
                    type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_aid_received <-lm(data = d1, 
                     aid_received_volume ~ log_income_total_2021 + income_own_pct + volunteers_number + humanitarian_hubs +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda)

m3_aid_received <-lm(data = d1, 
                     aid_received_volume ~ log_income_total_2021 + income_own_pct + volunteers_number + humanitarian_hubs +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda +
                   youth_councils + youth_centers + region_en * sum_osbb_2020)

stargazer(m1_aid_received, m2_aid_received, m3_aid_received, single.row = T, type = 'html', out = 'aid_received.html')


# Кількість релокованих бізнесів до громади 
# - кількість волонтерів 
# - вік голови 
# - кількість сільського населення 
# - економічні показники 
# - turnout на виборах
# - підтримка підприємництва, молодіжні центри, ОСББ

summary(d1$enterprises_relocated)


m1_relocated <-lm(data = d1, 
                  enterprises_relocated ~ log_income_total_2021 + income_own_pct + dfrr_executed +
                    type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary)

m2_relocated <-lm(data = d1, 
                  enterprises_relocated ~ log_income_total_2021 + income_own_pct + dfrr_executed + volunteers_number + humanitarian_hubs +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda)

m3_relocated <-lm(data = d1, 
                  enterprises_relocated ~ log_income_total_2021 + income_own_pct + dfrr_executed + volunteers_number + humanitarian_hubs +
                   type + area + log(total_popultaion_2022) + urban_pct + n_settlements + region_en + occupation + military_action + voluntary +
                   sex_head + age_head + education_head + incumbent + rda +
                   youth_councils + youth_centers + region_en * sum_osbb_2020)


stargazer(m1_relocated, m2_relocated, m3_relocated, single.row = T, type = 'html', out = 'relocated.html')
