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

#+ VISUALIZATIONS

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

#+ plot for state communication ------------------------------------------------

p1 <- d3 %>% count(state_communication) %>% mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = state_communication, y = freq)) +
  geom_col() +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Was there communication for preparation from the government?') +
  scale_y_continuous(labels = scales::percent)

p1 %>% quick_save("1-gov-communication", w= 12, h = 7)

#+ plot for preparation --------------------------------------------------------

g1 <- d2 %>% select(hromada_text, preparation) %>% 
  mutate(across(everything(.), as_factor)) %>% 
  pivot_longer(-hromada_text, names_to = 'preparation', values_to = 'degree') %>% 
  mutate(preparation = str_remove(preparation, 'prep_'),
         degree = case_when(degree == 0 ~ 'none',
                            degree == 1 ~ 'after 24',
                            degree == 2 ~ 'before 24'),
         degree = factor(degree, levels = c('none', 'after 24', 'before 24')))

g2 <- g1 %>% count(preparation, degree) %>% group_by(preparation) %>% 
  mutate(freq = n/sum(n),
         preparation = factor(preparation, levels = prep_levels)) %>%
  arrange(preparation, degree)

prep_levels <- c('first_aid_water', 'first_aid_fuel', 'reaction_plan', 'evacuation_plan',
                 'reaction_plan_oth_hromadas', 'reaction_plan_oda', 'dftg_creation',
                 'national_resistance', 'starosta_meeting', 'communal_meetiing',
                 'online_map', 'shelter_list', 'notification_check', 'backup', 
                 'partly_backup')

p1 <- ggplot(g2, aes(x = degree, y = freq, 
                     group = preparation, 
                     fill = degree)) +
  geom_col() +
  geom_text(aes(label = scales::percent(freq, accuracy = 1), vjust = -0.5)) +
  facet_wrap(~ preparation) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Hromadas preparation for invasion",
       fill = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
p1

p1 %>% quick_save("1-hromada-preparation", w= 12, h = 7)


hist(d2$prep_count)

#+ plot for head of hromada communication --------------------------------------

g1 <- d2 %>% select(hromada_text, comm_channels) %>% mutate(across(everything(.), as_factor)) %>% 
  pivot_longer(-hromada_text, names_to = 'channels', values_to = 'degree') %>%
  mutate(degree = case_when(degree == 0 ~ 'none',
                            degree == 1 ~ 'after 24',
                            degree == 2 ~ 'before 24'),
         degree = factor(degree, levels = c('none', 'after 24', 'before 24')))

g2 <- g1 %>% count(channels, degree) %>% group_by(channels) %>% 
  mutate(freq = n/sum(n),
         channels = factor(channels, levels = comm_channels)) %>%
  arrange(channels, degree)

p2 <- ggplot(g2, aes(x = degree, y = freq, group = channels, fill = degree)) +
  geom_col() +
  geom_text(aes(label = scales::percent(freq, accuracy = 1), vjust = -0.5)) +
  facet_wrap(~ channels) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Communication channels of hromadas',
       fill = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
p2

p2 %>% quick_save("2-hromada-communication", w= 12, h = 7)

head_hromada_communication_levels <- c('2_3_times', 'once_a_day', 'few_times_a_week',
                                       'once_a_week', 'none')

p3 <- d3 %>% count(head_hromada_communication) %>% mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(head_hromada_communication, levels = head_hromada_communication_levels), y = freq)) +
  geom_col() +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Frequency of head of hromada communication')+
  scale_y_continuous(labels = scales::percent)

p3 %>% quick_save("3-hromada-head-communication", w= 12, h = 7)


#+ national resistance

#+ dftg creation

dftg_levels <- c('yes', 'not_able', 'still_not')

p3 <- d3 %>% count(dftg_creation) %>% mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(dftg_creation, levels = dftg_levels), y = freq)) +
  geom_col() +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'How many hromadas created a DFTG?')+
  scale_y_continuous(labels = scales::percent)

p3 %>% quick_save("3-dftg-creation", w= 12, h = 7)

#+ dftg creation time

d4 <- d3 %>% select(idp_registration_time,dftg_creation_time) %>% 
  filter(dftg_creation_time>=0 & idp_registration_time>=0) %>%
  mutate(dftg_creation_time = as.numeric(dftg_creation_time),
         idp_registration_time = as.numeric(idp_registration_time))

sum(is.na(d3$idp_registration_date))
sum(is.na(d3$dftg_creation_date))

#+ correlation of dftg creation and idp registration data ----------------------
cor(d4)

p1 <- d4 %>% 
  ggplot(aes(x=idp_registration_time, y=dftg_creation_time))+
  geom_point()

p1 %>% quick_save("4-dftg-idp-correlation", w= 12, h = 7)

#+ date of dftg creation --------------------------------------------------------
g1 <- d3 %>% select(dftg_creation_date) %>% group_by(dftg_creation_date) %>% 
  summarise(n = n()) %>%
  filter(!is.na(dftg_creation_date) & dftg_creation_date > '2020-01-01') %>%
  mutate(cum = cumsum(n))

first_month <- lubridate::interval(as.POSIXct("2022-02-24"),
                                   as.POSIXct("2022-03-24"))

g1 %>% filter(dftg_creation_date %within% first_month) %>% summarise(sum = sum(n))

p3 <- g1 %>%
  ggplot(aes(x = dftg_creation_date, y = cum)) +
  # geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.02) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_datetime(date_breaks = '2 month') +
  annotate(geom = 'label', x = as.POSIXct('2022-01-03'), y = 60, 
           label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-01-05'), y = 90, 
           label = '55 DFTGs created \nin a first month', fontface = 'italic') +
  labs(title = 'Number of DFTG created by hromadas')
p3 

p3 %>% quick_save("3-dftg-creation-date-alternativ", w= 12, h = 7)

#+ date of idp creation --------------------------------------------------------

g1 <- d3 %>% select(idp_registration_date) %>% group_by(idp_registration_date) %>% 
  summarise(n = n()) %>%
  filter(!is.na(idp_registration_date) & idp_registration_date > '2022-02-23') %>%
  mutate(cum = cumsum(n))

first_month <- lubridate::interval(as.POSIXct("2022-02-24"),
                                   as.POSIXct("2022-03-24"))

g1 %>% filter(dftg_creation_date %within% first_month) %>% summarise(sum = sum(n))

p3 <- g1 %>%
  ggplot(aes(x = idp_registration_date, y = cum)) +
  # geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  theme_bw() +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.02) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_datetime(date_breaks = '2 weeks') +
  annotate(geom = 'label', x = as.POSIXct('2022-03-03'), y = 100, 
           label = 'Full-scale \nrussian invasion') +
  labs(title = 'Number of hromadas that started registering IDPs')
p3 

p3 %>% quick_save("3-idp-registration-date", w= 12, h = 7)

#+ help for military ------------------------------------------------------------

help_military_levels <- c('rooms', 'transport', 'money', 'products', 'other')

g1 <- d3 %>% select(hromada_text, starts_with('help_for_military/')) %>% 
  pivot_longer(-hromada_text, names_to = 'help', values_to = 'count') %>%
  count(help, count) %>% group_by(help) %>% 
  mutate(freq = n/sum(n),
         help = str_remove(help, 'help_for_military/')) %>%
  filter(count == 1)

p3 <- g1 %>% 
  ggplot(aes(x = factor(help, levels = help_military_levels), y = freq)) +
  geom_col() +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Provided assistance for military') +
  scale_y_continuous(labels = scales::percent)
p3

p3 %>% quick_save("3-help-for-military", w= 12, h = 7)

#+ administrative adaptation ---------------------------------------------------

com_hromadas_levels <- c('Daily', 'Several times a week', 'Several times a month',
                         'Once a month and less', 'No meetings/calls')

p4 <- d3 %>% select(commun_between_hromadas) %>% count(commun_between_hromadas) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(commun_between_hromadas, levels = com_hromadas_levels), y = freq)) +
  geom_col() +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Meetings/calls with other hromadas in first 3 months')+
  scale_y_continuous(labels = scales::percent)
p4

p4 %>% quick_save("4-meetings-other-hromadas", w= 12, h = 7)

#+ IDP correlation -------------------------------------------------------------

d4 <- d3 %>% 
  select(starts_with('idp'), income_tot_per_capita, income_total, total_population_2022, ends_with('prop')) %>%
  mutate(idp_registration_share = idp_registration_number / total_population_2022,
         idp_real_share = idp_real_number / total_population_2022,
         idp_child_share = idp_child_education / idp_registration_number) %>%
  filter(!is.na(idp_accept))

d4_cor <- d4 %>% select(-c(idp_accept, idp_registration_date, idp_help,
                           starts_with('idp_help/'), idp_room_number, idp_place_rooms,
                           idp_child_education, idp_child_share,
                           idp_real_number, idp_real_share, idp_registration_date, idp_registration_time))

# plot
cor_mat_idp <- 
  cor(d4_cor
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, file="./analysis/prints/cor_idp.png", type = "cairo")

corrplot::corrplot(cor_mat_idp, tl.col = "black",tl.cex = 1.5, addCoef.col = "black", number.cex=1.5, order = "FPC")

dev.off()


hist(d4$idp_registration_share)
hist(d4$idp_real_share)
hist(d4$idp_child_share)

d4_numbers <- d4 %>% select(ends_with('number')) %>%
  select(-c('idp_room_number')) %>% 
  summarise(across(everything(), list(sum = sum, mean = mean), .names = '{.col}_{.fn}', na.rm = TRUE))

d4_numbers %>% select(ends_with('sum')) %>% 
  t() %>%
  ggplot()+
  geom_col(aes(x=help, y=sum))

d4 %>% ggplot()+
  geom_histogram(aes(x=idp_real_number), fill = 'tomato1', position = 'identity', bins = 40)+
  geom_histogram(aes(x=idp_registration_number), fill = 'pink', alpha = 0.5, 
                 position = 'identity', bins = 40)

#+ population data -------------------------------------------------------------

d4 <- d3 %>% 
  select(hromada_name, hromada_name_right, raion_text, raion_name, oblast, oblast_name, 
         population_text,total_population_2022) %>%
  filter(hromada_name_right %ni% c('Дзвиняцька сільська громада'))

openxlsx::write.xlsx(d4, "./data-private/derived/survey-population-data.xlsx")