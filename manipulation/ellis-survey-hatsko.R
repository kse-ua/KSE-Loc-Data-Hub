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
prints_folder <- paste0("./manipulation/ellis-survey-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "n/a", "NOt available", '<NA>')
#+ declare-functions -----------------------------------------------------------
'%ni%' <- Negate(`%in%`)

##--- Output variables ---



##--- Read data ---

# Xls_form
survey_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
choices_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "choices")

# Survey data
d0 <- readxl::read_excel("./data-private/raw/Resilience_survey_2022_11_05_eng.xlsx")

ds_population <- readr::read_csv("./data-private/derived/ua-pop-2022.csv")

ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv") %>% 
  mutate(
    key = paste(hromada_name, type, "громада")
  )


ds_budget <- readxl::read_xlsx("./data-public/derived/hromada_budget_2020_2022.xlsx") 


ds_1 <- ds_budget %>% 
  filter(year == "2021") %>% 
  left_join(
    ds_population %>% select(hromada_code, total_population_2022)
    ,by = "hromada_code"
  ) %>% 
  mutate(
    income_tot_per_capita = income_total/total_population_2022
    ,income_own_per_capita = income_own/total_population_2022
  )




# #mcq questions from kobofile
#TO-DO: evacuation actions
mcq<-survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(str_detect(type, "select_multiple"))%>%
  dplyr::select(name)%>%
  pull()

oblasts <- readr::read_csv("./data-private/raw/oblast.csv") %>% 
  mutate(
    oblast_name_en = case_when(
      oblast_name_en == "Driproptrovska" ~ "Dnipropetrovska"
      ,oblast_name_en == "Ivano-Frankivsk" ~ "Ivano_Frankivsk"
      ,oblast_name_en == "Kyiv-oblast" ~ "Kyivska"
      ,oblast_name_en == "Vonyn" ~ "Volyn"
      ,TRUE ~ oblast_name_en
    )
  )


#select and store contact data
contact_data <- d0 %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  select(oblast, raion_text, hromada_text, telegram_link, facebook_link, contact_text)

openxlsx::write.xlsx(contact_data, "./data-private/derived/survey-contact-data.xlsx")



#---- clean data -------------------------------------------------------------------

##GENERAL CLEANING
d1 <- 
  d0 %>% 
  select(!c(start, end, deviceid, contains("note"), contains("group_"), 
            prep_labels, commun_labels, contains("evacuation_actions"), heat_season_labels,
            # contact_text:`_tags`
  )
  ) %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  rename(index = `_index`) %>% 
  left_join(
    oblasts %>% select(oblast_name_en, region_en)
    ,by = c("oblast"="oblast_name_en")
  )


#save text variables for coding
text <- survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(grepl("text",type))%>%
  dplyr::select(name)%>%
  pull()

text_data <- d1 %>% 
  select(index, contains(text)) 

openxlsx::write.xlsx(text_data, "./data-private/derived/survey-text-data.xlsx")



#create counters for mcq
preparation <- d1 %>% select(starts_with("prep_")) %>% colnames()
comm_channels <- d1 %>% select(telegram:hotline) %>% colnames()


d2 <- d1 %>% 
  mutate_at(
    vars(all_of(preparation)), ~case_when(
      . == "before_24" ~ 2
      ,. == "after_24" ~ 1
      ,. == "not_executed" ~ 0
      ,. == "doesnt_apply" ~ NA_real_
    ) 
  ) %>% 
  mutate_at(   
    vars(all_of(comm_channels)), ~case_when(
      . == "before_24" ~ 2
      ,. == "after_24" ~ 1
      ,. == "none" ~ 0
    )
  ) %>% 
  mutate(
    prep_count= rowSums(across(preparation), na.rm = T)
    ,comm_channels_count = rowSums(across(comm_channels), na.rm = T)
    ,help_military_count = rowSums(across("help_for_military/rooms":"help_for_military/other"), na.rm = T)
    ,idp_help_count = rowSums(across("idp_help/communal_placement":"idp_help/transit_center"), na.rm = T)
    ,dftg_creation_time = difftime(dftg_creation_date, "2022-02-24", unit = "day") #negative values - choose another date
    ,commun_between_hromadas = case_when(commun_between_hromadas == '__' ~ 'Daily',
                                         commun_between_hromadas == '______' ~ 'Several times a week',
                                         commun_between_hromadas == '_______1' ~ 'Several times a month',
                                         commun_between_hromadas == '________' ~ 'Once a month and less',
                                         commun_between_hromadas == '____' ~ 'No meetings/calls'))


#SPEARMEN RANK CORRELATION FOR Q on preparations (between 14 items + total score + financial metrics)
# ?financial metrics which represent preparation - income per capita, level of own income

coded_hromada_names <- readxl::read_excel("./data-private/derived/survey-contact-data-coded.xlsx")

d3 <- 
  d2 %>% 
  left_join(
    coded_hromada_names %>% select("_id", hromada_name_right, key )
    , by = "_id"
  ) %>% 
  left_join(
    ds_hromada %>% select(key, hromada_code)
    ,by = c("hromada_name_right"="key")
  ) %>% 
  left_join(
    ds_1 %>% select(hromada_code, income_tot_per_capita, income_own_per_capita)
    ,by = "hromada_code"
  )

cor_mat <- 
  cor(d3 %>% select(all_of(preparation), prep_count)
      ,use = "complete.obs"
      ,method = "spearman")

png(height=1800, width=1800, file="./analysis/prints/cor_preparation.png", type = "cairo")

corrplot::corrplot(cor_mat, tl.col = "black",tl.cex = 1.5, addCoef.col = "black", number.cex=1.5, order = "FPC")

dev.off()

#+ --------model for preparation and income before war --------------------------
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

#function for tables 

for (i in preparation) {
  b <- knitr::kable(d2 %>% count(.data[[i]]) %>% mutate(pct = round(n/sum(n)*100, 1)))
  print(b)
}

# plot for state communication

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
  
# plot for preparation
g1 <- d2 %>% select(hromada_text, preparation) %>% mutate(across(everything(.), as_factor)) %>% 
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

# Communication

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


# national resistance

# dftg creation

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

# dftg creation time

d3 %>% select(dftg_creation_date,dftg_creation_time) %>% filter(dftg_creation_time<0)

plot(d3$dftg_creation_time)

# date of dftg creation
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
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.02) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_datetime(date_breaks = '2 month') +
  annotate(geom = 'label', x = as.POSIXct('2022-01-03'), y = 60, label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-01-05'), y = 90, label = '55 DFTGs created \nin a first month', fontface = 'italic') +
  labs(title = 'Number of DFTG created by hromadas')
p3 

p3 %>% quick_save("3-dftg-creation-date-alternativ", w= 12, h = 7)

# help for military

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

# administrative adaptation

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

#COMPARISON OF SURVEYED HROMADAS WITH GENERAL POPULATION OF HROMADAS


#---- transformations-for-analysis-------------------------------------------------------------------

d3 <- 
  d2 %>% 
  mutate(
    shelter_capacity_changes <- shelter_capacity_before_text/shelter_capacity_now_text 
  )


# columns <- d1 %>%   
#   select(-c(note_0D2z0fonE:`_tags`)) %>%
#   colnames() %>% 
#   as_tibble() %>% 
#   mutate(value = str_remove(value, "\\.\\.\\.\\d*")) %>% 
#   left_join(
#     survey_xls %>% select(`label::Ukrainian (uk)`, name, type)
#     ,by = c("value"="label::Ukrainian (uk)")
#   ) %>% 
#   mutate(
#     ua_category = case_when(
#       str_detect(value, "(?<=проблеми|\\?)\\/") ~ str_extract(value, "(?<=\\/).+")
#       ,TRUE ~ value
#     )
#   ) %>% 
#   filter(value != "Який з перелічених нижче засобів використовується громадою для інформування населення" | 
#            name != "note_OO3UvM3QM") %>% 
#   fill(name)
# 
# 
# survey_xls %>% 
#   filter(str_detect(type, "select_multiple")) %>% 
#   select(type) %>% 
#   mutate(type = str_remove(type, "select_multiple ")) %>% 
#   left_join(
#     choices_xls %>% select(list_name, name, `label::Ukrainian (uk)`)
#     ,by = c("type"="list_name")
#   ) %>% View()





#--------------------------------------------rename MCQ columns (Survey CTO only)-----------------

# survey_xls %>% filter()
# 
# temp <- data %>% dplyr::select(any_of(contains(mcq)), KEY) %>%
#   mutate_all(function(x) as.character(x))%>%
#   pivot_longer(!KEY, names_to="question", values_to="response") %>%
#   select(-KEY, -response) %>% 
#   distinct() %>% 
#   filter(question %ni% mcq & !str_detect(question, "_text$") & question %ni% text)
# 
# 
# for(i in mcq[order(-nchar(mcq), mcq)]){
#   print(i)
#   temp$question<- if_else(str_detect(temp$question, i) & !str_detect(temp$question, "/"), str_replace(temp$question, paste(i, '_', sep = ""), paste(i, '/', sep = "")), temp$question)
# }
# 
# temp$original <- str_replace(temp$question, "/", "_")
# 
# colnames(data) <- dplyr::recode(
#   colnames(data), 
#   !!!setNames(as.character(temp$question), temp$original)
# )



#Make dummy variables for SCQ questions and change their names (/ instead of _ as a separator)


data <- dummy_cols(data, select_columns = scq) 

temp <- data %>% dplyr::select(any_of(contains(scq)), `_uuid`) %>% 
  mutate_all(function(x) as.character(x))%>%
  pivot_longer(!`_uuid`, names_to="question", values_to="response") %>%
  dplyr::select(-`_uuid`, -response) %>% 
  distinct() %>% 
  filter(question %ni% scq & !str_detect(question, "_text$") & question %ni% text)

for(i in scq[order(-nchar(scq), scq)]){
  print(i)
  temp$question<- if_else(
    str_detect(temp$question, i) & !str_detect(temp$question, "/")
    , str_replace(temp$question, paste(i, '_', sep = ""), paste(i, '/', sep = ""))
    , temp$question
  )
}

temp$original <- str_replace(temp$question, "/", "_")

colnames(data) <- dplyr::recode(
  colnames(data), 
  !!!setNames(as.character(temp$question), temp$original)
)

#PROJECT SPECIFIC CLEANING----------------------------------------------------------

#use combined likert question calculated in kobo and remove partial questions

# likert_full <- data %>% select(ends_with('_2')) %>% colnames()
# 
# likert_other <- survey_xls %>%
#   dplyr::select(type,name)%>%
#   dplyr::filter(grepl("select_one likert",type))%>%
#   dplyr::select(name)%>%
#   pull()
# 
# data <- select(data, -likert_other)
# 
# names(data) <- gsub(pattern = "_2.*", replacement = "", x = names(data))


#remove responses for participation and transparency (importance and reality) questions

data$importance_participation <- ifelse(data$admin2 %in% c('paoua', 'kaga_bandoro'), NA, data$importance_participation)
data$reality_participation <- ifelse(data$admin2 %in% c('paoua', 'kaga_bandoro'), NA, data$reality_participation)

data$importance_transparency <- ifelse(data$admin2 %in% c('paoua', 'kaga_bandoro'), NA, data$importance_transparency)
data$reality_transparency <- ifelse(data$admin2 %in% c('paoua', 'kaga_bandoro'), NA, data$reality_transparency)



#--- Write Clean Data in output file ---

write_xlsx(data, "Data_clean_test.xlsx")

#TODO (AgTa): I don't understand what the following block tries to achieve
# Anyway not used in the OUTPUT code
#####################################################
# #save to database folder
# 
# db_path = paste0("C:\\Users\\",Sys.getenv("USERNAME"), "\\Ground Truth Solutions\\Stats - General\\Database\\07_new_projects\\",project)
# # AT 04.08.2022: shoudl create a folder and put the stuff in the folder
# 
# 
# 
# # AT: first annoying point Mac/Win: there are some differences in how the addresses are written. This also works using the write_list function I wrote 
# # At: but it is necessary to feed it the right path
# 
# ifelse(!dir.exists(file.path(db_path)), dir.create(file.path(db_path)), FALSE)
# 
# require(openxlsx)
# list_of_datasets <- list("data" = data, "survey" = survey_xls, 'choices' = survey_xls)
# write.xlsx(list_of_datasets, file = paste(db_path, ".xlsx", sep = '')) #TODO: this isn't working yet/is incredibly slow
# 
# 
# # AT: done, the solution is not the best because of the for loop but XLCoonect requires rJava --- > possible conflicting with Mac/windows combi
# 
# 
# # AT: we could consider moving the functions outside and sourcing them in the code using "source(filename.R)"
# 
# write_list <-function(list, name) {    
#   wb <- openxlsx::createWorkbook(wb_name)
#   for (i in 1:length(names(list_of_datasets))){
#   addWorksheet(wb, names(list_of_datasets)[i])
#   writeData(wb, sheet = i, list_of_datasets[[i]])
#   }
#   saveWorkbook(wb, file = paste0('./02_data/',wb_name), overwrite = TRUE)
# }
# 
#  
# wb_name =  paste0(project, ".xlsx") # this can be easily personalised
# write_list(list_of_datasets,wb_name)

