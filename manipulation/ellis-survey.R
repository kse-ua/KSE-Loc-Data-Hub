# --- Load libraries ---
if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(tidyr,dplyr, ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(survey)
library(fastDummies)

rm(list = ls())

##--- Input variables ---

# user <-  Sys.info()['user']
# sysname <- Sys.info()['sysname']

# Project specific
# ADAPT (survey-specific)
# project_name = 'CAR_Q3_22'
# 
# form_path = 'GFFO-Cash Barometer - General/Countries/CAR/3. Data Analysis Visualisation/Round 3 (Q2 2022)/Stats/02_data/'
# xls_form = str_c(form_path,"kobo.xlsx")
# 
# data_path = form_path
# survey_data = str_c(data_path,"raw_data_0208_2.xlsx")

# Custom variables
# Define utils
'%ni%' <- Negate(`%in%`)
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "n/a", "NOt available", '<NA>')

##--- Output variables ---



##--- Read data ---

# Xls_form
survey_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "survey")
choices_xls <- readxl::read_excel("./data-private/raw/kobo.xlsx", sheet = "choices")

# Survey data
d0 <- readxl::read_excel("./data-private/raw/Resilience_survey_2022_11_05_eng.xlsx")

#mcq questions from kobofile
mcq<-survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(grepl("select_multiple",type))%>%
  dplyr::select(name)%>%
  pull()


#text response from kobofile



#select and store contact data
contact_data <- d0 %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  select(oblast, raion_text, hromada_text, telegram_link, facebook_link, contact_text)

openxlsx::write.xlsx(contact_data, "./data-private/derived/survey-contact-data.xlsx")

#clean data -------------------------------------------------------------------

##GENERAL CLEANING
d1 <- 
  d0 %>% 
  select(!c(start, end, deviceid, contains("note"), contains("group_"), 
            prep_labels, commun_labels, contains("evacuation_actions"), heat_season_labels,
            contact_text:`_tags`)
        ) %>% 
  filter(hromada_text %ni% c("Тест", "тест")) %>% 
  rename(index = `_index`)
  

#save text variables for coding
text <- survey_xls%>%
  dplyr::select(type,name)%>%
  dplyr::filter(grepl("text",type))%>%
  dplyr::select(name)%>%
  pull()

text_data <- d1 %>% 
  select(index, contains(text)) 

openxlsx::write.xlsx(text_data, "./data-private/derived/survey-text-data.xlsx")



#create indexes for mcq
d1$preparation <- 0
preparation <- d1 %>% select(starts_with("prep_")) %>% colnames()

for(l in preparation){
  d1 <- d1 %>% 
    mutate(
      prep_count = case_when(
        l == "before_24" ~ 2
        ,l == "after_24" ~ 1
        ,l == "not_executed" ~ 0
        ,l == "doesnt_apply" ~ NA_real_
      )
      ,preparation = preparation + prep_count
    )
}


d1$communication_index <- 0



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


#--- Summarise data ---


d <- data %>% 
  select(bars, scq, mcq) %>%
  replace_with_na_all(condition = ~.x == 98) %>%
  replace_with_na_all(condition = ~.x == 99) %>%
  Hmisc::describe()

cat(capture.output(d), file = str_c(cloud_path, output_path, project_name,'_preliminary_results.txt'), sep = '\n')