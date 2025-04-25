#' ---
#' title: "Ellis UA Admin"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-ua-admin.R") # run to knit, don't uncomment
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
#source: https://drive.google.com/file/d/1JLICa97syJPyGOvt-8zx8PuALzPzZzR1/view?usp=sharing
path_declarations<- "./data-public/raw/active_declarations_by_age_gender.csv"

#source: https://drive.google.com/file/d/1sJz53HVMEZlD3gSurkU0kd0-dwMDacGG/view?usp=sharing
path_doctors <- "./data-public/raw/hospitals-doctors.csv"

path_admin <- "./data-public/derived/ua-admin-map-2020.csv"

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_declarations, locale = locale(encoding = "UTF-8"))
ds_admin <- readr::read_csv(path_admin) %>% 
  #in koatuu (old codes), there were 4 different codes for the different parts of one settlements in Zaporizka oblast
  #in ds_admin, we have only one code for all of them, so let's change the codes in accordance with ds0
  mutate(
    settlement_code_old = case_when(
      grepl("село Велика Білозерка-3", full_name) ~ "2321187401"
      ,grepl("село Велика Білозерка-4", full_name) ~ "2321188401"
      ,TRUE ~ settlement_code_old
    )
  ) %>% 
  #2nd part of the settlements is absent in the health data, so let's exclude it in order to not duplicate rows during merging
  filter(!grepl("село Велика Білозерка-2", full_name)) %>% 
  #also exclude village Tsenzhiv which was created in 2021 and has the same old code as other village Maidan
  filter(settlement_code != "UA26040390080042180")

#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------

#merge the dataset of declarations with admin data by old koatuu codes (we need to add hromada codes and names)
ds1 <- ds0 %>% 
  left_join(
    ds_admin %>% select(settlement_code_old, settlement_code, hromada_code, hromada_name)
    ,by = c("settlement_koatuu" = "settlement_code_old")
  ) %>% 
  mutate(
    settlement = str_to_title(settlement)
    ,settlement = gsub("'", "’", settlement)
    ,area = str_to_title(area)
    ,key = paste(area, settlement)
  )

ds1 %>% filter(is.na(hromada_code)) %>% distinct(settlement_koatuu) #668 settlement in the health data have invalid codes


#create vectors of unique combination oblast-settlement - we will use these unique combinations 
#as a key variable to merge these 670 settlements with admin data
v_admin_unique_names <-  ds_admin %>% 
  select(hromada_code, settlement_code, settlement_name, oblast_name) %>% 
  mutate(key = paste(oblast_name, settlement_name)) %>% 
  count(key) %>% 
  filter(n == 1) %>% 
  pull(key)
  

#select settlements with old koatuu which don't have unique combination oblast-settlement for manual coding
ds_coding <- ds1 %>% 
  left_join(
    ds_admin %>% 
      mutate(key = paste(oblast_name, settlement_name)) %>% 
      filter(key %in% v_admin_unique_names) %>% 
      select(key, hromada_code, hromada_name, settlement_code, settlement_name)
    ,by = "key"
  ) %>% 
  filter(is.na(settlement_code.x) & is.na(settlement_code.y)) %>% 
  # filter(settlement_code.x != "UA26040390080042180") %>% #village Tsenzhiv which was created in 2021
  distinct(area, settlement,settlement_koatuu) 

#we receive the dataset with 165 settlements - save the file with them for manual coding (DONE)
# openxlsx::write.xlsx(ds_coding, "./data-private/raw/old-koatuu-coding-raw.xlsx") - DO NOT UNCOMMENT

#+ load-coded-data, eval=eval_chunks -------------------------------------------------
ds_koatuu <- 
  readxl::read_excel("./data-public/raw/old-koatuu-coding.xlsx") %>% 
  left_join(
    ds_admin %>% select(hromada_code, hromada_name, settlement_code)
    ,by = "settlement_code"
  ) %>% 
  mutate(
    hromada_code = ifelse(settlement_koatuu == "8000000000","UA80000000000093317", hromada_code)
    ,hromada_name = ifelse(settlement_koatuu == "8000000000","Київ", hromada_name)
  ) #edit Kyiv code and name

#+ merge-all-data, eval=eval_chunks -------------------------------------------------
ds2 <- ds1 %>% 
  #at first, merge the health data with admin data using unique oblast-settlement combinations as a key variable
  left_join(
    ds_admin %>% 
      mutate(key = paste(oblast_name, settlement_name)) %>% 
      filter(key %in% v_admin_unique_names) %>% 
      select(key, hromada_code, hromada_name, settlement_code, settlement_name)
    ,by = "key"
  ) %>%
  #now merge settlements with invalid koatuu codes using the coded dataset with 165 settlements
  left_join(
    ds_koatuu %>% select(settlement_koatuu, hromada_code, hromada_name, settlement_code)
    ,by = "settlement_koatuu"
  ) %>% 
  #concatenate 3 set of admin columns created due to merging into one set
  mutate(
    settlement_code = case_when(
      is.na(settlement_code.x) == T & is.na(settlement_code) == T ~ settlement_code.y
      ,is.na(settlement_code.y) == T & is.na(settlement_code) == T ~ settlement_code.x
      ,is.na(settlement_code.x) == F & is.na(settlement_code.y) == F ~ settlement_code.x
      ,is.na(settlement_code.x) == T & is.na(settlement_code.y) == T ~ settlement_code
    )
    ,hromada_code = case_when(
      is.na(hromada_code.x) == T & is.na(hromada_code) == T ~ hromada_code.y
      ,is.na(hromada_code.y) == T & is.na(hromada_code) == T ~ hromada_code.x
      ,is.na(hromada_code.x) == F & is.na(hromada_code.y) == F ~ hromada_code.x
      ,is.na(hromada_code.x) == T & is.na(hromada_code.y) == T ~ hromada_code
    )
    ,hromada_name = case_when(
      is.na(hromada_name.x) == T & is.na(hromada_name) == T ~ hromada_name.y
      ,is.na(hromada_name.y) == T & is.na(hromada_name) == T ~ hromada_name.x
      ,is.na(hromada_name.x) == F & is.na(hromada_name.y) == F ~ hromada_name.x
      ,is.na(hromada_name.x) == T & is.na(hromada_name.y) == T ~ hromada_name
    )
  ) %>% 
  select(-c(settlement_code.x:settlement_code.y, settlement_name))

ds2 %>% filter(is.na(hromada_code)) 

#check hromadas which do not have declarations
v_hromadas <- ds2 %>% distinct(area, hromada_code, hromada_name) %>% pull(hromada_code)

ds_admin %>% 
  filter(oblast_name != "Автономна Республіка Крим", !(hromada_code %in% v_hromadas)) %>% 
  distinct(oblast_name, hromada_name)
#68 hromadas do not have any declaration with GP


#+ tweak-data-doctors-hospitals, eval=eval_chunks ------------------------------------------------
# TO BE DONE FURTHER
# ds_doctors <- 
#   ds_doctors0 %>% 
#   group_by(legal_entity_id) %>% 
#   summarise(n_doctors = n())
# 
# #several hromadas for some hospitals
# ds2 %>% 
#   distinct(hromada_code,hromada_name,legal_entity_id) %>% 
#   group_by(legal_entity_id) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n))
# 
# 
# ds2 %>% 
#   group_by(hromada_code,hromada_name,legal_entity_id) %>% 
#   summarise(n_declarations = sum(count_declarations)) %>% View()
#   
# ds2 %>% 
#   group_by(legal_entity_id) %>% 
#   summarise(n_declarations = sum(count_declarations)) %>% 
#   left_join(
#     ds_doctors
#     ,by = "legal_entity_id"
#   ) %>% View()



#+ aggregate-on-hromada-level, eval=eval_chunks -------------------------------------------------
ds3 <- 
  ds2 %>% 
  mutate(
    person_age = as.numeric(case_when(
      person_age == "100+" ~ "100" #consider 100 years+ as 100 years old
      ,TRUE ~ person_age
    ))
   ,age_group = case_when(
     person_age < 18 ~ "0-17"
     ,person_age < 30 ~ "18-29"
     ,person_age < 45 ~ "30-44"
     ,person_age < 60 ~ "45-59"
     ,person_age >=60 ~ "60+"
   )
   ,working_age = case_when(
     person_age > 14 & person_age < 71 ~ 1
     ,TRUE ~ 0
   )
   ,settlement_type = case_when(
     settlement_type == "місто" ~ "urban"
     ,settlement_type == "селище" ~ "rural"
     ,settlement_type == "село" ~ "rural"
     ,settlement_type == "смт" ~ "urban"
   )
  ) %>% 
  group_by(
    area, hromada_code, hromada_name
  ) %>% 
  summarise(
    total = sum(count_declarations)
    ,female = sum(count_declarations[person_gender == "жіноча"])
    ,male = sum(count_declarations[person_gender == "чоловіча"])
    ,female_pct = round(female/total, 2)
    ,male_pct = round(male/total,2)
    ,urban = sum(count_declarations[settlement_type == "urban"])
    ,rural = sum(count_declarations[settlement_type == "rural"])
    ,urban_pct = round(urban/total,2)
    ,rural_pct = round(rural/total,2)
    ,youth =  sum(count_declarations[person_age < 30])
    ,youth_pct = round(youth/total, 2)
    ,working_age_total = sum(count_declarations[working_age == "1"])
    ,working_age_pct = round(working_age_total/total,2)
    ,male_18_60 = sum(count_declarations[person_gender == "чоловіча" & (person_age >= 18 & person_age < 61)])
  )

#+ save-data, eval=eval_chunks -------------------------------------------------
readr::write_csv(ds2, "./data-public/derived/declarations-all.csv") #long format
readr::write_csv(ds3, "./data-public/derived/declarations-hromada.csv") #aggregated on hromada level


#+ maps, eval=eval_chunks -------------------------------------------------
library(sf)
library(tmap)

path_polygons <-  "./data-public/raw/terhromad_fin.geojson"

ds_polygons <- st_read(path_polygons) %>% janitor::clean_names() %>% 
  mutate(
    admin_3 = str_replace_all(admin_3,c("a" = "а", "o" = "о", "p"="р", "e"="е", "'" = "’"))
  )

ds_map <- st_sf(
    ds3 %>% 
    left_join(
      ds_polygons %>% select(cod_3, geometry)
      ,by = c("hromada_code"="cod_3")
    )
  )

tmap_mode("view")
g1 <- 
  ds_map %>%
  tm_shape() + 
  tm_fill("youth_pct",
          palette = "RdBu",
          id="hromada_code",
          popup.vars=c("hromada_name")
  ) + 
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g1

#+ comparison-with-state-statistics, eval=eval_chunks -------------------------------------------------
path_stat <-  "./data-public/raw/pop-permanent-2022.xlsx"

stat_colnames <- c(
  "age"
  ,"both"
  ,"male"
  ,"female"
)

sheets <- readxl::excel_sheets(path_stat)[-c(1:11, 37:67)]

ds_stat <- tibble()
for (i in sheets){
  ds_stat0 <- readxl::read_excel(path_stat, sheet = i, col_names = stat_colnames)
  obl <- 
  if(ds_stat0$age[2] != "Kyiv city"){
  ds_stat1 <- 
    ds_stat0 %>% 
    select(-both) %>% 
    mutate(
      oblast = str_extract(age, ".+oblast|Kyiv")
      ,type = str_extract(age, ".+population")
    ) %>% 
    fill(oblast, type) %>% 
    filter(
        !is.na(as.numeric(male))
        ,!(age %in% c("Усього / Total", "60 years and older", "65 years and older"))
        ,!str_detect(age, "–")
        ,!str_detect(type, "Тotal population")
      ) %>% 
      pivot_longer(c(male, female), names_to = "gender", values_to = "n")
  } else {
    ds_stat1 <- 
      ds_stat0 %>% 
      select(-both) %>% 
      mutate(
        oblast = str_extract(age, ".+oblast|Kyiv")
        ,type = str_extract(age, ".+population")
      ) %>% 
      fill(oblast, type) %>%    
      filter(
        !is.na(as.numeric(male))
        ,!(age %in% c("Усього / Total", "60 years and older", "65 years and older"))
        ,!str_detect(age, "–")
      ) %>% 
      mutate(type = "Mіське населення / Urban population") %>% 
      pivot_longer(c(male, female), names_to = "gender", values_to = "n")
    }

  ds_stat <- rbind(ds_stat, ds_stat1)
}

ds_stat <- 
  mutate(
    
  )


ds_stat %>% summarise(n = sum(n))

  


ds_comp <- 
  ds2 %>% 
  select(area, settlement_type, person_gender, person_age)


