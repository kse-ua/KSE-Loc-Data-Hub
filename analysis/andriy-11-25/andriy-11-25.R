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
library(tmap)

#+ load-globals -------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/andriy-11-25/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }


#+ load-all-datasets -------------------------------------------------------------
#main datasets

path_general <- "./data-private/derived/full_dataset.csv"
path_survey  <- "data-private/derived/survey-general-linked.csv"
path_oblast <- "./data-private/raw/oblast.csv"

ds0 <-  readr::read_csv(path_general)
ds_survey <- readr::read_csv(path_survey)
ds_oblast <- readr::read_csv(path_oblast)

ds0 %>% glimpse()
ds_survey %>% glimpse()

ds0 %>% explore::describe_all() %>% print_all()



c_had_survey <-  (ds_survey %>% pull(hromada_code) %>% unique())

# ---- tweak-data-1 ---------------
ds1 <- 
  ds0 %>% 
  mutate(
    had_survey = hromada_code %in% c_had_survey
  )

ds1 %>% count(had_survey)
# ---- graph-1had_survey# ---- graph-1 --------------

# How representative are the surveyed hromada, judging from the 
# context of bivariate relationship of key economic indicators? 
g1 <-
  ds1 %>% 
  filter(income_military_2021 > 0L & income_transfert_2021 > 0L) %>% 
  # {
  ggplot(
    aes(
      # .
      x=log(income_military_2021)
      , y = log(income_transfert_2021)
      # ,color = had_survey
      # , fill = had_survey
    )
  )+
  geom_point(shape =21, alpha = .5, size = 2)+
  geom_point(shape=21, fill = "red", data = . %>% filter(had_survey))+
  scale_fill_manual(values = c("TRUE"="salmon", "FALSE"=NULL))+
  facet_wrap(facets = "oblast_name")+
  geom_smooth(
    method = "lm", se = FALSE, color = "red"
    ,size = .5
  ) +
  geom_smooth(
    method = "lm", se = FALSE, color = "black"
    , data = . %>% filter(!had_survey)
    ,size = .5
  ) +
  ggpmisc::stat_poly_eq(formula = y ~ + x
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0
                        ,label.y = 20
                        # , data = . %>% filter(!had_survey)
                        ) +
  labs(
    title = "Do surveyed hromadas follow the bivariate trend of the general population?"
    ,subtitle = "Economic measures: Own Income, Transfert"
    ,caption = "Among hromadas that have a military base"
  )
  # }

g1 %>% quick_save("1-bivariate-trend", h=10, w=14)

  
ggpmisc::stat_poly_eq(formula = y ~ + x 
                      ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                      ,parse = TRUE
                      ,label.x = 0.9
                      ,label.y = 0.05) +


# ---- serhii-solution --------------------
library(tableone)
ds_survey_codes <- 
  ds_survey %>% 
  filter(!is.na(hromada_code)) %>% 
  pull(hromada_code)

predictors_all <- 
  ds0 %>% 
  select(-c("hromada_code", "hromada_name","raion_code", "raion_name","oblast_code",
            "oblast_name","hromada_full_name","hromada_center_code","hromada_center",           
            "lat_center","lon_center")) %>% 
  colnames()



ds1 <- ds0 %>% 
  mutate(survey_participant = ifelse(hromada_code %in% ds_survey_codes, "surveyed", "non-surveyed"))

ls1 <- tableone::CreateTableOne(vars=predictors_all, strata = "survey_participant", data=ds1 ,addOverall  = T)
print(table_pre, smd=T)

ls1$MetaData$varNumerics

d1_cont <- 
  list(  
 ls1$ContTable$Overall %>% as_tibble() %>% mutate(measure = ls1$MetaData$varNumerics)
 ,ls1$ContTable$`non-surveyed` %>% as_tibble() %>% mutate(measure = ls1$MetaData$varNumerics) 
 ,ls1$ContTable$`surveyed` %>% as_tibble()  %>% mutate(measure = ls1$MetaData$varNumerics)
) %>% 
  bind_rows(.,.id = "survey") %>% 
  mutate(
    survey = case_when(
      survey == "1" ~ "Overal"
      ,survey == "3" ~ "Surveyed"
      ,survey == "2" ~ "Non-surveyed"
      ,TRUE ~ NA_character_
    )
  )
  
d1_cat <- 
  list(  
    ls1$CatTable$Overall %>% as_tibble()  %>% mutate(measure = ls1$MetaData$varFactors)
    ,ls1$CatTable$`non-surveyed` %>% as_tibble() %>% mutate(measure = ls1$MetaData$varFactors)
    ,ls1$CatTable$`surveyed` %>% as_tibble() %>% mutate(measure = ls1$MetaData$varFactors)
  ) %>% 
  bind_rows(.,.id = "survey") %>% 
  mutate(
    survey = case_when(
      survey == "1" ~ "Overall"
      ,survey == "3" ~ "Surveyed"
      ,survey == "2" ~ "Non-surveyed"
      ,TRUE ~ NA_character_
    )
  )  
d1_cat  
d1_cont %>% glimpse()


# ---- serhii-solution-g1 -------------------

g1 <- 
  d1_cont %>% #glimpse()
  # filter(!str_detect(measure,"^income")) %>% #glimpse()
  filter(str_detect(measure,"^income")) %>% #glimpse()
  ggplot(aes(y = measure, x = median, color = survey))+
  geom_point(shape = 21, size = 2, alpha = .7)+
  # scale_fill_manual(values = c("Overall" = "grey", "Surveyed" = "red", "Non-surveyed" = "blue"))+
  scale_color_manual(values = c("Overall" = "grey", "Surveyed" = "red", "Non-surveyed" = "blue"))+
  scale_x_continuous(labels = scales::comma_format())

g1
g1 %>% quick_save("1", w= 8, h = 6)

  




