#' ---
#' title: "Ellis Politics"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-economics.R") # run to knit, don't uncomment
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
library(lubridate)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

path_local_elections_2020_chesno <- "./data-private/raw/local_elections_2020_Chesno.xlsx"
path_local_elections_2020_opora <- './data-private/raw/all_results_2020_OPORA.xlsx'

names_local_elections_mayors <- c(
  "oblast"
  ,"raion"
  ,"rada"
  ,"rada_type"
  ,"fio"
  ,"party"
  ,"result"
  ,'info')

names_local_elections_candidates <- c(
  "id"
  ,"fio"
  ,'birthdate'
  ,"birthplace"
  ,"party"
  ,'oblast'
  ,"rada"
  ,'info'
  ,"registration_date")

types_local_elections_results_opora <- c('text', 'text', 'text', 'text', 'numeric', 
                                         'text', 'numeric', 'numeric', 'text',
                                         'text', 'text', 'text', 'text', 'date',
                                         'numeric', 'text', 'text')

types_local_elections_candidates <- c('numeric', 'text', 'date', 'text', 'text',
                                      'text', 'text', 'text', 'date')
                                         

ds_local_elections_mayors <- readxl::read_xlsx(path_local_elections_2020_chesno, 
                                               sheet = 'Обрані мерами', 
                                               col_names =  names_local_elections_mayors, skip = 1)

ds_local_elections_candidates <- readxl::read_xlsx(path_local_elections_2020_chesno, 
                                                   sheet = 'Кандидати в мери',
                                                   col_names = names_local_elections_candidates, 
                                                   col_types = types_local_elections_candidates, 
                                                   skip = 1)

ds_local_elections_results_opora <- readxl::read_xlsx(path_local_elections_2020_opora,
                                                      col_types = types_local_elections_results_opora)

#+ tweak-data ------------------------------------------------------------------



#+ inspect-data ----------------------------------------------------------------

ds_local_elections_mayors %>% glimpse(20)
ds_local_elections_candidates %>% glimpse(20)
ds_local_elections_results_opora %>% glimpse(20)

#+ merging datasets ----------------------------------------------------------------

ds1 <- ds_local_elections_mayors %>%
  left_join(ds_local_elections_candidates %>%
              select(fio, birthdate, birthplace, rada), 
            by = c('fio', 'rada'))

# in ds_local_elections_candidates only candidates from city hromadas 
ds1 %>% summarise(across(everything(), ~ sum(is.na(.))))
ds1 %>% skimr::skim()

# getting hromada and sex data from opora dataset
ds2 <- ds1 %>% left_join(ds_local_elections_results_opora %>%
                             select(elect_type, fio, rada_title, hromada, sex) %>%
                             filter(elect_type == 'міські голови'),
                           by = c('fio' = 'fio', 'rada' = 'rada_title')) %>%
  select(-c(result, elect_type)) %>%
  relocate(fio, hromada, rada, rada_type, raion, oblast, party, sex, birthdate, 
           birthplace, info)


# 54 rows with hromada name not matching
ds2 %>% summarise(across(everything(), ~ sum(is.na(.))))
ds2 %>% skimr::skim()

# filling NA in hromada
ds2 <- ds2 %>% left_join(ds2 %>% 
  filter(is.na(hromada)) %>% 
  select(rada, hromada) %>% 
  left_join(ds_local_elections_results_opora, by = c('rada' = 'rada_title')) %>% 
  distinct(rada, hromada.y),
  by = c('rada')) %>%
  mutate(hromada = coalesce(hromada.y, hromada)) %>%
  select(-c(hromada.y)) %>%
  mutate(hromada = ifelse(is.na(hromada), paste0(str_extract(rada, '[^\\s]+\\s+[^\\s]+'), ' громада'), hromada))

#+ getting new variables from info ----------------------------------------------------------------

ds3 <- ds2 %>%
  mutate(info = str_remove(info, 'Громадян(ин|ка) України, ')
         ,education = str_extract(info, 'освіта [^,]*')
         ,birthdate = as.Date(str_extract(info, '\\d{2}.\\d{2}.\\d{4}'), '%d.%m.%Y')
         ,age = trunc((birthdate %--% Sys.Date()) / years(1))
         ,workplace = str_match(info, '(?<= член |безпартійна|безпартійний)[^,]*, ([^,]*)')[,2]
         ,residence = str_match(info, '(?<=, місце проживання: )(.*)')[,2]
         ,position = str_match(info, paste0('(?<=', workplace, '), ([^,]*)'))[,2])

ds3 %>% filter(is.na(birthplace)) %>% select(fio, info) %>% neat_DT()
ds3 %>% skimr::skim()


#+ tweak-data-2 ----------------------------------------------------------------



#+ save-to-disk, eval=eval_chunks-----------------------------------------------
ds3 %>% openxlsx::write.xlsx("./data-private/derived/hromada_heads.xlsx")


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

