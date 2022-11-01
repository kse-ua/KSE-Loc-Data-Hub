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

#+ tweak-data-1 ----------------------------------------------------------------

d_1 <- ds_local_elections_mayors %>%
  mutate(fio = gsub("'",'', fio),
         rada = gsub("'",'', rada))

d_2 <- ds_local_elections_candidates %>%
  mutate(fio = gsub("'",'', fio),
         rada = gsub("'",'', rada))

ds_1 <- d_1 %>%
  left_join(d_2 %>%
              select(fio, birthdate, birthplace, rada), 
            by = c('fio', 'rada'))

# in ds_local_elections_candidates only candidates from city hromadas 
ds_1 %>% summarise(across(everything(), ~ sum(is.na(.))))
ds_1 %>% skimr::skim()


ds_2 <- ds_1 %>% left_join(ds_local_elections_results_opora %>%
                             select(elect_type, fio, rada_title, hromada, sex) %>%
                             filter(elect_type == 'міські голови') %>%
                             mutate(fio = gsub("'",'', fio),
                                    rada_title = gsub("'",'', rada_title)),
                           by = c('fio' = 'fio', 'rada' = 'rada_title')) %>%
  select(-c(result, elect_type)) %>%
  relocate(fio, hromada, rada, rada_type, raion, oblast, party, sex, birthdate, 
           birthplace, info)

# 54 rows with hromada name not matching
ds_2 %>% summarise(across(everything(), ~ sum(is.na(.))))
ds_2 %>% skimr::skim()
ds_2 %>% filter(is.na(hromada)) %>% view()

hromada_na_fill <- c('Ізмаїльська міська громада', '')

#+ tweak-data-2 ----------------------------------------------------------------



#+ save-to-disk, eval=eval_chunks-----------------------------------------------

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

