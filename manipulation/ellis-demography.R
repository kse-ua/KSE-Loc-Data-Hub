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
names_pop22 <- c(
  "object_name_ua"
  ,"Persons"
  ,"object_name_en"
)

path_pop22   <- "./data-private/raw/population-2022.xlsx"
path_oblasti <- "./data-private/raw/oblast.csv"

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------
# Наявне населення 2022. tabs "12-47", "48-81"
# 
ds_pop22_set <- readxl::read_excel(path_pop22, sheet = "12-47", col_names = names_pop22, skip=12)
ds_pop22_hrom <- readxl::read_excel(path_pop22, sheet = "48-81", col_names = names_pop22, skip=10)

#main admin dataset
ds_admin <- readr::read_csv("./data-private/derived/ua-admin-map.csv")

#hromada dataset
ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv")

#+ inspect-data ----------------------------------------------------------------
ds_pop22_set %>% glimpse()
ds_pop22_hrom %>% glimpse()

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds_admin_merge <-
  ds_admin %>% 
  mutate(
    key = paste(oblast_name, raion_name, settlement_type, settlement_name.x)
  ) %>%
  select(key, hromada_name,hromada_code)

ds_pop0 <- 
  ds_pop22_set %>% 
  mutate(
    object_name_ua = str_replace_all(
      object_name_ua, c("'"="’", "\\s\\s"=" ","Короcтишів"="Коростишів", "\\s+\\(.+\\)"=""))
    ,oblast = str_extract(object_name_ua, "(.+(?=\\sобласть))|(м\\.\\sКиїв)")
    ,raion = str_extract(object_name_ua, "(.+(?=\\sрайон))|(м\\.\\sКиїв)")
    ,settlement_type = case_when(
      str_detect(object_name_ua, "м\\.") ~ "місто"
      ,str_detect(object_name_ua, "смт") ~ "селище міського типу"
    )
    ,settlement_name = str_extract(object_name_ua, "(?<=м\\.\\s).+|(?<=смт\\s).+")
  ) %>% 
  fill(oblast, raion) %>% 
  filter(
    !str_detect(object_name_ua, "(.+\\sрайон)|(.+\\sобласть)|(.+\\sнаселення)|Севастополь")
    ,is.na(settlement_type) == F
  ) %>%
  mutate(
    key = paste(oblast, raion, settlement_type, settlement_name)
  ) %>%  
  left_join(
    ds_admin_merge
    ,by = "key"
  ) %>%
  filter(is.na(hromada_name) == F)


ds_pop0 %>% filter(settlement_name == "Олександрівка") %>% View()


ds_pop1 <- 
  ds_pop22_hrom %>% 
  mutate(
    oblast = str_extract(object_name_ua, "(.+(?=\\sобласть))|(м\\.\\sКиїв)")
    ,raion = str_extract(object_name_ua, "(.+(?=\\sрайон))|(м\\.\\sКиїв)")
    ,hromada_type = case_when(
      str_detect(object_name_ua, "сільська") ~ "сільська"
      ,str_detect(object_name_ua, "селищна") ~ "селищна"
      ,str_detect(object_name_ua, "міська") ~ "міська"
    )
    ,hromada_name = str_extract(object_name_ua, ".+(?=\\sміська|\\sсільська|\\sселищна)")
  ) %>% 
  fill(oblast, raion) %>%
  filter(
    !str_detect(object_name_ua, "(.+\\sрайон)|(.+\\sобласть)|(.+\\sнаселення)|Севастополь")
  )


#combination of oblast, raion, hromada_type, hromada_name variables give as a unique ID of hromada
nrow(ds_pop1) == nrow(ds_pop1 %>% distinct(oblast, raion, hromada_type, hromada_name))



ds_pop0 %>%
  group_by(hromada_name, hromada_code) %>%
  summarise(Population_2022 = sum(as.numeric(Persons))) %>% View()






#+ table-1 ---------------------------------------------------------------------



#+ combine ---------------------------------------------------------------------

#+ graph-1 ---------------------------------------------------------------------
#+ graph-2 ---------------------------------------------------------------------
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

