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


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
path_edem <- "./data-public/raw/edem-data.csv"
path_hromada <- "./data-public/derived/hromada.csv"

edem_names <- c(
  "oblast_name"
  ,"hromada_name"
  ,"petitions"
  ,"consultations"
  ,"participatory_budget"
  ,"open_hromada"
)

d0 <- readr::read_csv(path_edem, col_names = edem_names, skip = 1)
ds_hromada <- readr::read_csv(path_hromada) %>% 
  mutate(
    hromada_name = case_when(
      hromada_name == "Жданівська" & oblast_name == "Вінницька" ~ "Війтівецька"
      ,TRUE ~ hromada_name
      )
  )


#+ tweak-data, eval=eval_chunks ------------------------------------------------
hromdas_unique <- 
  ds_hromada %>% 
  mutate(
    key = paste(oblast_name, hromada_name)
  ) %>%
  group_by(key) %>% 
  mutate(n_key = n()) %>% 
  filter(n_key == 1) 

d1 <- 
  d0 %>% 
  mutate(
    oblast_name = str_remove(oblast_name, " область| ОДА область")
    ,hromada_name = str_to_title(hromada_name)
    ,key = paste(oblast_name, hromada_name)
  ) %>% 
  left_join(
    hromdas_unique %>% select(hromada_code, key)
    ,by = "key"
  ) %>% 
  mutate(
    index = seq(1:length(hromada_name))
    ,hromada_code = case_when(
      key == "Миколаївська Первомайська" ~ "UA48080130000028458"
      ,key == "Чернігівська Талалаївська" ~ "UA74080190000091939"
      ,key ==  "Одеська Чорноморська" ~ "UA51100370000040590"
      ,index == "239" ~ "UA12140250000015858"
      ,index == "240" ~ "UA12080090000039979"
      ,TRUE ~ hromada_code
    )
    ,edem_total = rowSums(across(petitions:open_hromada))
  ) %>% 
  select(-key, -index) %>% 
  rename(edem_petitions = petitions, edem_consultations = consultations,       
         edem_participatory_budget = participatory_budget, edem_open_hromada = open_hromada)
  

#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(d1, "./data-public/derived/edem-data.csv")



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

