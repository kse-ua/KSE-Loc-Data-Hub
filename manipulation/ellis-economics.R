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
Sys.setlocale("LC_CTYPE", "russian")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
#https://docs.google.com/spreadsheets/d/1F2d9EEx46llTMEFtgotzIRmDD0fx9sia/edit?usp=sharing&ouid=106674411047619625756&rtpof=true&sd=true
path_economics <- "./data-private/raw/economics.csv"
path_admin     <- "./data-private/derived/ua-admin-map.rds"    
path_time      <- "./data-private/derived/time_rada.csv"


metric_name_label <- c(
   population           = "Кількість населення"
  ,tax_revenue          = "Находження до бюджету"
  ,tax_revenue_q1       = "Надходження до бюджету за 1-й квартал"
  ,tax_revenue_share    = ""
  ,tax_revenue_share_q1 = ""
  ,tax_capacity_index   = ""
  ,reverse_grant        = ""
  ,basic_grant          = ""
  ,duty_tax_uahpp       = ""
  ,duty_tax_uahpp_q1    = ""
  ,land_tax_uahpp       = ""
  ,land_tax_uahpp_q1    = ""
  ,flat_tax_uahpp       = ""
  ,flat_tax_uahpp_q1    = ""
  ,labour_cost_share    = ""
  ,labour_cost_share_q1 = ""
)
#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")
#+ load-data, eval=eval_chunks -------------------------------------------------

ds0        <- readr::read_csv(path_economics, skip=2)
ds_admin   <- readr::read_rds(path_admin)
ds_time    <- readr::read_csv(path_time) 
#+ inspect-data ----------------------------------------------------------------
ds0 %>% glimpse()

ds0 %>% count(object_category)

#+ tweak-data, eval=eval_chunks ------------------------------------------------

ds0 %>% OuhscMunge::column_rename_headstart()

ds1 <- 
  ds0 %>%
dplyr::select(    # `dplyr::select()` drops columns not included.
  oblast                                                = `oblast`,
  raion                                                 = `raion`,
  hromada_name                                          = `hromada_name`,
  hromada_type                                          = `hromada_type`,
  hromada_code                                          = `hromada_code`,
  tot                                                   = `tot`,
  population_2020                                       = `population_2020`,
  population_2021                                       = `population_2021`,
  area_kmsq                                             = `area_kmsq`,
  tax_revenue_2020                                      = `tax_revenue_2020`,
  tax_revenue_2021                                      = `tax_revenue_2021`,
  # tax_revenue_growth_2020_2021                          = `tax_revenue_growth_2020_2021`,
  tax_revenue_q1_2021                                    = `tax_revenue_q1_2021`,
  tax_revenue_q1_2022                                    = `tax_revenue_q1_2022`,
  # tax_revenue_group                                     = `tax_revenue_group`,
  # tax_revenue_growth_q1_2021_q1_2022                      = `tax_revenue_growth_q1_2021_q1_2022`,
  tax_revenue_share_2020                                = `tax_revenue_share_2020`,
  tax_revenue_share_2021                                = `tax_revenue_share_2021`,
  tax_revenue_share_q1_2021                              = `tax_revenue_share_q1_2021`,
  tax_revenue_share_q1_2022                              = `tax_revenue_share_q1_2022`,
  # tax_revenue_share_group                               = `tax_revenue_share_group`,
  # tax_revenue_share_growth_q1_2021_q1_2022                = `tax_revenue_share_growth_q1_2021_q1_2022`,
  tax_capacity_index_2021                               = `tax_capacity_index_2021`,
  reverse_grant_2021                                    = `reverse_grant_2021`,
  basic_grant_2021                                      = `basic_grant_2021`,
  
  income_tax_uahpp                                      = `income_tax_uahpp`,
  
  tax_capacity_index_2022                               = `tax_capacity_index_2022`,
  reverse_grant_2022                                    = `reverse_grant_2022`,
  basic_grant_2022                                      = `basic_grant_2022`,
  
  duty_tax_uahpp_2020                                   = `duty_tax_uahpp_2020`,
  duty_tax_uahpp_2021                                   = `duty_tax_uahpp_2021`,
  duty_tax_uahpp_q1_2021                                 = `duty_tax_uahpp_q1_2021`,
  duty_tax_uahpp_q1_2022                                 = `duty_tax_uahpp_q1_2022`,
  # duty_tax_group_uahpp                                  = `duty_tax_group_uahpp`,
  # duty_tax_growth_q1_2021_q1_2022                         = `duty_tax_growth_q1_2021_q1_2022`,
  land_tax_uahpp_2020                                   = `land_tax_uahpp_2020`,
  land_tax_uahpp_2021                                   = `land_tax_uahpp_2021`,
  land_tax_uahpp_q1_2021                                 = `land_tax_uahpp_q1_2021`,
  land_tax_uahpp_q1_2022                                 = `land_tax_uahpp_q1_2022`,
  # land_tax_group_q1_2022                                 = `land_tax_group_q1_2022`,
  # land_tax_growth_q1_2021_q1_2022                         = `land_tax_growth_q1_2021_q1_2022`,
  flat_tax_uahpp_2020                                   = `flat_tax_uahpp_2020`,
  flat_tax_uahpp_2021                                   = `flat_tax_uahpp_2021`,
  flat_tax_uahpp_q1_2021                                 = `flat_tax_uahpp_q1_2021`,
  flat_tax_uahpp_q1_2022                                 = `flat_tax_uahpp_q1_2022`,
  # flat_tax_group                                        = `flat_tax_group`,
  # flat_tax_growth_q1_2021_2022q2                         = `flat_tax_growth_q1_2021_2022q2`,
  labour_cost_share_2020                                = `labour_cost_share_2020`,
  labour_cost_share_2021                                = `labour_cost_share_2021`,
  labour_cost_share_q1_2021                              = `labour_cost_share_q1_2021`,
  labour_cost_share_q1_2022                              = `labour_cost_share_q1_2022`,
  # labour_cost_share_group                               = `labour_cost_share_group`,
  # labour_cost_growth_q1_2021_q1_2022                      = `labour_cost_growth_q1_2021_q1_2022`,
)
ds1 %>% names() %>% tibble() %>% print_all()


ds1_basic <- 
  ds1 %>% 
  select(
    hromada_code
    # ,hromada_name
    ,hromada_type
    ,population_2020
    ,population_2021
    ,area_kmsq
    ,tot
  )
ds1_basic %>% glimpse(70)
ds_admin  %>% glimpse(70)




#+ tweak-data-2 ----------------------------------------------------------------
ds2_basic <-
  ds1_basic %>% 
  left_join(
    ds_admin %>% 
      select(!starts_with("settlement")) %>% 
      distinct()
    , by = "hromada_code"
  )
ds2_basic %>% glimpse(70)


ds2 <- 
  ds1 %>% 
  select(
    # ,oblast                   
    # ,raion                    
    # ,hromada_name             
    # ,hromada_type             
    hromada_code
    # ,tot                      
    ,population_2020          
    ,population_2021          
    # ,area_kmsq                
    ,tax_revenue_2020         
    ,tax_revenue_2021         
    ,tax_revenue_q1_2021      
    ,tax_revenue_q1_2022      
    ,tax_revenue_share_2020   
    ,tax_revenue_share_2021   
    ,tax_revenue_share_q1_2021
    ,tax_revenue_share_q1_2022
    ,tax_capacity_index_2021  
    ,reverse_grant_2021       
    ,basic_grant_2021         
    # ,income_tax_uahpp         
    ,tax_capacity_index_2022  
    ,reverse_grant_2022       
    ,basic_grant_2022         
    ,duty_tax_uahpp_2020      
    ,duty_tax_uahpp_2021      
    ,duty_tax_uahpp_q1_2021   
    ,duty_tax_uahpp_q1_2022   
    ,land_tax_uahpp_2020      
    ,land_tax_uahpp_2021      
    ,land_tax_uahpp_q1_2021   
    ,land_tax_uahpp_q1_2022   
    ,flat_tax_uahpp_2020      
    ,flat_tax_uahpp_2021      
    ,flat_tax_uahpp_q1_2021   
    ,flat_tax_uahpp_q1_2022   
    ,labour_cost_share_2020   
    ,labour_cost_share_2021   
    ,labour_cost_share_q1_2021
    ,labour_cost_share_q1_2022
  ) %>% 
  mutate_all(.funs = as.character) %>% 
  pivot_longer(
    cols = !starts_with("hromada_code")
    ,names_to = "metric"
  ) %>% 
  mutate(
    time = str_extract(name, "\\d{4}$") %>% as.integer()
    ,name = str_remove(name, "_\\d{4}$")
  ) %>% 
  pivot_wider(
     names_from = "measure"
     ,values_from = "value"
  )
ds2
ds2 %>% glimpse(70)

ds2 %>% distinct(name) %>% tibble()

#+ table-1 ---------------------------------------------------------------------
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

