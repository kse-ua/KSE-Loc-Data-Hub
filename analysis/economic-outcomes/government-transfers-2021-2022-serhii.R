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
library(tmap)
library(tmaptools)
library(sf)
library(RColorBrewer)
library(viridis)


#+ declare-globals -------------------------------------------------------------
#Data of changes in governmental transfers as a share of hromada income in 2021-2022
path_budget_data <- "./data-private/derived/budget-change-for-map.csv"

#Shapefile of hromada polygons
#Source: https://drive.google.com/file/d/1X3Ym-7s1RgQgsi_p4uJ3EGEWYGTd-UMz/view?usp=sharing
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"

path_admin <- "./data-private/derived/ua-admin-map.csv"

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

ds_budget_data <- readr::read_csv(path_budget_data)
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names() %>% 
  mutate(
    admin_3 = str_replace_all(admin_3,c("a" = "а", "o" = "о", "p"="р", "e"="е", "'" = "’"))
  )

ds_admin_full <- readr::read_csv(path_admin)

#+ inspect-data ----------------------------------------------------------------
ds_budget_data %>% glimpse()
ds_polygons %>% glimpse()


#+ tweak-data-1, eval=eval_chunks ------------------------------------------------
tor_before_22 <- c("05561000000","05556000000","12538000000","05555000000","12534000000"
                   ,"05549000000","05557000000","05551000000","12539000000","05547000000","05548000000"
                   ,"05563000000","12537000000","12540000000","05560000000","12533000000","05552000000"
                   ,"05554000000","05564000000","12532000000","12541000000","05562000000","12535000000"
                   ,"05566000000","12531000000","05565000000","05559000000","05558000000","05550000000"
                   ,"12536000000","05553000000") 


ds_1 <- ds_budget_data %>% 
  # filter(year == 2022) %>%
  mutate(
    # outlier_own_income_change = own_income_change > quantile(own_income_change, na.rm = TRUE)[4] +
    #   1.5*IQR(own_income_change, na.rm = TRUE) | own_income_change < 
    #   quantile(own_income_change, na.rm = TRUE)[2] - 1.5*IQR(own_income_change, na.rm = TRUE)
    # ,outlier_own_prop_change = own_prop_change > quantile(own_prop_change, na.rm = TRUE)[4] +
    #   1.5*IQR(own_prop_change, na.rm = TRUE) | own_prop_change < 
    #   quantile(own_prop_change, na.rm = TRUE)[2] - 1.5*IQR(own_prop_change, na.rm = TRUE)
    ntile = ntile(own_income_change,100)
    ,outlier_own_prop_change = ntile(own_prop_change, 100) > 99
    ,tor_before_22 = admin4_code %in% tor_before_22
  ) %>%
  left_join(
    ds_admin_full %>%
      mutate(budget_code = paste0(budget_code,"0")) %>%
      distinct(budget_code, hromada_name, hromada_code, oblast_name_display, map_position
               , region_ua, oblast_code)
    ,by = c("admin4_code"  = "budget_code")
  )

hist(ds_1$own_prop_change)

unique(ds_polygons$cod_3) %>% length()
unique(ds_1$hromada_code) %>% length()

#+ tweak-data-2, eval=eval_chunks ------------------------------------------------
#difference between admin and polygon datasets
# as_tibble(ds_polygons) %>% 
#   select(cod_3, admin_3) %>% 
#   left_join(
#     ds_admin_full %>% 
#       select(hromada_code, hromada_name) %>% 
#       distinct()
#     ,by = c("cod_3"="hromada_code")
#   ) %>% 
#   filter(is.na(hromada_name))

#adding polygones for oblasts
sf_use_s2(FALSE)
ds_polygons_oblasts <- 
  ds_polygons %>% 
  group_by(admin_1) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
sf_use_s2(TRUE)

#adding polygones for raions
sf_use_s2(FALSE)
ds_polygons_raions <- 
  ds_polygons %>% 
  group_by(admin_2) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
sf_use_s2(TRUE)

#combine with main dataset
d1 <- st_sf(
  right_join(
    ds_1 %>%
      filter(year == 2022) %>%
      select(oblast_code, hromada_code, hromada_name, own_income_change_pct, own_prop_change, tor_before_22,
             outlier_own_prop_change, own_prop, own_prop_change_pct, income_own)
    ,ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
)

d2 <- d1 %>%
  left_join(ds_1 %>% filter(year == 2021) %>% select(hromada_code, own_prop, income_own)
            , suffix = c('2022', '2021'), by = 'hromada_code') %>%
  mutate(
    own_prop_pct2021 = scales::percent(own_prop2021)
    ,own_prop_pct2022 = scales::percent(own_prop2022)
    ,own_prop_change = if_else(!tor_before_22, own_prop_change, NA_real_)
  )



#+ graph, eval=eval_chunks ------------------------------------------------
g1 <- 
  tm_shape(d2) + 
  tm_fill("own_prop_change",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Зміна частки власних доходів',
          palette = "RdBu",
          id="hromada_name",
          popup.vars=c('Зміна частки власних доходів' = "own_prop_change_pct",
                       'Частка власних доходів у 2021' = 'own_prop_pct2021',
                       'Частка власних доходів у 2022' = 'own_prop_pct2022',
                       'Зміна обсягу власних доходів' = 'own_income_change_pct',
                       'Обсяг власних доходів у 2021' = 'income_own2021',
                       'Обсяг власних доходів у 2022' = 'income_own2022'
          ),
          style = 'pretty',
          labels = c('-60% до -40%', '-40% до -20%', '-20% до 0%', '0% до +20%', 
                     '+20% до +40%', '+40% до +60%', 'Немає даних')) + 
  tm_borders('gray', lwd = 0.2) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(ds_polygons_oblasts) + 
  tm_borders('gray', lwd = 0.5)
g1 

tmap_save(g1, "./analysis/prints/interactive.html")


#barplot for top and bot performers
top5_v <- d1 %>% arrange(desc(own_prop_change)) %>% slice(1:5) %>% pull(hromada_code)
bot5_v <- d1 %>% arrange(own_prop_change) %>% slice(1:5) %>% pull(hromada_code)

d1 %>%
  filter(hromada_code %in% c(top5_v, bot5_v)) %>%
  left_join(ds_admin_full %>% distinct(hromada_code, oblast_name)) %>%
  mutate(hromada_name_display = paste0(oblast_name, ' - ', hromada_name)) %>%
  ggplot(aes(x = own_prop_change, y = fct_reorder(hromada_name_display, own_prop_change))) +
  geom_col()


g2 <- 
  d1 %>%
  tm_shape() + 
  tm_fill("own_income_change",
          palette = "RdBu",
          id="hromada_code",
          popup.vars=c("hromada_name")
  ) + 
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g2

g2 <- 
  d1 %>%
  mutate(own_income_change = if_else(!tor_before_22, own_income_change, NA_real_)) %>%
  tm_shape() + 
  tm_fill("own_income_change",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Зміна власних доходів громади 21-22',
          palette = "RdBu",
          id="hromada_name",
          popup.vars=c('Зміна власних доходів' = "own_income_change_pct"),
          style = 'pretty',
          # labels = c('-60 to -40%', '-40% to -20%', '-20% to 0%', '0% to +20%', 
          # '+20% to 40%', '40% to 60%', 'Немає даних')
  ) + 
  tm_borders('gray', lwd = 0.2) +
  # tm_shape(d1 %>% filter(tor_before_22)) + 
  # tm_borders('black', lwd = 1) +
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g2



tmap_save(g1, 'map.html')
