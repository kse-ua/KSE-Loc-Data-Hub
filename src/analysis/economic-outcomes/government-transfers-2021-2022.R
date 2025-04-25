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

prints_folder <- paste0("./manipulation/ellis-budget-prints/")

#Shapefile of hromada polygons
#Source: https://drive.google.com/file/d/1X3Ym-7s1RgQgsi_p4uJ3EGEWYGTd-UMz/view?usp=sharing
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"

path_admin <- "./data-private/derived/ua-admin-map.csv"

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------

ds_budget_data <- readr::read_csv(path_budget_data)
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names()
ds_admin_full <- readr::read_csv(path_admin)
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")

#+ inspect-data ----------------------------------------------------------------
ds_budget_data %>% glimpse()
ds_polygons %>% glimpse()


tor_before_22 <- c("05561000000","05556000000","12538000000","05555000000","12534000000"
                   ,"05549000000","05557000000","05551000000","12539000000","05547000000","05548000000"
                   ,"05563000000","12537000000","12540000000","05560000000","12533000000","05552000000"
                   ,"05554000000","05564000000","12532000000","12541000000","05562000000","12535000000"
                   ,"05566000000","12531000000","05565000000","05559000000","05558000000","05550000000"
                   ,"12536000000","05553000000") 

most_affected <- c('Малинівська', "Балаклійська", "Печенізька", "Ізюмська",
                   "Куньєвська", "Борівська", "Петропавлівська")

ds_1 <- ds_budget_data %>% 
  # filter(year == 2022) %>%
  mutate(
    tor_before_22 = admin4_code %in% tor_before_22
  ) %>%  
  left_join(
    ds_admin_full %>%
      mutate(budget_code = paste0(budget_code,"0")) %>%
      distinct(budget_code, hromada_name, hromada_code, oblast_name_display, 
               oblast_name_en, map_position, region_ua, oblast_code)
    ,by = c("admin4_code"  = "budget_code")
  )

hist(ds_1$own_prop_change)
hist(ds_1$own_prop_change_2021const)

unique(ds_polygons$cod_3) %>% length()
unique(ds_1$hromada_code) %>% length()


#+ data-for-map-wo-infl, eval=eval_chunks ------------------------------------------------
d1 <- st_sf(
  right_join(
    ds_1 %>%
      filter(year == 2022) %>%
      select(oblast_code, oblast_name_en, hromada_code, hromada_name, 
             own_income_change_pct, own_prop_change, tor_before_22, income_total,
             own_prop, own_prop_change_pct, income_own)
    ,ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
)

d2 <- d1 %>%
  left_join(ds_1 %>% filter(year == 2021) %>% select(hromada_code, own_prop)
            , suffix = c('2022', '2021'), by = 'hromada_code') %>%
  left_join(ds_general %>% select(hromada_code, total_population_2022)
            , by = 'hromada_code') %>%
  mutate(own_prop_pct2021 = scales::percent(own_prop2021, accuracy = 1L),
         own_prop_pct2022 = scales::percent(own_prop2022, accuracy = 1L),
         own_income_pp = round(income_own / total_population_2022),
         total_income_pp = round(income_total / total_population_2022),
         most_affected = case_when(hromada_name %in% most_affected ~ TRUE,
                                   .default = FALSE))

d3 <- d2 %>% 
  filter(oblast_name_en == "Kharkiv")

#+ data-for-map-with-infl, eval=eval_chunks ------------------------------------------------
d3 <- st_sf(
  right_join(
    ds_1 %>%
      filter(year == 2022) %>%
      select(oblast_code, hromada_code, hromada_name, own_income_change_pct_2021const,
             own_income_change_2021const, own_prop_change_2021const, tor_before_22, own_prop_2021const, 
             own_prop_change_pct_2021const, income_own_2021const)
    ,ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
)

d4 <- d3 %>%
  left_join(ds_1 %>% 
              filter(year == 2021) %>% 
              select(hromada_code, own_prop_2021const, income_own_2021const)
            , suffix = c('2022', '2021'), by = 'hromada_code') %>%
  mutate(own_prop_pct2021 = scales::percent(round(own_prop_2021const2021)),
         own_prop_pct2022 = scales::percent(round(own_prop_2021const2022)))


hist(d4$own_income_change_2021const)

d4 %>% slice_max(own_income_change_2021const, n = 10) %>% neat_DT()

#+ map-wo-infl, eval=eval_chunks ------------------------------------------------

tmap_mode('plot')
g1 <- 
  d3 %>%
  mutate(own_prop_change = if_else(!tor_before_22, own_prop_change, NA_real_)) %>%
  tm_shape() + 
  tm_fill("own_prop_change",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Зміна частки власних доходів',
          palette = "RdBu",
          id="hromada_name",
          style = 'pretty',
          labels = c('від -40 до -30%', 'від -30% до -20%', 'від -20% до -10%', 
                     'від -10 до 0%','від 0% до +10%', 'від +10% до +20%'),
          legend.show = T) + 
  tm_borders('gray', lwd = 0.2) +
  tm_shape(d3 %>% filter(most_affected)) +
  tm_borders('black', lwd = 1.8) +
  # tm_text('own_prop_change_pct', size = 1.2, xmod = 0, ymod = 1, fontface = 'bold') +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_layout(frame = F
            # ,legend.text.size = .6
            ,legend.outside.size = .3
            # ,legend.title.size = .6
            ,legend.position = c("right", "bottom")
            ,legend.outside = T
            ,title.position = c('left', 'top')) +  
  tmap_options(check.and.fix = TRUE)
g1

g2 <- 
  d3 %>%
  mutate(own_prop2022 = if_else(!tor_before_22, own_prop2022, NA_real_)) %>%
  tm_shape() + 
  tm_fill("own_prop2022",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Частка власних доходів',
          palette = "RdYlBu",
          id="hromada_name",
          style = 'pretty',
          labels = c('менше 20%', '20% to 40%', '40% to 60%', '60% to 80%',
                     'більше 80%'),
          # legend.show = F
  ) + 
  tm_borders('gray', lwd = 0.2) +
  tm_shape(d3 %>% filter(most_affected)) +
  tm_borders('black', lwd = 1.8) +
  # tm_text('own_prop_pct2022', size = 0.5) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_layout(frame = F
            # ,legend.text.size = .6
            ,legend.outside.size = .2
            # ,legend.title.size = .6
            ,legend.position = c("right", "bottom")
            ,legend.outside = T
            ,title.position = c('left', 'top')) +
  tmap_options(check.and.fix = TRUE)
g2

g3 <- 
  d3 %>%
  mutate(own_prop2022 = if_else(!tor_before_22, own_prop2022, NA_real_)) %>%
  tm_shape() + 
  tm_fill("own_income_pp",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Обсяг власних доходів на 1 особу',
          palette = "RdYlBu",
          id="hromada_name",
          style = 'pretty'
          # labels = c('менше 20%', '20% to 40%', '40% to 60%', '60% to 80%',
                     # 'більше 80%'),
          # legend.show = F
  ) + 
  tm_borders('gray', lwd = 0.2) +
  tm_shape(d3 %>% filter(most_affected)) +
  tm_borders('black', lwd = 1.8) +
  # tm_text('own_prop_pct2022', size = 0.5) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_layout(frame = F
            # ,legend.text.size = .6
            ,legend.outside.size = .3
            # ,legend.title.size = .6
            ,legend.position = c("right", "bottom")
            ,legend.outside = T
            ,title.position = c('left', 'top')) +
  tmap_options(check.and.fix = TRUE)
g3

g4 <- 
  d3 %>%
  mutate(own_prop2022 = if_else(!tor_before_22, own_prop2022, NA_real_)) %>%
  tm_shape() + 
  tm_fill("total_income_pp",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Обсяг загальних доходів на 1 особу',
          palette = "RdYlBu",
          id="hromada_name",
          style = 'pretty'
          # labels = c('менше 20%', '20% to 40%', '40% to 60%', '60% to 80%',
          # 'більше 80%'),
          # legend.show = F
  ) + 
  tm_borders('gray', lwd = 0.2) +
  tm_shape(d3 %>% filter(most_affected)) +
  tm_borders('black', lwd = 1.8) +
  # tm_text('own_prop_pct2022', size = 0.5) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_layout(frame = F
            # ,legend.text.size = .6
            ,legend.outside.size = .3
            # ,legend.title.size = .6
            ,legend.position = c("right", "bottom")
            ,legend.outside = T
            ,title.position = c('left', 'top')) +
  tmap_options(check.and.fix = TRUE)
g4
tmap_save(g1, "own_prop_change_kharkiv.png", dpi = 800)
tmap_save(g2, "own_prop_kharkiv.png", dpi = 800)
tmap_save(g3, "own_income_pp_kharkiv.png", dpi = 800)
tmap_save(g4, "total_income_pp_kharkiv.png", dpi = 800)


tmap_arrange(g1, g2, ncol = 2)
#+ map-with-infl, eval=eval_chunks ------------------------------------------------

tmap_mode('view')
g2 <- 
  d4 %>%
  mutate(own_prop_change_2021const = if_else(!tor_before_22, own_prop_change_2021const, NA_real_),
         own_prop_change_2021const = if_else(hromada_code == 'UA51120150000080138', NA_real_, own_prop_change_2021const),
         own_income_change_pct_2021const = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_income_change_pct_2021const),
         income_own_2021const2021 = if_else(hromada_code == 'UA51120150000080138', NA_real_, income_own_2021const2021),
         income_own_2021const2022 = if_else(hromada_code == 'UA51120150000080138', NA_real_, income_own_2021const2022),
         own_prop_change_pct_2021const = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_change_pct_2021const),
         own_prop_pct2021 = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_pct2021),
         own_prop_pct2022 = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_pct2022)) %>%
  tm_shape() + 
  tm_fill("own_prop_change_2021const",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Зміна частки власних доходів',
          palette = "RdBu",
          id="hromada_name",
          popup.vars=c('Зміна частки власних доходів, в.п.' = "own_prop_change_pct_2021const",
                       'Частка власних доходів у 2021' = 'own_prop_pct2021',
                       'Частка власних доходів у 2022' = 'own_prop_pct2022',
                       'Зміна обсягу власних доходів, %' = 'own_income_change_pct_2021const',
                       'Обсяг власних доходів у 2021' = 'income_own_2021const2021',
                       'Обсяг власних доходів у 2022' = 'income_own_2021const2022'
          ),
          style = 'pretty',
          textNA = 'Немає даних',
          labels = c('-60 to -40 в.п.', '-40 to -20 в.п.', '-20 to 0 в.п.', '0 to +20 в.п.',
                     '+20 to +40 в.п.')
  ) + 
  tm_borders('gray', lwd = 0.2) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g2

#+ map-with-infl-own-income, eval=eval_chunks ------------------------------------------------

tmap_mode('view')
g3 <- 
  d4 %>%
  mutate(own_income_change_2021const = if_else(!tor_before_22, own_income_change_2021const*100, NA_real_)) %>%
  mutate(own_income_change_2021const = if_else(hromada_code == 'UA51120150000080138', NA_real_, own_income_change_2021const),
         own_income_change_pct_2021const = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_income_change_pct_2021const),
         income_own_2021const2021 = if_else(hromada_code == 'UA51120150000080138', NA_real_, income_own_2021const2021),
         income_own_2021const2022 = if_else(hromada_code == 'UA51120150000080138', NA_real_, income_own_2021const2022),
         own_prop_change_pct_2021const = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_change_pct_2021const),
         own_prop_pct2021 = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_pct2021),
         own_prop_pct2022 = if_else(hromada_code == 'UA51120150000080138', NA_character_, own_prop_pct2022)
  ) %>%
  tm_shape() + 
  tm_fill("own_income_change_2021const",
          # title = 'Change in share of \n hromada own revenue',
          title = 'Зміна обсягу власних доходів, %',
          palette = "RdBu",
          id="hromada_name",
          popup.vars=c('Зміна обсягу власних доходів, %' = 'own_income_change_pct_2021const',
                       'Обсяг власних доходів у 2021' = 'income_own_2021const2021',
                       'Обсяг власних доходів у 2022' = 'income_own_2021const2022',
                       'Зміна частки власних доходів, в.п.' = "own_prop_change_pct_2021const",
                       'Частка власних доходів у 2021' = 'own_prop_pct2021',
                       'Частка власних доходів у 2022' = 'own_prop_pct2022'
          ),
          # style = 'fixed',
          n = 6,
          textNA = 'Missing data'
          # breaks = c(-100, -50, 0, 50, 100, 150, 200, 250, 300),
          # labels = c(' -100% - -50%', '-50% - 0%', '0% - +50%', '+50% - +100%', 
                      # '+100% - +150%', '+150% - +200%', '+200% - +250%', '+250% - +300%')
  ) + 
  tm_borders('gray', lwd = 0.2) +
  # tm_shape(d2 %>% distinct(oblast_code)) + 
  # tm_borders('oblast_code', 'black', lwd = 1) +
  tm_legend(outside=TRUE) +
  tm_layout(frame = FALSE) +
  tmap_options(check.and.fix = TRUE)
g3

#+ barplot for top and bot performers ------------------------------------------
top5_v <- d1 %>% arrange(desc(own_prop_change)) %>% slice(1:5) %>% pull(hromada_code)
bot5_v <- d1 %>% arrange(own_prop_change) %>% slice(1:5) %>% pull(hromada_code)

top5_v_infl <- d3 %>% arrange(desc(own_prop_change_2021const)) %>% slice(1:5) %>% pull(hromada_code)
bot5_v_infl <- d3 %>% arrange(own_prop_change_2021const) %>% slice(1:5) %>% pull(hromada_code)


g3 <- d1 %>%
  filter(hromada_code %in% c(top5_v, bot5_v)) %>%
  left_join(ds_admin_full %>% distinct(hromada_code, oblast_name)) %>%
  mutate(hromada_name_display = paste0(oblast_name, ' - ', hromada_name)) %>%
  ggplot(aes(x = own_prop_change, y = fct_reorder(hromada_name_display, own_prop_change))) +
  geom_col(aes(fill = ifelse(own_prop_change > 0, 'green', 'red'))) +
  scale_fill_manual(values = c("palegreen3", "lightsalmon3"))+
  scale_x_continuous(labels = scales::percent, limits = c(-0.55,0.4)) +
  geom_text(aes(x=ifelse(own_prop_change > 0, own_prop_change+0.06, own_prop_change-0.01), label = own_prop_change_pct, hjust = 1))+
  labs(
    title = "Hromadas that had the most change in proportion of own income"
    ,subtitle = 'Excluding Personal income tax on the financial support of military personnel'
    ,x = 'Change in proportion of own income'
    ,y = NULL
  ) +
  theme(legend.position = "none")
g3
g3 %>% quick_save("1-most-change-own-income", w = 9, h = 6)



d3 %>%
  filter(hromada_code %in% c(top5_v, bot5_v)) %>%
  left_join(ds_admin_full %>% distinct(hromada_code, oblast_name)) %>%
  mutate(hromada_name_display = paste0(oblast_name, ' - ', hromada_name)) %>%
  ggplot(aes(x = own_prop_change, y = fct_reorder(hromada_name_display, own_prop_change))) +
  geom_col() +
  scale_x_continuous(labels = scales::percent)


#+ save-to-disk ------------------------------------------------
tmap_save(g1, 'map_wo_infl.html')
tmap_save(g2, 'map_with_infl.html')
tmap_save(g3, 'map_own_income.html')

