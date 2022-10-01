rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels 
library(dplyr)  # labels 
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/growth-hromada/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# path_admin    <- "./data-private/derived/ua-admin-map.rds"
path_admin    <- "./data-private/derived/ua-admin-map.csv"
path_time         <- "./data-private/derived/time_rada.csv"
path_rada_hromada <- "./data-private/derived/rada_hromada.csv"


# ---- declare-functions -------------------------------------------------------
extract_hromada_label <- function(d, target_budget_code){
  
  d %>% 
    filter(budget_code == target_budget_code) %>%  #glimpse()
    distinct(hromada_name, raion_name, oblast_name) %>% 
    mutate(
      unit_label = paste0("Громада: ", hromada_name, " | Район:",raion_name, " | Область: ", oblast_name)
    ) %>% 
    pull(unit_label)
  
}
# how to use:
ds_admin %>% extract_hromada_label("1954800000")

# ---- load-data ---------------------------------------------------------------
ds_admin <- readr::read_csv(path_admin)
ds_rada_hromada <- readr::read_csv(path_rada_hromada)
ds_time <- readr::read_csv(path_time)
# ---- inspect-data ------------------------------------------------------------

ds_time %>% glimpse() # date when a rada joined a hromada
ds_rada_hromada %>% glimpse() # final structure of radas within hromadas
ds_admin %>% glimpse() # map of codes and labels settlement-hromada-raion-oblast
# ---- tweak-data --------------------------------------------------------------

ds0 <- 
  ds_time %>% 
  group_by(hromada_code, date) %>% 
  summarize(
    rada_count = n_distinct(rada_code, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  group_by(hromada_code) %>% 
  mutate(
    event_count = n_distinct(date, na.rm =T)
    ,stable_composition = n_distinct(rada_count,na.rm=T)==1L
  ) %>% 
  ungroup() %>% 
  arrange(hromada_code, date) %>% 
  mutate(
    trajectory_type = case_when(
      event_count == 1L ~ "Last minute"
      ,stable_composition ~ "Stable"
      ,TRUE ~ "Dynamic"
    )
  ) %>% 
  # print(n = 100)
  left_join(
    ds_admin
    ,by = "hromada_code"
  )

ds0 %>% glimpse()

#+ ---- graph-composition-function ----------------------------------------------------
# TODO: Valentyn, please compose a function that intakes hromada code
# and outputs a graph of events in the history of this hromada (see issue #20)

graph_hromada_history <- function(target_hromada_ua_code){
  
  path_admin    <- "./data-private/derived/ua-admin-map.csv"
  path_time <- "./data-private/derived/time_rada.csv"
  ds_admin <- readr::read_csv(path_admin)
  ds_time <- readr::read_csv(path_time)
  
  d <- 
  ds_time %>% 
    filter(hromada_code == target_hromada_ua_code) %>% 
    group_by(hromada_code, date) %>% 
    summarize(
      rada_count = n_distinct(rada_code, na.rm = T)
    )
  
  hromada_label <-
  ds_admin %>% 
    filter(hromada_code == target_hromada_ua_code) %>%
    distinct(hromada_name, raion_name, oblast_name) %>% 
    mutate(
      unit_label = paste0("Громада: ", hromada_name, " | Район: ",raion_name, " | Область: ", oblast_name)
    ) %>% 
    pull(unit_label)
  
  g <- 
  d %>% 
    ggplot(aes(x = date, y = rada_count))+
    geom_line() +
    geom_point() +
    geom_text(aes(label = date), size = 3.2, vjust = 2) +
    scale_y_continuous(breaks = seq(from = 0, to = max(d$rada_count), by = 1),
                       limits = c(0, max(d$rada_count)+1))+
    labs(title = hromada_label, x = 'Дата зміни складу громади', y = 'Кількість рад')
  g

}

graph_hromada_history('UA56040270000014394')


#+ ---- single-hromada-graph -----------------------------------------------------------------

target_hromada_budget_code <- "1954800000"
(target_hromada_ua_code <- 
    ds_admin %>% 
    filter(budget_code == target_hromada_budget_code)%>% 
    distinct(hromada_code) %>% pull(hromada_code))
length(target_hromada_ua_code)==1L# should be a single value to assert one-to-one match

d <- 
  ds_time %>% 
  filter(hromada_code == target_hromada_ua_code) %>% 
  group_by(hromada_code, date) %>% 
  summarize(
    rada_count = n_distinct(rada_code, na.rm = T)
  )
d

g <- 
  d %>% 
  ggplot(aes(x = date, y = rada_count))+
  geom_line()+
  geom_point()+
  # scale_y_continuous(breaks = seq(0,max(d$rada_count),1))+
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = ds_admin %>% extract_hromada_label(target_hromada_budget_code)
  )
g

# ---- graph-1 -----------------------------------------------------------------
d <-
  ds_time  %>% 
  group_by(hromada_code, date) %>% 
  summarize(
    rada_count = n_distinct(rada_code, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  left_join(
    ds_admin
    ,by = "hromada_code"
  )
d

g <-
  d %>% 
  filter(!is.na(hromada_code)) %>% 
  ggplot(aes(
    x=date
    , y = rada_count
    , group = hromada_code
    # ,color = oblast_name
  )
  )+
  geom_point(shape=21, alpha = .4, size = 1)+
  facet_wrap(facets = c("oblast_name"))+
  geom_line(alpha = .3)+
  theme(
    legend.position = "none"
  )+
  labs(
    # title = "Динаміка складу територіальних громад"
    # ,subtitle = "Як змінювалась кількість місцевих рад у складі громад?"
    # ,y = "Кількість місцевих рад у громаді"
    # ,x = "Дата зміни складу громад"
    title = "Change in composition of hromadas over time"
    ,subtitle = "How did the quantity of radas withing hromada change over time?"
    ,y = "Number of radas in the hromada"
    ,x = "The date of changing the composition of hromada"
  )

g %>% 
  quick_save("1-hromada-growth-n-rada",w=12, h=8)


# ---- graph-2 -----------------------------------------------------------------
# add classification



g <-
  ds0 %>% 
  filter(!is.na(hromada_name)) %>% 
  ggplot(aes(
    x=date
    , y = rada_count
    , group = hromada_code
    ,color = trajectory_type
    ,fill = trajectory_type
  )
  )+
  geom_point(shape=21, alpha = .4, size = 1)+
  facet_wrap(facets = c("oblast_name"))+
  geom_line(alpha = .3)+
  theme(
    legend.position = "bottom"
  )+
  labs(
    # title = "Динаміка складу територіальних громад"
    # ,subtitle = "Як змінювалась кількість місцевих рад у складі громад?"
    # ,y = "Кількість місцевих рад у громаді"
    # ,x = "Дата зміни складу громад"
    title = "Types of composition of hromadas over time"
    ,subtitle = "How did the quantity of radas within hromada change over time?"
    ,y = "Number of radas in the hromada"
    ,x = "The date of changing the composition of hromada"
    ,color = "Composition Type"
    ,fill = "Composition Type"
  )

g %>% 
  quick_save("2-composition-type",w=12, h=8)

# ---- graph-3 -----------------------------------------------------------------
# bar graph
ds0 %>% glimpse()
g <-
  ds0 %>% 
  filter(!is.na(hromada_name)) %>% 
  group_by(oblast_name, trajectory_type) %>% 
  summarize(
    type_count = n_distinct(hromada_code, na.rm = T)
    ,.groups = "drop"
  ) %>% 
  group_by(oblast_name) %>% 
  mutate(
    total_count = sum(type_count, na.rm = T)
    ,type_prop = type_count/total_count
    ,type_pct = type_prop %>% scales::percent(accuracy = 1)
  ) %>% 
  ggplot(
    aes(
      x=trajectory_type
      , y = type_count
      ,color = trajectory_type
      ,fill = trajectory_type
    )
  )+
  geom_col()+
  geom_text(aes(label = type_pct),nudge_y = 6)+
  facet_wrap(facets = c("oblast_name"))+
  scale_y_continuous(expand = expansion(mult=c(0,.1)))+
  # geom_line(alpha = .3)+
  theme(
    legend.position = "bottom"
  )+
  labs(
    # title = "Динаміка складу територіальних громад"
    # ,subtitle = "Як змінювалась кількість місцевих рад у складі громад?"
    # ,y = "Кількість місцевих рад у громаді"
    # ,x = "Дата зміни складу громад"
    title = "Types of composition of hromadas over time"
    ,subtitle = "Stable - initial and final compositions are identical/nLast "
    ,y = "Number of radas in the hromada"
    ,x = "Retrospective classification of hromadas' trajectory"
    ,color = "Composition Type"
    ,fill = "Composition Type"
  )

g %>% 
  quick_save("3-composition-type",w=12, h=8)
# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
