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
# prints_folder <- paste0("./analysis/.../prints/")
# if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

path_admin    <- "./data-private/derived/ua-admin-map.rds"
path_time         <- "./data-private/derived/time_rada.csv"
path_rada_hromada <- "./data-private/derived/rada_hromada.csv"


# ---- declare-functions -------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/growth-hromada/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# ---- load-data ---------------------------------------------------------------
ds_admin <- readr::read_rds(path_admin)
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
  # ungroup() %>% 
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
      ,stable_composition ~ "Stable Composition"
      ,TRUE ~ "Dynamic Composition"
    )
  ) %>% 
  # print(n = 100)
  left_join(
    ds_admin
    ,by = "hromada_code"
  )

ds0 %>% glimpse()

# ---- table-1 -----------------------------------------------------------------


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
  filter(!is.na(hromada_code)) %>% 
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
    ,subtitle = "How did the quantity of radas withing hromada change over time?"
    ,y = "Number of radas in the hromada"
    ,x = "The date of changing the composition of hromada"
    ,color = "Composition Type"
    ,fill = "Composition Type"
  )

g %>% 
  quick_save("2-composition-type",w=12, h=8)

# ---- graph-2 -----------------------------------------------------------------
# bar graph
ds0 %>% glimpse()
g <-
  ds0 %>% 
  filter(!is.na(hromada_name)) %>% 
  group_by(trajectory_type) %>% 
  summarize(
    
  )
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
    ,subtitle = "How did the quantity of radas withing hromada change over time?"
    ,y = "Number of radas in the hromada"
    ,x = "The date of changing the composition of hromada"
    ,color = "Composition Type"
    ,fill = "Composition Type"
  )

g %>% 
  quick_save("2-composition-type",w=12, h=8)
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
