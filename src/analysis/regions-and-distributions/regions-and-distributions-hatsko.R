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
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# for asserting conditions meet expected patterns.
requireNamespace("scales"   )# formatting

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/regions-and-distributions/regions-and-distributions/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_admin        <- "./data-private/derived/ua-admin-map.rds" # product of ./manipulation/ellis-ua-admin.R
path_time         <- "./data-private/derived/time_rada.csv"    # product of ./manipulation/ellis-rada-hromada.R

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds_admin <- readr::read_rds(path_admin)
ds_time  <- readr::read_csv(path_time)

# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------
ds0 <- 
  ds_time %>% 
  filter(!hromada_code == "#N/A") %>% 
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
  left_join(
    ds_admin %>% select(!starts_with("settlement")) %>% distinct()
    ,by = "hromada_code"
  ) %>% 
  select(-stable_composition)


ds0 %>% glimpse()

#+ tweak-data-1 ----------------------------------------------------------------
# Let's add hromada-level helpers (in-group counters and indicators)
d_event_order <- 
  ds_time %>% 
  distinct(hromada_code, date) %>% 
  group_by(hromada_code) %>% 
  arrange(hromada_code, date) %>% 
  mutate(
    event_order = row_number()
    ,event_first = event_order == min(event_order)
    ,event_last = event_order == max(event_order)
  ) %>% 
  ungroup()

ds1 <- 
  ds0 %>% 
  left_join(d_event_order,by = c("hromada_code", "date")) %>% 
  relocate(c("event_order", "event_first", "event_last"), .after = "event_count")

# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------

d <- ds1 %>% 
  filter(event_last == TRUE) %>%
  group_by(oblast_name_en, rada_count) %>%
  summarise(number_of_hromadas = n()) %>%
  left_join(ds_admin %>% 
              select(c('region_en', 'oblast_name_en', 'map_position')) %>% 
              distinct()
            , by =c('oblast_name_en')) %>%
  mutate(oblast_name_en = factor(oblast_name_en)) %>%
  ungroup()

# reorder oblast factor by map_position (didn't find tidy approach)
d$oblast_name_en <- fct_reorder(d$oblast_name_en, d$map_position)

g <-
  d %>%
  ggplot(aes(
    x       = rada_count
    , y     = as.numeric(number_of_hromadas)
    # , group = hromada_code
    ,color  = region_en
    ,fill   = region_en
  )
  )+
  geom_point(shape=21, alpha = .4, size = 1)+
  facet_wrap(facets = c("oblast_name_en")
             , scales = 'free_y'
             , nrow = 5
             , ncol = 5)+
  geom_line(alpha = .3)+
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Number of hromadas with certain number of radas"
    ,subtitle = "How many radas are in the hromada?"
    ,y = "Number of hromadas"
    ,x = "Number of radas in hromada"
    ,color = "Region"
    ,fill = "Region"
  )+
  scale_y_continuous(breaks = scales::pretty_breaks())
  # + scale_x_continuous(labels = scales::number_format(accuracy = 0.0))
  
g %>% quick_save("1-hromadas-size",w=12, h=9)


# ---- graph-2 -----------------------------------------------------------------

# we don't have to count hromadas for histogram, it does this by itself 
d <- ds1 %>% 
  filter(event_last == TRUE) %>%
  mutate(oblast_name_en = factor(oblast_name_en))

d$oblast_name_en <- fct_reorder(d$oblast_name_en, d$map_position)

g <-
  d %>%
  ggplot(aes(
    x       = rada_count
    # , group = hromada_code
    ,color  = region_en
    ,fill   = region_en
  )
  )+
  geom_histogram(bins = 20)+ # bins can be played with. 20 looks nice
  facet_wrap(facets = c("oblast_name_en")
             , scales = 'free_y'
             , nrow = 5
             , ncol = 5)+
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Number of hromadas with certain number of radas"
    ,subtitle = "How many radas are in the hromada?"
    ,y = "Number of hromadas"
    ,x = "Number of radas in hromada"
    ,color = "Region"
    ,fill = "Region"
  )+
  scale_y_continuous(breaks = scales::pretty_breaks())

g %>% quick_save("2-hromadas-size",w=12, h=9)

# ---- save-to-disk ------------------------------------------------------------



# ---- publish ------------------------------------------------------------
path <- "./analysis/regions-and-distributions/regions-and-distributions.Rmd" # connect with Rmd for publishing
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
