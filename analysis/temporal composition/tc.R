#' ---
#' title: "01 - Temporal Composition"
#' author: "Valentyn"
#' date: "Last updated: `r Sys.Date()`"
#' ---
#+ echo=F ----------------------------------------------------------------------
# rmarkdown::render(input = "./analysis/temporal composition/tc.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
requireNamespace("trelliscopejs"   )# Interactive faceting

# align the root with the project working directory
opts_knit$set(root.dir='../../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console


#+ load-sources ---------------------------------------------------------------
source("./scripts/common-functions.R")
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
path_rada    <- "./data-private/derived/rada_hromada.csv"
path_hromada <- "./data-private/derived/hromada.csv"
path_time   <- "./data-private/derived/time_rada.csv"  
path_admin    <- "./data-private/derived/ua-admin-map.rds"
path_plots <- './analysis/temporal composition/plots'


Sys.setlocale("LC_CTYPE", "russian")

prints_folder <- paste0("./analysis/temporal composition/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

#+ declare-functions -----------------------------------------------------------

plot_hromada <- function(x, path = getwd()) {
  tmp <- ds2 %>%
    filter(oblast_hromada == x)
  p <- ggplot(tmp, aes(x=year, y=rada_count)) +
    geom_bar(stat='identity', position = 'identity') +
    geom_text(aes(label=rada_count), vjust = -0.2) +
    theme_classic()
  ggsave(p, file=paste0(x, ".png"), path = path)
}


draw_random_id <- function(d, idvar="person_oid", n = 1){
  all_unique_ids <- d %>%
    pull(idvar) %>%
    unique()
  a_random_id <- sample(all_unique_ids, size = n)
  return(a_random_id)
}
# ds_is %>% draw_random_id()
# ds_ea %>% draw_random_id()

# see https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
# for details on using `.data[[]]` pronoun
get_sample <- function(
  d
  ,sample_size=10
  , idvar = "PERSON_OID"
  , uniques = TRUE # if TRUE, only unique values are sampled
  ,seed = 42
){
  # browser()
  sampling_universe <- d %>% pull(.data[[idvar]])
  if(uniques){
    sampling_universe <- sampling_universe %>% unique()
  }
  x_out <- sample(
    x        = sampling_universe
    ,size    = sample_size
    ,replace = FALSE
  )
  return(x_out)
}
#+ load-data -------------------------------------------------------------------
ds_time <- readr::read_csv(path_time)
ds_admin <- readr::read_rds(path_admin)

#+ inspect-data ----------------------------------------------------------------

ds0_time %>%
  explore::describe_all()

ds_admin %>%
  explore::describe_all()


#+ tweak-data-0 ----------------------------------------------------------------
# Let us create a classification of hromadas with respect to their temporal trajectory
# type 1 - "Stable" - hromadas that were formed (in any year) and did NOT change their composition 
# type 2 - "Last Minute" hromadas that were formed in 2020 (2020-08-16)and have no history of changes of their composition prior to that point in time
# type 3 - "Dynamic" - all other hromadas 

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
      ,stable_composition ~ "Stable"
      ,TRUE ~ "Dynamic"
    )
  ) %>% 
  # print(n = 100)
  left_join(
    ds_admin
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

d_event_order %>%  # to verify specific hromadas as example cases
  filter(hromada_code %in% c("UA14160270000099007","UA18080210000038722","UA07020050000082369"))

d_event_order %>% count(as_factor(event_order)) # Interesting!!



ds1 <- 
  ds0 %>% 
  left_join(d_event_order,by = c("hromada_code", "date")) %>% 
  relocate(c("event_order", "event_first", "event_last"), .after = "event_count")
  
ds1 %>% glimpse()
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
d <- 
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
  ) %>%  # add oblast-level helpers
  left_join(
    ds_admin %>%
      select(c("oblast_name","region"),ends_with("en")) %>% 
      distinct()
    ,by = "oblast_name"
  ) %>% # add display labels
  mutate(
    oblast_name_display = paste()
  )
g <-
  d %>% 
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
    title     = "Types of composition of hromadas over time"
    ,subtitle = "Stable - initial and final compositions are identical/nLast "
    ,y        = "Number of radas in the hromada"
    ,x        = "Retrospective classification of hromadas' trajectory"
    ,color    = "Composition Type"
    ,fill     = "Composition Type"
  )
g %>% 
  quick_save("3-composition-type",w=12, h=8)

# ---- graph-4 -----------------------------------------------------------------
# bar graph
ds0 %>% glimpse()
d <- 
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
  ) 
g <-
  d %>% 
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
    title     = "Types of composition of hromadas over time"
    ,subtitle = "Stable - initial and final compositions are identical/nLast "
    ,y        = "Number of radas in the hromada"
    ,x        = "Retrospective classification of hromadas' trajectory"
    ,color    = "Composition Type"
    ,fill     = "Composition Type"
  )

g %>% 
  quick_save("4-composition-type",w=12, h=8)

#+ ------------------------------------

