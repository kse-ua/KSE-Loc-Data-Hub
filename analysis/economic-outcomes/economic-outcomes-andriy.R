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
library(dplyr)     # data wrangling 
library(tidyr)     # tidy data
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/economic-outcomes/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_economics_wide    <- "./data-private/derived/economics-wide.csv"    # product of ./manipulation/ellis-economics.R
path_economics    <- "./data-private/derived/economics.csv"    # product of ./manipulation/ellis-economics.R
path_admin        <- "./data-private/derived/ua-admin-map.rds" # product of ./manipulation/ellis-ua-admin.R
path_time         <- "./data-private/derived/time_rada.csv"    # product of ./manipulation/ellis-rada-hromada.R

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds_economics    <- readr::read_csv(path_economics)
ds_economics_wide    <- readr::read_csv(path_economics_wide)
ds_admin        <- readr::read_rds(path_admin)
ds_time         <- readr::read_csv(path_time)
# ---- inspect-data ------------------------------------------------------------

ds_economics %>% glimpse(80)  # available economic indicators
ds_economics_wide %>% glimpse(80)
ds_time      %>% glimpse()  # date when a rada joined a hromada
ds_admin      %>% glimpse() # map of codes and labels settlement-hromada-raion-oblast


ds_admin %>% filter(hromada_code %in% c("UA80", "UA85"))
ds_time %>% filter(hromada_code %in% c("UA80", "UA85"))
ds_economics_wide %>% filter(hromada_code %in% c("UA80", "UA85"))

# ---- tweak-data --------------------------------------------------------------
ds0_wide <- 
  ds_economics_wide %>% 
  mutate(
    hromada_type2 = case_when(
      hromada_type == "міська територіальна громада" ~ "urban"
      ,hromada_type == "міська територіальна громада (ТОТ)*" ~ "urban"
      ,hromada_type == "сільська територіальна громада" ~ "rural"
      ,hromada_type == "селищна територіальна громада" ~ "rural"
      ,hromada_type == "селищна територіальна громада (ТОТ)*" ~ "rural"
      ,TRUE ~ NA_character_
    )
    ,hromada_type3 = case_when(
      hromada_type == "міська територіальна громада" ~ "urban"
      ,hromada_type == "міська територіальна громада (ТОТ)*" ~ "urban"
      ,hromada_type == "сільська територіальна громада" ~ "rural"
      ,hromada_type == "селищна територіальна громада" ~ "rural+"
      ,hromada_type == "селищна територіальна громада (ТОТ)*" ~ "rural+"
      ,TRUE ~ NA_character_
    )
    ,tot = case_when(
      tot == "так" ~ TRUE
      ,is.na(tot) ~ FALSE
      ,TRUE ~ NA
    )
  ) 

ds0_wide %>% 
  group_by(hromada_type, tot, hromada_type2, hromada_type3) %>%
  tally()

ds0_wide %>% glimpse()

ds0_long <- 
  ds_economics %>% 
  left_join(
    ds0_wide %>% select(hromada_code, hromada_type, tot)
  ) %>% 
  relocate("hromada_type", .after = "hromada_code")
ds0_long %>% glimpse()

ds0_wide %>% glimpse()
ds0_long %>% glimpse()

# ---- table-1 -----------------------------------------------------------------
# ds_economics %>% filter(hromada_code == "UA80000000000093317") %>% View()
ds_time %>% glimpse()
# d0 <- ds_time %>%   filter(hromada_code %in% c("UA14160270000099007","UA18080210000038722","UA07020050000082369"))
# d0 %>% print_all()
d0 <- ds_time

d1 <- 
  d0 %>% 
  filter(date < as.Date("2020-01-01")) %>%
  group_by(hromada_code) %>% 
  mutate(
    max_date = max(date, na.rm =T)==date
  ) %>% 
  ungroup() %>% 
  filter(max_date)
 
d2 <- 
  d0 %>% 
  group_by(hromada_code) %>% 
  mutate(
    max_date = max(date, na.rm =T)==date
  ) %>% 
  ungroup() %>% 
  filter(max_date)
d2
d1

d_change <-
  d2 %>% select(date, rada_code, hromada_code) %>% 
  left_join(
    d1 %>% select(hromada_code_20 = hromada_code, rada_code)
    ,by = "rada_code"
  ) %>% 
  group_by(hromada_code) %>% 
  mutate(
    changed_since_2020 = sum(is.na(hromada_code_20),na.rm=T)>0L
  ) %>% 
  ungroup() %>%
  distinct(hromada_code, changed_since_2020)

# d_change %>% print_all()


# ---- graph-1 -----------------------------------------------------------------
ds0_long %>% distinct(metric)
d <- 
  ds_economics %>% 
  filter(metric %in% c("tax_revenue")) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    metric = paste0(metric,"_",time)
  ) %>% 
  select(-c("time")) %>% 
  pivot_wider(
    names_from   = "metric"
    ,values_from = "value"
  ) %>% 
  left_join(
    ds0_wide %>% select(hromada_code, hromada_type2, hromada_type3, tot)
  ) %>% 
  mutate(
    tax_revenue_2020_kuah = tax_revenue_2020/1000
    ,tax_revenue_2021_kuah = tax_revenue_2021/1000
  ) %>% 
  left_join(ds_admin %>% select(!starts_with("settlement")) %>% distinct() ) %>% 
  filter(!is.na(hromada_name)) %>% 
  left_join(d_change)

d %>% glimpse()

g <- 
  d %>% 
  filter(hromada_type3 !="urban") %>% 
  # filter(changed_since_2020) %>%
  filter(!changed_since_2020) %>%
  filter(!tax_revenue_2021_kuah==max(tax_revenue_2021_kuah)) %>% 
  ggplot(
    aes(
      x=tax_revenue_2020_kuah
      , y=tax_revenue_2021_kuah
      , color=region_ua
      , fill = region_ua
      , shape = hromada_type3
    )
  )+
  # geom_point(shape = 21, fill = NA)+
  geom_point()+
  scale_shape_manual(values = c("rural"=1, "rural+"=3))+
  # facet_wrap(facets = "oblast_name_display", scales = "free")+
  facet_wrap(facets = "oblast_name_display", scales = "fixed")+
  scale_y_continuous(
    labels = scales::comma_format()
    ,limits = c(0,505)
  )+
  scale_x_continuous(
    labels = scales::comma_format()
    ,limits = c(0, 505)
    )+
  labs(
    # title = "Tax Revenue among hromadas that changed composition since 2020-01-01"
    title = "Tax Revenue among hromadas that DID NOT changed composition sicne 2020-01-01"
  )

# g %>% quick_save("1-outcome-scatterplot-changed", w=12, h = 9)
g %>% quick_save("1-outcome-scatterplot-same", w=12, h = 9)
# ---- graph-2 -----------------------------------------------------------------
# distribution of tax revenue across regions 

d <- 
  ds0_wide %>% 
  # filter(time %in% 2020:2021) %>% #glimpse()
  filter(time %in% 2021) %>% #glimpse()
  select(hromada_code, hromada_type3, tot, time, tax_revenue) %>%
  left_join(
    ds_admin %>% select(hromada_code, region_ua)
  ) %>% 
  group_by(time) %>% 
  mutate(
    percentile = dplyr::ntile(tax_revenue, 100 )
  ) %>% 
  filter(percentile < 95)
g <-
  d %>% 
  {
    ggplot(
      .
      ,aes(
        x = tax_revenue
        # ,fill = as.factor(time)
        # ,fill = region_ua
        # ,color = time
      )
    )+
    geom_density( alpha = .1)+
    # facet_wrap(facets = "time")+
    facet_wrap(facets = "region_ua")+
    scale_x_continuous(label = scales::comma_format())+
    scale_y_continuous(label = scales::percent_format())+
    labs()
  }
g
g %>% quick_save("2-regions-tax", w=12, h=6)

# ---- graph-3 -----------------------------------------------------------------

d_boxplot <- 
  ds_economics %>% 
  filter(metric %in% c("tax_revenue")) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    metric = paste0(metric,"_",time)
  ) %>% 
  select(-c("time")) %>% 
  pivot_wider(
    names_from   = "metric"
    ,values_from = "value"
  ) %>% 
  # left_join(
  #   ds0_wide %>% select(hromada_code, hromada_type2, hromada_type3, tot)
  # ) %>% 
  mutate(
    tax_revenue_2020_kuah = tax_revenue_2020/1000
    ,tax_revenue_2021_kuah = tax_revenue_2021/1000
  ) %>% 
  # glimpse()
  # left_join(ds_admin %>% select(!starts_with("settlement")) %>% distinct() ) %>% 
  # filter(!is.na(hromada_name)) %>% 
  left_join(d_change) %>% #glimpse()
  # filter(time %in% c(2020:2021)) %>% 
  mutate(
    delta = tax_revenue_2021_kuah - tax_revenue_2020_kuah
    ,delta_prop = delta/tax_revenue_2020_kuah
    ,delta_pct = delta_prop %>% scales::percent(accuracy = .01)
  ) %>% 
  left_join(
    ds0_wide %>% distinct(hromada_code, hromada_type2, hromada_type3, tot)
  ) %>% 
  left_join(ds_admin %>% distinct(hromada_code, region_ua))

d_boxplot %>% glimpse()
  
g3 <- 
  d_boxplot %>% 
  {
    ggplot(
      .
      ,aes(
        x = changed_since_2020
        ,y = delta_prop
        # , color = tot
        # , fill = tot
      )
    )+
      geom_boxplot()+
      geom_jitter(
        shape = 21, alpha = .4
      )+
      # facet_wrap(facets = "hromada_type3")
      facet_grid(hromada_type3 ~ region_ua, scales = "fixed")
  }
g3 
g3 %>% quick_save("3-growth-and-change", h=7, w=14)
# conclusion
# we observe that those hromadas thave have changed their composition since 2020-01-01
# appear to report higher growth when compared to 2021
# we think this is an artifact of the metric:  the contribution rada which didn't belong to hromada
# before 2020-08-16 might have been discounted in the final count of the tax contribution for that hromada


# ---- graph-4 -----------------------------------------------------------------
ds0_wide %>% glimpse()
ds0_wide %>% filter(hromada_code %in% c("UA85", "UA80"))

d <-
  ds0_wide %>% 
  select(hromada_code, hromada_type3, time, outcome =  tax_revenue) %>% 
  filter(time == 2021)
d

g <- 
  d %>% 
  mutate(
    percentile = dplyr::ntile(outcome, 100 )
    ,pctl_group = case_when(
      percentile > 99 ~ "top 1%"
      ,percentile > 95 ~ "top 2-5%"
      ,TRUE ~ "bottom 95%"
    ) %>% fct_relevel(c("top 1%","top 2-5%", "bottom 95%"))
    ,hromada = hromada_code %>% as_factor() %>% fct_reorder2( percentile,desc(outcome))
    ,outcome = outcome/1000000
  ) %>% 
  left_join( ds_admin %>% distinct(hromada_code, hromada_name, region_ua)) %>%  
  {
    ggplot(
      .
      ,aes(
        x = outcome
        ,y = hromada
        ,fill = region_ua
      )
    )+
      geom_col()+
      geom_text(aes(label = hromada_name, x =-Inf), hjust = -.1,  data=. %>% filter(pctl_group=="top 1%"))+
      geom_text(aes(label = hromada_name, x =-Inf), hjust = -.1,  data=. %>% filter(pctl_group=="top 2-5%"), alpha = .3)+
      facet_wrap(facets = "pctl_group", scales = "free")+
      scale_x_continuous(labels = scales::comma_format())+
      theme(
        axis.text.y = element_blank()
      )+
      labs(
        x = "Tax Revenue (Million UAH)"
        ,fill = "Region"
        ,title = "Contribution of tax revenue (million UAH) by hromada in 2021"
      )
  }
g
g %>% quick_save("4-tax-contribution",h=8,w=10)
# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/economic-outcomes/economic-outcomes.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
