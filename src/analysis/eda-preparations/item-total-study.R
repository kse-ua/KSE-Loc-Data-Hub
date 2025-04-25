rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
library(tidyverse)
base::source("./scripts/graphing/graph-presets.R")     
base::source("./scripts/common-functions.R")  
prints_folder <- paste0("./analysis/eda-preparations/prints/item-total/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# What is wrong with item-total correlation ? 
ds_survey <- readxl::read_excel("./data-private/derived/survey_hromadas_clean.xlsx")

# --- function ------
make_corr_matrix <- function(d,na_action="remove", method="pearson"){
  
  item_names <- names(d)
  # browser()
  d1 <- 
    d %>% 
    select(all_of(item_names)) %>% 
    mutate(
      across(
        .cols = everything()
        ,.fns = ~as.numeric(.)
      )
    ) 

  if(na_action == "remove"){
    d2 <- d1 %>% drop_na()
  }

  if(na_action == "replace"){
    d2 <-
      d1 %>%
      mutate(
        across(
          .cols = everything()
          ,.fns = ~replace_na(.,0L)
        )
      )
  }
  cormat <- cor(d2,method = method)
  # row.names(cormat) <- item_names
  return(cormat)
}
preparation <- 
  ds_survey %>% 
  select(starts_with("prep_"), -prep_winter_count, -prep_count) %>% 
  colnames() %>% 
  print()


# ----- prep-scores --------------------
# compute all scores (prep_feb, prep_oct, combo), but correlate with unaltered 

d1 <- 
  ds_survey %>% 
  mutate(
    # sum of 0|1|2 where larger numbers indicate more preparedness
    prep_score_combo = rowSums(across(all_of(preparation)),na.rm = T) 
    ,prep_score_feb = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 0 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
    ,prep_score_oct = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 1 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
  )  %>% 
select(hromada_code, starts_with("prep_score"))
d1  
d1 %>% select(-1) %>% GGally::ggpairs()
# ---- matrices ----------------

# As of February 2022, how many of these security steps have been implemented?
d_feb <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 0 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)


# As of October 20200, how many of these security steps have been implemented?
d_oct <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 1 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)

d_combo <- 
  ds_survey %>% 
  select(hromada_code, preparation)




# ----- prep_scores-vs-raw --------------------

discrimination_levels <- c(
  "problematic" = "#d01c8b"
  ,"poor"        = "#f1b6da"
  ,"good"        = "#b8e186"
  ,"very good"   = "#4dac26"
)

# Let's see how all three prep_scores correlate with raw scores

m_combo <- 
  d1 %>% 
  left_join(d_combo) %>% 
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

d_raw <- 
  list(
  "Combo" =  m_combo[,"prep_score_combo"]
  ,"Feb"= m_combo[,"prep_score_feb"]
  ,"Oct" = m_combo[,"prep_score_oct"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_combo)) %>% 
  filter(item_name != c("prep_score_combo","prep_score_feb","prep_score_oct")) %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name) %>% 
  pivot_longer(
    cols = 2:4
    ,names_to = "scenario"
    ,values_to = "correlation"
  ) %>% 
  mutate(
    discrimination = case_when(
      correlation <= 0                     ~ "problematic"
      ,correlation > 0 & correlation < .2  ~ "poor"
      ,correlation >=.2 & correlation < .4 ~ "good"
      ,correlation >=.4                    ~ "very good"
    ) %>% factor(levels = names(discrimination_levels))
    ,scenario = scenario %>% factor(level=c("Feb","Oct","Combo"))
    ,item_name = factor(item_name,levels = preparation) %>% fct_rev()
  ) 


g_raw <- 
  d_raw %>% 
  ggplot(aes(x = item_name, y = correlation, color = discrimination, group = scenario))+
  geom_line(aes(group = "scenario"))+
  geom_point()+
  geom_text(aes(label=correlation %>% scales::number(accuracy = .01) %>% RemoveLeadingZero()),hjust=-.3
            ,size = 3)+
  geom_hline(aes(x=0, yintercept = 0))+ 
  facet_wrap("scenario",nrow=1)+
  scale_y_continuous(limits = c(-.3,.7), expand = expansion(add = c(0,.2)))+
  scale_color_manual(
    values = discrimination_levels
    , limits = names(discrimination_levels)
  )+
  coord_flip() +
  labs(
    title = "Item-total corellations for raw scores (0|1|2) under three scoring scenarios"
    # ,subtitle = "Before = prior to Feb 24, After = at time of interview, Oct-Nov 2022, Total = Before + After"
    ,y = "Item-total Correlation (Spearman)"
    ,x = NULL
    ,color = "Discrimination"
  )
g_raw %>%  quick_save("1-item-total-raw",w=8,h=4)
# ----- prep_scores-vs-binary --------------------

# item-total correaltion on disentangled scores

m_feb <- 
  d1 %>% 
  left_join(d_feb) %>% # raw scores have been converted to binary for as of Feb
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_oct <- 
  d1 %>% 
  left_join(d_oct) %>% # raw scores have been converted to binary for as of Feb
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_combo <- 
  d1 %>% 
  left_join(d_combo) %>% 
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")



d_bin <- 
  list(
    "Combo" = m_combo[,"prep_score_combo"]
    ,"Feb"  = m_feb[,"prep_score_feb"]
    ,"Oct"  = m_oct[,"prep_score_oct"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_combo)) %>% 
  filter(item_name != c("prep_score_combo","prep_score_feb","prep_score_oct")) %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name) %>% 
  pivot_longer(
    cols = 2:4
    ,names_to = "scenario"
    ,values_to = "correlation"
  ) %>% 
  mutate(
    discrimination = case_when(
      correlation <= 0  ~ "problematic"
      ,correlation > 0 & correlation < .2 ~ "poor"
      ,correlation >=.2 & correlation < .4 ~ "good"
      ,correlation >=.4  ~ "very good"
    ) %>% factor(levels = c("problematic","poor","good","very good"))
    ,scenario = scenario %>% factor(level=c("Feb","Oct","Combo"))
    ,item_name = factor(item_name,levels = preparation) %>% fct_rev()
  )

g_bin <-
  d_bin %>% 
  ggplot(aes(x = item_name, y = correlation, color = discrimination, group = scenario))+
  geom_line(aes(group = "scenario"))+
  geom_point()+
  geom_text(aes(label=correlation %>% scales::number(accuracy = .01) %>% RemoveLeadingZero()),hjust=-.3
            ,size = 3)+
  geom_hline(aes(x=0, yintercept = 0))+ 
  facet_wrap("scenario",nrow=1)+
  scale_y_continuous(limits = c(-.3,.7), expand = expansion(add = c(0,.2)))+
  scale_color_manual(
    values = discrimination_levels
    , limits = names(discrimination_levels)
  )+
  coord_flip() +
  coord_flip() +
  labs(
    title = "Item-total corellations for binary scores (0|1) under three scoring scenarios"
    # ,subtitle = "Before = prior to Feb 24, After = at time of interview, Oct-Nov 2022, Total = Before + After"
    ,y = "Item-total Correlation (Spearman)"
    ,x = NULL
    ,color = "Discrimination"
  )

g_bin %>%   quick_save("2-item-total-bin",w=8,h=4)



png(paste0(prints_folder,"/after-total.png"),width = 12,height = 12,units = "in",res = 100)
m %>% make_corr_plot(title = "XXX")
dev.off()
  
