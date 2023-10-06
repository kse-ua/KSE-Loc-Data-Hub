library(readxl)
library(tidyverse)

# loading data
vpo0 <- read_excel("./data-public/raw/B&R__2024.xlsx", 
                         sheet = "Table 4")
vpo1 <- read_excel("./data-public/raw/B&R__2024.xlsx", 
                       sheet = "Table 5")

ds_admin <- readr::read_csv("./data-public/derived/ua-admin-map-2020.csv") 

# getting only needed
ds_code <- ds_admin %>% select(hromada_code, budget_name, budget_code) %>% distinct()

# putting together

ds_vpo <- vpo1 %>% rbind(vpo0) %>% 
  rename("budget_name" = "Назва місцевого бюджету адміністративно- територіальної одиниці", 
         "budget_code" = "Код бюджету") %>%
  mutate(budget_code = ifelse(nchar(trimws(budget_code)) != 10, 
                              sprintf("%010.0f", as.numeric(trimws(budget_code))), 
                              trimws(budget_code))) %>%
  left_join(ds_code, by = "budget_code") %>%
  rename("budget_name" = "budget_name.x") %>%
  select(-budget_name.y) %>%
  mutate(hromada_code = case_when(
    budget_name == "Бюджет Криворізької міської територіальної громади" ~ "UA12060170000091033",
    budget_name == "Бюджет Кропивницької міської територіальної громади" ~ "UA35040210000014072",
    budget_name == "Бюджет Полтавської міської територіальної громади" ~ "UA53080370000025447",
    .default = hromada_code)) %>%  
  filter(!is.na(hromada_code)) %>%
  select(c(6,7,13)) %>%
  rename(vpo_thsd = 1,
         new_population_thsd = 2)

file_path <- "./data-public/derived/vpo_budget.csv"
write.csv(ds_vpo, file = file_path, row.names = FALSE)
