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
Sys.setlocale("LC_CTYPE", "ukr")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(gptstudio) 


'%ni%' <- Negate(`%in%`)

#+ load-data -------------------------------------------------------------
path_partnerships <- "./data-private/raw/partnerships.xlsx"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"
path_admin_old <- "./data-public/derived/ua-admin-old.csv"


names_partnerships <- c(
  "register_number", "contract_name", "register_date", "contract_date", "municipalities",
  "main_municipality", "cooperation_form", "cooperation_period", "changes"
)

types_partnerships <- c("text", "text", "date", "skip", "text", "text", "text", "text", "text")

ds0_partnerships <- readxl::read_excel(path_partnerships
                                       ,col_names = names_partnerships
                                       ,col_type = types_partnerships
                                       ,skip = 1)

ds_admin <- readr::read_csv(path_admin)
ds_admin_old <- readr::read_csv(path_admin_old)



ds0 <- ds0_partnerships %>% 
  separate(cooperation_period, into = c('start', 'end'), sep = "–|-|до|упродовж|по|на", remove = F) %>% 
  mutate(
    years = str_extract(end, "\\d(?= років)")
    ,month = str_extract(end, "\\d+(?= місяців)")
    ,start = str_remove(start, "З |з ")
  )

# openxlsx::write.xlsx(ds0, "./data-private/raw/partnerships_dates_coding.xlsx")  
ds0 <- readxl::read_xlsx("./data-private/raw/partnerships_dates_coded.xlsx") %>% 
  mutate(
    start = ifelse(nchar(start) == 5, as_date(as.numeric(start), origin = "1899-12-30"), as_date(start, format = "%d.%m.%Y"))
    ,end = ifelse(nchar(end) == 5, as_date(as.numeric(end),origin = "1899-12-30"), as_date(end, format = "%d.%m.%Y"))
    ,end = ifelse(is.na(end), today(), end)
    ,start = as_date(start)
    ,end = as_date(end)
  ) %>% 
  select(-changes, -cooperation_period)



#+ tweak-data, eval=eval_chunk ----------------------------------------------------------------

#select cities and urban-type settlements with uniques names when combining with oblast name
unique_urban <- ds_admin_old %>% 
  filter(category_label %in% c("місто", "селище міського типу")) %>% 
  mutate(n = n(), .by = c(oblast_name, category_label, settlement_name)) %>% 
  mutate(key = paste(oblast_name, category_label, settlement_name)) %>% 
  filter(n==1) %>% 
  select(key, settlement_code)



#function to derive oblast, raion and settlement name from each string
admin_names <- function(x) {
  # extract the information from the column
  # settlement <- str_extract(x, "(?:с\\.|м\\.|смт)\\s+([\\p{L}\\d\\-'’]+)")
  settlement <- str_extract(x, "(?:с\\.|м\\.|смт|смт\\. |с\\. |м\\. )\\p{L}+(\\s?|-?)\\p{L}+")
  raion <- str_extract(x, "([\\p{L}\\d\\-']+?)\\s*р-н")
  oblast <- str_extract(x, "([\\p{L}\\d\\-']+?)\\s*обл")
  # return a data frame with the extracted values as columns
  data.frame(settlement = settlement, raion = raion, oblast = oblast)
}

  
ds1_partnerships <- 
  ds0 %>% 
  separate(municipalities, into = paste0("municipality_", 1:34), sep = "(?<=обл.)") %>% 
  mutate_at(vars(starts_with("municipality_")), .funs = admin_names) %>% 
  unnest(starts_with("municipality_"), names_sep = "_") 




# mutate(
#   register_date = as_date(register_date)
#   ,cooperation_period = str_squish(cooperation_period)
#   ,duration = case_when(
#     str_detect(cooperation_period, "(?<=упродовж\\s)\\d+(?=\\s+рок)") ~ as.numeric(str_extract(cooperation_period, "(?<=упродовж\\s)\\d+(?=\\s+рок)")) 
#     ,str_detect(cooperation_period, "(?<=упродовж\\s)\\d+(?=\\s+місяц)") ~ as.numeric(str_extract(cooperation_period, "(?<=упродовж\\s)\\d+(?=\\s+місяц)"))/12 
#   )
# )

# create a list of data frames, each containing a sequence of 3 admin columns
municipality_cols <- ds1_partnerships %>% 
  select(starts_with("municipality_")) %>% 
  colnames()

ds2_partnerships <-
  ds1_partnerships %>% 
  pivot_longer(cols = starts_with("municipality_"),
               names_to = c("municipality_num", "name"),
               names_pattern = "municipality_(\\d+)_(.*)") %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  filter(!is.na(settlement)) %>% 
  mutate(
    raion = str_remove(raion, " р-н")
    ,raion = str_replace(raion, "ого", "ий")
    ,raion = case_match(
      raion 
      ,"Мелітопільський" ~ "Мелітопольський"
      ,"Шевченківськи" ~ "Шевченківський"
      ,"Лановицький" ~ "Лановецький"
      ,"янка-Бузький" ~ "Кам’янка-Бузький"
      ,"Полтвський" ~ "Полтавський"
      ,"янець-Подільський" ~ "Кам’янець-Подільський"
      ,"Таврівський" ~ "Тиврівський"
      ,"Пустопитівський" ~ "Пустомитівський"
      ,"Білгород-Дністровький" ~ "Білгород-Дністровcький"
      ,"Білгород-Дістровський" ~ "Білгород-Дністровський"
      ,"Вінницкий" ~ "Вінницький"
      ,"Глухівськаий" ~ "Глухівський"
      ,"Дрийбицький" ~ "Дрогобицький"
      ,"Новоселький" ~ "Новоселицький"
      ,"Переяслів-Хмельницький" ~ "Переяслав-Хмельницький"
      ,"Самбірськи" ~ "Самбірський"
      ,"Твальнівський" ~ "Тальнівський"
      ,"Уманьський" ~ "Уманський"
      ,"Твальнівський" ~ "Тальнівський"
      ,"полянський" ~ "Полянський"
      ,.default = raion
    )
    ,raion = str_replace_all(raion, c("’"="’", "'"="’", "’"="’", "'"="’"))
    ,oblast = str_remove(oblast, " обл.+| обл")
    ,oblast = str_replace(oblast, "ої", "а")
    ,oblast = case_match(
      oblast
      ,"Полтавсбка" ~ "Полтавська"
      ,"Полтавськ" ~ "Полтавська"
      ,"Сумськ" ~ "Сумська"
      ,"Харкывська" ~ "Харківська"
      ,"Внницька" ~ "Вінницька"
      ,"Віницька" ~ "Вінницька"
      ,"Вінницьк" ~ "Вінницька"
      ,"Вінницькаобл" ~ "Вінницька"
      ,"Вінниціка" ~ "Вінницька"
      ,"Закарпатьська" ~ "Закарпатська"
      ,"Київсбка" ~ "Київська"
      ,"Львівськаобл" ~ "Львівська"
      ,"Львівуська" ~ "Львівська"
      ,"Лівівська" ~ "Львівська"
      ,"Рувненська" ~ "Рівненська"
      ,.default = oblast
    )
    ,key = case_when(
      str_detect(settlement, "м. |смт ") == T ~ paste(oblast, settlement)
      ,str_detect(settlement, "м. |смт ") == F ~ paste(oblast, raion, settlement)
    )
  ) 
  #TO-DO: add check for uniqness of settlements and cities


#+ tweak-admin-data, eval=eval_chunks ------------------------------------------------

#select settlements with unique combination of oblast, raion, type and settlement name

ds1_admin_old <- 
  ds_admin_old %>% 
  mutate(
    settlement_type = case_match(
      category_label
      ,"місто" ~ "м."
      ,"село" ~ "с."  
      ,"селище" ~ "c-ще"  
      ,"селище міського типу" ~ "смт"  
    )
    ,key = case_when(
      settlement_type %in% c("смт", "м.") ~ paste(oblast_name, settlement_type, settlement_name)
      ,settlement_type %ni% c("смт", "м.") ~ paste(oblast_name, raion_name, settlement_type, settlement_name)
    )
  ) %>% 
  mutate(n = n(), .by = key) %>% 
  filter(n==1)
  

#+ combine, eval=eval_chunks ------------------------------------------------

ds3_partnerships <- ds2_partnerships %>% 
  mutate(
    key = str_replace_all(key, c("с\\."="с\\. ", "смт\\." = "смт", "м\\."="м\\. "))
    ,key = str_squish(key)
  ) %>% 
  left_join(
    ds1_admin_old %>% select(key, settlement_name, settlement_code)
    ,by = "key"
  )

unmatched <- ds3_partnerships %>% filter(is.na(settlement_code)) 

#save for coding
# openxlsx::write.xlsx(unmatched, "./data-private/raw/unmatched_partnerships_coding.xlsx")

coded <- readxl::read_excel("./data-private/raw/unmatched_partnerships_coded.xlsx")

ds4_partnerships <- ds3_partnerships %>% 
  filter(!is.na(settlement_code)) %>% 
  rbind(coded) %>% 
  mutate(
    settlement_code = case_when(
      str_detect(settlement_code, "^(72|52)") ~ paste0(0, settlement_code)
      ,.default = settlement_code
    )
  ) %>% 
  left_join(
    ds_admin %>% select(hromada_code, hromada_name, settlement_code_old)
    ,by = c("settlement_code"="settlement_code_old")
  )



#leave only hromadas (collapse settlements) and only agreements signed before 24.02.2022
ds5_partnerships <- ds4_partnerships %>% 
  filter(!is.na(start)) %>% 
  distinct(register_number, start, end, hromada_code, hromada_name) %>% 
  mutate(active_2402 = ifelse(end >= "2022-02-24", 1,0)) %>% 
  filter(start < "2022-02-24") #do not account agreements started after 24.02.2022


#+ aggregate on the hromada level ------------------------------------------------

#count number of agreements for each hromada for all time and number of active agreements for each hromada as of 24.02
nagreements <- ds5_partnerships %>% 
  group_by(hromada_code) %>% 
  summarise(n_agreements = n(), n_agreements_active = sum(active_2402))


#count the number of unique hromadas with which each hromada has had experience of cooperation
nagreements_hromadas <- ds5_partnerships %>% 
  inner_join(ds5_partnerships, by = "register_number") %>%
  filter(hromada_code.x != hromada_code.y) %>% 
  group_by(hromada_code.x) %>%
  summarise(n_agreements_hromadas = n_distinct(hromada_code.y)) %>% 
  rename(hromada_code = hromada_code.x)

#count the number of unique hromadas with which each hromadas had active agreements as of 24.02
nagreements_hromadas_active <- ds5_partnerships %>% 
  filter(active_2402 == 1) %>% 
  inner_join(ds5_partnerships %>% filter(active_2402 == 1), by = "register_number") %>% 
  filter(hromada_code.x != hromada_code.y) %>%
  group_by(hromada_code.x) %>%
  summarise(n_agreements_hromadas_active = n_distinct(hromada_code.y)) %>% 
  rename(hromada_code = hromada_code.x)

#combine all in one dataset
ds6_partnerships <- ds_admin %>% 
  filter(oblast_name != "Автономна Республіка Крим") %>% 
  distinct(hromada_code, hromada_name) %>% 
  left_join(nagreements, by = "hromada_code") %>% 
  left_join(nagreements_hromadas, by = "hromada_code") %>% 
  left_join(nagreements_hromadas_active, by = "hromada_code") %>% 
  mutate_at(vars(-hromada_code, -hromada_name), ~replace(., is.na(.), 0))

#+ save ------------------------------------------------
#save full dataset will all settlements, agreements and their descriptions
readr::write_csv(ds4_partnerships, "./data-public/derived/partnerships-all-settlements.csv")

#save aggregated data on agreements per each hromada
readr::write_csv(ds6_partnerships, "./data-public/derived/partnerships-hromadas.csv")


#save the dataset with hromada pairs for network analysis

hromadas_network <- ds5_partnerships %>% 
  filter(active_2402 == 1) %>% 
  inner_join(ds5_partnerships %>% filter(active_2402 == 1), by = "register_number") %>% 
  filter(hromada_code.x != hromada_code.y) %>% 
  select(-start.y, -end.y, -active_2402.y) %>% 
  rename(start = start.x, end = end.x, active_2402 = active_2402.x) %>% 
  relocate(active_2402, .after = end)

readr::write_csv(hromadas_network, "./data-public/derived/partnerships-hromadas-network.csv")


                            