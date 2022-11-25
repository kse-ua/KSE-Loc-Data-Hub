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
#+ load-sources ----------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
path_youth_councils <- "./data-private/raw/interns_data.xlsx"
path_youth_centers <- "./data-private/raw/interns_data.xlsx"
path_business_support <- "./data-private/raw/interns_data.xlsx"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
youth_councils <- readxl::read_excel(path_youth_councils, 
                                     sheet = "Youth_councils")
youth_centers <- readxl::read_excel(path_youth_centers, 
                                    sheet = "Youth_centers")
business_support <- readxl::read_excel(path_business_support, 
                                       sheet = "Entrepreneurs_support")
ds_admin <- readr::read_csv(path_admin)

## Correcting mistakes
youth_centers$hromada_name_short <- gsub("ої","а", youth_centers$hromada_name_short)
youth_centers$hromada_name_short <- gsub("'","’", youth_centers$hromada_name_short)
youth_councils$hromada_name_short <- gsub("'","’", youth_councils$hromada_name_short)
youth_councils$hromada_name_short <- gsub("'","’", youth_councils$hromada_name_short)
youth_centers<- youth_centers %>% 
  mutate(hromada_name_short = case_when(hromada_name_short=="Війтівецька"~"Жданівська",
                                     TRUE ~ hromada_name_short))
youth_councils <- youth_councils[!(youth_councils$hromada_name_short=="м. Київ"),]
youth_centers <- youth_centers[!(youth_centers$town=="Київ"),]

## Correcting mistakes
business_support$town <- gsub("'","’", business_support$town)
business_support$town <- gsub("Володимир-Волинський","Володимир", business_support$town)
business_support<-business_support[!(business_support$town=="Миколаївка"),]
business_support$town <- gsub("Запоріжжя","місто Запоріжжя", business_support$town)
ds_admin<- ds_admin %>% 
  mutate(settlement_name = case_when(settlement_name=="Запоріжжя"&settlement_type=="місто"~"місто Запоріжжя",
                                     TRUE ~ settlement_name))
business_support <- business_support[!is.na(business_support$name),]
business_support <- business_support[!(business_support$town=="Київ"),]
business_support<- business_support %>% 
  mutate(oblast_name_short = case_when(town=="місто Запоріжжя"&oblast_name_short=="Луганська"~"Запорізька",
                                     TRUE ~ oblast_name_short))


## Looking at youth councils:
table(youth_councils$oblast) # regional distribution
n_distinct(youth_councils$hromada_name_short) # number of ATCs

## Number of youth councils for each ATC in the sample 
youth_councils_atc <- youth_councils %>% group_by(oblast_name_short,
                                                  rayon_name_short,
                                                  hromada_name_short) %>%
  summarise(Youth_councils = n())
  
## Looking at youth centers:
table(youth_centers$oblast) # regional distribution
n_distinct(youth_centers$hromada_name_short) # number of ATCs
table(youth_centers$type) # distribution of centers by type

## Number of youth centers for each ATC in the sample 
youth_centers_atc <- youth_centers %>% group_by(oblast_name_short,
                                                  rayon_name_short,
                                                  hromada_name_short) %>%
  summarise(Youth_centers= n())

## Merging data sets with counted councils and centers
youth_count <- youth_centers_atc %>% 
  full_join(
    youth_councils_atc
    ,by = c("oblast_name_short" = "oblast_name_short",
            "rayon_name_short" = "rayon_name_short",
            "hromada_name_short" = "hromada_name_short")
  )

youth_count <- youth_count %>%
  mutate_all(~replace(., is.na(.),0))

## How many ATCs have both youth councils and youth centers
nrow(subset(youth_count, Youth_centers >=1 &
              Youth_councils >= 1))


## Looking at entrepreneurship support centers:
table(business_support$oblast) # regional distribution
table(business_support$town) # settlement distribution - really a lot of in Vertokyivka - Why?
n_distinct(business_support$town) # number of settlements
table(business_support$category) # distribution by type


## Merging admin data to youth centers and councils data set
admin_short <- ds_admin %>% distinct(hromada_code, .keep_all = TRUE) %>%
  select(hromada_code,
         hromada_name,
         raion_name,
         oblast_name)


youth_count_admin <- youth_count %>% 
  left_join(
    admin_short
    ,by = c("oblast_name_short" = "oblast_name",
            "rayon_name_short" = "raion_name",
            "hromada_name_short" = "hromada_name")
  )

## Merging admin data to entrepreneurship support centers
business_support <- business_support[!duplicated(business_support$name),]

business_support_merge <- business_support %>% 
  left_join(
    ds_admin
    ,by = c("town" = "settlement_name",
            "oblast_name_short" = "oblast_name")
  )


business_support_merge_duplicated <- business_support_merge %>%
  filter(,duplicated(name)) %>% distinct(name, .keep_all = TRUE) %>%
  select(oblast,
         oblast_name_short,
         settlement_type.x,
         town,
         name,
         category,
         description,
         address)

colnames(business_support_merge_duplicated)[3] <- "settlement_type"

business_supportmerge__without_duplicated <- business_support_merge[!(business_support_merge$name %in% 
                                                            business_support_merge_duplicated$name),]

business_support_merge_add <- business_support_merge_duplicated %>% 
  left_join(
    ds_admin
    ,by = c("town" = "settlement_name",
            "oblast_name_short" = "oblast_name",
            "settlement_type" = "settlement_type")
  )


business_supportmerge__without_duplicated$settlement_type.x <- business_supportmerge__without_duplicated$settlement_type.y
colnames(business_supportmerge__without_duplicated)[3] <- "settlement_type"
business_supportmerge__without_duplicated <- business_supportmerge__without_duplicated %>% select(-settlement_type.y)

business_support_merge_full <- rbind(business_supportmerge__without_duplicated, business_support_merge_add)
business_support_merge_full %>% filter(,duplicated(name)) %>%View()

business_support_atc <- business_support_merge_full %>%
  group_by(hromada_code, hromada_name) %>%
  summarise(Business_support_centers = sum(!is.na(name)))

#colnames(business_support_atc)[2] <- "hromada_name_short"

summary_community_competence <- business_support_atc %>% 
  full_join(
    youth_count_admin
    ,by = c("hromada_code" = "hromada_code",
            "hromada_name" = "hromada_name_short")
  ) %>% select(hromada_code,
               hromada_name,
               Youth_councils,
               Youth_centers,
               Business_support_centers)
summary_community_competence <- summary_community_competence[!(summary_community_competence$hromada_name=="м.Київ"),]
summary_community_competence[is.na(summary_community_competence)] <- 0

#+ save-data, eval=eval_chunks -------------------------------------------------
readr::write_csv(summary_community_competence, "./data-private/derived/community-competence-hromada.csv") #aggregated on hromada level