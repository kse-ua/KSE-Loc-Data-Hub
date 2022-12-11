
#+ load-all-datasets -------------------------------------------------------------
#main datasets

# Where to obtain data sets

# data-private/raw       - from shared Google drive folder `data-private/raw` (https://drive.google.com/drive/u/0/folders/1awYh3F0gc0bWqEmjkTMDMOfFi8Io2aRo)
# data-private/derived   - by running a respective ellis script in ./manipulation/ 
# data-public/derived    - magically appear on your machine when you clone repo

path_heads                <- "./data-private/raw/hromada_heads.xlsx"

path_hromada              <- "./data-private/derived/hromada.csv"                     # ellis-admin-ua.R
path_time                 <- "./data-private/derived/time_rada.csv"                   # ellis-rada-hromada.R #TO-DO: check the dates
path_demography           <- "./data-private/derived/ua-pop-2022.csv"                 # ellis-demography.R    
path_osbb                 <- "./data-private/derived/osbb-hromada.csv"                # ellis-osbb.R         
path_zno                  <- "./data-private/derived/zno-2022-aggragated.csv"         # ellis-zno.R
path_dfrr                 <- "./data-private/derived/dfrr_hromadas.csv"               # ellis-dfrr.R
path_edem                 <- "./data-private/derived/edem-data.csv"                   # ellis-edem.R
path_community_competence <- "./data-private/derived/community-competence-hromada.csv"# ellis-community-competence.R
path_war                  <- "./data-private/derived/minregion-war-status.csv"        # ellis-war-status.R

path_admin                <- "./data-public/derived/ua-admin-map-2020.csv"            # ellis-ua-admin.R
path_geography            <- "./data-public/derived/geography.csv"                    # ellis-geography.R
path_budget_income        <- "./data-public/derived/hromada_budget_2020_2022.xlsx"    # ellis-budget-2020-2022.R
path_declarations         <- "./data-public/derived/declarations-hromada.csv"         # ellis-health.R

# path_budget_expences <- 

#additional datasets
path_polygons             <- "./data-private/raw/terhromad_fin.geojson"
path_oblast               <- "./data-private/raw/oblast.csv"
path_passangers           <- "./data-private/derived/passangers.csv"                  # ellis-uz.R

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds_heads <- readxl::read_xlsx(path_heads) # missing

ds_hromada                <- readr::read_delim(path_hromada)
ds_time                   <- readr::read_csv(path_time) #TO-DO: check the dates
ds_demography             <- readr::read_csv(path_demography) 
ds_osbb                   <- readr::read_csv(path_osbb)
ds_zno                    <- readr::read_csv(path_zno)
ds_dfrr                   <- readr::read_csv(path_dfrr)
ds_edem                   <- readr::read_csv(path_edem)
ds_community_competence   <- readr::read_csv(path_community_competence) %>%
  janitor::clean_names() #TODO: check NAs
ds_war                    <- readr::read_csv(path_war)
 

ds_admin                  <- readr::read_csv(path_admin)
ds_geography              <- readr::read_csv(path_geography)
ds_budget_income          <- readxl::read_xlsx(path_budget_income)
ds_declarations           <- readr::read_csv(path_declarations)

# ds_budget_expences <- readr::read_csv(path_budget_expences)
ds_oblasts                <- readr::read_csv(path_oblast)
ds_passangers             <- readr::read_csv(path_passangers)

