#new - old admin
library(tidyverse)
base <- readr::read_csv("~/Documents/ua-de-center/data-public/derived/ua-admin-map.csv") 

base_check <- base %>% select(settlement_name, settlement_code, hromada_name, hromada_code , raion_name, raion_code, 
                              raion_code_old, raion_name_old)

base_check <- base_check %>% group_by(raion_code_old, hromada_code) %>% 
  mutate(former_raion = 0)

base_check %>% filter(is.na(raion_name_old)) %>% view()

same_city_regions <- list()

# Iterate through unique old regions
unique_old_regions <- unique(base_check$raion_name_old)
for (old_region in unique_old_regions) {
  # Extract cities associated with the current old region
  cities_in_old_region <- base_check$settlement_code[base_check$raion_name_old == old_region]
  
  # Extract unique new regions for these cities
  unique_new_regions <- unique(base_check$hromada_code[base_check$settlement_code %in% cities_in_old_region])
  
  # Add the current old region and its corresponding unique new regions to the list
  same_city_regions[[old_region]] <- unique_new_regions
}

# fill in the results
for (old_region in unique_old_regions) {
  if (length(same_city_regions[[old_region]]) == 1) {  
    base_check$former_raion[base_check$raion_name_old == old_region] <- 1
  }
  if (is.na(old_region)) {
    base_check$former_raion[base_check$raion_name_old == old_region] = 0
  }
}

file_path <- "/ua-de-center/data-public/derived/former_raions.csv"

# Save the dataset to a CSV file
write.csv(base_check, file = file_path, row.names = FALSE)
