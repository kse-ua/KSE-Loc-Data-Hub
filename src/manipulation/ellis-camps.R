# Load required libraries
library(sf)
library(dplyr)
library(readxl)

# setwd('D:/stuff/code/camps')
# Set the paths
map_path <- "./ADMIN_1.shp"
excel_path <- "./Camps and ghettos.xlsx"

# Import shapefile
shape_data <- st_read(map_path)

# Import Excel data
excel_data <- readxl::read_excel(excel_path, sheet = "Sheet1")

# Select columns A to P
excel_data <- excel_data[, 1:16]

# Remove rows where column A is empty
excel_data <- excel_data[!is.na(excel_data$`Тип населеного пункту`), ]

# Rename columns
colnames(excel_data)[2] <- "oblast"
colnames(excel_data)[4] <- "name"
colnames(excel_data)[5] <- "X"
colnames(excel_data)[6] <- "Y"
colnames(excel_data)[7] <- "prison_str"
colnames(excel_data)[15] <- "nprisoners"
colnames(excel_data)[13] <- "ncausalities"

# Format columns
excel_data$ncausalities <- ifelse(excel_data$ncausalities == "1500-3000", "3000", excel_data$ncausalities)
excel_data$ncausalities <- ifelse(excel_data$ncausalities == "2000-3000", "3000", excel_data$ncausalities)
excel_data$nprisoners <- ifelse(excel_data$nprisoners == "1500-3000", "3000", excel_data$nprisoners)

# Create 'ghetto' and 'prison' columns
excel_data$ghetto <- ifelse(excel_data$prison_str == "гетто", 1, 0)
excel_data$ghetto <- ifelse(excel_data$prison_str == "гетто, табір примусової праці для євреїв", 1, excel_data$ghetto)
excel_data$ghetto <- ifelse(excel_data$prison_str == "тюрма, гетто", 1, excel_data$ghetto)
excel_data$ghetto <- ifelse(grepl("гетто", excel_data$prison_str), 1, excel_data$ghetto)

excel_data$prison <- ifelse(excel_data$prison_str == "тюрма", 1, 0)
excel_data$prison <- ifelse(excel_data$prison_str == "тюрма, гетто", 1, excel_data$prison)
excel_data$prison <- ifelse(grepl("тюрма", excel_data$prison_str), 1, excel_data$prison)

# Convert columns to numeric
excel_data$ncausalities <- as.numeric(excel_data$ncausalities)
excel_data$nprisoners <- as.numeric(excel_data$nprisoners)

# Remove rows without coordinates
ds1 <- excel_data %>% filter(!is.na(X) | !is.na(Y)) %>% 
  mutate(ghetto = as.factor(ghetto))


# excel_sf <- st_as_sf(ds1, coords = c("X", "Y"), crs = st_crs(shape_data))
excel_sf <- st_as_sf(ds1, coords = c("Y", "X"), crs = st_crs(shape_data))

shape_data <- st_make_valid  (shape_data)

tm_shape(excel_sf) +
  tm_dots("ghetto") +
  tm_shape(shape_data) +
  tm_borders()

merged_data <- st_join(excel_sf, 
                        shape_data %>% select(NAME_LAT, KOATUU)
                       )

merged_data1 <- as_tibble(merged_data) %>% 
  select(-geometry) %>% 
  mutate(lat = ds1$X, lon = ds1$Y)


openxlsx::write.xlsx(merged_data1 , "./camps_ghettos_merged.xlsx")

