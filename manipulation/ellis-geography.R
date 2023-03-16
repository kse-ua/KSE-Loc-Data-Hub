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
library(tmap)
library(tmaptools)
library(osrm)
library(sf)

#+ declare-globals -------------------------------------------------------------
path_file <- "./data-private/raw/ua-admin-codes.csv"

path_oblast <- "./data-private/raw/oblast.csv"

#Data on areas and number of settlements parsed from decenrtalization.org.ua
#Source: https://docs.google.com/spreadsheets/d/19Xxsq9O7fuHdNB4_GDyEMVSWBx3ENu3x9gNnnPrkqyo/edit?usp=sharing, sheet "parsed"
path_areas <- "./data-private/raw/hromada_areas.csv"

#Shapefile of hromada polygons
#Source: https://drive.google.com/file/d/1X3Ym-7s1RgQgsi_p4uJ3EGEWYGTd-UMz/view?usp=sharing
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"

names_admin_ua <- c(
  "level_1"
  ,"level_2"
  ,"level_3"
  ,"level_4"
  ,"level_extra"
  ,"object_category"
  ,"object_name"
)

names_areas <- c(
  "oblast"
  ,"raion"
  ,"hromada_name"
  ,"hromada_type"
  ,"date_formed"
  ,"n_settlements"
  ,"square"
  ,"pop_21"
)


#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readr::read_csv(path_file, col_names = names_admin_ua, skip=1)
ds_hromada <- readr::read_csv("./data-private/derived/hromada.csv")
ds0_oblast <- readr::read_csv(path_oblast, skip=0)
ds0_areas <- readr::read_csv(path_areas, col_names = names_areas, skip=1)
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names()

#+ inspect-data ----------------------------------------------------------------

#+ tweak-data, eval=eval_chunks ------------------------------------------------
#selection of hromadas and hromada centers - they are arranged sequentially in the admin dataset
data = tibble()
i=0
for (i in 1:length(ds0$level_1)) {
  if (ds0$object_category[i] == "H"){
    data <- rbind(data, ds0[i,], ds0[i+1,])
  }
}

ds1 <- 
  data %>% 
  mutate(
    hromada_type = case_when(
      object_category == "C" ~ "сільська"
      ,object_category == "X" ~ "сільська"
      ,object_category == "M" ~ "міська"
      ,object_category == "T" ~ "селищна")
    ,object_category = case_when(
      object_category ==  "H" ~ "hromada"
      ,object_category %in% c("C", "M", "T", "X") ~ "hromada_center"
    )
  ) %>%
  pivot_wider(
    names_from = object_category
    ,values_from = object_name) %>%
  fill(hromada) %>%
  filter(is.na(hromada_center) == F)

ds2 <- 
  ds1 %>%
  left_join(
    ds_hromada %>% select(hromada_code, oblast_name, raion_name)
    ,by = c("level_3"="hromada_code")
  ) %>% 
  select(-c(level_1, level_2, level_extra)) %>%
  rename(hromada_code = level_3, hromada_center_code = level_4) %>% 
  filter(is.na(oblast_name) == F) %>%
  mutate(
    key1=paste(oblast_name,raion_name,hromada,hromada_center)
    ,key2=paste(oblast_name,hromada_center)
  )

#+ add-areas, eval=eval_chunks ------------------------------------------------
ds_areas <- 
  ds0_areas %>% 
  mutate(
    hromada_name = str_extract(hromada_name, ".+(?=\\sтериторіальна громада)")
    ,hromada_name = str_replace(hromada_name, "'", "’")
    ,oblast = str_extract(oblast, "(.+(?=\\sобласть))|(м\\.\\sКиїв)")
    ,raion = str_extract(raion, "(.+(?=\\sрайон))|(м\\.\\sКиїв)")
    ,key = paste(oblast,raion,hromada_name,hromada_type)
  )


#+ add-geocodes, eval=eval_chunks ------------------------------------------------
#add geocodes through request to API using combination of oblast_name,
# raion_name,hromada,hromada_center
ds2_geo1 <- 
  ds2 %>% 
  pull(key1) %>% 
  geocode_OSM(keep.unfound = T)

# combination of oblast_name and hromada_center for hromada centers with invalid geocodes
ds2_geo2<- 
  ds2 %>% 
  pull(key2) %>% 
  geocode_OSM(keep.unfound = T)

# geocodes for oblast centers
ds_oblast_geo <- 
  ds0_oblast %>% 
  mutate(key = paste(oblast_name, oblast_center)) %>% 
  pull(key) %>% 
  geocode_OSM(keep.unfound = T) 


#+ combine ---------------------------------------------------------------------
ds_oblast <- 
  ds0_oblast %>% 
  cbind(
    ds_oblast_geo %>% 
      mutate(
        lat = ifelse(query == "Київська Київ", "50.45183", lat)
        ,lon = ifelse(query == "Київська Київ", "30.61140", lon)
      ) %>% 
      select(lat, lon) %>% 
      rename(oblast_center_lat = lat,oblast_center_lon=lon)
  ) %>%
  filter(!oblast_name %in% c("Автономна Республіка Крим", "Севастополь"))

ds_geo <- 
  ds2 %>% 
  cbind(
    ds2_geo1 %>% select(lat, lon) %>% rename(lat1 = lat, lon1=lon)
    ,ds2_geo2 %>% select(lat, lon) %>% rename(lat2 = lat, lon2=lon)
  ) %>%
  mutate(
    lat_center = case_when(
      is.na(lat1) == F ~ lat1
      ,is.na(lat1) == T ~ lat2
    )
    ,lon_center = case_when(
      is.na(lon1) == F ~ lon1
      ,is.na(lon1) == T ~ lon2
    )
  ) %>% 
  select(-c(lat1, lon1, lat2, lon2, key1, key2)) %>% 
  filter(oblast_name != "Автономна Республіка Крим")


#calculation of travel times between oblast centers and hromada centers
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

times <- tibble()
for (i in 1:length(ds_oblast$oblast_name)) {
  obl <- ds_oblast$oblast_name[i]
  a <- ds_oblast %>% 
    filter(oblast_name == obl) %>% 
    select(oblast_name, oblast_center_lat, oblast_center_lon)
  a <- st_as_sf(x = a,                         
                coords = c("oblast_center_lon", "oblast_center_lat"),
                crs = projcrs)
  b <- ds_geo %>% 
    filter(oblast_name == obl) %>% 
    select(hromada_code, lat_center, lon_center)
  b <- st_as_sf(x = b,                         
                coords = c("lon_center", "lat_center"),
                crs = projcrs)
  travel_times <- osrmTable(src = b, dst = a, measure = 'duration')$durations %>% as.vector()
  times_obl <- tibble(travel_time = travel_times, hromada_code = b$hromada_code)
  times <- rbind(times, times_obl)
}



#combine all geo info in one dataset
ds_geo_full <- 
  ds_geo %>% 
  mutate(
    hromada = case_when(
      hromada == "Вiйтовецька" ~ "Війтовецька"
      ,hromada == "Жданівська" & oblast_name == "Вінницька" ~ "Війтівецька"
      , TRUE ~ hromada)
  ) %>% 
  left_join(
    times
    , by = "hromada_code") %>% 
  mutate(key = paste(oblast_name,raion_name,hromada,hromada_type)) %>% 
  left_join(
    ds_areas %>% select(key, n_settlements, square)
    ,by = "key"
  ) 
  # left_join(
  #   ds_polygons %>% select(cod_3,geometry)
  #   ,by = c("hromada_code"="cod_3")
  # )


#+ save-to-disk, eval=eval_chunks-----------------------------------------------
readr::write_csv(ds_geo_full, "./data-public/derived/geography.csv")

#+ closest-distance-russia, eval=eval_chunks-----------------------------------------------

library(osmdata)
library(ggmap)
library(geosphere)
library(geojsonsf)

ds3 <- readr::read_csv("./data-public/derived/geography.csv")

#upload polygons of Russia (with Crimea due to deployment of armed forces there) + Belarus 
#and only Russian with Crimea (also excluded Kaliningrad)

russia_bel_border <-st_read("./data-public/derived/shapefiles/russia_bel_crimea.shp")
russia_border <- st_read("./data-public/derived/shapefiles/russia_crimea.shp")

#polygon for the neighbours of Ukraine which are EU members
eu_border <- st_read("./data-public/derived/shapefiles/eu_neighbours.shp")

#create dataframe of coordinates of hromada centers
centers <- ds3 %>% 
  select(hromada_code, hromada_center_code, lon_center, lat_center) %>% 
  st_as_sf(coords = c("lon_center","lat_center"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#calculate distances and merge with the main geography dataset
distance_to_russia_belarus <- st_distance(centers, russia_bel_border)
distance_to_russia <- st_distance(centers, russia_border)
distance_to_eu <- st_distance(centers, eu_border)

ds4 <- cbind(ds3, distance_to_russia_belarus, distance_to_russia, distance_to_eu) %>% 
  mutate(
    distance_to_russia_belarus = as.numeric(distance_to_russia_belarus/1000)
    ,distance_to_russia = as.numeric(distance_to_russia/1000)
    ,distance_to_eu = as.numeric(distance_to_eu/1000)
  )
  

#+ mountain-hromadas, eval=eval_chunks-----------------------------------------------
d0_mountain <- readxl::read_xlsx("./data-private/raw/mountain-hromadas.xlsx", col_names = "all")

d1_mountain <- d0_mountain %>% 
  mutate(
    oblast_name = str_to_title(str_extract(all, ".+(?= ОБЛАСТЬ)"))
    ,raion_name = str_extract(all, ".+(?= район)")
    ,hromada_name = str_extract(all, "(?<=Територія ).+(?= територіальної)")
    ,hromada_name = case_when(
      str_detect(hromada_name,"Красноїльської") ~ 	"Красноїльська селищна"
      ,!str_detect(hromada_name,"Красноїльської") ~ str_replace_all(hromada_name, c("ої"="а", "'"="’"))
    )
  ) %>% 
  fill(oblast_name, raion_name) %>% 
  filter(!is.na(hromada_name)) %>% 
  mutate(
    key = paste(oblast_name, raion_name, hromada_name)
    ,mountain_hromada = 1
  )

ds5 <- ds4 %>% 
  left_join(
    d1_mountain %>% select(key, mountain_hromada)
    ,by = "key"
  )


#+ additional-geographic-data, eval=eval_chunks-----------------------------------------------
sf::sf_use_s2(FALSE)

near_seas <- st_read("./data-public/derived/shapefiles/near_seas.shp") %>% 
  janitor::clean_names() %>% 
  mutate(near_seas = 1)

bordering_hromadas <- st_read("./data-public/derived/shapefiles/bordering_hromadas.shp") %>% 
  janitor::clean_names()%>% 
  mutate(bordering_hromadas = 1)

hromadas_30km_from_border <- st_read("./data-public/derived/shapefiles/hromadas_30km_from_border.shp") %>% 
  janitor::clean_names() %>% 
  mutate(hromadas_30km_from_border = 1)

hromadas_30km_russia_belarus <- st_read("./data-public/derived/shapefiles/hromadas_30km_russia_belarussia.shp") %>% 
  janitor::clean_names() %>% 
  mutate(hromadas_30km_russia_belarus = 1)
  
buffer_int_15km <- st_read("./data-public/derived/shapefiles/buffer_int_15km.shp") 
buffer_nat_15km <- st_read("./data-public/derived/shapefiles/buffer_nat_15km.shp") 

#select hromdas inside 15 km buffers from national and international roads
buffer_nat_15km <- st_filter(ds_polygons, buffer_nat_15km) %>% mutate(buffer_nat_15km = 1)
buffer_int_15km <- st_filter(ds_polygons, buffer_int_15km) %>% mutate(buffer_int_15km = 1)


ds6 <- ds5 %>% 
  left_join(
    near_seas %>% select(cod_3, near_seas)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  left_join(
    bordering_hromadas %>% select(cod_3, bordering_hromadas)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  left_join(
    hromadas_30km_from_border %>% select(cod_3, hromadas_30km_from_border)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  left_join(
    hromadas_30km_russia_belarus %>% select(cod_3, hromadas_30km_russia_belarus)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  left_join(
    buffer_nat_15km %>% select(cod_3, buffer_nat_15km)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  left_join(
    buffer_int_15km %>% select(cod_3, buffer_int_15km)
    ,by = c("hromada_code" = "cod_3")
  ) %>% 
  select(-starts_with("geometry")) %>% 
  left_join(
    ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code" = "cod_3")
  )


#visualize all layers

ds7 <- st_as_sf(ds6, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

roads_national <- st_read("./data-public/derived/shapefiles/roads_national.shp")
roads_international <- st_read("./data-public/derived/shapefiles/roads_international.shp")

g1 <- tm_shape(ds7) +
  tm_polygons("near_seas", alpha = 0.5, palette = "cadetblue1", group = "Громади на відстані 30км від моря") +
  tm_shape(roads_international) +
  tm_lines(col = "red", group = "Міжнародні транспортні коридори") +
  tm_shape(roads_national) +
  tm_lines(col = "blue", group = "Дороги національного значення") +
  tm_shape(ds7) +
  tm_polygons("buffer_int_15km", alpha = 0.4, palette = "red", group = "Громади на відстані 15 км від міжнародних транспортних шляхів") +
  tm_shape(ds7) +
  tm_polygons("buffer_nat_15km", alpha = 0.4, palette = "blue", group = "Громади на відстані 15 км від національних транспортних шляхів") +
  tm_shape(ds7) +
  tm_polygons("mountain_hromada", alpha = 0.4, palette = "brown", group = "Гірські громади") +
  tm_shape(ds7) +
  tm_polygons("bordering_hromadas", alpha = 0.4, palette = "green", group = "Громади, що прилягають до кордону") +
  tm_shape(ds7) +
  tm_polygons("hromadas_30km_from_border", alpha = 0.4, palette = "lightgreen", group = "Громади в межах 30 км зони від кордону") +
  tm_shape(ds7) +
  tm_polygons("hromadas_30km_russia_belarus", alpha = 0.4, palette = "salmon", group = "Громади в межах 30 км зони від кордонів Росії та Білорусі")
  
tmap_save(g1, "./analysis/map_geography.html")


tm_shape(ds7)+
  tm_polygons(col = "grey", alpha = 0.1) +
  tm_shape(ds7 %>% filter(!is.na(near_seas))) +
  tm_polygons("near_seas", alpha = 0.5, palette = "cadetblue1", group = "Громади на відстані 30км від моря") +
  tm_shape(roads_international) +
  tm_lines(col = "red", group = "Міжнародні транспортні коридори")


#+ save-data, eval=eval_chunks-----------------------------------------------
readr::write_csv(ds6, "./data-public/derived/geography.csv")


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# A. Session Information{#session-info}")
#+ results="show", echo=F ------------------------------------------------------
#' For the sake of documentation and reproducibility, the current report was rendered in the following environment.
if( requireNamespace("devtools", quietly = TRUE) ) {
  devtools::session_info()
} else {
  sessionInfo()
}
report_render_duration_in_seconds <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="secs")),accuracy=1)
report_render_duration_in_minutes <- scales::comma(as.numeric(difftime(Sys.time(), report_render_start_time, units="mins")),accuracy=1)
#' Report rendered by `r Sys.info()["user"]` at `r strftime(Sys.time(), "%Y-%m-%d, %H:%M %z")` in `r report_render_duration_in_seconds` seconds ( or `r report_render_duration_in_minutes` minutes)

