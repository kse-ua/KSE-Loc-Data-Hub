rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.

#+ load-packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(tmap)

#+ load-data -----------------------------------------------------------

#load survey data
ds0 <-  readxl::read_xlsx("./agro-survey-workshop.xlsx") %>% 
  janitor::clean_names()

colnames(ds0)[1:6] <- c("oblast", "raion", "hromada", "military_action", "pct_idp", "n_idp") 

#load general KSE dataset
ds_general <- readr::read_csv("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/full_dataset.csv")

# #load polygons of hromadas
polygons <-  st_read("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/shapefiles/terhromad_fin.geojson") %>%
  janitor::clean_names()


#+ merge-data-with-polygons -----------------------------------------------------------

#combination of oblast-raion-hromada gives us a unique identifier - we can use this to merge with the full dataset
ds_general %>% distinct(oblast_name, raion_name, hromada_full_name)

ds1 <- ds0 %>% 
  mutate(
    oblast = str_remove(oblast, " область")
    ,raion = str_remove(raion, " район")
    ,raion = str_replace_all(raion, c("'"="ʼ", "’"="ʼ"))
    ,hromada = str_replace_all(hromada, c("a"="а", "o"="о", "e"="е", "O"="О", "p"="р", "'"="ʼ", "ʼ"="ʼ"))
    ,key = paste(oblast, raion, hromada)
  ) %>% 
  left_join(
    ds_general %>% 
      select(-geometry) %>% 
      mutate(
        key = paste(oblast_name, raion_name, hromada_full_name)
        ,key = str_replace_all(key, c("'"="ʼ", "’"="ʼ"))
      )
    ,by = "key"
  ) %>% 
  distinct(hromada_code, .keep_all = T) %>% 
  filter(hromada != "Війтівецька сільська громада") %>% 
  left_join(
    polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  ) 


#create sf object - required for mapping using tmap
ds2 <- st_as_sf(ds1, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#+ simple static plots -----------------------------------------------------------

#set the viewer mode as "plot"
tmap_mode("plot")

#create simple map
g1 <- tm_shape(ds2) +
  tm_polygons("n_idp")+ 
  tmap_options(check.and.fix = TRUE)
g1

#add borders - Ukriane and oblasts
sf_use_s2(FALSE)
ds_polygon_Ukraine <- 
  polygons %>% 
  summarise(geometry = sf::st_union(geometry))

ds_polygons_oblasts <- 
  polygons %>% 
  group_by(admin_1) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

g1 <- 
  tm_shape(ds2) +
   tm_fill("n_idp")+ 
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g1

#add map with %
g2 <- 
  tm_shape(ds2) +
    tm_fill(c("n_idp", "pct_idp"))+ 
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g2


#add hromada names
g3 <- 
  tm_shape(ds2) +
    tm_fill("n_idp")+ 
    tm_text("hromada_name", size = 0.5) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g3


#+ simple interactive plots -----------------------------------------------------------

#set the viewer mode as "view"
tmap_mode("view")

g3



#plot youth population

g1 <- tm_shape(ds_kharkiv) +
  tm_layout(legend.position = c("right", "bottom")
            ,frame = F
            ,legend.outside = T
            ,title.position = c('left', 'top')) +
  tm_fill("youth_pct_declarations"
          ,title = "% від загальної кількості декларацій"
          ,palette = "RdBu"
          ,style = 'pretty'
          ,labels = c('22-24%', '24-26%', '26-28%', '28-30%', 
                      '30-32%', '32-34%', 'Немає даних')
  ) +
  tm_borders('gray', lwd = 0.2) +
  tm_shape(ds_kharkiv %>% filter(most_affected)) +
  tm_borders("black", lwd = 1.8)


tmap_mode("view")



