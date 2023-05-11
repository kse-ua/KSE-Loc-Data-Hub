rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.

#+ load-packages -----------------------------------------------------------
library(tidyverse) #data wrangling
library(sf) #geospatial operations
library(tmap) #visualisation on maps

#+ load-data -----------------------------------------------------------

#load survey data
ds0 <-  readxl::read_xlsx("./agro-survey-workshop.xlsx") %>% 
  janitor::clean_names()

colnames(ds0)[1:6] <- c("oblast", "raion", "hromada", "military_action", "pct_idp", "n_idp") 

#load general KSE dataset
ds_general <- readr::read_csv("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/full_dataset.csv")

# #load polygons of hromadas
polygons <-  st_read("https://raw.githubusercontent.com/kse-ua/ua-de-center/main/data-public/derived/shapefiles/admin/terhromad_fin.geojson") %>%
  janitor::clean_names()


#+ merge-data-with-polygons -----------------------------------------------------------

#combination of oblast-raion-hromada gives us a unique identifier - we can use this to merge with the full dataset
ds_general %>% distinct(oblast_name, raion_name, hromada_full_name)

ds1 <- 
  #clean our test dataset and create key variable
  ds0 %>% 
  mutate(
    oblast = str_remove(oblast, " область")
    ,raion = str_remove(raion, " район")
    ,raion = str_replace_all(raion, c("'"="ʼ", "’"="ʼ"))
    ,hromada = str_replace_all(hromada, c("a"="а", "o"="о", "e"="е", "O"="О", "p"="р", "'"="ʼ", "ʼ"="ʼ"))
    ,key = paste(oblast, raion, hromada)
  ) %>% 
  #merge out test dataset with the KSE main dataset by key variable
  left_join(
    ds_general %>% 
      select(hromada_code, oblast_name, raion_name, hromada_full_name, total_population_2022) %>% 
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
  ) %>% 
  #add variable with % of IDPs based on the total population from the general dataset
  mutate(
    pct_idp = n_idp/total_population_2022 * 100
    ,pct_idp_rounded = scales::percent(round(pct_idp, 2))
  ) %>% 
  filter(pct_idp <= 1)

# Checks
ds1 %>% summarise(across(everything(), ~ sum(is.na(.x)))) %>% t()
ds1 %>% select(oblast, oblast_name, raion, raion_name, hromada, hromada_full_name) %>% view()
# Graphs
summary(ds1$n_idp)

ds1 %>% 
  ggplot(aes(x = n_idp)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  theme_bw() +
  labs(x = 'Number of IDPs') +
  xlim(0, 20000)

ds1 %>% 
  ggplot(aes(x = pct_idp)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  theme_bw() +
  labs(x = 'Share of IDPs in total population')

ds1 %>% 
  ggplot(aes(x = n_idp, y = total_population_2022)) +
  geom_point() +
  theme_bw() +
  labs(x = "Number of IDPs", y = 'Population as of Jan 2022') +
  xlim(0, 50000) +
  ylim(0, 500000)


## Regression

ds2 <- ds1 %>% 
  left_join(ds_general %>% select(hromada_code, type, area, income_total),
            by = 'hromada_code')

model1 <- lm(n_idp ~ log10(total_population_2022), data = ds2)
model2 <- lm(n_idp ~ log10(total_population_2022) + type + area,data = ds2)
model3 <- lm(n_idp ~ log10(total_population_2022) + type + area + income_total,
             data = ds2)

stargazer::stargazer(model1, model2, model3, type = 'text')


#create sf object - required for mapping using tmap
ds2 <- st_as_sf(ds1, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#+ static maps -----------------------------------------------------------

#set the viewer mode as "plot"
tmap_mode("plot")

#create simple map
g1 <- tm_shape(ds2) +
  tm_polygons("n_idp")+ 
  tmap_options(check.and.fix = TRUE)
g1

#add borders - Ukriane and oblasts - based on the existing hromada polygons
sf_use_s2(FALSE)
ds_polygon_Ukraine <- 
  polygons %>% 
  summarise(geometry = sf::st_union(geometry))

ds_polygons_oblasts <- 
  polygons %>% 
  group_by(admin_1) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

#alternative way - just download from the repository to your computer 
#NB! You should download all files for the respective level (.CPG, .dbf, .prj, .shp, .shx)
ds_polygon_Ukraine <- 
  st_read("./data-public/derived/shapefiles/admin/ADMIN_0.shp")

ds_polygons_oblasts <- 
  st_read("./data-public/derived/shapefiles/admin/ADMIN_1.shp")

g1 <- 
  tm_shape(ds2) +
    tm_fill("n_idp", title = "Кількість ВПО")+ 
    tm_borders('black', lwd = 0.2) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g1


#add map with % of IDPs
g2 <- 
  tm_shape(ds2) +
    tm_fill(c("n_idp", "pct_idp"), 
            title = c("Кількість ВПО", "% ВПО від населення громади"))+ 
    tm_borders('black', lwd = 0.2) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g2


#overlap two layers
g3 <- 
  tm_shape(ds2) +
    tm_fill("pct_idp", palette = "PuBu", title = "% ВПО від населення громади")+
    tm_bubbles(size = "n_idp", title.size = "Кількість ВПО")+
    tm_borders('black', lwd = 0.2) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g3

#add facets by oblast
g3_oblast <- 
  tm_shape(ds2) +
    tm_fill("pct_idp", palette = "PuBu", title = "% ВПО від населення громади")+
    tm_bubbles(size = "n_idp", title.size = "Кількість ВПО")+
    tm_borders('black', lwd = 0.2) +
    tm_facets(by = "oblast_name") +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g3_oblast

  
#add hromada names
g4 <- 
  tm_shape(ds2) +
    tm_fill("pct_idp", palette = "PuBu", title = "% ВПО від населення громади")+
    tm_bubbles(size = "n_idp", title.size = "Кількість ВПО")+
    tm_borders('black', lwd = 0.2) +
    tm_text("hromada", size = 0.5) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g4


#+ interactive maps -----------------------------------------------------------

#set the viewer mode as "view"
tmap_mode("view")

g4

#improve aesthetics
g5 <- 
  tm_shape(ds2) +
    tm_fill("pct_idp"
            ,palette = "PuBu"
            ,style = 'pretty'
            ,title = "% ВПО від населення громади"
            ,labels = c('0-20%', '20-40%', '40-60%', '60-80%', 
                        '80-100%')
            ,popup.vars=c("Громада" = "hromada", 
                          "Область" = "oblast", 
                          "Частка ВПО від населення громади станом на 01.01.2022" = "pct_idp_rounded", 
                          "Кількість ВПО" = "n_idp"))+
    tm_borders('black', lwd = 0.2) +
    # tm_text("hromada", size = 0.5) +
  tm_shape(ds_polygon_Ukraine) +
    tm_borders('gray', lwd = 0.5) +
  tm_shape(ds_polygons_oblasts) +
    tm_borders('gray', lwd = 0.3) +
  tmap_options(check.and.fix = TRUE)
g5


#+ save-maps-----------------------------------------------------------
tmap_save(g3, "./workshop-charts/map_static.png")
tmap_save(g3_oblast, "./workshop-charts/map_oblasts_static.png")
tmap_save(g5, "./workshop-charts/map_interactive.html")



