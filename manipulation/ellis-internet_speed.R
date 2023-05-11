
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
library(ooklaOpenDataR)
library(tidyverse) 
library(sf) 
library(knitr)
library(kableExtra) 
library(RColorBrewer) 
library(here) 
library(usethis) 


#+ load-data, eval=eval_chunks ----------------------------------------------

# download the zip folders from s3 and save to working directory
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2021/quarter=3/2021-07-01_performance_mobile_tiles.zip")

# mobile_2021q3 <- get_performance_tiles(service = "mobile", quarter = 3, year = 2021, sf = TRUE)
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2021/quarter=3/2021-07-01_performance_fixed_tiles.zip")

#polygons of hromadas
path_hromadas <- "./data-private/raw/terhromad_fin.geojson"

hromadas_polys <- st_read(path_hromadas) %>% janitor::clean_names() %>% 
  filter(!is.na(cod_3))

hromadas_polys <- st_make_valid(hromadas_polys)

#+ tweak-mobile-data, eval=eval_chunks ----------------------------------------------

#read the mobile shapefile 
tiles_mobile <- 
  read_sf("./2021-07-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  mutate(avg_d_kbps = as.numeric(avg_d_kbps),
         avg_u_kbps = as.numeric(avg_u_kbps),
         avg_lat_ms = as.numeric(avg_lat_ms))

#assign tiles to hromadas
tiles_mobile_hromadas <- st_join(hromadas_polys, tiles_mobile, left = FALSE)

#calculate weighted average speed per hromada
hromada_stats_mobile <- tiles_mobile_hromadas %>%
  st_set_geometry(NULL) %>%
  group_by(cod_3, admin_3) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests)) %>%
  ungroup() 


#+ tweak-fixed-data, eval=eval_chunks ----------------------------------------------

#read the fixed shapefile 
tiles_fixed <- 
  read_sf("./2021-07-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
  mutate(avg_d_kbps = as.numeric(avg_d_kbps),
         avg_u_kbps = as.numeric(avg_u_kbps),
         avg_lat_ms = as.numeric(avg_lat_ms))

#assign tiles to hromadas
tiles_fixed_hromadas <- st_join(hromadas_polys, tiles_fixed, left = FALSE)

#calculate weighted average speed per hromada
hromada_stats_fixed <- tiles_fixed_hromadas %>%
  st_set_geometry(NULL) %>%
  group_by(cod_3, admin_3) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests)) %>%
  ungroup() 

#combine
hromada_stats <- hromadas_polys %>% 
  dplyr::select(admin_3, cod_3) %>% 
  left_join(
    hromada_stats_mobile %>% 
      rename(mean_dl_mbps_wt_mobile = mean_dl_mbps_wt
             ,mean_ul_mbps_wt_mobile = mean_ul_mbps_wt
             ,mean_lat_ms_wt_mobile = mean_lat_ms_wt
             ,tests_mobile = tests)
    ,by = "cod_3"
  ) %>% 
  left_join(
    hromada_stats_fixed %>% 
      rename(mean_dl_mbps_wt_fixed = mean_dl_mbps_wt
             ,mean_ul_mbps_wt_fixed = mean_ul_mbps_wt
             ,mean_lat_ms_wt_fixed = mean_lat_ms_wt
             ,tests_fixed = tests)
    ,by = "cod_3"
  ) 

#+ write-data, eval=eval_chunks ----------------------------------------------

readr::write_csv(hromada_stats, "./data-private/derived/internet-speed.csv")




#завантаження шейпфайлу потрібної території
# Sys.locale()
# nb <- st_read("~/Desktop/KSE Institute/shapefiles/terhromad_fin.geojson")
# oblast <- nb[nb$ADMIN_3 == "Дніпpoвськa", ]
# 
# nb %>%
#   filter(grepl("орост", ADMIN_3)) %>% view()
# 
#          
# ggplot() +
#   geom_sf(data = oblast,
#           color = "gray20",
#           fill = "gray98",
#           lwd = 0.1) +
#   theme_void()
# 
# #домоміжні функції для візуалізації
# as_binary = function(x){
#   tmp = rev(as.integer(intToBits(x)))
#   id = seq_len(match(1, tmp, length(tmp)) - 1)
#   tmp[-id]
# }
# 
# deg2num = function(lat_deg, lon_deg, zoom) {
#   lat_rad = lat_deg * pi /180
#   n = 2.0 ^ zoom
#   xtile = floor((lon_deg + 180.0) / 360.0 * n)
#   ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
#   c(xtile, ytile)
# }
# 
# # reference JavaScript implementations
# # https://developer.here.com/documentation/traffic/dev_guide/common/map_tile/topics/quadkeys.html
# 
# tileXYToQuadKey = function(xTile, yTile, z) {
#   quadKey = ""
#   for (i in z:1) {
#     digit = 0
#     mask = bitwShiftL(1, i - 1)
#     xtest = as_binary(bitwAnd(xTile, mask))
#     if(any(xtest)) {
#       digit = digit + 1
#     }
#     
#     ytest = as_binary(bitwAnd(yTile, mask))
#     if(any(ytest)) {
#       digit = digit + 2
#     }
#     quadKey = paste0(quadKey, digit)
#   }
#   quadKey
# }
# 
# get_perf_tiles <- function(bbox, tiles){
#   bbox <- st_bbox(
#     st_transform(
#       st_as_sfc(bbox),
#       4326
#     ))
#   tile_grid <- bbox_to_tile_grid(bbox, zoom = 16)
#   # zoom level 16 held constant, otherwise loop through the tile coordinates calculated above
#   quadkeys <- pmap(list(tile_grid$tiles$x, tile_grid$tiles$y, 16), tileXYToQuadKey)
#   perf_tiles <- tiles %>%
#     filter(quadkey %in% quadkeys)
# }
# 
# 
# #завантаження даних по швидкості інтернету
# 
# data <- get_performance_tiles(service = "mobile", quarter = 2, year = 2020, sf = TRUE)
# 
# oblast_bbox <- st_bbox(oblast)
# 
# oblast_perf_tiles <- get_perf_tiles(oblast_bbox, data) %>%
#   st_as_sf(wkt = "tile", crs = 4326)
# 
# ggplot() +
#   geom_sf(data = oblast_perf_tiles,
#           aes(fill = cut(avg_d_kbps / 1000,
#                          breaks=c(quantile(avg_d_kbps / 1000,
#                                            probs = seq(0, 1, by = 0.20))),
#                          include.lowest=TRUE))) +
#   scale_fill_brewer(palette = "BuPu", name = "Mean Download Mbps") +
#   geom_sf(data = oblast,
#           color = "gray20",
#           fill = NA) +
#   theme_void() + 
#   OpenStreetMap::autoplot.OpenStreetMap(map_osm) 
# 
# OpenStreetMap::autoplot.OpenStreetMap(map_osm) +
#   geom_sf(data = oblast_perf_tiles,
#           aes(fill = cut(avg_d_kbps / 1000,
#                          breaks=c(quantile(avg_d_kbps / 1000,
#                                            probs = seq(0, 1, by = 0.20))),
#                          include.lowest=TRUE))) +
#   scale_fill_brewer(palette = "BuPu", name = "Mean Download Mbps") +
#   geom_sf(data = oblast,
#           color = "gray20",
#           fill = NA) +
#   theme_void()
# 
# upper_left  <- c(34.74907, 48.35583);
# lower_right <- c(35.24445, 48.57642 );
# 
# map_osm  <- openmap(upper_left, lower_right, type = 'opencyclemap');
# map_osm <- openproj(map_osm)
# 
