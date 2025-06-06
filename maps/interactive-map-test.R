#' ---
#' title: "Interactive Map Test"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-budget-hatsko.R") # run to knit, don't uncomment
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
library(leaflet)
library(sf)
library(mapview)
library(htmltools)
library(shiny)
library(rsconnect)

#+ declare-globals -------------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./manipulation/ellis-budget-prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }
path_polygons <-  "./data-private/raw/terhromad_fin.geojson"

geographic_vars <- c("mountain_hromada", "near_seas", "bordering_hromadas", 
                     "hromadas_30km_from_border", "hromadas_30km_russia_belarus",
                     "buffer_nat_15km", "buffer_int_15km")

factor_vars <- c('type', 'oblast_center', 'mountain_hromada', 'near_seas',
                 'bordering_hromadas', 'hromadas_30km_from_border', 'hromadas_30km_russia_belarus',
                 'buffer_nat_15km', 'buffer_int_15km', 'occipied_before_2022', 'region_en',
                 'oblast_name_en', 'sex_head', 'education_head', 'incumbent', 'rda',
                 'not_from_here', 'party', 'enterpreuner', 'unemployed', 'priv_work',
                 'polit_work', 'communal_work', 'ngo_work', 'party_national_winner',
                 'no_party', 'male', 'high_educ', 'edem_petitions', 'edem_consultations',
                 'edem_participatory_budget', 'edem_open_hromada', 'creation_year',
                 'voluntary', 'war_zone_27_04_2022', 'war_zone_20_06_2022', 'war_zone_23_08_2022',
                 'war_zone_10_10_2022', 'train_station')
# palletes
point_pal <- colorFactor(c('orange3', 'green4', 'red4'), domain = c('міська', 'сільська', 'селищна'))
qual_pal <- colorFactor(RColorBrewer::brewer.pal(8, "Pastel2"), NULL)
num_pal <- colorNumeric("viridis", NULL)

list_variable <- dataset %>%
  select(is.numeric | is.factor | 'oblast_name_en') %>%
  select(-c(lat_center, lon_center, geometry, oblast_name_en, region_en, male, 
            high_educ)) %>%
  colnames()

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./data-private/derived/full_dataset.csv")
ds_polygons <- st_read(path_polygons) %>% janitor::clean_names() %>% 
  mutate(
    admin_3 = str_replace_all(admin_3,c("a" = "а", "o" = "о", "p"="р", "e"="е", "'" = "’"))
  )

#+ tweak-data-1 ----------------------------------------------------------------
ds_general0 <- ds_general %>% select(-ends_with('.y')) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = list(~ sub("[.]x$", "", .))) %>%
  mutate(
    across(
      .cols = all_of(geographic_vars),
      .fns = ~replace_na(., 0L)
    )
  ) %>% 
  mutate(
    across(
      .cols = all_of(factor_vars),
      .fns = ~as.factor(.)
    )
  )

dataset <- st_sf(
  right_join(
    ds_general0 %>%
      select(-c('geometry'))
    ,ds_polygons %>% select(cod_3, geometry)
    ,by = c("hromada_code"="cod_3")
  )
)

cleantable <- dataset %>%
  select(-c(ends_with('code'), budget_name, region_code_en, creation_date)) 
# %>% 
  # rename(
  #   Hromada = hromada_full_name,
  #   Oblast = oblast_name_en,
  #   Raion = raion_name,
  #   Type = type,
  #   'Hromada center' = hromada_center,
  #   Population = total_popultaion_2022,
  #   "Number of settlements" = n_settlements
  # )

test_data <- cleantable %>% 
  select(hromada_full_name, travel_time, n_settlements)

#+ Leaflet Map -----------------------------------------------------------------
# can filter by oblast if put in leaflet start command
oblast_subset <- subset(dataset, dataset$oblast_name_en == 'Kharkiv')

leaflet(dataset, options = leafletOptions(minZoom = 5)) %>%
  setView(lng = 30.45, lat = 50.36, zoom = 5) %>% 
  fitBounds(lng1 = 21.39, lat1 = 44.79, lng2 = 41.16, lat2 = 52.75) %>% 
  addTiles() %>%
  addProviderTiles(providers$Thunderforest.MobileAtlas, 
                   options = providerTileOptions(opacity = 0.2)) %>%
  addMapPane(name = 'poly', zIndex = 400) %>% 
  addMapPane(name = 'markers', zIndex = 410) %>% 
  addMapPane(name = 'circles', zIndex = 420) %>% 
  addPolygons(label = ~paste0(hromada_full_name),
              fillColor = ~num_pal(n_settlements), fillOpacity = .8,
              color = qual_pal, weight = .5,
              highlightOptions = highlightOptions(color = "grey", weight = 3,
                                                  bringToFront = T),
              options = leafletOptions(pane = 'poly'),
              group = 'map_labels') %>%
  addCircleMarkers(lng = ~lon_center, lat = ~lat_center, 
             # clusterOptions = markerClusterOptions(),
             radius = 1, col = ~point_pal(type),
             label = ~htmlEscape(hromada_center),
             options = leafletOptions(pane = 'markers'),
             group = 'map_labels', opacity = .2) %>% 
  # addCircles(lng = ~lon_center, lat = ~lat_center, weight = .2,
  #            radius = ~sqrt(total_popultaion_2022)*15,
  #            options = leafletOptions(pane = 'circles'),
  #            group = 'map_labels') %>% 
  addLegend(pal = num_pal, values = ~n_settlements, title = "Number of settlements")


##
leaflet(oblast_subset, options = leafletOptions(minZoom = 5)) %>%
  setView(lng = 30.45, lat = 50.36, zoom = 5) %>% 
  fitBounds(lng1 = 21.39, lat1 = 44.79, lng2 = 41.16, lat2 = 52.75) %>% 
  addTiles() %>%
  addProviderTiles(providers$Thunderforest.MobileAtlas, 
                   options = providerTileOptions(opacity = 0.2)) %>%
  addPolygons(label = ~paste0(hromada_full_name, "\nPopulation: ", total_popultaion_2022, " people"),
              fillColor = ~qual_pal(type), fillOpacity = .8,
              color = 'red4', weight = .5,
              highlightOptions = highlightOptions(color = "grey", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addCircleMarkers(lng = ~lon_center, lat = ~lat_center, 
                   # clusterOptions = markerClusterOptions(),
                   radius = 2, col = ~point_pal(type),
                   label = ~htmlEscape(hromada_center)) %>% 
  addCircles(lng = ~lon_center, lat = ~lat_center, weight = .2,
             radius = ~sqrt(total_popultaion_2022)*15) %>% 
  addLegend(pal = qual_pal, values = ~type, title = "Type of hromada")


#+ Shiny app -------------------------------------------------------------------

vars <- dataset %>%
  select(is.numeric | is.factor | 'oblast_name_en') %>%
  select(-c(lat_center, lon_center, geometry, oblast_name_en, region_en, male, 
            high_educ)) %>%
  st_drop_geometry() %>% 
  colnames()

#create the UI (ver 2)
ui <- fluidPage(
  #create title
  titlePanel("Hromada Dashboard"),
  
  fluidRow(
    column(4,
      
      selectInput(inputId = "metric",
                  label = "Choose a Metric",
                  choices = vars,
                  selected = "travel_time")
    )
    ),
    
      leafletOutput("map", height = 800)
  
)
# Server

#Create the server

server <- function(input, output) {
  
  #select the input
  metric_to_map <- reactive({
    input$metric
  })
  
  #
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 5)) %>%
      setView(lng = 30.45, lat = 50.36, zoom = 5) %>% 
      fitBounds(lng1 = 21.39, lat1 = 44.79, lng2 = 41.16, lat2 = 52.75) %>% 
      addTiles() %>%
      addProviderTiles(providers$Thunderforest.MobileAtlas, 
                       options = providerTileOptions(opacity = 0.2))
    
  })
 
  #
  
  observeEvent(input$metric, {
    
    pal <- if(is.numeric(dataset[[metric_to_map()]])) {
              colorNumeric("viridis", NULL)
    } else {
              colorFactor('viridis', NULL)
    }
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = dataset,
                  label = ~lapply(paste0('<p>', hromada_full_name, '</p><p>', 
                                         dataset[[metric_to_map()]], '</p>'), 
                                 htmltools::HTML),
                  fillColor = ~pal(dataset[[metric_to_map()]]),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "grey", 
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = dataset[[metric_to_map()]],
        title = ""
      )
  })
}
  
app <- shinyApp(ui, server)


deployApp()
