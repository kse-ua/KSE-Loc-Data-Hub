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
#+ load-packages -----------------------------------------------------------
library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(htmltools)
library(shiny)
library(rsconnect)

#+ declare-globals -------------------------------------------------------------
path_polygons <-  "./terhromad_fin.geojson"

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

#+ declare-functions -----------------------------------------------------------

#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
# the product of ./manipulation/ellis-general.R
ds_general <- readr::read_csv("./full_dataset.csv")
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
      colorBin("viridis", dataset[[metric_to_map()]], 8, pretty = F)
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

shinyApp(ui, server)
