#install.packages("shiny")
library(shiny)
library(tmap)
library(tmaptools)
library(leaflet)

ui <- fluidPage(
  leafletOutput("mapPlot"),
  actionButton("pl","Begin Plotting")
)

server <- function(input, output, session) {

  current_state <- eventReactive(input$pl, {
    coc_shp_16[coc_shp_16$ST=="CA",]
  }, ignoreNULL = FALSE)
  
  state_layer <- US_county[US_county$STATE=="06",]
  
  output$mapPlot <- renderLeaflet({
    tm <- tm_shape(current_state()) +
      tm_polygons("Total Homeless, 2016", style="quantile", border.alpha = 0, alpha = 0.8) +
      tm_shape(state_layer) +
      tm_polygons("County", alpha = 0, border.alpha = 0.5, border.col = "green", legend.show = FALSE) +
      tm_shape(current_state) +
      tm_polygons("COCNAME", alpha = 0, legend.show = FALSE, border.col = "red")
      tmap_mode("view")
    tmap_leaflet(tm)
  })
}


shinyApp(ui = ui, server = server)