library(leaflet)
library(shiny)

# SIMPLE LEAFLET EXAMPLE
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
# m 


# m <- leaflet()
# m <- addTiles(m)
# m <- addMarkers(m, lng=-87.772, lat=41.916675, popup="The birthplace of R")
# 
# print(m)
# 


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()



ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)



server <- function(input, output, session) {
  
  x <- rnorm(40) * 2 + 13
  y <- rnorm(40) + 48
  xything <- cbind(x, y)
  
  print(x)
  print(y)
  print(xything)
  
  points <- eventReactive(input$recalc, {
    xything
  }, ignoreNULL = FALSE)
  
  
  
  output$mymap <- renderLeaflet({ 
    
    # print(points())
    
    
    #
    # ADRIAN, restart the app and print some more stufff, figure out how to input actual world coordinates  
    #
    # I think you do it like this: https://stackoverflow.com/questions/69412282/leaflet-plot-with-gps-coordinates-in-r
    
    
    # DEMO
    # 
    # m <- leaflet() %>%
    #   addProviderTiles(providers$Stamen.TonerLite,
    #                    options = providerTileOptions(noWrap = TRUE)
    #   ) %>%
    #   addMarkers(data = points())
    
    
    leaflet(df) %>% addTiles() %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, 
                       popup = ~Place)
    
  })
}



# START SERVER
shinyApp(ui, server)

