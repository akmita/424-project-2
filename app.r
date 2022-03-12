library(leaflet)
library(shiny)
library(stringr)
library(DT)
library(lubridate)
library(sqldf)
library(ggplot2)

source("./dataParsers.r", local = FALSE)
source("./plotHelpers.r", local = FALSE)  

# INPUTS FROM LEAFLET MAP - this will be hard, but getting the inputs from the leaftlet map, 
# on click of each station might look something like this
# https://stackoverflow.com/questions/45700647/select-input-for-leaflet-in-shiny

# TABLE CLICK EVENTS EXPLAINED
# https://stackoverflow.com/questions/47371525/shiny-renderdatatable-table-cell-clicked


################################
#            INIT              #
################################

# CREATE DATA FRAMES
if (!exists("rides") || !exists("stops")) {
  stops <- getStopData()
  stops <- stops[!duplicated(stops$MAP_ID),]
  rides <- getridesData()
  # merge coordinate data with rides data
  rides <- sqldf("SELECT station_id, stationname, date, daytype, rides, newDate, year, month, day, s.lng, s.lat
                FROM stops s
                LEFT JOIN rides r ON s.MAP_ID = r.station_id")
}



################################
#            UI                #
################################

ui <- fluidPage(
  wellPanel(
    # MAIN BAR CHART
    column(9, 
      conditionalPanel(
        condition = "input.visType == 'bar'",
        plotOutput("rides_per_day"),
      ),
      conditionalPanel(
        condition = "input.visType == 'table'",
        DT::dataTableOutput("rides_per_day_table"),
      ),
    ),
    # PARTICULAR STATION CHARTS
    column(3, DT::dataTableOutput("rides_per_year_tab"))
  ),
  
  
  fluidRow(
    # CONTROL PANEL
    column(2, 
           # DATE PICKER
           column(12, dateInput("datepicker", "Date:", value = "2021-08-23", min = "2001-01-01", max = "2021-11-30")),
           column(6, actionButton("prev_day", "Prev Day")),
           column(6, actionButton("next_day", "Next Day")),
           # BAR/TABLE SORTER
           column(12, radioButtons("mainBarSort", "Sort Stations Chart",
                  c("Alphabetical" = "alpha",
                    "Ascending" = "min",
                    "Descending" = "max"))),
           column(12, radioButtons("visType", "Select View For Main (Stations) Data",
                  c("Bar" = "bar",
                    "Table" = "table"))),
    ),
    # STATION TABLE/BAR  -- == TODO == -- 
    column(3, DT::dataTableOutput("mytable")), 
    # STATION MAP
    column(5, leafletOutput("mymap")),   
  )
)



################################
#         SERVER               #
################################

server <- function(input, output, session) {
  selectedStopID <- reactiveVal(0)
  
  #
  # Print station map
  # 
  output$mymap <- renderLeaflet({ 
    
    activeStops <- getActiveStopsList(rides, input$datepicker, "map")
    
    m <- leaflet(activeStops) %>% 
      addTiles() %>%
      # different map backgrounds
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "World Street Map") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # add data points
      addCircleMarkers(lng = activeStops$lng, lat = activeStops$lat, popup = activeStops$stationname) %>%
      # add controls
      addLayersControl(
        baseGroups = c("OSM (default)", "World Street Map", "Satellite", "Toner"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  #
  # map marker on-click 
  #
  observeEvent(input$mymap_marker_click, { # 
    clickEvent <- input$mymap_marker_click
    print("clicked on map marker")
    selectedStopID(findStationIDByCoords(stops, clickEvent$lng, clickEvent$lat))
  })
  
  
  #
  # list of all stations
  #
  output$mytable = DT::renderDataTable({
    getActiveStopsList(rides, input$datepicker, "table")
  })
  
  
  #
  # rides/day vs stations bar graph  OR  table
  # 
  output$rides_per_day <- renderPlot({
    getMainBarGraph(rides, input$datepicker, input$mainBarSort, input$visType)
  })
  output$rides_per_day_table <- DT::renderDataTable(
    getMainBarGraph(rides, input$datepicker, input$mainBarSort, input$visType)
  )
  
  # 
  # prev/next day
  # 
  observeEvent(input$prev_day, {
    print("prev day")
    updateDateInput(session, "datepicker", value=input$datepicker-1)
  })
  observeEvent(input$next_day, {
    print("next day")
    updateDateInput(session, "datepicker", value=input$datepicker+1)
  })
  
  
  #
  # project 1 visuaizations - yearly, monthly, daily, day-of-the-week
  #
  output$rides_per_year_tab <- DT::renderDataTable(filterByStationName(rides, selectedStopID()))
  
}


# ------------------- START SERVER ----------------- #
shinyApp(ui, server)
# -------------------------------------------------- #











