library(leaflet)
library(shiny)
library(stringr)
library(DT)
library(lubridate)
library(sqldf)
library(ggplot2)


source("./globals.r", local = FALSE)
source("./dataParsers.r", local = FALSE)
source("./plotHelpers.r", local = FALSE)  



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
    
    #
    # LEFT PANEL   
    #
    column(9, 
      # MAIN BAR CHART
      wellPanel(
        tabsetPanel(
          id="visType",
          tabPanel("Bar Graph", id="bar", plotOutput("rides_per_day")),
          tabPanel("Table", id="table", DT::dataTableOutput("rides_per_day_table")),
        )
      ),
      
      wellPanel( 
        fluidRow(
          # CONTROL PANEL
          column(2, 
                 # DATE PICKER
                 tabsetPanel(
                   id="datePicker_tab",
                   tabPanel("Single Day",
                      column(12, dateInput("datepicker", "Date:", value = "2021-08-23", min = minDate, max = maxDate)),
                      column(8, actionButton("prev_day", "Prev Day")),
                      column(2, actionButton("next_day", "Next Day")),
                   ),
                   tabPanel("Date Range",
                     column(12,
                         dateRangeInput("daterange", "Date range:",
                                        start  = "2001-01-01",
                                        end    = "2010-12-31",
                                        min    = minDate,
                                        max    = maxDate), 
                     ),  
                   ),
                 ),
                 
                 hr(),
                 # BAR/TABLE SORTER
                 column(12, radioButtons("mainBarSort", "Sort Stations Chart",
                                         c("Alphabetical" = "alpha",
                                           "Ascending" = "min",
                                           "Descending" = "max")), style=paddingTop),
          ),
          # STATION TABLE 
          column(5, DT::dataTableOutput("mytable")), 
          # STATION MAP
          column(5, leafletOutput("mymap")), 
        )
      ),
    ),
    
    
    #
    # RIGHT PANEL - DETAILS RIDE DATA (Project 1)
    #
    column(3, 
      wellPanel(
        h3(textOutput("currentStationSelected")),
        fluidRow(
          # CHARTS
          tabsetPanel(id="tab_details", type = "tabs",
            tabPanel("Table", column(12, DT::dataTableOutput("rides_per_year_tab")), style=paddingTop,),
            tabPanel("Bar Graph", column(12, plotOutput("rides_per_year_bar")))
          ),
          # CONTROL PANEL # 2
          column(12, 
                 sliderInput(
                   inputId = "details_year",
                   label = "Choose a year",
                   value = 2021, min = 2001, max = 2021, ticks = FALSE),
                 radioButtons(
                   inputId = "details_graphType",
                   label = "Choose the bar graph to show",
                   choices = graphChoices),
          ),
        )
      )
    ),
)



################################
#         SERVER               #
################################

server <- function(input, output, session) {
  selectedStop <- reactiveVal(stops[1,])
  
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
  observeEvent(input$mymap_marker_click, { 
    clickEvent <- input$mymap_marker_click
    print("clicked on map marker")
    selectedStop(findStationByCoords(stops, clickEvent$lng, clickEvent$lat))
    print(paste("found:", selectedStop()))
    
    print(input$tab_details)
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
    print("graphing main bar")
    getMainBarGraph(rides, input$mainBarSort, "Bar Graph", input$datePicker_tab, input$datepicker, input$daterange)
    # getMainBarGraph(rides, input$datepicker, input$mainBarSort, "Bar Graph")
  })
  output$rides_per_day_table <- DT::renderDT({
    print("graphing main table")
    getMainBarGraph(rides, input$mainBarSort, "Table", input$datePicker_tab, input$datepicker, input$daterange)
    # getMainBarGraph(rides, input$datepicker, input$mainBarSort, "Table")
  })
  
  # 
  # prev/next day
  # 
  observeEvent(input$prev_day, {
    print("prev day")
    updateDateInput(session, "datepicker", value=input$datepicker-1, )
  })
  observeEvent(input$next_day, {
    print("next day")
    updateDateInput(session, "datepicker", value=input$datepicker+1)
  })
  
  
  #
  # project 1 visuaizations - yearly, monthly, daily, day-of-the-week - table and bar
  #
  # output$rides_per_year_tab <- DT::renderDataTable(filterByStationName(rides, selectedStopID())) 
  output$rides_per_year_tab <- DT::renderDataTable(getTable(
    input$details_graphType, 
    input$details_year,
    "<location/title PLACEHOLDER TABLE>",
    filterByStationName(rides, selectedStop()$MAP_ID)))
  
  output$rides_per_year_bar <- renderPlot({
    createBarGraph(
      "<location/title PLACEHOLDER BAR GRAPH>", 
      input$details_graphType, 
      input$details_year, 
      filterByStationName(rides, selectedStop()$MAP_ID))  
  })
  
  # currently selected station
  output$currentStationSelected <- renderText({ paste(selectedStop()$STATION_NAME, "Station - ", input$details_graphType) })
}


# ------------------- START SERVER ----------------- #
shinyApp(ui, server)
# -------------------------------------------------- #











