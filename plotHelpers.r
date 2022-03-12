getMainBarGraph = function(D, date, order, visType) {

  D_ <- orderBy(filterByDate(D, date), order) 

  # prevent ggplot from sorting automatically
  D_$stationname <- factor(D_$stationname, levels = D_$stationname)
  
  if (visType == "bar") {
    (ggplot(
      data=D_, 
      aes(x=stationname, y=rides)) 
     + geom_bar(stat="identity")
     + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # fixes overlapping names
     + labs(title = paste("Number Of Rides On", weekdays(as.Date(D_[1,"newDate"])), date), x = "station name")
    )
  }
  else if (visType == "table") {
    return(D_)
  }
  else {
    print("ERROR: invalid visualization type")
  }
}


#
# order dataframe based on user specified order 
#
orderBy = function(D, order) {
  if (order == "alpha") {
    sorted <- D[order(D$stationname),]
    return(sorted)
  }
  else if (order == "min") {
    sorted <- D[order(D$rides),]
    return(sorted)
  }
  else if (order == "max") {
    sorted <- D[order(-D$rides),]
    return(sorted)
  }
  else {
    print("ERROR: invalid order choice, ignoring.")
    return(D)
  }
}


#
# filters dataframe based on date 
#
filterByDate = function(D, date) {
  return(D[D$newDate == date, ])
}


# 
#  assuming we provided merged rides data, (with coordinates), we will get a list of active stops given a certain date
#  active stops means stops that they had at least one ride on that date, inactive are typically closed/under contruction stations 
# 
getActiveStopsList = function(rides, date, dataDestination) {
  D <- filterByDate(rides, date)
  D <- D[D$rides != 0, ]
  
  if (dataDestination == "table") {
    return(D[,c("stationname", "station_id")])  
  }
  else if (dataDestination == "map") {
    return(D[,c("stationname", "station_id", "lng", "lat")])
  }
}


#
# filters rides data based on the station ID
#
filterByStationName = function(rides, ID) {
  print(paste("filtering by station id: ", ID))
  return(rides[rides$station_id == ID, ])
}



#
# we use the smaller stops dataset to get a matching stationID
#
findStationIDByCoords = function(stops, lng, lat) {
  matchingStation <- stops[stops$lng == lng & stops$lat == lat, ]
    
  if (nrow(matchingStation) > 0) {
    print("valid station id")
    matchingStation[1,"MAP_ID"] # map id is the same as station_id  
  }
  else {
    print("invalid station id")
  }
}


# -------------------------------------------------------------
#
#    helper methods below, these won't be called in the app file
#
# ------------------------------------------------------------- 


