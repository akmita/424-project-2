#
#  plotHelper.r - contains various functions to help manipulate and plot data
#


getMainBarGraph = function(D, date, order, visType) {

  D_ <- orderBy(filterByDate(D, date), order) 

  # prevent ggplot from sorting automatically
  D_$stationname <- factor(D_$stationname, levels = D_$stationname)
  
  if (visType == "Bar Graph") {
    (ggplot(
      data=D_, 
      aes(x=stationname, y=rides)) 
     + geom_bar(stat="identity")
     + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # fixes overlapping names
     + labs(title = paste("Number Of Rides On", weekdays(as.Date(D_[1,"newDate"])), date), x = "station name")
    )
  }
  else if (visType == "Table") {
    return(D_)
  }
  else {
    print(paste("ERROR: invalid visualization type", visType))
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
findStationByCoords = function(stops, lng, lat) {
  matchingStation <- stops[stops$lng == lng & stops$lat == lat, ]
    
  if (nrow(matchingStation) > 0) {
    print("valid station id")
    return(matchingStation[1, ])
  }
  else {
    print("invalid station id")
  }
}


# -------------------------------------------------------------
#
#    functions from project 1
#
# ------------------------------------------------------------- 


#
# creates a bar graph based on user criteria 
# 
# @location - location of stations
# @barGraphSelect - type of bar graph user selected
# @yearSelected - year the user selected
# @D - station and ride count dataset 
# 
createBarGraph <- function(location, barGraphSelect, yearSelected, D) {
  print(head(D))
  
  # yearly view
  if (barGraphSelect == graphChoices[1]) {
    (ggplot(
      data=parseByYear(D, location), 
      aes(x=year, y=rides)) 
     + geom_bar(stat="identity") 
    )
  }
  # daily view
  else if (barGraphSelect == graphChoices[2]) {
    (ggplot(
      data=parseByDay(D, yearSelected, location), 
      aes(x=newDate, y=rides))
     + geom_bar(stat="identity")
     + labs(x = "Date")
    )
  }
  # monthly view
  else if (barGraphSelect == graphChoices[3]) {
    # TODO
    (ggplot(data=parseByMonth(D, yearSelected, location), aes(x=month, y=rides))
     + geom_bar(stat="identity")
     + labs(x = "Month")
    )
  }
  # day of the week view
  else if (barGraphSelect == graphChoices[4]) {
    # TODO
    
    (ggplot(data=parseByWeekday(D, yearSelected, location), aes(x=dayOfWeek, y=rides))
     + geom_bar(stat="identity")
     + labs(x = "Day of the Week")
    )
  }
  else {
    print("failed to load graph")
  }
}



#
# get appropriate graph data for table - virtually identical to createBarGraph() - except it generates a table
#
getTable = function(barGraphSelect, yearSelected, location, D) {
  
  
  
  # yearly view
  if (barGraphSelect == graphChoices[1]) {
    return(parseByYear(D, location))
  }
  # daily view
  else if (barGraphSelect == graphChoices[2]) {
    return(parseByDay(D, yearSelected, location))
  }
  # monthly view  
  else if (barGraphSelect == graphChoices[3]) {
    return(parseByMonth(D, yearSelected, location))
  }
  # day of the week view
  else if (barGraphSelect == graphChoices[4]) {
    return(parseByWeekday(D, yearSelected, location))
  }
  else {
    print("failed to load table")
  }
}



#
# parse dataset to show rides per year
#
parseByYear = function(D, location) {
  D <- aggregate(rides~year,D,sum)        # group by year
  return(D);
}


#
# parse dataset to show rides per day, for specific year
#
parseByDay = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # filter by year
  return(D)
}


#
# parse dataset to show rides per month, specific year
#
parseByMonth = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # get subset only selected year
  D <- aggregate(rides~month,D,sum) # aggregate per month
  return(D)
}


#
#  parse dataset to show rides per day of week, given year
#
parseByWeekday = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected)  # get subset only selected year
  D$dayOfWeek <- weekdays(as.Date(D$newDate))   # get weekdays 
  D <- aggregate(rides~dayOfWeek,D,sum)         # group by weekday
  # D <- D[c(4,5,6,7,1,2,3),] # try to rearrange days in better order
  return(D)
}
