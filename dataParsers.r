
# 
#  reads data from already split tsv file and formats date using lubridate
# 
getridesData = function() {
  # load first frame
  DF <- read.table(
    file = "./datasets/CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_1.tsv", 
    sep = "\t", header = TRUE)
  names(DF)<-c("station_id","stationname", "date", "daytype", "rides")
  
  # load and concatenate following frames
  i <- 2
  while (i <= 11) {
    DF <- rbind(DF, read.table(
      file = paste0("./datasets/CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_", i, ".tsv"), 
      sep = "\t", header = TRUE)
    )
    i <- i+1
  }
  
  # convert dates to newDate
  DF$date <- mdy(DF$date)
  DF$newDate <- as.Date(DF$date, "%Y/%m/%d")
  
  # split date into components
  DF$year <- format(DF$newDate, format="%Y")
  DF$month <- format(DF$newDate, format="%m")
  DF$day <- format(DF$newDate, format="%d")
  
  return(DF)
}


#
# parse date into separate columns in stops dataframe
#
getStopData = function() {
  stops <- read.csv(file = './datasets/CTA_-_System_Information_-_List_of__L__Stops.csv')
  stops$Location <- str_replace_all(stops$Location, "\\(|\\)|,", "") 
  
  lat_lon_pairs <- strsplit(stops$Location, " ", 2) 
  
  vec_lon <- vector()
  vec_lat <- vector()
  for (pair in lat_lon_pairs) {
    vec_lon <- c(vec_lon, pair[2])
    vec_lat <- c(vec_lat, pair[1])
  }
  
  stops <- cbind(stops, data.frame(lng = vec_lon, lat = vec_lat))
  
  stops$lng <- as.numeric(stops$lng)
  stops$lat <- as.numeric(stops$lat)
  return(stops)
}





