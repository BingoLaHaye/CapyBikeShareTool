library(googleway)
library(dplyr)
library(jsonlite)
library(ggmap)
library(rgeos)
#used to pull capital bikeshare data
get_Capital <- function(url) {
  doot <- fromJSON(url)
  doot2 <- doot$data
  doot3 <- do.call(what = "rbind",
                   args = lapply(doot2, as.data.frame))
  return(doot3)
}
station_DataDF <- get_Capital("https://gbfs.capitalbikeshare.com/gbfs/en/station_information.json")
station_statusDF <- get_Capital("https://gbfs.capitalbikeshare.com/gbfs/en/station_status.json")
station_BigData <- station_DataDF %>%
  left_join(station_statusDF, by = "station_id")
#Define for closest Location
get_closest <- function(location){
  location <- geocode(location, output = c("latlon"), source = "google")
  station_BigData <- station_BigData %>% filter(num_bikes_available > 0)
  set1sp <- SpatialPoints(location) #define location points
  station_sub <- station_BigData[, 6:5]#subset for lon lat
  station_subSP <- SpatialPoints(station_sub) #match it back to a spatial data frame
  blep <- gDistance(set1sp, station_subSP, byid = TRUE)
  n <- station_BigData %>% #cbind back into larger data frame
    cbind(blep)
  dock <- n %>%
    arrange(`1`) %>%
    head(1L) %>%
    select(name, lat, lon)
  return(dock)
}
current_loc <- get_closest("3230 7th Street NE")
current_coord <- c(current_loc$lat, current_loc$lon)
#walking to station
google_map_directions(origin = "Home", destination = current_coord, 
                      travel_mode = "walking")
#bike from station to station

get_closest_end <- function(location){
  location <- geocode(location, output = c("latlon"), source = "google")
  station_BigData <- station_BigData %>% filter(num_docks_available > 0)
  set1sp <- SpatialPoints(location) #define location points
  station_sub <- station_BigData[, 6:5]#subset for lon lat
  station_subSP <- SpatialPoints(station_sub) #match it back to a spatial data frame
  blep <- gDistance(set1sp, station_subSP, byid = TRUE)
  n <- station_BigData %>% #cbind back into larger data frame
    cbind(blep)
  dock <- n %>%
    arrange(`1`) %>%
    head(1L) %>%
    select(name, lat, lon)
  return(dock)
}
end_loc <- get_closest_end("Lincoln Memorial") #where I'm trying to go
end_coord <- c(end_loc$lat, end_loc$lon)
google_map_directions(origin = c(current_loc$lat, current_loc$lon), destination = end_coord, 
                      travel_mode = "bicycling")

#walking from bike to end destination
google_map_directions(origin = end_coord, destination = "Lincoln Memorial", travel_mode = "walking")
#total map with stops included(maybe i can paste together segments as a full trip)

