destinations <- paste0("https://www.google.com/maps/dir/?api=1&", "origin=", "3230 7th Street Ne Washington DC", "&destination=", 
                       "Lincoln Memorial", "&travelmode=bicycling", "&waypoints=", blep)
url <- a("Map Link", href="destinations")
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
doot <- get_closest_end("3230 7th Street Ne Washington DC")
c(doot$lat, doot$lon)
blep <- paste0(doot$lat, ",", doot$lon)
