library(googleway)
library(dplyr)
library(jsonlite)
library(ggmap)
library(rgeos)
library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("sandstone"),
    # Application title
    titlePanel("Handy Dandy Bike Navigator"),
    
    tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
        verticalLayout(
                p(strong("Consent to use your location?")),
                verbatimTextOutput("geolocation"),
                hr(),
                actionButton(label = "Use current location?", inputId = "UseLOC"),
                textInput(label = "Enter Starting Address", inputId = "Home_LOC", placeholder = "Where you want to go"),
                textInput(label = "Enter ending location", inputId = "End_LOC", placeholder = "The place you want to go"),
                numericInput(label = "How many bikes do you need?", inputId = "BikeNumber", value = 1),
                actionButton(label = "Full Map", inputId = "button"),
                uiOutput("dankmap")
    )

)

# Define server logic required to proc the maps
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
#define server
server <- function(input, output, session) {
  
    output$loc <- renderPrint({
        paste0(input$lat, ",", input$long)
    })
    
    
    output$geolocation <- renderPrint({
        input$geolocation
    })
    observeEvent(input$UseLOC, {
      x <- input$lat
      y <- input$long
      updateTextInput(session,"Home_LOC", value = paste0(x, ",", y))
    })
    observeEvent(input$button, {
        get_closest <- function(location){
            location <- geocode(location, output = c("latlon"), source = "google")
            station_BigData <- station_BigData %>% filter(num_bikes_available >= input$BikeNumber)
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
        #bike from station to station
        get_closest_end <- function(location){
            location <- geocode(location, output = c("latlon"), source = "google")
            station_BigData <- station_BigData %>% filter(num_docks_available > input$BikeNumber)
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
        end_loc <- get_closest_end(input$End_LOC) #where I'm trying to go
        end_coord <- c(end_loc$lat, end_loc$lon)
        end_string <- paste0(end_loc$lat, ",", end_loc$lon)
        #define locations
        current_loc <- get_closest(input$Home_LOC)
        current_coord <- c(current_loc$lat, current_loc$lon)
        current_string <- paste0(current_loc$lat, ",", current_loc$lon)
        destinations <- paste0("https://www.google.com/maps/dir/?api=1&", "origin=", input$Home_LOC, "&destination=", 
                               input$End_LOC, "&travelmode=bicycling", "&waypoints=", current_string, "|", end_string)
        url <- a("Map Link", href= destinations)
        output$dankmap <- renderUI({
            tagList("URL link:", url)
        })
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
