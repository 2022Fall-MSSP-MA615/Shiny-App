library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(sf)
library(readxl)
library(rgdal)
library(maptools) 
library(sp) 
library(leaflet)

data <- read.csv("Cannabis_Registry.csv")
data <- data.frame(data)
data <- data %>% rename(Longitude = x)
data <- data %>% rename(Latitude = y)
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data %<>% rename(Application_Status = "\u9518\u7dfcpplication_Status")
data %<>% select(ObjectId, updated_timestamp, SAM_ID, Application_Status, Facility_Address, Main_Entity_Name,
                 Type_of_Marijuana_License, Latitude, Longitude)


spdata <- st_read("Boston_Neighborhoods.shp")
map <- ggplot(spdata) + 
  geom_sf(size = 1, color = "blue") + 
  ggtitle("Boston") + 
  coord_sf()+
  geom_sf_text(aes(label = Name, geometry = geometry), color = 'red', size=2)+
  theme_bw()

ui <- fluidPage(
  sidebarLayout(mainPanel(navbarPage("Cannabis Registry", id="main",
                                     tabPanel("Map",plotOutput("map")),
                                     tabPanel("Data", DT::dataTableOutput("data")))),
                sidebarPanel(top = 50, right = 10,
                             pickerInput("Application Status", label = "Select a status:",
                                         choices = list("Approved BCB - Executed HCA - Pending CCC",
                                                        "Open for Operations", "Open for operation",
                                                        "Conditionally approved", "Approved", 
                                                        "Withdrawn", "Executed HCA - Pending CCC", 
                                                        "Approved BCB - Executed HCA - Pending CCC - Equity"),
                                         options = list(
                                           `live-search` = TRUE)))
  ))

server <- function(input, output, session) {
  filteredData <- reactive({
    if (input$Application_Status == "All status") {
      data
    } else {
      filter(data, Application_Status == input$Application_Status)
    }
  })

  output$map <- renderPlot(ggplot()+
                                geom_sf(data=spdata,colour='#0C090A',fill=NA)+
                                theme_bw())
  
  output$data <-DT::renderDataTable(datatable(
    data,filter = 'top',
    colnames = c("ObjectId", "updated_timestamp", "SAM_ID", "Application_Status", "Facility_Address",
                 "Main_Entity_Name", "Type_of_Marijuana_License", "Latitude", "Longitude" )
  ))
}

shinyApp(ui, server)