library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(magrittr)

data <- read.csv("D:/R dataset/615/tmp7_f32p54.csv")
data <- data.frame(data)
data <- data[data$MONTH == "12",]
data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)
data <- filter(data, Lat != 0)
data <- filter(data, DISTRICT != "")
data$SHOOTING[data$SHOOTING == "1"] <- "Shooting"
data$SHOOTING[data$SHOOTING == "0"] <- "Not Shooting"

data %<>% select(INCIDENT_NUMBER, DISTRICT, REPORTING_AREA, SHOOTING, OCCURRED_ON_DATE, STREET, Lat, Long, Location)

pal <- colorFactor(pal = c("#a31013", "#0259fa"), domain = c('Shooting','Not Shooting'))

ui <- fluidPage(
  sidebarLayout(mainPanel(navbarPage("2021 Crime Incident Report", id="main",
                                     tabPanel("Map", leafletOutput("bbmap", height=1000)),
                                     tabPanel("Data", DT::dataTableOutput("data")))),
                sidebarPanel(top = 50, right = 10,
                             pickerInput("DISTRICT", label = "Select a district:",
                                         choices = list("All districts", 
                                                        "A1","A7","A15","B2","B3","C6","C11","D4","D14","E5",
                                                        "E13","E18","External"),
                                         options = list(
                                           `live-search` = TRUE)))
))

server <- shinyServer(function(input, output, session) {
  filteredData <- reactive({
    if (input$DISTRICT == "All districts") {
      data
    } else {
      filter(data, DISTRICT == input$DISTRICT)
    }
  })

  
  output$data <-DT::renderDataTable(datatable(
    data,filter = 'top',
    colnames = c("INCIDENT_NUMBER", "DISTRICT", "REPORTING_AREA", "SHOOTING", 
                 "OCCURRED_ON_DATE", "STREET", "Lat", "Long", "Location")
  ))
})

shinyApp(ui, server)




###
