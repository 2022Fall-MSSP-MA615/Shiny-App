library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(magrittr)

data <- read.csv("D:/R dataset/615/Cannabis_Registry.csv")
data <- data.frame(data)
data <- data %>% rename(Longitude = x)
data <- data %>% rename(Latitude = y)
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data %<>% select(ObjectId, updated_timestamp, SAM_ID, Application_Status, Facility_Address, Main_Entity_Name,
                 Type_of_Marijuana_License, Latitude, Longitude)

ui <- fluidPage(
  sidebarLayout(mainPanel(navbarPage("Cannabis Registry", id="main",
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
  
  
  output$data <-DT::renderDataTable(datatable(
    data,filter = 'top',
    colnames = c("ObjectId", "updated_timestamp", "SAM_ID", "Application_Status", "Facility_Address",
                 "Main_Entity_Name", "Type_of_Marijuana_License", "Latitude", "Longitude" )
  ))
}

shinyApp(ui, server)