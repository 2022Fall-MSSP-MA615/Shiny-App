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
library(rsconnect)
library(rjson)
library(RColorBrewer)

data <- read.csv("Cannabis_Registry.csv")
data <- data.frame(data)
data <- data %>% rename(Longitude = x)
data <- data %>% rename(Latitude = y)
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data %<>% select(ObjectId, updated_timestamp, SAM_ID, Application_Status, Facility_Address, Main_Entity_Name,
                 Type_of_Marijuana_License, Latitude, Longitude)
data1 <- data %>%
  group_by(Type_of_Marijuana_License) %>%
  group_by(Application_Status) %>%
  summarise(COUNT=n())%>%
  slice_head(n=10)

data2 <- data %>%
  group_by(Type_of_Marijuana_License) %>%
  summarise(COUNT=n())

spdata <- st_read("City_of_Boston_Boundary.geojson")

map <- ggplot()+
  geom_sf(data=spdata,colour='#0C090A',fill=NA)+
  theme_bw()
map

ui <- navbarPage("Cannabis Registry", id="main",
                 tabPanel("Liscense Type and Location",
                          selectInput("weedid", "Weed Licsense Type", 
                                      choices = data2$Type_of_Marijuana_License,
                                      selected = "Adult use dispensary"),
                          fluidRow(plotOutput("Licsense_Plot", width=700,height=700))),
                                     tabPanel("Interactive Map",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  titlePanel("Location of Cannabis Shop"),
                                                  fluidRow(column(12, selectInput(
                                                    inputId = 'License_Type',
                                                    label = 'Select Type',
                                                    choices = unique(data$Type_of_Marijuana_License)),
                                                    selected = "Medical"),
                                                    column(12, selectInput(
                                                      inputId ='Application_Status',
                                                      label = 'Select status',
                                                      choices = unique(data$Application_Status),
                                                      selected = "Approved")))),
                                                mainPanel(fluidRow(plotOutput("Cannabis_Plot", width=700,height=700))))),
                                     tabPanel("Data", DT::dataTableOutput("data")))

server <- function(input, output, session) {
  filteredData <- reactive({
    if (input$Application_Status == "All status") {
      data
    } else {
      filter(data, Application_Status == input$Application_Status)
    }
  })

  #output$map <- renderPlot(ggplot()+
  #                              geom_sf(data=spdata,colour='#0C090A',fill=NA)+
  #                              theme_bw())
  output$Licsense_Plot = renderPlot({
    Weed_Lic = data %>%
      filter(Type_of_Marijuana_License == input$weedid) %>%
      group_by(Longitude,Latitude) %>%
      summarise(n=n()) 
    total <- data2[match(input$weedid, data2$Type_of_Marijuana_License,"COUNT")]
    
    map+
      geom_point(mapping=aes(x=Longitude,y=Latitude,color=n,size=n),data=Weed_Lic,alpha=1,pch=20)+
      scale_colour_gradientn(colors=c(rev(brewer.pal(9,'BuGn'))))+
      labs(x='',y='',title=paste0('Distribution of Cannabis Business in Boston'))+
      scale_size_continuous(range=c(2,10))
  })
  output$data <-DT::renderDataTable(datatable(
    data,filter = 'top',
    colnames = c("ObjectId", "updated_timestamp", "SAM_ID", "Application_Status", "Facility_Address",
                 "Main_Entity_Name", "Type_of_Marijuana_License", "Latitude", "Longitude" )
  ))
  output$Cannabis_Plot <- renderPlot({
    weed1 <- data %>% 
      filter(Type_of_Marijuana_License==input$License_Type) %>%
      filter(Application_Status==input$Application_Status) %>%
      group_by(Longitude,Latitude) %>%
      summarise(n=n())
    totalN<-data1[match(input$License_Type,
                                input$Application_Status,
                                data1$Application_Status),'COUNT']
    
    map+
      geom_point(mapping=aes(x=Longitude,y=Latitude,color=n,size=n),data=weed1,alpha=1,pch=20)+
      scale_colour_gradientn(colors=c(rev(brewer.pal(9,'YlGnBu'))))+
      labs(x='',y='',title=paste0('Distribution of Cannabis Business in Boston'))+
      scale_size_continuous(range=c(0.6,6))
})
}
shinyApp(ui, server)


