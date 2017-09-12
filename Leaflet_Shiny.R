## LIBRARIES ##
.libPaths("C:/R Packages")
library(shiny)
library(shinythemes)
library(stringr)
library(htmltools)
library(leaflet)
library(rvest)
library(dplyr)

## DATASET ##
Irma_data <- read.csv("H:/Irma_data.csv")
Irma_data$Date <- paste("2017",Irma_data$Date,sep="/")
Irma_data$Date <- Irma_data$Date %>% gsub(" ","/",.) %>% gsub("Aug","08",.) %>% gsub("Sep","09",.)  
Irma_data$Date <- as.Date(Irma_data$Date)
Irma_data$Category_chr <- as.character(Irma_data$Category_chr)
  
## Setup Icons ##
HurricaneIcons <- iconList(
  None = makeIcon("H:/Hurricane_green.png",30,30),
  One = makeIcon("H:/Hurricane_green.png",30,30),
  Two = makeIcon("H:/Hurricane_green.png",30,30),
  Three = makeIcon("H:/Hurricane_yellow.png",30,30),
  Four = makeIcon("H:/Hurricane_orange.png",30,30),
  Five = makeIcon("H:/Hurricane_red.png",30,30)
)

## SHINY APPLICATION CODE ##

## CODE FOR THE USER INTERFACE ##
ui <- navbarPage(
  "Leaflet & Shiny Integration",
  
  theme = shinytheme("paper"),
  
  tabPanel(
    "Trajectory of Hurricane Irma & Hurricane Jose",
    
    sidebarLayout(
      
      sidebarPanel(
        p("This Shiny application is a simple example of how Leaflet and Shiny can be integrated
          to produce powerful data visualizations."),
        p("The map to the right details the trajectory of hurricane Irma or Jose based on the hurricane 
          and date range selected. The play button can be used to see how each of the hurricane's trajectories 
          has moved over time."),
        br(),
        
        selectInput(inputId = "inputHurricane",label = "Choose Hurricane:",
                    choices = c("Irma" = "Irma","Jose" = "Jose")),
        
        conditionalPanel(
          condition = "input.inputHurricane == 'Irma'",
          sliderInput(inputId = "rangeDate",label = "Date range:",
                      min = as.Date("2017-08-29"), max = as.Date("2017-09-11"),
                      value = as.Date("2017-08-29"),step = 1,timeFormat = "%Y-%m-%d",
                      animate = animationOptions(interval = 900, loop = FALSE))
        ),
        
        conditionalPanel(
          condition = "input.inputHurricane == 'Jose'",
          sliderInput(inputId = "rangeDate2",label = "Date range:",
                      min = as.Date("2017-09-04"), max = as.Date("2017-09-11"),
                      value = as.Date("2017-09-04"),step = 1,timeFormat = "%Y-%m-%d",
                      animate = animationOptions(interval = 900, loop = FALSE))
        ),
        width = 3
      ),
      
      mainPanel(
        leafletOutput("plot1", height = "700")
      )
      
    )
  )
)

## SERVER CODE ##
server <- shinyServer(function(input, output) {
  
  new_Data <- reactive({
    if(input$inputHurricane == "Irma") {
      Irma_data %>% filter(Date <= as.Date(input$rangeDate) & Name == input$inputHurricane)
    }
    else{
      Irma_data %>% filter(Date <= as.Date(input$rangeDate2) & Name == input$inputHurricane)
    }
  })
  
  output$plot1 <- renderLeaflet({
    leaflet(new_Data()) %>% 
      setView(lat = 13.1132219, lng = -59.5988, zoom = 4) %>%
      addTiles() %>%
      addMarkers(
        icon = ~HurricaneIcons[Category_chr],
        popup = paste(
          "<b>Hurricane:</b> ",new_Data()$Name,"<br>",
          "<b>Date:</b> ",new_Data()$Date,"<br>",
          "<b>Time:</b> ",new_Data()$Time,"<br>",
          "<b>Wind:</b> ",new_Data()$Wind,"<br>",
          "<b>Pressure:</b> ",new_Data()$Pressure,"<br>",
          "<b>Storm Type:</b> ",new_Data()$Storm.Type,"<br>",
          "<b>Category:</b> ",new_Data()$Category,sep=""
        )
      )
  })
})

## RUN THE SHINY APPLICATION CODE ##
shinyApp(ui = ui, server = server)
