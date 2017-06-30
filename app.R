##### FOOTBALL EXAMPLE #####

## LIBRARIES ##
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stats)

## DATASET ##
Footy <- read.csv("C:/inst/shinyapp/Footballstats_2016_2017.csv")
Footy$Game_Number <- factor(Footy$Game_Number,levels=unique(Footy$Game_Number))

# Define UI for application that draws a histogram
ui <- fluidPage( 
  
  theme = shinytheme("cerulean"),
  
   titlePanel(tags$b("English Premier Football League")),
  
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "teamInput",label = "Select Teams",
                           choices = levels(Footy$Team),selected = levels(Footy$Team))
      ),
      
      mainPanel(
        
          tabsetPanel(
            
          tabPanel(
            "Premiere League Total Points per Team",
            br(),
            plotOutput("plot1")
          ),
          
          tabPanel(
            "Cumulative Team Points Over Time",
            br()
            ,
            plotOutput("plot2")
          )
      
          )
        
   )
))


server <- function(input, output) {
  
  Footy_new <- reactive({
    Footy[Footy$Team %in% input$teamInput,]
  })
  
  Footy_teams <- reactive({
    Footy_team <- Footy_new()
    Footy_team <- transform(Footy_team,Team = reorder(Team,-Total_points))
    Footy_team <- aggregate(Footy_team,by=list(key=Footy_team$Team),FUN=function(x){x[1]})
  })
  
  output$plot1 <- renderPlot({
    print(Footy_teams())
    ggplot(Footy_teams(),
           aes(x=Team,y=Total_points,fill=Total_points))+
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(
        plot.background = element_rect(fill = "transparent", colour = NA)
      )+ theme(axis.text = element_text(size = 14))+ theme(axis.title = element_text(size = 14))
  }, height = 650, bg = "transparent")
   
  output$plot2 <- renderPlot({
    print(Footy_new())
    ggplot(Footy_new(),
           aes(x=Game_Number,y=Cumulative_points,group=Team,color=Team))+
      geom_step(lwd=1.15)+ theme(axis.text = element_text(size = 14))+ theme(axis.title = element_text(size = 14))
  }, height = 800)
    
}

shinyApp(ui = ui, server = server)

