## LIBRARIES ##
.libPaths("C:/R Packages")
library(shiny)
library(shinythemes)
library(ggplot2)
library(plyr)
library(dplyr)
library(nnet)
library(leaflet)

## DATASET ##
Footy <- read.csv("H:/Shiny Tutorial Documents/Footballstats_2016_2017.csv")
Footy_loc <- read.csv("H:/Shiny Tutorial Documents/Footy_location.csv")

Footy_loc$Date <- as.Date(as.character(Footy_loc$Date),format="%d/%m/%Y")
choices_list = list("Chelsea" = "Chelsea","Tottenham" = "Tottenham", "Man City" = "Man City",
                    "Liverpool" = "Liverpool", "Arsenal" = "Arsenal", "Man United" = "Man United",
                    "Everton" = "Everton", "Southampton" = "Southampton", "Bournemouth" = "Bournemouth",
                    "West Brom" = "West Brom", "West Ham" = "West Ham", "Leicester" = "Leicester",
                    "Stoke" = "Stoke", "Crystal Palace" = "Crystal Palace", "Swansea" = "Swansea",
                    "Burnley" = "Burnley", "Watford" = "Watford", "Hull" = "Hull", "Middlesbrough" = 
                      "Middlesbrough", "Sunderland" = "Sunderland")

FootballIcons <- iconList(
  W = makeIcon("H:/Shiny Tutorial Documents/Football_green.png",30,30),
  D = makeIcon("H:/Shiny Tutorial Documents/Football_orange.png",30,30),
  L = makeIcon("H:/Shiny Tutorial Documents/Football_red.png",30,30)
)

## SHINY APPLICATION CODE ##

## CODE FOR THE USER INTERFACE ##
ui <- 
  
  navbarPage(
  
  "English Premier Football League",
  
  ## Sets the theme for the Shiny application ##
  theme = shinytheme("yeti"),
  
  ## Sets the title for the Shiny application ##

  tabPanel(
    "Points per Team",
  ## Sets the side bar layout ##
    sidebarLayout(
    
    ## Sidebar panel ##
      sidebarPanel(
      
        checkboxGroupInput(inputId = "teamInput",label = "Select Teams",
                    choices = choices_list, selected = choices_list
        ),
        sliderInput(inputId = "gameInput",label = "Select Games",
                    min = 1, max = 38, step = 1,value=38,
                    animate = animationOptions(interval = 2000, loop = FALSE)),
       width = 3),
          mainPanel(
            plotOutput("plot1", height = "700")
          )
        )
      ,icon = icon("soccer-ball-o")),
    
    ## Main panel ##
        ## Tab panel: Model Output (this is where the model prediction is displayed) ##
      tabPanel(
          "Team vs. Rival Point Analysis",
          sidebarLayout(
            
            ## Sidebar panel ##
            sidebarPanel(
              selectInput(inputId = "myTeam", label = "Select Favourite Team",
                          choices = choices_list),
              selectInput(inputId = "teamHate", label = "Select Your Rival",
                          choices = choices_list),
              # checkboxGroupInput(inputId = "teamInput2",label = "Select Teams",
              #                    choices = choices_list
              # ),
              sliderInput(inputId = "gameInput2",label = "Select Games",
                          min = 1, max = 38, step = 1,value=1,
                          animate = animationOptions(interval = 400, loop = FALSE)),
              width = 3),
              mainPanel(
                plotOutput("plot2", height = "700")
              )
            )
          ,icon = icon("soccer-ball-o")),
    tabPanel(
      "Team Away Game Travel",
      ## Sets the side bar layout ##
      sidebarLayout(
  
        ## Sidebar panel ##
        sidebarPanel(
          
          radioButtons(inputId = "teamInput3",label = "Select Team",
                             choices = choices_list
          ),
          # sliderInput(inputId = "rangeDate",label = "Date range:",
          #             min = as.Date("2017-08-29"), max = max(Irma_data$Date),
          #             value = as.Date("2017-08-29"),step = 1,timeFormat = "%Y-%m-%d",
          #             animate = animationOptions(interval = 650, loop = FALSE)),
          width = 3)
        ,
        mainPanel(
          leafletOutput("plot3", height = "700")
        )
      )
      ,icon = icon("soccer-ball-o"))
          ## Outputs the ggplot bar chart for the model output ##
          
        )


## SERVER CODE (creating the model predictions/model ouput table/model output graph) ##
server <- shinyServer(function(input, output) {
  
  # This dataset is generated based on the patient information inputed on the Patient Input tab ##
  Footy_new <- reactive({
    Footy[Footy$Team %in% input$teamInput,] %>% filter(Game_Number <= input$gameInput)
  })
  
  Footy_new_2 <- reactive({
    Footy[Footy$Team %in% c(input$myTeam,input$teamHate),] %>% filter(Game_Number <= input$gameInput2)
  })
  
  Footy_location <- reactive({
    Foot <- Footy_loc[Footy_loc$AwayTeam == input$teamInput3,] 
    Foot <- mutate(Foot,Result = 
              as.factor(ifelse(Foot$FTAG > Foot$FTHG,"W",ifelse(Foot$FTAG < Foot$FTHG,"L","D")))
            )  
  })

  Footy_teams <- reactive({
    Footy_team <- Footy_new()
    Footy_team <- aggregate(Footy_team,by=list(key=Footy_team$Team),FUN=function(x){x[input$gameInput]})
    Footy_team <- transform(Footy_team,Team = reorder(Team,-Cumulative_points))
  })

  ## This creates and renders a bar chart based on the model predictions ##
  output$plot1 <- renderPlot({
    print(Footy_teams())
    ggplot(Footy_teams(),
           aes(x=Team,y=Cumulative_points,fill=Cumulative_points))+
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("")+
      xlab("")+
      coord_flip()+
      scale_x_discrete(limits = rev(levels(Footy_teams()$Team)))+
      theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
      theme(text = element_text(size = 16),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position="none")+
      geom_text(aes(label = Cumulative_points),position = position_dodge(width=0.9), hjust = -0.5)
  })

  output$plot2 <- renderPlot({
    print(Footy_new_2())
    ggplot(Footy_new_2(),
           aes(x=Game_Number,y=Cumulative_points,group=Team,color=Team,order=Team))+
      xlim(c(0,38))+
      geom_line(lwd = 3,alpha=0.5)+
      theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
      theme(text = element_text(size = 16),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none"
            )+
      geom_text(aes(label = Cumulative_points), vjust = -0.8)+
      scale_x_continuous(breaks=c(1:38), labels=c(1:38),limits=c(1,38))+
      geom_point(aes(size=3),alpha=0.5)+
      xlab("")+
      ylab("")+
      scale_colour_manual(values = c("green3","red2"))
  })
  
  ## Let's make a map! ##
  output$plot3 <- renderLeaflet({
    leaflet(Footy_location()) %>%
      addTiles() %>%
      addMarkers(
        icon = ~FootballIcons[Result],
        popup = paste(
          "<b>Date:</b> ",Footy_location()$Date,"<br>",
          "<b>Opponent:</b> ",Footy_location()$HomeTeam,"<br>",
          "<b>Stadium:</b> ",Footy_location()$Stadium.name,"<br>",
          "<b>Capacity:</b> ",Footy_location()$Capacity,"<br>",
          "<b>Result:</b> ",Footy_location()$Result,"<br>"
        )
      )
  })
  
})

## RUN THE SHINY APPLICATION CODE ##
shinyApp(ui = ui, server = server)