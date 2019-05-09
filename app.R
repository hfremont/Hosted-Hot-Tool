# Load required packages
library("shiny")
library("devtools")
if(!require("HOT")){
  devtools::install_github("GHI-UW/HOT")
}
  
library("HOT")
library("plotly")
library("shinydashboard")
library("leaflet")
library("shinyWidgets")

# Source related functions 
source("./allFunctions.R")


## UI definition
ui <- fluidPage(
  
  # Static header to app
  fluidRow(
    column(4,
           h3("Health-Oriented Transportation Model"),
           h4("Calculating the Health Benefits of Active Transportation")
    )),
  
  hr(),
  
  # Dynamic body of app
  uiOutput("mainOut"),
  br(), br(),
  
  # Start Over/Next buttons definition
  div(style = "position:absolute;left:1em;bottom:1em;",
      actionButton("reset", "Start Over", style = "color: white; background-color: #383838;"),
      actionButton("goNext", "Next Page", style = "color: white; background-color: #383838;")
  ),
  
  # Global Health Institute @ UW-Madison logo
  div(img(src='GlobalUW.png', align = "bottom"), style = "position:absolute;right:1em;bottom:1em;")
)


## Server definition

# Server function
server <- function(input, output, session) {
  
  
  ## Page number initialization code
  
  # Initialise page number variable
  pageNum <- reactiveVal(1)
  
  # Next/Reset/newMetric button definitions
  observeEvent(input$goNext, { if( pageNum() < 4 ){newVal <- pageNum() + 1 ; pageNum(newVal) } })
  observeEvent(input$reset, { pageNum(1) })
  observeEvent(input$newMetric, { pageNum(3) })
  
  
  ## Page number-based dynamic UI code
  
  # Define dynamic frame of UI
  output$mainOut <- renderUI({
    
    if( pageNum() == 1 ){
      # Welcome/reset page
      tagList(
        titlePanel("HOT Shiny App"),
        mainPanel(tags$style(type = "text/css", "a{color: black;}"), getAppDescription())
      )
      
    }else if( pageNum() == 2 ){
      # Travel Survey selection/description page
      tagList(
        titlePanel("Select a location"),
        mainPanel("We use travel survey data to estimate model parameters. To analyze a travel survey not listed here select the \"Custom\" option below.",
                  br(), br(),
                  radioButtons("survey", "Choose a Travel Survey:", choices = c("London", "France", "USA", "Custom")),
                  br(),
                  uiOutput("customTS")
        ))
    }else if( pageNum() == 3 ){
      # HOT metric selection
      tagList(
        titlePanel("Select a HOT metric"),
        mainPanel(getMetricDescription(),
                  br(), br(),
                  radioButtons("metric", "Choose a metric:", choices = c("CRA", "Participation", "Proportion", "Frequency", "Intensity", "Duration", "Trips")),
                  br(),
                  
                  # Include previous page selection for user reference
                  h5(paste("Location selected:", input$survey)))
      )
    }else if( pageNum() == 4 ){
      # Final plot page
      tagList(
        titlePanel("Visualization"),
        sidebarLayout(
          sidebarPanel(
            
            # Display options dependent on metric selected
            switch(input$metric,
                   "CRA" = {
                     # CRA-specific selection menu
                     tagList(
                       "Select a scenario for which to assess risk: calculate relative change in risk from each location's participation level to the given scenario.",
                       br(), br(),
                       radioButtons("scenario", "Choose a scenario:", choices = c("Full", "Zero", "Custom")),
                       "Select an exposure-response curve from related literature, demonstrating relative risk of all-cause mortality as a function of physical activity.",
                       br(), br(),
                       radioButtons("author", "Choose an author:", choices = c("Arem", "Lear", "Wen"), selected = "Arem"),
                       uiOutput("learIncome"),
                       br())
                   },
                   "Duration" = {
                     # Duration-specific selection menu
                     tagList(
                       "Select a mode for which to calculate mean daily total duration (min/day).",
                       br(), br(),
                       radioButtons("mode", "Choose a mode:", choices = c("Walk", "Cycle", "Other")),
                       br())
                   },
                   "Trips" = {
                     # Trips-specific selection menu
                     tagList(
                       "Select a mode for which to calculate mean individual number of trips (trips/day).",
                       br(), br(),
                       radioButtons("mode", "Choose a mode:", choices = c("Walk", "Cycle", "Other")),
                       br())
                   },
                   { # Default -- no additional options. Metrics: participation, proportion, frequency, intensity, TA
                     tagList( "No additional options to select.", br(), br(), "Click 'Analyze' below to generate visualization.", br(), br() )
                   }),
            
            # Include previous page selections for user reference
            h5(paste("Location selected:", input$survey)),
            h5(paste("Metric selected:", input$metric)),
            br(),
            
            # Analyse button to generate map
            actionButton("goMap", "Analyze", style = "color: white; background-color: #383838;"),
            
            # Change metric button to select a new metric
            actionButton("newMetric", "Change Metric", style = "color: white; background-color: #383838; float: right;")
          ),
          
          # Main panel plot of the generated distribution, shown as a heat map of London
          mainPanel(plotlyOutput('mainMap'))
        ))
    }
  })
  
  
  ## Dynamic UI subsections: custom TS, Lear income
  
  # Dynamic code to display Travel Survey upload options when "Custom" is selected as TS
  output$customTS <- renderUI({
    if( input$survey == "Custom" ){
      tagList(
        h3("Create your own travel survey object."),
        br(),
        fileInput("custom", "Upload custom travel survey (.rds)", accept = "rds"),
        "Instructions on how to create travel survey"
      )
    }
  })
  
  # Dynamic code to display income options when Lear is selected as author
  output$learIncome <- renderUI({
    if( input$author == "Lear" ){
      tagList(
        br(),
        radioButtons("income", "Lear's function requires an income. Choose an income level to investigate:", choices = c("High", "Upper-middle", "Lower-middle", "Low")))
    }
  })
  
  
  ## Map rendering at button trigger code
  
  # Analyse button to generate London map, given user input in sidebar
  mapEvent <- eventReactive(input$goMap,{
    
    # Map generation code outsourced to allFunctions.R
    getShinyMap(inputSurvey = input$survey, inputCustom = input$custom, inputMetric = input$metric, inputMode = input$mode, inputScenario = input$scenario, inputAuthor = input$author, inputIncome = input$income)
  })
  
  # Overall plot rendering
  output$mainMap <- renderPlotly({
    
    # Generate plot/map
    map.df <- mapEvent()
    
    # Make plot interactive, with dynamic width/height
    ggplotly(map.df) %>% config(displayModeBar = FALSE)
  })
  
  
  ## Final app closure code
  
  # App closes once browser window is terminated and cleans up generated file
  session$onSessionEnded(function() {
    
    # Terminate app process
    stopApp()
    
    # Clear out app-generated files (function sourced from 'allFunctions.R')
    cleanup()
  })
}


## Run the application
shinyApp(ui = ui, server = server)

