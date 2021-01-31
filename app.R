#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(data.table)
library(tidyverse)
library(gganimate)
library(gifski)

source("www/global.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "racers.io"),
    dashboardSidebar(
        downloadButton("sampleDownload","Download Sample Data"),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "racers.css")
        ),
        fileInput("dataFile","Upload data",multiple = FALSE,accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")),
        selectInput("colCategory",label = "Category(Cities, Countries etc.)", choices = c("upload file"),selected = "gif"),
        selectInput("colValue",label = "Value(Population, Users etc.)", choices = c("upload file"),selected = "gif"),
        selectInput("colTime",label = "Time(Year, Month etc.)", choices = c("upload file"),selected = "gif"),
        textInput("chartTitle",label = "Chart Title",value = "racers.io",placeholder = "Enter chart title"),
        textInput("chartSubTitle",label = "Chart Sub Title",value = "racers.io",placeholder = "Enter chart subtitle"),
        textInput("chartCaption",label = "Chart Caption",value = "racers.io",placeholder = "Enter chart caption"),
        selectInput("outFormat",label = "Output Format", choices = c("gif"),selected = "gif"),
        actionButton("generate",label = "Generate Bar Race"),
        downloadButton("chartDownload","Download GIF")
    ),
    dashboardBody(
        
        useShinyjs(),
    conditionalPanel("input.generate > 0",
    div(id = "test",
            HTML("<p id = 'ptm' style='color:green'> For 2500 rows it take 2 minutes to process </p>"),
            withSpinner(imageOutput("chartDisplay"),color.background = "white",image = "loading.gif")
    ))
))

# Define server logic required to draw a histogram
server <- function(input, output,session){
    data_input <- reactive({
        req(input$dataFile)
        data <- fread(input$dataFile$datapath)
        data
    })
    
    observe({
        disable("chartDownload")
        disable("colTime")
        disable("colCategory")
        disable("colValue")
        disable("chartTitle")
        disable("chartSubTitle")
        disable("chartCaption")
        disable("outFormat")
        disable("generate")
        req(input$dataFile)
        enable("colTime")
        enable("colCategory")
        enable("colValue")
        enable("chartTitle")
        enable("chartSubTitle")
        enable("chartCaption")
        cols <- colnames(data_input())
        updateSelectInput(session = session, inputId = "colCategory",choices = c("select",cols),selected = input$colCategory)
        updateSelectInput(session = session, inputId = "colValue",choices = c("select",cols),selected = input$colValue)
        updateSelectInput(session = session, inputId = "colTime",choices = c("select",cols),selected = input$colTime)
        req(input$colCategory)
        req(input$colValue)
        req(input$colTime)
        enable("outFormat")
        enable("generate")
    })
    
    output$sampleDownload <- downloadHandler(
        filename = function() {
            paste("sample_racers_io.csv")
        },
        content = function(file) {
            file.copy("www/gdp_tidy.csv",file)
        }
    )
    
    
    observeEvent(input$generate,{   
        req(input$dataFile)

        
        GENERATE_BARRACE(input,output,session,
            data = data_input(),
            varTime = input$colTime,
            varCat = input$colCategory,
            varValue = input$colValue,
            varTitle = input$chartTitle,
            varSubTitle = input$chartSubTitle,
            varCaption = input$Caption,
            varOut = tolower(input$outFormat)
            ,frames = 200
    )
        
        output$chartDownload <- downloadHandler(
            filename = function() {
                paste('racingbars_', 'racers_io', '.gif', sep='')
            },
            content = function(file) {
                file.copy(paste0("www/gganim.",input$outFormat),file)
            }
        )
       })
}

# Run the application 
shinyApp(ui = ui, server = server)
