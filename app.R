#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(base64enc)
library(shinyFeedback)
library(locfit)
library(EBImage)
library(fs)
source("jpegread.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Whole stained cells count"),
  
  sidebarPanel(
    h4("1/Choose a JPEG file"),
    fileInput("file1"," ", accept = "image/jpg"),
    br(),
    h4("2/Please select all JPEG files to analyse"),
    fileInput('Allfiles', ' ', multiple = TRUE),
    br(),
    br()
    ),
  mainPanel(p("This app allows to count stained cells (DAPI or dead (e.g. BOBO I 3))."),
            p("To proceed, please select a folder with ",strong("ONLY"),"jpeg files."),
            p("First, select a JPEG file from the folder to analyse. Then determine a value for the blurring filter (background noise, >=1) and a value for the threshold (>=1).
              Once these values are set, select all the files to analyse with these parameters."),
            br(),
            br(),
            br(),
            shinycssloaders::withSpinner(uiOutput("image")),
            shinyFeedback::useShinyFeedback(),
            br(),
            br(),
            numericInput("n1", "Choose a value for the blurring filter (>=1)", value = 0),
            submitButton("Apply change", icon("sync")),
            br(),
            displayOutput("imageblur"),
            br(),
            numericInput("n2", "Choose a value for the threshold (>=1)", value = 0),
            submitButton("Apply change", icon("sync")),
            br(),
            displayOutput("imagethreshold"),
            br(),
            # shinycssloaders::withSpinner(tableOutput("tbl_out")),
            # shinyFeedback::useShinyFeedback(),
            # br(),
            shinycssloaders::withSpinner(tableOutput("res")),
            shinyFeedback::useShinyFeedback()
            )
)

server <- function(input, output, session) {
  base64 <- reactive({
    inFile <- input[["file1"]]
    dataURI(file = req(inFile$datapath), mime = "image/png")
  })
  
  output[["image"]] <- renderUI({
    if(!is.null(base64())){
      tags$img(src= base64(), width="80%")
    }
  })
  
  blurry_test <-reactive({
    imgtest <- readImage(input$file1$datapath)
    wtest = makeBrush(size = 11, shape = 'gaussian', sigma = 5)
    filter2(imgtest * req(input$n1), wtest)
  })
  
  
  output[['imageblur']] <- renderDisplay({
    if(!is.null(base64())){
      display(blurry_test())
    }
  })

  threshold_test <-reactive({
    imgtest <- readImage(input$file1$datapath)
    nmaskt_test = thresh(imgtest *input$n2, w=10, h=10, offset=0.05)
  })
  
  
  output[['imagethreshold']] <- renderDisplay({
    if(!is.null(base64())){
      display(threshold_test())
    }
  })
  
  # filelist <- reactive({
  #   df1 <- input$Allfiles$name
  #   return(df1)
  # })
  # 
  # output[['tbl_out']] <- renderTable({
  #   filelist()
  # })
  # 
  
  plop = reactive({
  req(input$Allfiles)
  jpegread(input$Allfiles$name, input$Allfiles$datapath, input$n1, input$n2)
  })
   
  output[['res']] = renderTable({plop()})
}

# Run the application
shinyApp(ui = ui, server = server)
