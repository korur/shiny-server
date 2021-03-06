## Data labelling app for machine learning  ##
## Swiping pictures ##
## Buttons          ##

library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(markdown)

# Data
# Connect to Image database

img_db <- list.files("/srv/shiny-server/aimate/images")
len  <- length(img_db)
maxrv <- length(img_db)
responsesDir <- file.path("/srv/shiny-server/aimate/responses")


# Read or Create mode

if (file.exists('/srv/shiny-server/aimate/responses/labels.csv')) {
  
  data <- read.csv('/srv/shiny-server/aimate/responses/labels.csv')
  
} else {
  
  data <- data.frame(filename = img_db, 
                     label= rep(NA,len), 
                     stringsAsFactors = T)
  write.csv(x = data, file = file.path(responsesDir, "labels.csv"),
            row.names = FALSE, quote = TRUE)
}


# Preset values

if(anyNA(data$label)){ # checks whether there is any unlabelled data
  start <- min(which(is.na(data$label))) # finds the location of the first unlabeled data
} else {
  start <- len+1 # if all data is labelled
}


mycolors=c("#1ee6be","#040000","gray")

# Functions

saveData <- function(x) {
  if(length(x) < 1) { # if no data is labeled  do nothing
    
  }else if(start != len+1){ # if there is any unlabeled data in the original data & some are labelled now execute save
    rv <- unlist(x)
    data$label[start:(start+length(rv)-1)] <- rv
    write.csv(x = data, file = file.path(responsesDir, "labels.csv"),
              row.names = FALSE, quote = TRUE)
  } else { # if there is no unlabeled data in the original, do not execute save
    # show modal can come here? to be tested
  }
}


#
#
# 
#
# UI
#
# # # # # # # #

ui <- navbarPage( 
  title="AIMATE", 
  theme = shinytheme("sandstone"),
  tabPanel("Home", icon = icon("binoculars"),
           
           div(style="margin-top:-3.5em",       
               
               fluidRow(tags$hr(),
                        useShinydashboard(), 
                        column(12, 
                               htmlOutput(outputId = "main"), 
                               valueBoxOutput("total"), 
                               valueBoxOutput("labeled"), 
                               valueBoxOutput("remaining")))),
           
           
           fluidRow(
             column(4,
                    dataTableOutput("table"),downloadButton("downl", "Export Labels", style="color: #1ee6be; background-color: #00000;font-size: 20px;
                 border-color: #2e6da4; height:50px;")),
             column(4,
                    div(imageOutput("image1"), style="text-align: center;"), 
                    tags$style(type='text/css', "button#cat {margin-left: 20%;}"),
                    tags$style(type='text/css', "button#update {margin-left: 27%;}"),
                    shinyjs::useShinyjs(),
                    
                    actionButton("cat", "Cat", icon = shiny::icon("paw", lib = "font-awesome") , 
                                 style="color: #000000; background-color: #1ee6be;font-size: 20px;
                 border-color: #2e6da4; height:50px;"),
                    actionButton("dog", "Dog", icon = shiny::icon("bone", lib = "font-awesome"),
                                 style="color: #000000; background-color: #1ee6be;font-size: 20px;
                 border-color: #2e6da4; height:50px"), 
                    actionButton("save", "save", icon = shiny::icon("save", lib = "font-awesome"),
                                 style="color: #1ee6be; background-color: #00000;font-size: 20px;
                 border-color: #2e6da4; height:50px"), br(), br(),
                    actionButton("update", "update results", icon = shiny::icon("sync", lib = "font-awesome"),
                                 style="color: #1ee6be; background-color: #00000;font-size: 20px;align: center;
                 border-color: #2e6da4; height:50px")
                    
             ),
             column(4,
                    plotOutput("count"))
             
           )),
  
  
  tabPanel(
    "About", icon = icon("info"),
    includeMarkdown("about.md")
  ) # end of about
  
)
# Server 

server <- function(input, output, session) {
  
  readdata <- eventReactive(input$update, {
    read.csv('/srv/shiny-server/aimate/responses/labels.csv', stringsAsFactors = TRUE)
  },ignoreNULL = FALSE)
  

  
  # ML labelling title
  output$main <- renderUI({
    shinyjs::hide('downl')
    HTML(paste0(tags$h2(tags$strong("Data Labeling for Machine Learning Workflows"), align = "center"), br()))
  })
  
  # reactiveValues
  rv <- reactiveValues()
  rv$value <- start
  rv$inp <- list()
  rv$stop <- start
  
  
  observeEvent(input$cat,{
    if(rv$value-1 < maxrv){
      newValue <- rv$value + 1
      rv$value <- newValue
    } else {
      newValue <- rv$value 
      rv$value <- newValue
    }
  })
  
  observeEvent(input$dog,{
    if(rv$value-1 < maxrv){
      newValue <- rv$value + 1
      rv$value <- newValue
    } else {
      newValue <- rv$value 
      rv$value <- newValue
    }
  })
  
  observeEvent(input$cat,{
    if(rv$stop-3 < maxrv){
      newStop <- rv$stop + 1
      rv$stop <- newStop
    } else {
      newStop <- rv$stop 
      rv$stop <- newStop
    }
  })
  
  observeEvent(input$dog,{
    if(rv$stop-3 < maxrv){
      newStop <- rv$stop + 1
      rv$stop <- newStop
    } else {
      newStop <- rv$stop 
      rv$stop <- newStop
    }
  })
  
  
  
  filename <- reactive({
    normalizePath(file.path('./images', img_db[rv$value]))
  })
  
  
  output$image1 <- renderImage({
    if(rv$value <= maxrv-1){
      list(
        src=filename(), height = 370,
        filetype = "image/jpeg",
        alt = "Which animal is this?"
      )
    }else{
      list(
        src=normalizePath(file.path('./images', img_db[len])), height = 370,
        filetype = "image/jpeg",
        alt = "Which animal is this?"
      )
    }
    
  },deleteFile=FALSE)
  
  # Saving Data
  
  observeEvent(input$cat,{
    if(rv$stop-2 < maxrv){
      newInp <- c("cat",rv$inp)
      rv$inp <- newInp
    } else {
      newInp <- rv$inp
      rv$inp <- newInp
    }
  })
  
  observeEvent(input$dog,{
    if(rv$stop-2 < maxrv){
      newInp <- c("dog",rv$inp)
      rv$inp <- newInp
    } else {
      newInp <- rv$inp
      rv$inp <- newInp
    }
  })
  
  inp <- reactive({
    rv$inp
  })
  
  
  # Warning message when nothing to save
  observeEvent(input$save, {
    if(len+1-rv$value == 0){
      showModal(modalDialog(
        title = h3("Important message"),
        h4("Task is complete, all saved.")
      ))
    } else if(length(inp()) > 0){
      return()
    }else{
      showModal(modalDialog(
        title = h3("Important message"),
        h4("Label some images first")
      ))
    }
  })
  
  observeEvent(input$update,{
    if(len+1-rv$value == 0){
      showModal(modalDialog(
        title = h3("Important message"),
        h4("Thank you, task is complete")
      ))
    }
  })
  
  observeEvent((input$cat|input$dog), {
    if(len+1-rv$value == 0){
      shinyjs::disable('cat')
      shinyjs::disable('dog')
    }
  })
  
  
  observeEvent( (input$save | input$update), {
    saveData(inp())
    shinyjs::disable('save')
    shinyjs::show('downl')
  })
  
  observeEvent( (input$cat|input$dog), {
    shinyjs::enable('save')
  })
  
  
  
  output$count <- renderPlot({
    if(is.null(levels(readdata()$label))) {
      subcol <- 1
    } else {
      subcol <- length(levels(readdata()$label))+as.numeric(any(is.na(readdata()$label)))
    }
    ggplot(readdata(), aes(x=label, fill=label)) + 
      geom_bar(fill=mycolors[1:subcol]) +
      theme_minimal()+theme(text=element_text(size=28))
  })
  output$table <- renderDataTable({
    datatable(readdata(), options = list(sDom  = '<"top">lrt<"bottom">ip', order = list(list(2, 'asc')), aLengthMenu = c(8, 16, 32)))
  })
  
  
  # Value boxes
  
  output$total <- renderValueBox({
    valueBox(
      value = prettyNum(len, big.mark = " "),
      subtitle = HTML("<b>Total number of files</b>"),
      icon = icon("file-image"),
      color = "blue"
    )
  })
  
  output$labeled <- renderValueBox({
    valueBox(
      value = prettyNum(rv$value-1, big.mark = " "),
      subtitle = HTML("<b>Labelled</b>"),
      icon = icon("flag"),
      color = "blue"
    )
    
  })
  
  output$remaining<- renderValueBox({
    valueBox(
      value = prettyNum((len+1-rv$value), big.mark = " "),
      subtitle = HTML("<b>Remaining</b>"),
      icon = icon("swimmer"),
      color = "blue"
    )
    
  })
  
  
  output$downl <- downloadHandler(
    filename = "labels.csv",
    content = function(file) {
      write.csv(readdata(), file)
    },
    contentType = "text/csv") 
}

# Run the application 
shinyApp(ui = ui, server = server)