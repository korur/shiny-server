library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(golem)

data <- readRDS('mycalldataforTcgaApp_with_cancerclass.rds')
data$type = as.factor(data$type)
data$study = as.factor(data$study)

share <- list(
  title = "Genome Explorer",
  url = "http://https://tools.dataatomic.com/shiny/TCGA_dashboard_on_server/",
  image = "http://tools.dataatomic.com/shiny/img/tcga.png",
  description = "TCGA Genome Explorer",
  twitter_user = "dataatomic"
)
# Shiny dashboard App

header <- dashboardHeader(
    title= "TCGA DATA Analysis",
    dropdownMenu(type = "notifications", 
                 notificationItem(
                     text = "Build by Serdar Korur | www.dataatomic.com", 
                     icon = shiny::icon("atom"),
                     status = "success",
                     href = "https://www.dataatomic.com"))
)

sidebar <- dashboardSidebar(
    
    sidebarMenu(
        selectInput("c_type", "Cancer type",
                    choices = c("All", levels(data$type)), selected = "All"
        ),
        
        numericInput("pointsize", "Point size", 1),
    
        menuItem("Dashboard", icon = icon("dashboard"),
                 tabName = "dashboard"
        ) ,
        menuItem("Workflow", icon = icon("wrench"),
                 tabName = "workflow"
        ),
        menuItem("Rawdata", icon =icon("database"),
                 tabName = "Rawdata"
        ),
        menuItem("My Website",  icon =icon("paper-plane"),
                 tabName = "My Website",
                 menuSubItem(text= "Dataatomic", href = "http://www.dataatomic.com"),
                 menuSubItem(text= "Medium", href = "https://medium.com/@serdarkorur"),
                 menuSubItem(text= "Linkedin", href = "https://www.linkedin.com/in/serdar-korur/"),
                 menuSubItem(text= "Github", href = "https://github.com/korur")
        )
    )
)

# combine the two fluid rows to make the body
body <- dashboardBody(  tags$head(golem::activate_js(),
                                  HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
       <script async src='https://www.googletagmanager.com/gtag/js?id=UA-148414815-3'></script>
       <script>
       window.dataLayer = window.dataLayer || [];
     function gtag(){dataLayer.push(arguments);}
     gtag('js', new Date());
     
     gtag('config', 'UA-14841481  5-3');
     </script>"
                      ),
                                  tags$link(rel = "shortcut icon", type="image/x-icon", href="http://tools.dataatomic.com/shiny/img2/favicon.ico"),
                                  # Facebook OpenGraph tags
                                  tags$meta(property = "og:title", content = share$title),
                                  tags$meta(property = "og:type", content = "website"),
                                  tags$meta(property = "og:url", content = share$url),
                                  tags$meta(property = "og:image", content = share$image),
                                  tags$meta(property = "og:description", content = share$description)),
  
    tabItems(
        tabItem("dashboard",
                
fluidRow(
    valueBoxOutput("mean_mutations", width = 3)
    ,valueBoxOutput("mean_alt", width = 3)
    ,valueBoxOutput("study", width = 3)
    ,valueBoxOutput("patient", width = 3)
    ),

fluidRow( 
    box(
        title = h3("Genome Alteration Landscape of Cancers")
        ,width = "10"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput("mut_genome", height = "600px")

    ),
    
          
)  # fluid row
), #tabitem1
tabItem("Rawdata",
        verbatimTextOutput("rawtable"),
        DT::dataTableOutput("mut_table"),
        downloadButton("downloadCsv", "Download as CSV")
        
        
        ), #tabitem2
tabItem("workflow", width =6,
        verbatimTextOutput("Workflow"),
        imageOutput("wflow", width = "200%")
) 
) #tabitems
) #dash


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'TCGA App Shiny dashboard', header, sidebar, body, skin= "blue")


# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    sum_frac_mut <- data %>% group_by(ID, study, type) %>% summarise(altered_gen = round(mean(FRACTION_GENOME_ALTERED),3), mutations = round(mean(MUTATION_COUNT),0)) 
    #creating the valueBoxOutput content
    output$mean_mutations <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% summarise(n = mean(MUTATION_COUNT)) %>% round(0),
            data %>% filter(type == input$c_type) %>% summarise(n = mean(MUTATION_COUNT)) %>% round(0)
            ), "AVG. MUTATION COUNT"
            ,icon = icon("dna")
            ,color = "red")  
    })
    output$mean_alt <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% summarise(n = 100 * mean(FRACTION_GENOME_ALTERED)) %>% round(1),
            data %>% filter(type == input$c_type) %>% summarise(n = 100 * mean(FRACTION_GENOME_ALTERED)) %>% round(1)
            ), "AVG. % GENOME ALTERED"
            ,icon = icon('percent')
            ,color = "green")  
    })
    output$study <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% distinct(study) %>% count(),
                   data %>% filter(type == input$c_type) %>% distinct(study) %>% count()
            ), "STUDIES"
            ,icon = icon("folder-open")
            ,color = "yellow")  
    })
   
        output$patient <- renderValueBox({
            valueBox(
                ifelse(input$c_type == "All",  nrow(data),
                       data %>% filter(type == input$c_type) %>% nrow()
                ), "PATIENTS"
                ,icon = icon("users")
                ,color = "blue")  
        })
        output$mut_genome <- renderPlotly({
            # Plot
            ifelse(input$c_type == "All", sum_frac_mut, sum_frac_mut <- sum_frac_mut %>% filter(type == input$c_type))
            
        p <- ggplot(sum_frac_mut, aes(altered_gen, mutations, color = study)) + geom_point(size = input$pointsize) +
                    theme(text = element_text(size=16), legend.position = "none") + xlab('Percent Genome altered (mean)') + ylab("Number of Mutations (mean)") + 
                    scale_x_continuous(labels = function(x) paste0(100*x, "%")) + scale_y_log10()
           
            ggplotly(p) %>% plotly::layout(
                xaxis = list(range = c(-0.02, 0.85)),
                yaxis = list(range = c(0, 3.3)))
        })
            
        output$mut_table <- renderDataTable({
            
            ifelse(input$c_type == "All", sum_frac_mut, sum_frac_mut <- sum_frac_mut %>% filter(type == input$c_type))
            datatable(sum_frac_mut[, c(1,2,4,5)], options = list(paging = TRUE))
            
        })
        
        output$raw_table <- renderTable({
            
           data[1:input$maxrows,]
       
                 
        })
        
        output$wflow <- renderImage({
            return(list(src = "workflow_tcga.png",contentType = "image/png",alt = "Alignment"))
        }, deleteFile = FALSE) #where the src is wherever you have the picture
        
        
        
    
        
        output$downloadCsv <- downloadHandler(
            filename = "tcgadata.csv",
            content = function(file) {
                write.csv(data, file)
            },
            contentType = "text/csv"
        )
}
shinyApp(ui = ui, server = server)
