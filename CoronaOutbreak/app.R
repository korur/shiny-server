###################################
###################################
###################################
#######                     #######
#######   CORONA OUTBREAK   #######
#######                     #######
###################################
###################################
###################################


library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(viridis)
library(leaflet)

ASIA <- c("Hong Kong","Japan", "Macau", "Mainland China", "Singapore ", "South Korea", "Taiwan", "Thailand", "Vietnam")
US <- "US"
EU <- c("France", "UK", "United Kingdom")

# Shiny dashboard App
data <- readRDS("c_df1.rds")
data2 <- readRDS("map.rds")
value <- attr(data,'update')
header <- dashboardHeader(
    title= "Coronavirus Cases",
    dropdownMenu(type = "notifications", 
                 
                 notificationItem(
                     text = "Data: Johns Hopkins University",
                     icon = shiny::icon("database"),
                     status = "success"),
                 
                 notificationItem(
                         text = "Created by Serdar Korur | www.dataatomic.com", 
                         icon = shiny::icon("atom"),
                         status = "success",
                         href = "https://www.dataatomic.com"))
)

sidebar <- dashboardSidebar(
    
    sidebarMenu(
      
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
body <- dashboardBody( 
    
    fluidRow(
                    valueBoxOutput("numcases", width = 2)
                    ,valueBoxOutput("numchina", width = 2)
                    ,valueBoxOutput("numeu", width = 2)
                    ,valueBoxOutput("numus", width = 2)
                    , valueBoxOutput("up", width = 3)
), #fluidrow

fluidRow(
  valueBoxOutput("death", width = 2)
  ,valueBoxOutput("rate", width = 2)
  ,valueBoxOutput("count", width = 2)
  ,valueBoxOutput("continents", width = 2)
  ,valueBoxOutput("update", width = 3)
),
fluidRow( 
    box(
        width = "4"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput("countries", width = "500px", height = "700px") 
        ), #box 
    box(
      width = "8"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,leafletOutput("map", width = "1200px", height = "700px") 
    )
       ) #fluidrow
                    ) #dashboardbody
               
               

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Interactive app - Coronavirus Outbreak', header, sidebar, body, skin= "blue")





###################################
###################################
###################################
#######                     #######
#######         SERVER      #######
#######                     #######
###################################
###################################
###################################


# create the server functions for the dashboard  
server <- function(input, output) { 
   
    #creating the valueBoxOutput content
    output$numcases <- renderValueBox({
        valueBox( sum(data$confirmed)
         , "Total Number of Cases"
            ,icon = icon("procedures")
            ,color = "red")  
    })
    output$numchina <- renderValueBox({
        valueBox(data %>% filter(country == "Mainland China") %>% summarise(n=sum(confirmed))
           
            , "CHINA"
            ,icon = icon('procedures')
            ,color = "red")  
    })
    output$numeu <- renderValueBox({
        valueBox(
            data %>% filter(country %in% EU) %>% summarise(n=sum(confirmed))
            
            , "Europe"
            ,icon = icon("procedures")
            ,color = "red")  
    })
    
    output$numus <- renderValueBox({
        valueBox( 
            data %>% filter(country %in% US) %>% summarise(n=sum(confirmed))
            , "US"
            ,icon = icon("procedures")
            ,color = "red")  
    })
    output$update <- renderValueBox({
        valueBox( 
            data %>% attr("update")
            , "Last updated"
            ,icon = icon("hourglass-start")
            ,color = "blue") 
        
    })
    
    output$death <- renderValueBox({
      valueBox( print(41)
               
                , "Total Deaths"
                ,icon = icon("cross")
                ,color = "red")  
    })
    output$rate <- renderValueBox({
      valueBox( round(41/sum(data$confirmed) *100,1)
               
               , "Death rate"
               ,icon = icon('percent')
               ,color = "red")  
    })
    output$count <- renderValueBox({
      valueBox(
        data2 %>% distinct(country) %>% count()
        
        , "Countries"
        ,icon = icon("flag")
        ,color = "red")  
    })
    
    output$continents <- renderValueBox({
      valueBox( 
        print(4)
        , "Continents"
        ,icon = icon("globe-asia")
        ,color = "red")  
    })
    output$up <- renderValueBox({
      valueBox( 
        print("Wuhan Coronavirus Outbreak")
        , "2019-nCoV"
        ,icon = icon("virus")
        ,color = "black") 
      
    })
    output$countries <- renderPlotly({
        # Plot
       df_sum <-  data %>% group_by(country) %>% summarise(n=sum(confirmed)) %>% arrange(-n)
         df_sum =df_sum  %>% mutate(country=fct_reorder(country, n, .desc=TRUE))
        p <- df_sum %>% ggplot(aes(x=country,y=n, fill =n )) + 
            geom_col() + 
            scale_fill_viridis( direction =1) + 
            theme_minimal() + 
            theme(legend.position = "none", axis.text.x=element_text(angle=90),text = element_text(size=12)) +
            labs(
                caption= "   www.dataatomic.com",
                x = "", 
                y = "Number of cases",
                title = "Wuhan Coronavirus: \nConfirmed Cases in the World") + 
            coord_flip()
        ggplotly(p)
    })
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>% addCircleMarkers(lng=data2$lon, lat=data2$lat, radius = 2* data2$radius, color = "red") %>% 
        setView(lng = 100, lat = 30, zoom = 3)
    })
}
shinyApp(ui = ui, server = server)

