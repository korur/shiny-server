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
library(leaflet)
library(DT)

ASIA <- c("Hong Kong","Japan", "Macau", "Mainland China", "Singapore ", "South Korea", "Taiwan", "Thailand", "Vietnam")
US <- "US"
EU <- c("France", "UK", "United Kingdom", "Germany")


# Shiny dashboard App
data <- readRDS("c_df1.rds")
data2 <- readRDS("map.rds")
value <- attr(data,'update')
df_merge <- readRDS("df_merge.rds")

###################################
#######                     #######
#######        HEADER       #######
#######                     #######
###################################



header <- dashboardHeader(
    title= "Coronavirus Cases",
    dropdownMenu(type = "notifications", 
                 
                 notificationItem(
                     text = "Data: JHU,WHO, CDC, NHC, Dingxiangyuan",
                     icon = shiny::icon("database"),
                     status = "success"),
                 
                 notificationItem(
                         text = "Created by Serdar Korur | www.dataatomic.com", 
                         icon = shiny::icon("atom"),
                         status = "success",
                         href = "https://www.dataatomic.com"))
)

###################################
#######                     #######
#######        SIDEBAR      #######
#######                     #######
###################################

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


###################################
#######                     #######
#######          BODY       #######
#######                     #######
###################################


# combine the two fluid rows to make the body
body <- dashboardBody( 
    
################################### 
#######        TOP ROW      #######
###################################


    fluidRow(
                    valueBoxOutput("numcases", width = 2)
                    ,valueBoxOutput("numchina", width = 2)
                    ,valueBoxOutput("numeu", width = 2)
                    ,valueBoxOutput("numus", width = 2)
                    , valueBoxOutput("up", width = 3)
), #fluidrow

################################### 
#######     SECOND ROW      #######
###################################

fluidRow(
  valueBoxOutput("death", width = 2)
  ,valueBoxOutput("rate", width = 2)
  ,valueBoxOutput("count", width = 2)
  ,valueBoxOutput("continents", width = 2)
  ,valueBoxOutput("update", width = 3)
),


################################### 
#######        BOXES 1      #######
###################################

fluidRow( 
  box(
    width = "6"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,leafletOutput("map", width = "900px", height = "700px") 
  ),
  box(
    width = "4"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("casetimeline", width = "700px", height = "700px") 
  ) #box 
    
       ), #fluidrow


################################### 
#######        BOXES 2      #######
###################################

fluidRow( 
  
  box(
    width = "6"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,DT::dataTableOutput("df_wide") 
    
  ),
  box(
    width = "4"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("countries", width = "500px", height = "700px") 
  ) #box 
  
),  # fluidrow
fluidRow( 
  
  box(
    width = "10"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("facet", width = "1600px", height = "1200px") 
    
  ) #box 
  
)  # fluidrow
                    ) # end of dashboardbody
               
###################################
################################### 
#######                     #######
#######       SET UI        #######
#######                     #######
###################################
###################################


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
   
  
  ###################################
  #######                     #######
  #######     VALUE BOXES     #######
  #######                     #######
  ###################################
  
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
      valueBox( print(107)
               
                , "Total Deaths"
                ,icon = icon("cross")
                ,color = "red")  
    })
    output$rate <- renderValueBox({
      valueBox( round(107/sum(data$confirmed) *100,1)
               
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
        print("Coronavirus Outbreak")
        , "Wuhan Coronavirus 2019-nCoV"
        ,icon = icon("users")
        ,color = "blue") 
      
    })
    
    ###################################
    #######                     #######
    #######     PLOT BOXES 1    #######
    #######                     #######
    ###################################
    
    output$countries <- renderPlot({
        # Plot
       df_sum <-  data %>% group_by(country) %>% summarise(n=sum(confirmed)) %>% arrange(-n)
         df_sum =df_sum  %>% mutate(country=fct_reorder(country, n, .desc=TRUE))
       df_sum %>% ggplot(aes(x=country,y=n, fill =n )) + 
            geom_col() + 
            theme_minimal() + 
            theme(legend.position = "none",text = element_text(size=20), plot.title = element_text( hjust=0.5, vjust = -1)) +
            labs(
                caption= "www.dataatomic.com",
                x = "", 
                y = "Number of cases",
                title = "Confirmed Coronavirus Cases per Country") + 
            coord_flip()
        
    })
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
        addProviderTiles(providers$Stamen.TonerLite) %>% 
        addCircleMarkers(lng=data2$lon, lat=data2$lat, radius = 2* data2$radius, color = "red") %>% 
        setView(lng = 125, lat = 25, zoom = 4)
    })
    
    ###################################
    #######                     #######
    #######     PLOT BOXES 1    #######
    #######                     #######
    ###################################
    
    output$casetimeline <- renderPlot({
      
      # Plot
      summary <- df_merge %>% group_by(`Last Update`) %>% summarise(n=sum(Confirmed, na.rm = TRUE))
      
      
      summary %>% ggplot(aes(x=`Last Update`, y=n)) +
        geom_line(size=2,color='red') + 
        geom_point(size=8, color='red')+theme_minimal() +
        theme(legend.position = "none",text = element_text(size=20), plot.title = element_text( hjust=0.5, vjust = -1)) + 
        labs(
          caption= "   www.dataatomic.com",
          x = "Date", 
          y = "Number of infected people",
          title = "Global Coronavirus Cases are increasing")
      
    }) 
    
    output$df_wide <- renderDataTable({
      
      dfm <- df_merge[,c(1,3,4)]
      df_wide <- dfm %>% spread(`Last Update`, Confirmed)
      df_wide[is.na(df_wide)] <- 0
   datatable(df_wide, options = list(paging = TRUE), height='400px') 
      
    }) 
    
    output$facet <- renderPlot({
      
      dfm <- df_merge[,c(1,3,4)]
      
      
      dfm %>% filter(Confirmed >1) %>% mutate(`Province/State`=fct_reorder(`Province/State`,Confirmed, .desc=TRUE))  %>% 
        ggplot(aes(x=`Last Update`, y=Confirmed))+facet_wrap(.~`Province/State`, scales = "free_y", nrow=10) + 
        geom_line(color='red') + 
        geom_point(color='red',size=2) +
        theme_minimal() + 
        theme(legend.position="none" , text = element_text(size=20), axis.text.x=element_text(angle=45)) + 
        labs(
        caption= "www.dataatomic.com",
        x = "Time", 
        y = "Number of cases",
        title = "Regional increases of Coronavirus Cases with Time")
      
    }) 
}
shinyApp(ui = ui, server = server)

