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

ASIA <- c("Hong Kong","Japan", "Macau", "Mainland China", "Singapore ", "South Korea", "Taiwan", "Thailand", "Vietnam", "United Arab Emirates", "Cambodia", "Sri Lanka","India", "Nepal", "Russia",
          "Philippines", "Hong Kong", "Malaysia", "Macau", "Tibet")
America <- c("US", "Canada")
EU <- c("France", "UK", "Germany", "Italy", "Finland", "Sweden", "Spain" , "Norway")


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
    title= "Coronavirus Outbreak",
    dropdownMenu(type = "notifications", 
                 
                 notificationItem(
                         text = "Created by www.dataatomic.com", 
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
      menuItem("Dashboard", icon = icon("dashboard"),
               tabName = "dashboard"
      ) ,
      menuItem("Cases Outside China", icon =icon("globe-americas"),
               tabName = "countries"
      ),
      menuItem("Simulations", icon = icon("chart-line"),
               tabName = "prediction"
      ),
      menuItem("Raw Data", icon = icon("table"),
               tabName = "rawdata"
      ),
      menuItem("Data Sources",  icon =icon("database"),
               tabName = "Sources",
               menuSubItem(text= "WHO", href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports"),
               menuSubItem(text= "CDC", href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html"),
               menuSubItem(text= "NHCPRC", href = "http://www.nhc.gov.cn/yjb/s3578/new_list.shtml"),
               menuSubItem(text= "DXC", href = "https://3g.dxy.cn/newh5/view/pneumonia?scene=2&clicktime=1579582238&enterid=1579582238&from=singlemessage&isappinstalled=0"),
               menuSubItem(text= "ECDC", href = "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases"),
              menuSubItem(text= "JHU", href = "https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?usp=sharing&sle=true")),
               
     menuItem("Contact Us",  icon =icon("paper-plane"),
                 tabName = "My Website",
                 menuSubItem(text= "Email", icon = shiny::icon("envelope-open-text"), href = "mailto:serdar.korur@gmail.com"),
                 menuSubItem(text= "Dataatomic", icon = icon("atom"), href = "http://www.dataatomic.com"),
                 menuSubItem(text= "Linkedin", icon = icon("linkedin"), href = "https://www.linkedin.com/in/serdar-korur/")

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

tabItems(
  tabItem("dashboard",

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
  ,valueBoxOutput("recovered", width = 2)
  ,valueBoxOutput("rate", width = 2)
  ,valueBoxOutput("count", width = 2)
  ,valueBoxOutput("update", width = 3)
),


################################### 
#######        BOXES 1      #######
###################################

fluidRow( 
  box(
    width = "12"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,leafletOutput("map", height = "700px") 
  ),
  box(
    width = "12"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("casetimeline", height = "700px") 
  ) #box 
    
       ) #fluidrow
), # tabItem dashboard

################################### 
#######        BOXES 2      #######
###################################
tabItem("rawdata",

  box(
    width = "12"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,column(width=12, DT::dataTableOutput("df_wide"), 
    style = "height:500px; overflow-y: scroll;overflow-x: scroll;") 
  ), downloadButton("downloadCsv", "Download as CSV") #box
), #rawdata,

tabItem("countries",
  
        box(width = "12",
    solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("countries") 
  ) #box 
  
),
tabItem("prediction",
        box(
          width = "12"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,verbatimTextOutput("prediction") , imageOutput("wflow", width = "100%")
        ) #box 
        
) # tabItem
)# fluidrow
) # tabItems

               
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
        valueBox( value = tags$p( sum(data$confirmed), style = "font-size: 70%;"),
                  subtitle = tags$p("Total Cases", style = "font-size: 100%;") 
            ,icon = icon("procedures")
            ,color = "red")  
    })
    output$numchina <- renderValueBox({
        valueBox(
          value = tags$p( data %>% filter(country == "Mainland China") %>% summarise(n=sum(confirmed)), style = "font-size: 70%;"),
          subtitle = tags$p("China", style = "font-size: 100%;")
            ,icon = icon('procedures')
            ,color = "red")  
    })
    output$numeu <- renderValueBox({
        valueBox(
          value = tags$p( data %>% filter(country %in% EU) %>% summarise(n=sum(confirmed)), style = "font-size: 70%;"),
          subtitle = tags$p("Europe", style = "font-size: 100%;")
            
            ,icon = icon("procedures")
            ,color = "red")  
    })
    
    output$numus <- renderValueBox({
        valueBox( 
          value = tags$p(data %>% filter(country %in% America) %>% summarise(n=sum(confirmed)), style = "font-size: 70%;"),
          subtitle = tags$p("AMERICA", style = "font-size: 100%;")
            , "AMERICA"
            ,icon = icon("procedures")
            ,color = "red")  
    })
    output$update <- renderValueBox({
        valueBox( 
          value = tags$p(print("Last Updated"), style = "font-size: 70%;"),
          subtitle = tags$p(data %>% attr("update"), style = "font-size: 100%;")
            ,icon = icon("hourglass-start")
            ,color = "blue") 
        
    })
    
    output$death <- renderValueBox({
      valueBox(value = tags$p(print(sum(data$deaths)), style = "font-size: 70%;"),
      subtitle = tags$p("Total Deaths", style = "font-size: 100%;")
                ,icon = icon("cross")
                ,color = "red")  
    })
    output$rate <- renderValueBox({
      valueBox( value = tags$p( round(sum(data$deaths)/sum(data$confirmed) *100,1), style = "font-size: 70%;"),
                subtitle = tags$p("Death rate", style = "font-size: 100%;")
               ,icon = icon('percent')
               ,color = "red")  
    })
    output$count <- renderValueBox({
      valueBox(value = tags$p( data2 %>% distinct(country) %>% count(), style = "font-size: 70%;"),
               subtitle = tags$p("Countries", style = "font-size: 100%;")
        ,icon = icon("flag")
        ,color = "red")  
    })
    
    output$recovered<- renderValueBox({
      valueBox( value = tags$p( print("487"), style = "font-size: 70%;"),
                subtitle = tags$p("Recovered", style = "font-size: 100%;")
        ,icon = icon("check-circle")
        ,color = "green")  
    })
    output$up <- renderValueBox({
      valueBox( value = tags$p( print("Outbreak"), style = "font-size: 70%;"),
                subtitle = tags$p("2019-nCoV", style = "font-size: 100%;")
                ,icon = icon("procedures")
        ,color = "blue") 
      
    })
    
    ###################################
    #######                     #######
    #######     PLOT BOXES 1    #######
    #######                     #######
    ###################################
    
    output$countries <- renderPlot({
        # Plot
       df_sum <-  data %>% filter(country != "Mainland China") %>% group_by(country) %>% summarise(n=sum(confirmed)) %>% arrange(-n)
         df_sum =df_sum  %>% mutate(country=fct_reorder(country, n, .desc=TRUE))
       df_sum %>% ggplot(aes(x=country,y=n, fill =n, height = 1200 )) + 
            geom_col() + 
            theme_minimal() + 
            theme(legend.position = "none",text = element_text(size=20), plot.title = element_text( hjust=0.5, vjust = -1)) +
            labs(
                caption= "www.dataatomic.com",
                x = "", 
                y = "Number of cases",
                title = "Cases Outside China") + 
            coord_flip()
        
    })
    
    output$map <- renderLeaflet({
     
      
      m <- leaflet(data2) %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
        addProviderTiles(providers$Stamen.TonerLite) %>% 
        addCircleMarkers(lng=data2$lon, lat=data2$lat, radius = 3* data2$radius, color = "green") %>% 
        addAwesomeMarkers(~lon, ~lat,  label=~confirmed, 
                    labelOptions = labelOptions(noHide = F, direction = "bottom",
                                               style = list(
                                                     "color" = "black",
                                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                   "font-size" = "15px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
    ))) %>% 
        setView(lng = 125, lat = 25, zoom = 4)
    })
    
    output$prediction <- renderText({
    "Coming Soon"
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
        theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=20), plot.title = element_text( hjust=0.5, vjust = -1)) + 
        labs(
          caption= "   www.dataatomic.com",
          y = "Number of infected people",
          title = "Global Cases")
      
    }) 
    
    output$df_wide <- renderDataTable({
      
      df_wide <- df_merge[, c(2,3,4)] %>% group_by( `Country/Region`, `Last Update`) %>% summarise(Confirmed = sum(Confirmed)) %>% spread(`Last Update`, Confirmed)
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
    output$wflow <- renderImage({
      return(list(src = "ai2.jpg",contentType = "image/png",alt = "Alignment"))
    }, deleteFile = FALSE)
    
    output$downloadCsv <- downloadHandler(
      filename = "coronavirusdata.csv",
      content = function(file) {
        write.csv(df_wide, file)
      },
      contentType = "text/csv"
    )
}
shinyApp(ui = ui, server = server)

