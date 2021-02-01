# With auto updates from the database
# Connect
# config <- yaml::read_yaml("/etc/skconfig")   # for digitalocean ubuntu 

# Not needed anymore We are using sqlite!!!


# Connection script local or server
#if (file.exists("D:/dwdir/CoronaOutbreak/_coronavirus.yml")) { 
#config <- yaml::read_yaml("D:/dwdir/CoronaOutbreak/_coronavirus.yml") 
#} else { 
#config <- yaml::read_yaml("/etc/skconfig") 
#} 




share <- list(
  title = "Coronavirus Tracker",
  url = "http://tools.dataatomic.com/shiny/CoronaOutbreak/",
  image = "http://tools.dataatomic.com/shiny/img/virus.png/",
  description = "Track global Coronavirus cases",
  twitter_user = "dataatomic"
)


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
library(countup)
library(waiter)
library(DBI)
library(pool)
library(golem)
library(RSQLite)


con <- dbConnect(SQLite(), "/srv/shiny-server/covid.db")
print(con)



ASIA <- c("Hong Kong","Japan", "Macau", "China", "Singapore ", "South Korea", "Taiwan", "Thailand", "Vietnam", "United Arab Emirates", "Cambodia", "Sri Lanka","India", "Nepal", "Russia",
          "Philippines", "Hong Kong", "Malaysia", "Macau", "Tibet", "Iran")
America <- c("US", "Canada", "United States of America")
EU <- c("France", "UK", "Germany", "Italy", 
        "Finland", "Sweden", "Spain" , "Norway", "Belgium", 
        "Greece", "Switzerland", "Austria", "Portugal", 
        "Turkey", "Poland", "Croatia", "United Kingdom", "Estonia", "Belarus", 
        "Monaco", "North Macedonia", "San Marino", "Iceland", "Lithuania",
        "Romania", "Hungary", "Netherlands", "Serbia,", "Czechia", "Ireland", "Slovenia")






###################################
###################################
#######                     #######
#######        HEADER       #######
#######                     #######
###################################
###################################


header <- dashboardHeader(
  title= "Coronavirus Outbreak",
  dropdownMenu(type = "notifications", 
               
               notificationItem(
                 text = tags$b("Created by www.dataatomic.com"), 
                 icon = shiny::icon("atom"),
                 status = "success",
                 href = "https://www.dataatomic.com"),
               
               notificationItem(
                 text = tags$b("Update: Death rate is calculated as",
                               tags$br(),
                               "death / (confirmed + recovered)", style = "display: inline-block; vertical-align: middle;", 
                               icon = shiny::icon("atom"),
                               status = "success",
                               href = "https://www.dataatomic.com"))
               
  )
)

###################################
#######                     #######
#######        SIDEBAR      #######
#######                     #######
###################################

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", icon = icon("dashboard"),
             tabName = "dashboard" , badgeLabel   = "  map", badgeColor = "blue"
    ) ,
    menuItem("Cases Outside China", icon =icon("globe-americas"),
             tabName = "countries", badgeLabel   = "table", badgeColor = "blue"
    ),
    
    menuItem("Animation", icon = icon("chart-line"),
             tabName = "prediction", badgeLabel   = "new", badgeColor = "green"
    ),
    menuItem("Download Data", icon = icon("table"),
             tabName = "rawdata"
    ),
    menuItem("Data Sources",  icon =icon("database"),
             tabName = "Sources",
             menuSubItem(text= "WHO", href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports"),
             menuSubItem(text= "CDC", href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html"),
             menuSubItem(text= "NHCPRC", href = "http://www.nhc.gov.cn/yjb/s3578/new_list.shtml"),
             menuSubItem(text= "DXC", href = "https://3g.dxy.cn/newh5/view/pneumonia?scene=2&clicktime=1579582238&enterid=1579582238&from=singlemessage&isappinstalled=0"),
             menuSubItem(text= "ECDC", href = "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases"),
             menuSubItem(text= "JHU", href = "https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?usp=sharing&sle=true") ),
    
    menuItem("Contact Us",  icon =icon("paper-plane"),
             tabName = "My Website",
             menuSubItem(text= "Email", icon = shiny::icon("envelope-open-text"), href = "mailto:serdar.korur@gmail.com"),
             menuSubItem(text= "Dataatomic", icon = shiny::icon("atom"), href = "http://www.dataatomic.com"),
             menuSubItem(text= HTML("&nbsp;&nbsp;&nbsp;Linkedin"), icon = shiny::icon("linkedin"), href = "https://www.linkedin.com/in/serdar-korur/")
             
    )
  )
)


###################################
#######                     #######
#######          BODY       #######
#######                     #######
###################################


# combine the two fluid rows to make the body
body <- dashboardBody(  tags$head(golem::activate_js(),
                                  HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
       <script async src='https://www.googletagmanager.com/gtag/js?id=UA-148414815-3'></script>
       <script>
       window.dataLayer = window.dataLayer || [];
     function gtag(){dataLayer.push(arguments);}
     gtag('js', new Date());
     
     gtag('config', 'UA-148414815-3');
     </script>"
                                  ),
                                  tags$link(rel = "shortcut icon", type="image/x-icon", href="http://tools.dataatomic.com/shiny/CoronaOutbreak_test/favicon.ico"),
                                  # Facebook OpenGraph tags
                                  tags$meta(property = "og:title", content = share$title),
                                  tags$meta(property = "og:type", content = "website"),
                                  tags$meta(property = "og:url", content = share$url),
                                  tags$meta(property = "og:image", content = share$image),
                                  tags$meta(property = "og:description", content = share$description)
),
use_waiter(), # dependencies
waiter_show_on_load(spin_3circles(), color = "#ffffff"),
sever::use_sever(),


################################### 
#######        TOP ROW      #######
###################################

tabItems(
  tabItem("dashboard",
          
          fluidRow(
            valueBoxOutput("numcases", width = 3)
            ,valueBoxOutput("numchina", width = 3)
            ,valueBoxOutput("numeu", width = 3)
            ,valueBoxOutput("numus", width = 3)
          ), #fluidrow
          
          ################################### 
          #######     SECOND ROW      #######
          ###################################
          
          fluidRow(
            valueBoxOutput("death", width = 3)
            ,valueBoxOutput("rate", width = 3)
            ,valueBoxOutput("count", width = 3)
            ,valueBoxOutput("update", width = 3)
          ),
          
          
          ################################### 
          #######        BOXES 1      #######
          ###################################
          
          fluidRow( 
            box(
              width = "11"
              ,solidHeader = TRUE 
              ,collapsible = TRUE
              ,leafletOutput("map", height = "700px") 
            ),
            box(
              width = "11"
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
          
          box(width = "6"
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,column(width=12, DT::dataTableOutput("countries"), 
                      style = "height:500px; overflow-y: scroll;overflow-x: scroll;") 
          ) #box 
          
  ),
  tabItem("prediction", 
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/FeDqOKDzoVs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
          
          
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


ui <- dashboardPage( header, sidebar, body, skin= "blue")



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
server <- function(input, output, session) { 
  
  sever::sever(
    tagList(
      h1("Whoops!"),
      p("It looks like you were disconnected"),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
  ####
  df <- reactivePoll(14400000,session, 
                     checkFunc = function(){ 
                       log <- DBI::dbGetQuery(con, "SELECT MAX(last_updated) FROM log;")},
                     valueFunc = function() {
                       df <- DBI::dbReadTable(con, "jhu")
                     })
  diff <- reactive({
    log <- DBI::dbGetQuery(con, "SELECT MAX(last_updated) as max FROM log;")
    diff <- difftime(as.POSIXct(Sys.time(), origin="1970-01-01"), as.POSIXct(log$max, origin="1970-01-01"), units = "min") %>% as.integer()
    dbDisconnect(con)
  })
  dflight <- reactive({
    dflight <- df() %>% filter(date==max(date))   
  })
  

  ###################################
  #######                     #######
  #######     VALUE BOXES     #######
  #######                     #######
  ###################################
  
  #creating the valueBoxOutput content
  output$numcases <- renderValueBox({
    valueBox( value = tags$p( countup(dflight() %>% filter(type=="confirmed") %>% select(cases) %>% sum()), style = "font-size: 70%;"),
              subtitle = tags$p("Total Cases", style = "font-size: 100%;") 
              ,icon = icon("procedures")
              ,color = "red")  
  })
  output$numchina <- renderValueBox({
    valueBox(
      value = tags$p(countup(dflight() %>% filter(country == "China" & type=="confirmed") %>% summarise(n=sum(cases)) %>% pull), style = "font-size: 70%;"),
      subtitle = tags$p("China", style = "font-size: 100%;")
      ,icon = icon('procedures')
      ,color = "red")  
  })
  output$numeu <- renderValueBox({
    
    valueBox(
      value = tags$p( countup(dflight() %>% filter(country %in% EU & type=="confirmed") %>% summarise(n=sum(cases)) %>% pull ), style = "font-size: 70%;"),
      subtitle = tags$p("Europe", style = "font-size: 100%;")
      
      ,icon = icon("procedures")
      ,color = "red")  
  })
  
  output$numus <- renderValueBox({
    valueBox( 
      value = tags$p( countup(dflight() %>% filter(country == "US" & type=="confirmed") %>% summarise(n=sum(cases)) %>% pull), style = "font-size: 70%;"),
      subtitle = tags$p("USA", style = "font-size: 100%;")
      , "USA"
      ,icon = icon("procedures")
      ,color = "red")  
  })
  output$update <- renderValueBox({
    valueBox( 
      value = tags$p("Auto Updates", style = "font-size: 70%;"),
      subtitle = tags$p(paste("Last update:", diff(), "minutes ago"), style = "font-size: 100%;")
      ,icon = icon("hourglass-start")
      ,color = "blue") 
    
  })
  output$death <- renderValueBox({
    valueBox(value = tags$p(countup(dflight() %>% filter(type=="death") %>% select(cases) %>% sum()), style = "font-size: 70%;"),
             subtitle = tags$p("Total Deaths", style = "font-size: 100%;")
             ,icon = icon("cross")
             ,color = "red")  
  })
  output$rate <- renderValueBox({
    valueBox( value = tags$p( round((dflight() %>% filter(type=="death") %>% select(cases) %>% sum())/(df() %>% filter(type =="confirmed" & date==max(date)) %>% select(cases) %>% sum()) *100,1), style = "font-size: 70%;"),
              subtitle = tags$p("Death rate", style = "font-size: 100%;")
              ,icon = icon('percent')
              ,color = "red")  
  })
  output$count <- renderValueBox({
    valueBox(value = tags$p( countup(df() %>% distinct(country) %>% dplyr::count() %>% pull), style = "font-size: 70%;"),
             subtitle = tags$p("Countries", style = "font-size: 100%;")
             ,icon = icon("flag")
             ,color = "red")  
  })
  

  
  ###################################
  #######                     #######
  #######     PLOT BOXES 1    #######
  #######                     #######
  ###################################
  
  output$countries <- renderDataTable({
    # Plot
    df_sum <- dflight() %>% filter(country != "China", type == "confirmed") %>% 
      group_by(country) %>% 
      summarise(cases=sum(cases)) %>% 
      arrange(-cases)%>% 
      mutate(country=fct_reorder(country, cases, .desc=TRUE))
    
    datatable(df_sum[,1:2], options = list(paging = TRUE, pageLength = 65), height='400px')
  })
  
  output$map <- renderLeaflet({
    
    dfmap <- dflight() %>% filter(type=="confirmed") 
    dfmap$radius <- as.numeric(cut(dfmap$cases, breaks =c(-Inf,4,16,64,128,256,512,1024,2048,4096,8192,16384,32768,Inf)))
    # labels = c("<4", "4-16", "16-64", "64-128","128-256","256-512","512-1024","1024-2048","2048-4096", "> 4096" )
    m <- leaflet(dfmap) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addCircleMarkers(lng=dfmap$lon, lat=dfmap$lat, radius = 3* dfmap$radius, color = "red") %>% 
      addMarkers(dfmap$lon, dfmap$lat,  popup =   paste("<h4>","<b>", dfmap$state, "</b>", "<br>", dfmap$cases, "case/s","</h4>")) %>% 
      setView(lng = 15, lat = 47, zoom = 4)
  })
  
  url <- a("youtube", href="https://www.youtube.com/embed/FeDqOKDzoVs")
  
  output$prediction <- renderUI({
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/FeDqOKDzoVs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  ###################################
  #######                     #######
  #######     PLOT BOXES 1    #######
  #######                     #######
  ###################################
  
  output$casetimeline <- renderPlot({
    
    # Plot
    
    summary <- df() %>% filter(type =="confirmed") %>%  group_by(date) %>% summarise(n=sum(cases))
    summary %>% ggplot(aes(x=date, y=n)) +
      geom_smooth(method = "loess",color='red', size =2) +
      geom_point(size=8, color='red')+theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=20), plot.title = element_text( hjust=0.5, vjust = -1)) + 
      labs(
        caption= "www.dataatomic.com",
        y = "Number of infected people",
        title = "Global Cases") + expand_limits(x = Sys.Date()+2)
    
  }) 
  
  dt <- reactive({
    dt <- df() %>% filter(type=="confirmed") %>% spread(date, cases)
  })
  output$df_wide <- renderDataTable({
    datatable(dt(), options = list(paging = TRUE), height='400px') 
    
  }) 
  
  output$wflow <- renderImage({
    return(list(src = "ai2.jpg",contentType = "image/png",alt = "Alignment"))
  }, deleteFile = FALSE)
  
  output$downloadCsv <- downloadHandler(
    filename = "coronavirusdata.csv",
    content = function(file) {
      write.csv(dt(), file)
    },
    contentType = "text/csv"
  )
  Sys.sleep(1.6)
  waiter_hide()
}
shinyApp(ui = ui, server = server)


