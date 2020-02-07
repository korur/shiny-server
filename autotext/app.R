# Text mining dashboard

# Packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(qdapTools)


###################################
###################################
#######                     #######
#######        HEADER       #######
#######                     #######
###################################
###################################

header <- dashboardHeader(
    title= "Personalized Text mining app (by www.dataatomic.com)", titleWidth = 650,
    dropdownMenu(type = "notifications", 
                 
                 notificationItem(
                   text = "Created by www.dataatomic.com", 
                   icon = shiny::icon("atom"),
                   status = "success",
                   href = "https://www.dataatomic.com")
                 
    )
  )


###################################
###################################
#######                     #######
#######        SIDEBAR      #######
#######                     #######
###################################
###################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", icon = icon("dashboard"),
             tabName = "dashboard"
    ),
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
body <- dashboardBody(

  tabItems(
    tabItem("dashboard",
            
            
  fluidRow(  valueBoxOutput("fund", width = 4)
            ,valueBoxOutput("series", width = 4)
            ,valueBoxOutput("clinical", width = 4) 
            ),
            
             
  fluidRow( 
    
    box(title = "Biotech Press Releases", status = "primary",
      width = "3"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,textAreaInput("text",
                     "",
                     height = "522px", 
                     value = "Paste the text",  resize = "both")
    ),
    
    box(title = "Data", status = "warning",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("data")),
   
    box(title = "Top Word pairs", status = "danger",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("bigram")),
    box(title = "Contact", status = "primary",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("phone")),
    box(title = "Entity names", status = "warning",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("entities")),
    
    box(title = "Total emotional value", status = "danger",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("sentiment")),
    
    box(title = "Contact", status = "primary",
        width = "3", height = "300px"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        , tableOutput("email")),
   
)
  )
    )
  
) # Dashboard

ui <- dashboardPage(title = 'Personalized - Text mining app', header, sidebar, body, skin= "blue")



server <- function(input, output) {
  
  df_reac <- reactive({
    
    df <- data.frame(highlights = input$text)
    df$highlights <- as.character(df$highlights)
    df <- df %>% 
      unnest_tokens(word, highlights) %>% 
      anti_join(stop_words)
  })
  
  high <- reactive({
    
    input$text
  })
  
  
  output$fund <- renderValueBox({
    
    start <- str_which(high(), pattern = ("rais*|clos*|secur*"))
    pattern = "([€$£]|CHF|USD|EURO)[ ]?\\d+\\.?\\d+"
    b <- high()[start] %>% str_extract(pattern = pattern)
    c <-  paste(b[1], "million")
    valueBox(value = c, subtitle = "Raised capital",  icon = shiny::icon("coins")
             ,color = "purple") 
  }) 
  
  output$series <- renderValueBox({
    
    start <- str_which(high(), pattern = ("[Ss]eries"))
    pattern = "[Ss]eries\\s[ABCDE]"
    b <- high()[start] %>% str_extract(pattern = pattern)
    c <- ifelse(is.na(b[1]), paste("Series Unknown"), paste(b[1]))
    valueBox(value = c, subtitle = "Financial series",  icon = shiny::icon("users")
             ,color = "purple") 
  }) 
  
  # CLINICAL
  output$clinical <- renderValueBox({
    df <- data.frame(h = high())
    df$h <- as.character(df$h)
    df <- df %>% unnest_tokens(word, h, token = "sentences")
    start <- str_which(df$word, pattern = ("[Pp]hase")) 
    df <- str_extract_all(df$word[start], pattern = "[Pp]hase[ ][\\S]{1,3}")
    if (length(start) > 0) {
      df <- data.frame("clinicalstage" = unlist(df)) %>% 
        count(clinicalstage) %>% top_n(1)
    }
    else {
      df <- 0
    }
    valueBox(value = df[1], subtitle = "Clinical phase",  icon = shiny::icon("syringe")
             ,color = "purple") 
    
  })
  output$phone <- renderTable({
    pattern <- "(?: *[-+().]? *\\d){10,14}"
    df <- data.frame(h = high())
    df$h <- as.character(df$h)
    df <- df %>% unnest_tokens(word, h, token = "sentences")
    start <- str_which(df$word, pattern = pattern)
    df <- str_extract_all(df$word[start], pattern = pattern)
    df <- data.frame("Phone numbers" = unlist(df))
    
  })
  
  output$email <- renderTable({
    df <- data.frame(h = high())
    df$h <- as.character(df$h)
    df <- df %>% unnest_tokens(word, h, token = "sentences")
    start <- str_which(df$word, pattern = ("\\@"))
    df <- str_extract_all(df$word[start], pattern = "\\S*\\@\\S*")
    df <- data.frame("Email contact" = unlist(df)) 
  })

  output$data <- renderTable({
    
    
    top_words <- df_reac() %>% group_by(word) %>% 
      count() %>% 
      arrange(desc(n))
    
    top_words %>% head()
    
  })
  
  output$bigram <- renderTable({
    
    # Combine 
    df_com <- str_c(df_reac()$word, " ") 
    df_com <- data.frame(df_com) 
    # Most Frequent word pairs
    
    df_bigram <- df_com %>% 
      unnest_tokens(bigram, df_com, token = "ngrams", 
                    n = 3, n_min = 2)
    
    top_bigrams <-  df_bigram %>% 
      group_by(bigram) %>% 
      count() %>% 
      arrange(desc(n))
    
    top_bigrams %>% head()
    
  })
  
  output$sentiment <- renderTable({
    bing_word_counts <- df_reac() %>% inner_join(get_sentiments("bing")) %>% 
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    bing_word_counts
    bing_word_counts %>% count(sentiment, wt=n)
  })
  
  output$entities <- renderTable({
    pattern <- "[A-Z][a-z]+"
    start <- gregexpr(high(), pattern = pattern)
    entities <- unlist(regmatches(high(), start))
    enti <- data.frame(w = entities)
    enti$w <-  as.character(enti$w)
    
    enti <- enti %>% unnest_tokens(word, w,token = "ngrams", n = 3, n_min = 2) %>% 
      anti_join(stop_words) %>% 
      group_by(word) %>% 
      count() %>% 
      arrange(desc(n))
    enti %>% head()
  })
  
}

shinyApp(ui = ui, server = server)