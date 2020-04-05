library(shiny)
library(shinyMobile)
library(shinydashboard)
library(shinyscroll)
library(countup)
library(sever)
library(waiter)
library(dplyr)
library(fireData)
library(firebase)
library(leaflet)
library(yaml)
library(shinyjs)

ccc = c('fever', 'cough', 'breath', 'home', "home2", 'work', 'lat', 'long', "usercon", "timecon")

if (file.exists("~/workingdirectory/CoronaOutbreak/douhavefever/_firebase.yml")) { 
  config <- yaml::read_yaml("~/workingdirectory/CoronaOutbreak/douhavefever/_firebase.yml") 
} else { 
  config <- yaml::read_yaml("/etc/_firebase.yml")
}

# Create config
firebase::create_config(api_key = config$database$apiKey, project_id = config$database$projectId)
databaseURL <- config$database$databaseURL


# data preparation and functions

loader <- tagList(
  waiter::spin_flower(),
  br(),br(),
  h3("Sisteme baglaniyor...",  style = "color:#1ee6be")
)

ui <- function() { 
  f7Page( useFirebase(), # import dependencies,
          useFirebaseUI(), 
          shinyjs::useShinyjs(),
    # Tracking ID
    tags$head(
      HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
       <script async src='https://www.googletagmanager.com/gtag/js?id=UA-148414815-3'></script>
       <script>
       window.dataLayer = window.dataLayer || [];
     function gtag(){dataLayer.push(arguments);}
     gtag('js', new Date());
     
     gtag('config', 'UA-148414815-3');
     </script>"
      )),
    shinyscroll::use_shinyscroll(),
    sever::use_sever(),
    waiter::use_waiter(), # dependencies
    waiter::waiter_show_on_load(loader, color = "#000000"),
    title = "Koranavirus ",
    dark_mode = FALSE,
    init = f7Init(
      skin = "auto", 
      theme = "dark"
    ), #f7init
    f7TabLayout(
      navbar = f7Navbar(
        title = "Hakkinda",
        hairline = TRUE,
        shadow = TRUE,
        left_panel = TRUE,
        right_panel = FALSE
      ), #end of navbar
      f7Panel(
        title = h4("Kovit Takip",style = "color:#1ee6be"), 
        side = "left", 
        theme = "light",
        effect = "cover",
        p("Kovid-19 risk bolgelerinin belirlenerek onceden tedbir alinmasina yardimci takip sistemi. 
        Yeterli veri miktarina ulasildiktan sonra bulundugunuz bolgenin risk orani otomatik olarak guncellenecektir.", style = "color:#1ee6be"),
        f7Link(label = p("www.dataatomic.com tarafindan gelistirilmistir.", style = "color:#1ee6be"), src = "https://dataatomic.com", external = TRUE),
        f7Link(label = p("Saglik Bakanligi",style = "color:#1ee6be"), src = "https://www.saglik.gov.tr/", external = TRUE),
        f7Link(label = p("Dunya'da son durum",style = "color:#1ee6be"), src = "http://tools.dataatomic.com/shiny/CoronaOutbreak/", external = TRUE)
      ), # end panels 
  
      f7Tabs(
        animated = TRUE,
        id = 'tabs',
      f7Tab(
        tabName = "Home",
        icon = f7Icon("rocket", old = FALSE),
        active = TRUE,
        swipeable = TRUE,
      f7Card(tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
f7Row(f7Toggle(
  inputId = "fever",
  label = tags$p("Atesim var", style = "font-size: 18px;"),
  color = "pink",
  checked = FALSE
))),
f7Card(f7Row( f7Toggle(
  inputId = "cough",
  label = tags$p("Oksuruyorum",style = "font-size: 18px;"),
  color = "pink",
  checked = FALSE
))),
f7Card(f7Row( f7Toggle(
  inputId = "breath",
  label = tags$p("Nefes darligim var",style = "font-size: 18px;"),
  color = "pink",
  checked = FALSE
))),
f7Card(f7Row( 
  f7Toggle(
    inputId = "home",
    label = tags$p("Evden cikmiyorum",style = "font-size: 18px;"),
    color = "green",
    checked = FALSE
  ))), 
f7Card(f7Row(  f7Toggle(
    inputId = "home2",
    label = tags$p("Bazen cikiyorum",style = "font-size: 18px;"),
    color = "pink",
    checked = FALSE
  ))),
f7Card(f7Row(f7Toggle(
  inputId = "work",
  label = tags$p("Ise gidiyorum",style = "font-size: 18px;"),
  color = "pink",
  checked = FALSE
))),
f7Card(title="Lokasyon Bilgileri",f7Col(width = 2,color="red",
                    verbatimTextOutput("lat"),
                    verbatimTextOutput("long"),
                    verbatimTextOutput("usercon"),
                    )), 



reqSignin(f7Card(f7Col(width= 2, f7Button(inputId ='save_inputs', label='Gonder',  color = NULL,
                                fill = TRUE,
                                outline = FALSE,
                                shadow = FALSE,
                                rounded = TRUE
                                )),div(id = "form"),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h3("Thanks, your response was submitted successfully!"),
                     actionLink("submit_another", "Submit another response")
                   )
                 ) ))


),
  f7Tab(
    tabName = "Haritam",
    icon = f7Icon("map", old = FALSE),
    active = FALSE,
    swipeable = TRUE,
    f7Card(f7Row(leafletOutput("mapp")
    ))#f7card
  ),

f7Tab(
  tabName = "Risk analizi",
  icon = f7Icon("waveform", old = FALSE),
  active = FALSE,
  swipeable = TRUE,
  f7Row(f7Col(h2("Toplam veri miktari", align="center"),
f7Card(
  h2(countupOutput("dpsa"), style="text-align: center;")
        )), 
f7Col(h2("Atesi olan", align="center"),
f7Card(
  h2(
    countupOutput("fant"), style="text-align: center;"
    )
      )#f7Card
     )#f7col


) #frow


  ), f7Tab(
    tabName = "Genel Bilgiler",
    icon = f7Icon("info", old = FALSE),
    active = FALSE,
    swipeable = TRUE,
    f7ExpandableCard(
      id = "Home",
      title = "Home", subtitle = "acmak icin dokunun",
      fullBackground = FALSE,
      color="blue",
      "Kovit Takip sistemini kullanmak icin Home bolumundeki sorulari yanitlayip gonder buttonuna basin. Toplanan veriler yerel konumlardaki risk bolgelerinin hesaplanmasinda kullanilacaktir.
      Eger calisiyorsaniz ya da evinizden ayrilmak zorunda kaldiginizda, risk durumunuzun daha iyi hesaplanabilmesi icin yeni konumunuzda uygulamayi tekrar acip form gonderebilirsiniz"
    ),
    f7ExpandableCard(
      id = "Haberler",
      title = "Haberler",
      fullBackground = FALSE,
      color = "blue", subtitle = "acmak icin dokunun",
      "Onumuzde ki gunlerde ilgili haberleri burada bulabilirsiniz"
    ))#tabend

)#ftabs
)

)
}

server <- function(input, output, session) {
  
  abcd <- reactive({
    
  aa <- download(projectURL = databaseURL, fileName = "fire", token=tkn())
  abc <-  list()
  
  for(i in 1:length(aa)){
    abc[[i]] <- aa[[i]]$value
  }
  
  abc <- as.data.frame(do.call(rbind,abc))
  colnames(abc) <- ccc
  abc$lat <- as.numeric(as.character((abc$lat)))
  abc$long <- as.numeric(as.character((abc$long)))
  abc
  
  })
  
  output$mapp <- renderLeaflet({
    m <- leaflet(abcd())  %>% 
      addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addMarkers(input$long, lat = input$lat,  popup =   paste("<h3>","<b>", "Buradasiniz", "</b>", "</h3>")) %>% 
      addCircleMarkers(lng=abcd()$long, lat=abcd()$lat, color = ifelse(abcd()$fever == 1, "red", "green")) %>% setView(lng = input$long, lat = input$lat, zoom = 7) 
     m
                 
  })
  sever::sever(
    tagList(
      h1("Whoops!"),
      p("Tekrar baglan"),
      shiny::tags$button(
        "BAGLAN",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
  
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers( # define providers
      email = TRUE, 
      google = TRUE
    )$
    launch() # launch

  output$lat <- renderText({
    paste("Enlem:", input$lat)
  })
  
  output$long <- renderText({
    paste("Boylam:", input$long)
  }) 
  
  output$dpsa <- renderCountup({
    nrow(abcd()) %>% countup()
  })
  
  output$fant <- renderCountup({
    df_a <- abcd() %>% filter(fever == 1) %>% count() %>% pull 
    countup(df_a, duration=10)
  })

  output$usercon <- eventReactive(f$req_sign_in(), {   
    f$signed_in$response$email
  })
  
  usercon <- eventReactive(f$req_sign_in(), {   
    f$signed_in$response$email
  })
  
  timecon <- eventReactive(input$save_inputs, { 
    Sys.time()
  })
  
  tkn <- eventReactive(f$req_sign_in(), {   
    f$signed_in$response$stsTokenManager$accessToken
     }) 
  
  observeEvent(input$save_inputs, {
    shinyjs::disable('save_inputs')
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::enable('save_inputs')
    shinyjs::hide("thankyou_msg")
  })
  
  observeEvent(input$save_inputs, {
    # Define inputs to save
    inputs_to_save <- c('fever', 'cough', 'breath', 'home', "home2", 'work', 'lat', 'long')
    # Declare inputs
    inputs <- NULL
    # Append all inputs before saving to folder
    for(input.i in inputs_to_save){
      inputs <- append(inputs, input[[input.i]])
    }
    inputs <- append(inputs, c(timecon(), usercon()))
    # Inputs data.frame
    inputs_data_frame <- data.frame(inputId = c(inputs_to_save, c("timecon", "usercon")), value = inputs)
    # Save Inputs
    upload(x = inputs_data_frame, projectURL = databaseURL, directory = "fire", token=tkn())
  }) 
  
 
  Sys.sleep(3.6)
  waiter::waiter_hide()
}

shinyApp(ui = ui, server = server)
