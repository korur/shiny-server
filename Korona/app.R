library(shiny)
library(shinyMobile)
library(shinyscroll)
library(sever)
library(waiter)
library(readxl)
library(echarts4r)
library(dplyr)
library(tidyr)

toplam_vaka <- read_excel("data/vaka_sayisi_tur.xlsx")
toplam_vaka_p <- toplam_vaka %>%  gather("tip", "hasta", -c("ulke","gun","tarih"))
toplam_vaka_p <- toplam_vaka_p %>% filter(tip != "aktif")


# data preparation and functions

loader <- tagList(
  waiter::spin_3circles(),
  br(),br(),
  h3("Son veriler aliniyor...")
)

ui <- function() {
  f7Page(
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
      theme = "light"
    ),
    f7SingleLayout(
      navbar = f7Navbar(
        title = "Koronavirus Takibi",
        hairline = TRUE,
        shadow = TRUE,
        left_panel = TRUE,
        right_panel = FALSE
      ), 
      f7Panel(
        title = "Hakkimizda", 
        side = "left", 
        theme = "dark",
        effect = "cover",
        p("Saglik Bakanligi verilerine gore en son Koronavirus anlik hasta ve olen sayilari"),
        f7Link(label = "www.dataatomic.com", src = "https://dataatomic.com", external = TRUE),
        f7Link(label = "TSB", src = "https://www.saglik.gov.tr/", external = TRUE),
        f7Link(label = "Dunya'da Koronavirus Vakalari", src = "http://tools.dataatomic.com/shiny/CoronaOutbreak/", external = TRUE)
      ),
     f7Card(f7Row(echarts4rOutput("timeline")  ))))
  
}
server <- function(input, output,session) {
  
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
  
  
  output$timeline <- renderEcharts4r({
    
    vk <- toplam_vaka_p %>% 
      group_by(tip) %>% 
      e_charts(
        tarih
      ) %>% 
      e_scatter(
        hasta, 
        symbol_size = 15,
        itemStyle = list(
          shadowColor = "rgba(0,0,0,0.4)",
          shadowBlur = 3,
          shadowOffsetY = 3
        )
      ) %>%
      e_datazoom(show = FALSE, y_index = 0) %>% 
      e_legend(TRUE, orient = "horizontal", top=30) %>% 
      e_title("Turkiye - Koronavirus Hasta Sayilari") %>% 
      e_tooltip(
        trigger = "item",
        axisPointer = list(
          type = "cross"
        )
      ) %>% 
      e_image_g(left = 100,
                top = 100,
                z = 999,
                style = list(
                  image = "sk_mob_logo_black_extended.png", opacity=0.30
                )
      ) 
    #nice themes chalk infographic purple-passion 
    
    vk
    #  e_theme(theme="purple-passion")%>%
  })
  
  Sys.sleep(1.6)
  waiter::waiter_hide()
}
shinyApp(ui = ui, server = server)
