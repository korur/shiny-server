library(shiny)
library(shinyMobile)

shiny::shinyApp(
  ui = f7Page(
    color = "pink",
    title = "Floating action buttons",
    f7SingleLayout(
      navbar = f7Navbar(title = "f7Fabs"),
      f7Fabs(
        extended = TRUE,
        label = "Menu",
        position = "center-top",
        color = "yellow",
        sideOpen = "right",
        lapply(1:4, function(i) f7Fab(paste0("btn", i), i))
      ),
      lapply(1:4, function(i) verbatimTextOutput(paste0("res", i))),
      
      f7Fabs(
        position = "center-center",
        color = "purple",
        sideOpen = "center",
        lapply(5:8, function(i) f7Fab(paste0("btn", i), i))
      ),
      lapply(5:8, function(i) verbatimTextOutput(paste0("res", i))),
      
      f7Fabs(
        position = "left-bottom",
        color = "pink",
        sideOpen = "top",
        lapply(9:12, function(i) f7Fab(paste0("btn", i), i))
      )
    )
    
  ),
  server = function(input, output) {
    lapply(1:12, function(i) {
      output[[paste0("res", i)]] <- renderPrint(input[[paste0("btn", i)]])
    })
  }
)