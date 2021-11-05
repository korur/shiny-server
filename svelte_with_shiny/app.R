library(shiny)
getwd()

ui <- fluidPage(

  
  htmlTemplate("template.html", document_ = FALSE),
  
)

server <-  function(input, output) {}
  
shinyApp(ui, server)






