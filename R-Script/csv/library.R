library(shiny)

ui <- fluidPage(
  titlePanel(""),
 mainPanel(
      textOutput("ID")
    ) 
)

server <- function(input, output) {
  output$ID = renderText({
    paste0("Hello Shiny!")
  })
}

shinyApp(ui ,  server)
