#
library(shiny)

ui <- fluidPage(
  plotOutput("plot", width = "400px")
)
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
