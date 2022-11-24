#Call library(shiny) to load the shiny package.
library(shiny)
library(ggplot2)

Data <- readRDS("data/MultiCultiData.Rds")

#implement rules on which variables are available for which selections
#https://shiny.rstudio.com/reference/shiny/1.6.0/varSelectInput.html

ui <- fluidPage(
  titlePanel("XY Data Exploration"),
  varSelectInput("XVar", label = "Select Variable for X Axis", Data),
  varSelectInput("YVar", label = "Select Variable for Y Axis", Data),
  #selectInput("ColourVar", label = "Select Variable for Symbol Colour", choices = c("None", colnames(Data)), selected = "Null"),
  #selectInput("RowVar", label =  "Select Variable for Facet Row", choices = c("None", colnames(Data)), selected = "Null"),
  #selectInput("ColVar", label = "Select Variable for Facet Column", choices = c("None", colnames(Data)), selected = "Null"),
  
  plotOutput("plot", width = "400px")
)

#Specify the behaviour of the app by defining a server function.

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(Data, aes(x = !!input$XVar, y = !!input$YVar)) + geom_point()
    }, res = 96)
}

#Execute shinyApp(ui, server) to construct and start a Shiny application from UI and server.
shinyApp(ui, server)