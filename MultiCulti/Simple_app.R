#Call library(shiny) to load the shiny package.
library(shiny)
library(ggplot2)

#Data <- readRDS("data/MultiCultiData.Rds")
Data <- readRDS(file.path("data", "MultiCultiData.Rds"))

#implement rules on which variables are available for which selections
#https://shiny.rstudio.com/reference/shiny/1.6.0/varSelectInput.html

#Set up user interface that will run under .html
ui <- fluidPage(
  titlePanel("XY Data Exploration"),
  varSelectInput("XVar", label = "Select Variable for X Axis", Data),
  varSelectInput("YVar", label = "Select Variable for Y Axis", Data),
  varSelectInput("ColourVar", label = "Select Variable for Symbol Colour", Data),
  #selectInput("RowVar", label =  "Select Variable for Facet Row", choices = c("None", colnames(Data))),
  #selectInput("ColVar", label = "Select Variable for Facet Column", Data),
  
  plotOutput("plot", width = "400px")
)

#Specify the behaviour of the app by defining a server function.
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(Data) + 
      geom_point(aes(x = !!input$XVar, y = !!input$YVar, colour = !!input$ColourVar)) +
      geom_smooth(aes(x = !!input$XVar, y = !!input$YVar, colour = !!input$ColourVar), method = "lm")
    
  }, res = 96)
}

#Execute shinyApp(ui, server) to construct and start a Shiny application from UI and server.
shinyApp(ui, server)