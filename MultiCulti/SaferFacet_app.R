#Call library(shiny) to load the shiny package.
library(shiny)
library(ggplot2)
library(dplyr)

Data <- readRDS("data/MultiCultiData.Rds")

#implementing rules on which variables are available for which selections
#https://shiny.rstudio.com/reference/shiny/1.6.0/varSelectInput.html
#https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame

ui <- fluidPage(
  titlePanel("XY Data Exploration"),
  varSelectInput("XVar", label = "Select Variable for X Axis", Data),
  varSelectInput("YVar", label = "Select Numeric Variable for Y Axis", Data %>% select(where(is.numeric))),
  varSelectInput("ColourVar", label = "Select Variable for Symbol Colour", Data),
  #selectInput("RowVar", label =  "Select Variable for Facet Rows", choices =  colnames(Data), ""),
  varSelectInput("RowVar", label =  "Select Character Variable for Facet Rows", Data %>% select(where(is.character))),
  selectInput("ColVar", label = "Select Variable for Facet Columns", choices =  colnames(Data), ""),
  
  plotOutput("plot", width = "400px")
)

#Specify the behaviour of the app by defining a server function.

#remember !! to bring in values of external variables for plotting; 'sym' needed with 'selectInput', but not with varSelectInput
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(Data) + 
      geom_point(aes(x = !!input$XVar, y = !!input$YVar, colour = !!input$ColourVar)) +
      facet_grid(rows = vars(!!input$RowVar), cols =  vars(!!sym(input$ColVar)))
     # facet_grid(rows = vars(!!sym(input$RowVar)), cols =  vars(!!sym(input$ColVar)))
  }, res = 96)
}


#Execute shinyApp(ui, server) to construct and start a Shiny application from UI and server.
shinyApp(ui, server)