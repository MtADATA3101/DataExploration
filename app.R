# Shiny app for PICO Project
library(tidyverse)
library(stats)
library(shinydashboard)

print(getwd())
data <- readRDS("data/PICO_MiSa_GrowthLogs_.Rds")


choiceVariables <- c("PrimaryOperator", "Photoperiod", "Run", "WL", "O2_Category", "Strain", "Par_ue", "GrowthAmpOD720Flag", "GrowthAmpdeltaODFlag", "OD720GmuGrowthFlag","deltaODGmuGrowthFlag","SetActinic_day", "IsdeltaOD_GLagNeg","IsOD720_GLagNeg","None")
#axisVariables <- 


ui <- dashboardPage(
  
  dashboardHeader(title = "PICO"),
  
  dashboardSidebar(
    width = 500,
    fluidRow(
      box(
        background = "black",
        sliderInput(
          "height",
          label = "Plot Height",
          min = 0, max = 1000, value = 500,
          step = 50
        )
      ),
      box(
        background = "black",
        radioButtons("plotType", label = "Plot Type",
                     choices = list("All Strains" = 1, "Compare Strains" = 2), 
                     selected = 1
        )
        )
      ),
      fluidRow( 
      box(
        background = "black",
        checkboxGroupInput(
          "photoperiod",
          "Photoperiod",
          choices = setNames(as.list(unique(data$Photoperiod)), unique(data$Photoperiod)),
          selected = unique(data$Photoperiod)
        )
      ),
      box(
        background = "black",
        checkboxGroupInput(
          "oxygen",
          "O2_Category",
          choices = setNames(as.list(unique(data$O2_Category)), unique(data$O2_Category)),
          selected = unique(data$O2_Category)
        )
      ),
      # box(
      #   background = "black",
      #   checkboxGroupInput(
      #     "Growth",
      #     "Growth",
      #     choices = setNames(as.list(unique(data$GrowthFlag_OD720, data$GrowthFlag_deltaOD)), unique(data$GrowthFlag_OD720, data$GrowthFlag_deltaOD)),
      #     selected = unique(data$GrowthFlag_OD720, data$GrowthFlag_deltaOD)
      #   )
      # ),
      box(
        background = "black",
        selectInput(
          "color",
          label = "Choose color",
          choices = choiceVariables,
          selected = "None"
        )
      )
    ),
   
    fluidRow(
      box(
        background = "black",
        selectInput(
          "xaxis",
          label = "X axis",
          choices = c("Par_ue", "OD720_Lmu_corr", "OD720_Lmu_raw","deltaOD_Lmu_corr", "deltaOD_Lmu_raw", "deltaODtrunc_Lmu_corr", "deltaOD_Gmu_corr", "SetActinic_day", "Photoperiod", unique(data$ProjectID)),
          selected = "Par_ue"
        )
      ),
      box(
        background = "black",
        selectInput(
          "yaxis",
          label = "Y axis",
          choices = c("Par_ue", "OD720_Lmu_corr", "OD720_Lmu_raw","deltaOD_Lmu_corr", "deltaOD_Lmu_raw", "deltaODtrunc_Lmu_corr", "deltaOD_Gmu_corr","SetActinic_day", "Photoperiod", unique(data$ProjectID)),
          selected = "deltaOD_Lmu_corr"
        )
      )
    ),
    
    fluidRow(
      box(
        background = "black",
        sliderInput(
          "size",
          label = "Size",
          min = 0, max = 5, value = 3.5,
          step = 0.5
        )
      ),
      box(
        background = "black",
        selectInput(
          "ProjectID",
          label = "ProjectID",
          choices = unique(data$ProjectID),
          selected = "PICO"
        )
      )
    ),
    fluidRow(
      box(
        background = "black",
        selectInput(
          "facetY",
          label = "Choose facet row",
          choices = choiceVariables,
          selected = "None"
        )
      ),
      box(
        background = "black",
        selectInput(
          "facetX",
          label = "Choose facet column",
          choices = choiceVariables,
          selected = "None"
        )
      )
    ),
    
    fluidRow(
      box(
        background = "black",
        checkboxGroupInput(
          "run",
          "Run Number",
          choices = setNames(as.list(unique(data$Run)), unique(data$Run)),
          selected = unique(data$Run)
        )
      ),
      box(
        background = "black",
        checkboxGroupInput("strain", "Strain",
          choices = setNames(as.list(unique(data$Strain)), unique(data$Strain)),
          selected = unique(data$Strain)
        )
      ),
      box(
        background = "black",
        checkboxGroupInput("light", "Spectral light",
          choices = setNames(as.list(unique(data$WL)), unique(data$WL)),
          selected = unique(data$WL)
        )
      )
    )
    ),
  
  dashboardBody(plotOutput("lightPlot"),
                height = "5000px")
)


# Server logic ----
server <- function(input, output) {
  filterData <- reactive({
    data %>%  
      filter(WL %in% input$light, Run %in% input$run, Strain %in% input$strain, O2_Category %in% input$oxygen)
  })
  
 # finalData <- reactive({
 # filterdata() %>%
 #  filter(Strain %in% input$Strain,
 #        Photoperiod == input$photoperiod)
 # })
  
  finalData <- reactive({
    filterData() %>%
      filter(ProjectID == input$ProjectID,
             Photoperiod %>% as.numeric() %in% input$photoperiod)
  })
  
 
  compareData <- reactive({
    filterData() %>%
      pivot_wider(id_cols = c(Par_ue, Photoperiod, WL, O2_Category, Strain, OD720_Gmu_mu, deltaOD_Lmu_corr), 
                  names_from = Strain, 
                  values_from = deltaOD_Lmu_corr)
  })
  
  output$lightPlot <- renderPlot({
    
    aesInput <- list(x = input$xaxis,
                     y = input$yaxis,
                     color = input$color)
    
    aesInput_GrowthFlag <- list(x = min(input$xaxis),
                     y = min(input$yaxis),
                     label = "*",
                     color = input$color)
    
    aesInputFiltered <- lapply(
      aesInput[aesInput != "None"],
      sym)
    
    aesInputFiltered_GrowthFlag <- lapply(
      aesInput_GrowthFlag[aesInput_GrowthFlag != "None"],
      sym)
    
    facetInput <- c(input$facetX,
                    input$facetY)
    
    facetInputFiltered <- facetInput[facetInput != "None"]
    
    if(input$plotType == 1){
      ggplot(finalData()) +
        geom_point(do.call(aes, aesInputFiltered), 
                   size = input$size) +
        theme_bw() +
        # if(input == "OD720"){
        #   geom_text(data = . %>% filter(GrowthFlag_OD720 == 0), do.call(aes, aesInputFiltered_GrowthFlag)) 
        #   } +
        ggtitle(paste("Project:", input$ProjectID)) +
        if(length(facetInputFiltered) == 1){
          facet_grid(reformulate(facetInputFiltered))
        } else if(length(facetInputFiltered) == 2){
          facet_grid(reformulate(input$facetX, input$facetY))
        } 
    } else {
      ggplot(compareData()) +
        geom_point(do.call(aes, aesInputFiltered), 
                   size = input$size) +
         theme_bw() +
        theme(aspect.ratio = 1) +
        ggtitle(paste("Compare Growths mu:", input$yaxis, "vs", input$xaxis)) 
      
    }
  },
  height = function(){input$height},
  width = "auto")
}

# Run app ----
shinyApp(ui, server)