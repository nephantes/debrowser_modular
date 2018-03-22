library(shiny)
library(heatmaply)
library(shinyjs)
source("../R/plotSize.R")
source("../R/pca.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    shinydashboard::dashboardPage(
        dbHeader,
        shinydashboard::dashboardSidebar(
            pcaPlotControlsUI("pca"),
            plotSizeMarginsUI("pca", w=600, h=400, t=50, b=50, l=60, r=0)
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                getPCAPlotUI("pca"),
                column(4,
                       verbatimTextOutput("pca_hover"),
                       verbatimTextOutput("pca_selected")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    dat <- generateTestData()
    selected <- callModule(debrowserpcaplot, "pca", dat$data, dat$metadata)
    
    #output$main_hover <- renderPrint({
    #    selected$shgClicked()
    #})

    #output$main_selected <- renderPrint({
    #     selected$selGenes()
    #})

}

shinyApp(ui, server)