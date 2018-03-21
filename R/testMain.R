library(shiny)
library(heatmaply)
library(shinyjs)
source("plotSize.R")
source("mainScatter.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    shinydashboard::dashboardPage(
        dbHeader,
        shinydashboard::dashboardSidebar(
            mainPlotControlsUI("main"),
            plotSizeMarginsUI("main")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                getMainPlotUI("main"),
                column(4,
                       verbatimTextOutput("main_hover"),
                       verbatimTextOutput("main_selected")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    #xdata <- generateTestData()
    selected <- callModule(debrowsermainplot, "main", xdata)
    
    output$main_hover <- renderPrint({
        selected$shgClicked()
    })

    output$main_selected <- renderPrint({
         selected$selGenes()
    })

}

shinyApp(ui, server)