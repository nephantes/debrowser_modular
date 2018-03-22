library(shiny)
library(heatmaply)
library(shinyjs)
source("../R/plotSize.R")
source("../R/heatmap.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    getJSLine(),
    shinydashboard::dashboardPage(
        
        dbHeader,
        shinydashboard::dashboardSidebar(
            plotSizeMarginsUI("heatmap"),
            heatmapControlsUI("heatmap")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                getHeatmapUI("heatmap"),
                column(4,
                       verbatimTextOutput("heatmap_hover"),
                       verbatimTextOutput("heatmap_selected")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    selected <- callModule(debrowserheatmap, "heatmap", mtcars)
    
    output$heatmap_hover <- renderPrint({
        if (selected$shgClicked() != "")
            return(paste0("Clicked: ",selected$shgClicked()))
        else
            return(paste0("Hovered:", selected$shg()))
    })

    output$heatmap_selected <- renderPrint({
         selected$selGenes()
    })


}

shinyApp(ui, server)