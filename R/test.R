library(shiny)
library(heatmaply)
library(shinyjs)
source("plotSize.R")
source("heatmap.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    getJSLine("heatmap"),
    shinydashboard::dashboardPage(
        dbHeader,
        shinydashboard::dashboardSidebar(
            plotSizeMarginsUI("heatmap"),
            heatmapControlsUI("heatmap")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                tags$head(tags$title("DEBrowser")),
                getHeatmapUI("heatmap") 
            )
        )
    )
)

server <- function(input, output, session) {
    callModule(debrowserheatmap, "heatmap", mtcars)
}

shinyApp(ui, server)