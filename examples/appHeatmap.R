library(shiny)
library(heatmaply)
library(shinyjs)
source("../R/plotSize.R")
source("../R/heatmap.R")

header <- dashboardHeader(
    title = "DEBrowser Heatmap"
)
sidebar <- dashboardSidebar(  getJSLine(), sidebarMenu(id="DataAssessment",
           menuItem("Heatmap", tabName = "Heatmap"),
           plotSizeMarginsUI("heatmap"),
           heatmapControlsUI("heatmap")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Heatmap",  getHeatmapUI("heatmap"),
                column(4,
                       verbatimTextOutput("heatmap_hover"),
                       verbatimTextOutput("heatmap_selected")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
        selected <- callModule(debrowserheatmap, "heatmap", mtcars)
    })
    
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