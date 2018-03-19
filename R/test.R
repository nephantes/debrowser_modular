library(shiny)
library(heatmaply)
library(shinyjs)
source("plotSize.R")
source("heatmap.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    getJSLine(),
    shinydashboard::dashboardPage(
        
        dbHeader,
        shinydashboard::dashboardSidebar(
            plotSizeMarginsUI("heatmap1"),
            heatmapControlsUI("heatmap1"),
            plotSizeMarginsUI("heatmap2"),
            heatmapControlsUI("heatmap2")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                getHeatmapUI("heatmap1"),
                column(4,
                       verbatimTextOutput("heatmap_hover1"),
                       verbatimTextOutput("heatmap_selected1")
                ),
                getHeatmapUI("heatmap2"),
                column(4,
                       verbatimTextOutput("heatmap_hover2"),
                       verbatimTextOutput("heatmap_selected2")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    selected <- callModule(debrowserheatmap, "heatmap1", mtcars)
    selected2 <- callModule(debrowserheatmap, "heatmap2",getSelGenes() )
    
    output$heatmap_hover1 <- renderPrint({
        if (selected$shgClicked() != "")
            return(paste0("Clicked: ",selected$shgClicked()))
        else
            return(paste0("Hovered:", selected$shg()))
    })
    getSelGenes <- reactive({
        data <- mtcars 
        sel <- selected$hselGenes()
        print(length(sel))
        if (length(sel)>2)
            data <- mtcars[sel,]
        data
    })
    output$heatmap_selected1 <- renderPrint({
         selected$hselGenes()
    })
    output$heatmap_hover2 <- renderPrint({
        if (selected2$shgClicked() != "")
            return(paste0("Clicked: ",selected2$shgClicked()))
        else
            return(paste0("Hovered:", selected2$shg()))
    })
    output$heatmap_selected2 <- renderPrint({
        return(selected2$hselGenes())
    })
}

shinyApp(ui, server)