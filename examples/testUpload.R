library(debrowser)
source("../R/plotSize.R")
source("../R/dataLoad.R")

options(shiny.maxRequestSize = 30*1024^2)

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    shinydashboard::dashboardPage(
        dbHeader,
        shinydashboard::dashboardSidebar(
            #dataLoadControlsUI("load")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                dataLoadUI("load"),
                column(4,
                       verbatimTextOutput("counttable"),
                       verbatimTextOutput("metadatatable")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    data <- callModule(debrowserdataload, "load")

    output$counttable <- renderPrint({
        head( data$load()$count )
    })
    output$metadatatable <- renderPrint({
        head( data$load()$meta)
    })
}

shinyApp(ui, server)