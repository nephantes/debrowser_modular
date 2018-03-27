library(debrowser)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/lowcountfilter.R")

dbHeader <- shinydashboard::dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(style='color: white;',
        id="top_logo" , "DEBrowser")

ui <- fluidPage(
    shinydashboard::dashboardPage(
        dbHeader,
        shinydashboard::dashboardSidebar(
            #dataLCFControlsUI("lcf")
        ),
        shinydashboard::dashboardBody(
            mainPanel(
                dataLCFUI("lcf"),
                column(4,
                    verbatimTextOutput("filtertable")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    
    ldata <- reactiveValues(count=NULL, meta=NULL)
    ldata$count <- demodata
    ldata$meta <- metadatatable
    
    data <- callModule(debrowserlowcountfilter, "lcf", ldata)

    output$filtertable <- renderPrint({
        head( data$filter()$count )
    })
}

shinyApp(ui, server)