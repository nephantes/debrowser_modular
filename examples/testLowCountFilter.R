library(debrowser)
library(shiny)
library(shinydashboard)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/lowcountfilter.R")
source("../R/downloadData.R")

header <- dashboardHeader(
    title = "DEBrowser Filter"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
       menuItem("Filter", tabName = "Filter")))

body <- dashboardBody(
    tabItems(
        #########################################
        ## Introduction tab panel
        tabItem(tabName="Filter", dataLCFUI("lcf"),
                column(4,
                       verbatimTextOutput("filtertable")
                )
        )
    ))
                
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    
    ldata <- reactiveValues(count=NULL, meta=NULL)
    ldata$count <- demodata
    ldata$meta <- metadatatable
    data <- callModule(debrowserlowcountfilter, "lcf", ldata)
    observe({
        output$filtertable <- renderPrint({
            head( data$filter()$count )
        })
    })
}

shinyApp(ui, server)