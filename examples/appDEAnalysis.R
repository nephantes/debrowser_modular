library(shiny)
library(shinyjs)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/deprogs.R")

header <- dashboardHeader(
  title = "DEBrowser DE Analysis"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnalysis",
    menuItem("DEAnalysis", tabName = "DEAnalysis")
))

body <- dashboardBody(
  tabItems(
    #########################################
    ## Introduction tab panel
    tabItem(tabName="DEAnalysis", 
    fluidRow(
        column(12,
        getDEResultsUI("DEResults"))
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  filtd <- reactive({
      # Filter out the rows that has maximum 100 reads in a sample
      subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  10)
  })
  observe({
      if(!is.null(filtd())){
          callModule(debrowserdeanalysis, "DEResults", filtd())
      }
  })
}

shinyApp(ui, server)