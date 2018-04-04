library(shiny)
library(shinyjs)
source("../R/funcs.R")
source("../R/condSelect.R")

header <- dashboardHeader(
  title = "DEBrowser Condition Selector"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
    menuItem("CondSelect", tabName = "CondSelect")
))

body <- dashboardBody(
  tabItems(
    #########################################
    ## Introduction tab panel
    tabItem(tabName="CondSelect", 
    condSelectUI())
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  debrowsercondselect(input, output, session, demodata, metadatatable)
    
}

shinyApp(ui, server)