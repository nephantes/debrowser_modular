library(shiny)
library(shinyjs)
library(DESeq2)
library(edgeR)
library(limma)
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
    tabItem(tabName="DEAnalysis", 
    fluidRow(
        column(12,
        getDEResultsUI("DEResults"))
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  ######## Prepare demo data to use the module ########
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  filtd <- reactive({
      # Filter out the rows that has maximum 100 reads in a sample
      subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  10)
  })
  params <- reactive({
      #Run DESeq2 with the following parameters
      c("DESeq2", "parametric", "0", "Wald")
  })
  cols <- reactive({
      #For demo purpose use the coumns below;
      c("exper_rep1", "exper_rep2", "exper_rep3", "control_rep1", "control_rep2", "control_rep3")
  })
  conds <- reactive({
      #For each column, there is matching condition defined
      c("exper", "exper", "exper", "control", "control", "control")
  })
  ######################################################
  
  observe({
      if(!is.null(filtd())){
          callModule(debrowserdeanalysis, "DEResults", data = filtd(), 
                     columns = cols(), conds = conds(), params = params())
      }
  })
}

shinyApp(ui, server)