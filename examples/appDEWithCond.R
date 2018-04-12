library(shiny)
library(shinyjs)
library(DESeq2)
library(edgeR)
library(limma)
source("../R/funcs.R")
source("../R/condSelect.R")
source("../R/deprogs.R")

header <- dashboardHeader(
  title = "DEBrowser Condition Selector"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
    menuItem("CondSelect", tabName = "CondSelect")
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName="CondSelect", 
    condSelectUI(),
    column(12,
           verbatimTextOutput("dcres")
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  filtd <-
        # Filter out the rows that has maximum 100 reads in a sample
        subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  10)
    
  sel <- debrowsercondselect(input, output, session, demodata, metadatatable)
  dc <- reactive({
      if (input$startDE)
            prepDataContainer(filtd, sel$cc(), input)
  })
  
  output$dcres <- renderPrint({
      if ( sel$cc() > 0 ){
          for (i in seq(1:sel$cc()))
          {
              if(is.list(dc()))
                    print(isolate( dc()[i]))
          }
      }
  })
}

shinyApp(ui, server)