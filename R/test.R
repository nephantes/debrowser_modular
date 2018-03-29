library(shiny)
library(shinyBS)
source("../R/funcs.R")

ui <- fluidPage(
  mainPanel(
    getBSTableModal('startupModal', 'Dum Dum', 'start'),
    actionButton("start", "Start"),
    width = 12
  )
)

server <- function(input, output, session) {

  output$startupModal <- DT::renderDataTable({
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    if (!is.null(demodata)){
      DT::datatable(demodata,
                    list(lengthMenu = list(c(10, 25, 50, 100),
                                           c("10", "25", "50", "100")),
                         pageLength = 25, paging = TRUE, searching = TRUE))
    }
  })
}

shinyApp(ui = ui, server = server)