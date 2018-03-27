#' debrowserlowcountfilter
#'
#' Module to filter low count genes/regions
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserlowcountfilter()
#'
debrowserlowcountfilter <- function(input, output, session, ldata) {
  
  fdata <- reactiveValues(count=NULL, meta = NULL)
  
  observeEvent(input$submitLCF, {
    if (is.null(ldata$count)) return (NULL)
    filtd <- ldata$count
    filtd[, colnames(filtd)] <- apply(filtd[, colnames(filtd)], 2, function(x) as.integer(x))

    if (input$lcfmethod == "Max"){
      filtd <- subset(filtd, apply(filtd, 1, max, na.rm = TRUE)  >=  as.numeric(input$maxCutoff))
    } else if (input$lcfmethod == "RowSum") {
      filtd <- subset(filtd, rowSums(filtd) >= as.numeric(input$rowsumCutoff))
    }
    else if (input$lcfmethod == "CPM") {
      cpmcount <- edgeR::cpm(filtd)
      filtd <- subset(filtd, rowSums(cpmcount > as.numeric(input$CPMCutoff) ) >= as.numeric(input$numSample))
    }
    fdata$count <- filtd
    fdata$meta <- ldata$meta
  })
  
  output$cutoffLCFMet <- renderUI({
    ret<-textInput(session$ns("maxCutoff"), "Filter features where Max Value <", value = "10" )
    if (input$lcfmethod == "RowSum"){
      ret<-textInput(session$ns("rowsumCutoff"), "Filter features where Row Sum <", value = "10" )
    }
    else if (input$lcfmethod == "CPM"){
      ret <- list(textInput(session$ns("CPMCutoff"), "Filter features where CPM <", value = "1" ),
                  textInput(session$ns("numSample"), "at least # of samples", value = toString(ncol(ldata$count)-1) ))
    }
    ret
  })

  filtereddata <- reactive({
    ret <- NULL
    if(!is.null(fdata$count)){
      ret <- fdata
    }
    return(ret)
  })
  
  observe({
    getSampleDetails(output, "uploadSummary", "sampleDetails", ldata)
    getSampleDetails(output, "filteredSummary", "filteredDetails", filtereddata())
  })
  
  list(filter=filtereddata)
}

#' dataLCFUI
#' Creates a panel to filter low count genes and regions
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- dataLCFUI("lcf")
#'
#' @export
#'
dataLCFUI<- function (id) {
  ns <- NS(id)
  list(
    fluidRow(
      shinydashboard::box(title = "Low Count Filtering",
                          solidHeader = T, status = "info",
                          width = 12,
                          fluidRow(
                            column(5,div(style = 'overflow: scroll',
                                tableOutput(ns("uploadSummary")),
                                DT::dataTableOutput(ns("sampleDetails")))
                            ),
                            column(2,
                                   shinydashboard::box(title = "Filtering Methods",
                                       solidHeader = T, status = "info",
                                       width = 12, 
                                       lcfMetRadio(id),
                                       uiOutput(ns("cutoffLCFMet")),
                                       actionButton(ns("submitLCF"), label = "Filter")
                                   )
                            ),
                            column(5,div(style = 'overflow: scroll', 
                                         tableOutput(ns("filteredSummary")),
                                         DT::dataTableOutput(ns("filteredDetails")))
                            )
                          )
      )
    ))
}

#' lcfMetRadio
#'
#' Radio buttons for low count removal methods
#'
#' @note \code{lcfMetRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- lcfMetRadio()
#'
#' @export
#'
lcfMetRadio <- function(id) {
  ns <- NS(id)
  radioButtons(inputId=ns("lcfmethod"), 
               label="Low count filtering method:",
               choices=c(Max='Max',
                         RowSum='RowSum',
                         CPM='CPM'
               ),
               selected='Max'
  )
}