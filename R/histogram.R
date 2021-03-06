#' getHistogramPlotUI
#'
#' Histogram plots UI.  
#'
#' @note \code{getHistogramPlotUI}
#' @return the panel for PCA plots;
#'
#' @examples
#'     x <- getHistogramPlotUI()
#'
#' @export
#'
getHistogramUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("histogramUI"))
}

#' debrowserhistogram
#'
#' Module for a histogram that can be used in data prep and 
#' low count removal modules
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param pcadata, a matrix that includes expression values
#' @return histogram 
#' @export
#'
#' @examples
#'     x <- debrowserhistogramplot(data = data)
#'
debrowserhistogram <- function(input, output, session, data = NULL) {
    output$histogram <- renderPlotly({
       
      h <- hist(log10(rowSums(data)), breaks = as.numeric(input$breaks), plot = FALSE)
      
      p <- plot_ly(x = h$mids, y = h$counts, 
          width = input$width, height=input$height) %>% 
      add_bars() %>%
      plotly::layout(
        margin = list(l = input$left,
                      b = input$bottom,
                      t = input$top,
                      r = input$right
        ))
      p$elementId <- NULL
      p
    })
    output$histogramUI <- renderUI({
    shinydashboard::box(
        collapsible = TRUE, title = session$ns("plot"), status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE,  plotlyOutput(session$ns("histogram"),
             width = input$width, height=input$height))
    })
}

#' histogramControlsUI
#'
#' Generates the controls in the left menu for a histogram
#'
#' @note \code{histogramControlsUI}
#' @return returns the left menu
#' @examples
#'     x <- histogramControlsUI()
#' @export
#'
histogramControlsUI <- function(id) {
  ns <- NS(id)
  textInput(ns("breaks"), "Breaks", value = "100" )
}