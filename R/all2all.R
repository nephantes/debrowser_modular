#' debrowserall2all
#'
#' Module for a all2all plots. 
#' low count removal modules or any desired module
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return density plot 
#' @export
#'
#' @examples
#'     x <- debrowserall2all(data = data)
#'
debrowserall2all <- function(input, output, session, data = NULL) {
    output$all2allplotly <- renderPlotly({
        withProgress(message = 'Drawing', detail = " All2All plot", value = NULL, {
        all2allplotly(data, input)
        })
    })
    output$all2allplot <- renderPlot({
        all2all(data, input)
    })
    output$All2AllUI <- renderUI({
        getPlotUI(input, session)
    })
}

#' getPlotUI
#'
#' @note \code{getPlotUI}
#' @return the plot for two types below
#' 
#' @examples
#'     x <- getPlotUI()
#'
#' @export
#'
getPlotUI <- function(input = NULL, session = NULL) {
    if (is.null(input)) return(NULL)
    shinydashboard::box(
        collapsible = TRUE, title = session$ns("plot"), status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE,  getPlotType(input, session))
}

#' getPlotUI
#'
#' @note \code{getPlotUI}
#' @return the plot for two types below
#' 
#' @examples
#'     x <- getPlotUI()
#'
#' @export
#'
getAll2AllMenuUI <- function(id) {
    ns <- NS(id)
    radioButtons(inputId=ns("all2alltype"), 
             label="All2all plot method:",
             choices=c(standard='standard',
                       plotly='plotly'
             ),
             selected='standard'
)
}

#' getPlotType
#'
#' @note \code{getPlotType}
#' @return the plot for two types below
#' 1. With plotly
#' 2. standard plot functions
#' Only the view is different, the information is the same in both. First option is more interactive.
#' 
#' @examples
#'     x <- getPlotType()
#'
#' @export
#'
getPlotType <- function(input, session) {

    plotout <- plotOutput(session$ns("all2allplot"),  height = input$height, width = input$width)
    if (is.null(input$all2alltype))
        return(plotout)
    else if(!is.null(input$all2alltype) && input$all2alltype=="plotly")
        plotout <- plotlyOutput(session$ns("all2allplotly"), height = input$height, width = input$width)

    return(plotout)
}


#' getAll2AllPlotUI
#'
#' All2All plots UI. If   
#'
#' @note \code{getAll2AllPlotUI}
#' @return the panel for all2all plots, there are two options here;
#' 1. With plotly
#' 2. plot functions
#' Only the view is different, the information is the same in both. First option is more interactive.
#' 
#' @examples
#'     x <- getAll2AllPlotUI()
#'
#' @export
#'
getAll2AllPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("All2AllUI"))
}


#' all2allplotly
#'
#' Prepares all2all scatter plots for given datasets using plotly
#'
#' @param data, data that have the sample names in the header.
#' @param input, input
#' @return all2all scatter plots
#' @examples
#'     input <- c()
#'     input$cex <- 0.7
#'     plot<-all2allplotly(mtcars, input)
#'
#' @export
#'
all2allplotly <- function(data, input) {
    nr <- nrow(data)
    cex <- 0.7
    if (!is.null(input$cex))
        cex <- input$cex
    if (nr > 1000)
        nr <- 1000
    dat <-data.frame(log10(data[1:nr,] + 0.1))
    pm <- ggpairs(dat, type="scatter",
        lower = list(continuous =
        wrap("points", alpha = 0.3, size=cex))) 
    return(pm)
}


#' all2all
#'
#' Prepares all2all scatter plots for given datasets. 
#'
#' @param data, data that have the sample names in the header.
#' @param input, input for input cex size
#' @return all2all scatter plots
#' @examples
#'     input <- c()
#'     input$cex <- 2
#'     plot<-all2all(mtcars)
#'
#' @export
#'
all2all <- function(data, input) {
    pcor <- function(x, y, ...) panel.cor(x, y, cex.cor = input$cex)
    nr <- nrow(data)
    if (nr > 1000)
        nr <- 1000
    pairs(log10(data[1:nr, ]), cex = 0.25,
          diag.panel = panel.hist, lower.panel = pcor)
}

#' panel.hist
#'
#' Prepares the historgram for the all2all plot. 
#'
#' @param x, a vector of values for which the histogram is desired
#' @param ..., any additional params
#' @return all2all histogram plots
#' @examples
#'     panel.hist(1)
#'
#' @export
#'
panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nb <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(breaks[-nb], 0, breaks[-1], y, col = "red", ...)
}

#' panel.cor
#'
#' Prepares the correlations for the all2all plot. 
#'
#' @param x, numeric vector x
#' @param y, numeric vector y
#' @param prefix, prefix for the text
#' @param cex.cor, correlation font size
#' @param ..., additional parameters
#' @return all2all correlation plots
#' @examples
#'     panel.cor(c(1,2,3), c(4,5,6))
#'
#' @export
#'
panel.cor <- function(x, y, prefix = "rho=", cex.cor=2, ...){
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor.test(x, y, method = "spearman",
                  na.rm = TRUE, exact = FALSE)$estimate
    txt <- round(r, digits = 2)
    txt <- paste0(prefix, txt)
    text(0.5, 0.5, txt, cex = cex.cor)
}