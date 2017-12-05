#' getMainPanel
#'
#' main panel for volcano, scatter and maplot.  
#' Barplot and box plots are in this page as well.
#'
#' @param randstr, random string for the plot containers
#' @note \code{getMainPanel}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPanel()
#'
#' @export
#'
getMainPanel <- function(randstr = NULL) {
    if (is.null(randstr)) return (NULL)
    list(
        column( 6, wellPanel( plotlyOutput(
            "vplot1") ) ),
        column( 6, wellPanel( plotlyOutput(
            "vplot2"
        ) ) ),
        column( 6, wellPanel( plotlyOutput("vplot3") ) ),
        column( 6, wellPanel( plotlyOutput("vplot4") ) )) 
}

#' getMainPanelPlots
#'
#' Gathers the the plots to be used within the main panel.
#'
#' @param filt_data, filtered data
#' @param cols, selected columns
#' @param conds, seleced conditions
#' @param input, input from ui
#' @param compselect, selected comparison number
#' @param output, output
#' @return panel
#' @export
#'
#' @examples
#'     x <- getMainPanelPlots()
#'
getMainPanelPlots <- function(filt_data = NULL, 
    cols = NULL, conds = NULL,
    input = NULL, compselect = NULL, output = NULL) {
    if (is.null(filt_data) || 
        is.null(cols) || is.null(conds) ||
        is.null(input$padjtxt) || is.null(input$foldChangetxt)  )
            return(NULL)

    domains <- getDomains(filt_data)
    colors <- getColors(domains)
    randstr <- reactive({
        stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
    })
    filt_data_rest <- filt_data[ filt_data$Legend!="NS",]
    filt_data_NS <- filt_data[ filt_data$Legend=="NS",]
    datapoints <- as.integer(nrow(filt_data_NS) * input$backperc / 100)
    if (nrow(filt_data_NS) > datapoints)
       filt_data_rand <- filt_data_NS[sample(1:nrow(filt_data_NS), datapoints,
            replace=FALSE),]
    else
       filt_data_rand  <- filt_data_NS
    filt_data <- rbind(filt_data_rand, filt_data_rest)
    dat <-c()
    if (input$mainplot == "volcano") {
        filt_data[which(!is.na(filt_data$log2FoldChange)
                    & !is.na(filt_data$log10padj)
                    & !is.na(filt_data$Legend)),]
        dat <- filt_data
        dat$x <- filt_data$log2FoldChange
        dat$y <- filt_data$log10padj
        x <- "log2FoldChange"
        y <- "log10padj"
    } else if (input$mainplot == "scatter") {
        x <- paste0("Cond", 2*compselect - 1) 
        y <- paste0("Cond", 2*compselect) 
        dat <- filt_data
    } else if (input$mainplot == "maplot") {
        dat <- filt_data
        dat$x <- (filt_data$x + filt_data$y) / 2
        dat$y <- filt_data$x - filt_data$y
        x <- "A"
        y <- "M"
    }

    
    scatter_plot <- reactive({ 
        mainScatter(dat, x, y, domains, colors)
    })
    
    selectedPoint <- reactive({
        eventdata <- event_data("plotly_click", source = "source")
        if (is.null(eventdata)){
            eventdata <- event_data("plotly_hover", source = "source")
            validate(need(!is.null(eventdata), "Hover over the main plots to show "))
        }
        f <-dat
        datapoint <- as.numeric(eventdata$pointNumber)[1]
        return(datapoint+1)
    })

    getSelected  <- reactive({
        selected <- event_data("plotly_selected", source = "source")
        dat[(as.numeric(selected$pointNumber)[1] + 1),]
    })
    
    output$vplot1 <- renderPlotly({
        scatter_plot()
    })
    output$vplot2 <- renderPlotly({
        
    })
    output$vplot3 <- renderPlotly({
        
        # Get point number
        datapoint <- selectedPoint()
        
        f <- dat
        # Pick out the gene with this ID
        data <- f[datapoint, ]
        genename <- data$ID
        bardata <- as.data.frame(cbind(cols,
            t(data[, cols]), conds) )
        colnames(bardata) <- c("libs", "count", "conds")
        bardata$count <- as.numeric(as.character(bardata$count))
        data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
            bardata[bardata$conds == levels(bardata$conds)[2], ])    
        title3 <- paste(genename, " variation")
        plot_ly(data, x = ~libs, y = ~count, color = ~conds, colors=c("Red", "Blue"))%>% 
           layout(title = title3,
           xaxis = list(title = "Samples"),
           yaxis = list(title = "Read Count"))
    })
    output$vplot4 <- renderPlotly({
    })
    list( getSelected = isolate(getSelected), 
        randstr=isolate(randstr) )
}
