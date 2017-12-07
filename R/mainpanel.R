#' getMainPanel
#'
#' main panel for volcano, scatter and maplot.  
#' Barplot and box plots are in this page as well.
#'
#' @note \code{getMainPanel}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPanel()
#'
#' @export
#'
getMainPanel <- function() {
    list(fluidRow(splitLayout(cellWidths = c("50%", "50%"),
            absolutePanel( draggable = T, resizable = T, plotlyOutput("vplot1") ),
            absolutePanel( draggable = T, resizable = T, plotlyOutput("vplot2") ) ),
            splitLayout(cellWidths = c("50%", "50%"),
            absolutePanel( draggable = T, resizable = T, plotlyOutput("vplot3") ),
            absolutePanel( draggable = T, resizable = T, plotlyOutput("vplot4") ) ) ))
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
    filt_data_rest <- filt_data[ filt_data$Legend!="NS",]
    filt_data_NS <- filt_data[ filt_data$Legend=="NS",]
    datapoints <- as.integer(nrow(filt_data_NS) * input$backperc / 100)
    if (nrow(filt_data_NS) > datapoints)
       filt_data_rand <- filt_data_NS[sample(1:nrow(filt_data_NS), datapoints,
            replace=FALSE),]
    else
       filt_data_rand  <- filt_data_NS
    plot_init_data <- rbind(filt_data_rand, filt_data_rest)
    plot_init_data$Legend  <- factor(plot_init_data$Legend  , levels = c("NS", "Up", "Down" ))
    
    plot_data <-c()
    if (input$mainplot == "volcano") {
        plot_init_data[which(!is.na(plot_init_data$log2FoldChange)
                    & !is.na(plot_init_data$log10padj)
                    & !is.na(plot_init_data$Legend)),]
        plot_data <- plot_init_data
        plot_data$x <- plot_init_data$log2FoldChange
        plot_data$y <- plot_init_data$log10padj
        x <- "log2FC"
        y <- "log10padj"
    } else if (input$mainplot == "scatter") {
        x <- paste0("Cond", 2*compselect - 1) 
        y <- paste0("Cond", 2*compselect) 
        plot_data <- plot_init_data
    } else if (input$mainplot == "maplot") {
        plot_data <- plot_init_data
        plot_data$x <- (plot_init_data$x + plot_init_data$y) / 2
        plot_data$y <- plot_init_data$x - plot_init_data$y
        x <- "A"
        y <- "M"
    }
    plot_data$key <-plot_data$ID
    scatter_plot <- mainScatter(plot_data, x, y)
    
    selectedPoint <- reactive({
         eventdata <- event_data("plotly_click", source = "source")
         if (is.null(eventdata)){
             eventdata <- event_data("plotly_hover", source = "source")
             validate(need(!is.null(eventdata), "Hover over the main plots to show "))
         }
         key <- eventdata$key
         return(key)
    })
    getVariationData <- reactive({
        # Get point number
        key <- selectedPoint()
        # Pick out the gene with this ID
        vardata <- plot_data[key, ]
        bardata <- as.data.frame(cbind(key, cols,
            t(vardata[, cols]), conds) )
        colnames(bardata) <- c("genename", "libs", "count", "conds")
        bardata$count <- as.numeric(as.character(bardata$count))
        data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
                      bardata[bardata$conds == levels(bardata$conds)[2], ])
    })
    getSelected  <- reactive({
        selected <- event_data("plotly_selected", source = "source")
        plot_data[selected$key,]
    })
    
    output$vplot1 <- renderPlotly({
        scatter_plot
    })
    output$vplot2 <- renderPlotly({
        
    })
    output$vplot3 <- renderPlotly({
        vardata <- getVariationData()
        title <- paste(vardata$genename, " variation")
        p <- plot_ly(vardata, x = ~libs, y = ~count, type = "bar",
            colors=~c("Red", "Blue"))%>%
           layout(title = title,
           xaxis = list(title = "Samples"),
           yaxis = list(title = "Read Count"),
           margin = list(pad=10))
        p$elementId <- NULL
        p
    })
    output$vplot4 <- renderPlotly({
        data <- getVariationData()
        title <- paste(data$ID, " variation")
        p <- plot_ly(data, x = ~conds, y = ~count, 
            boxpoints = "all", type = "box")%>%
            layout(title = title,
                   xaxis = list(title = "Conditions"),
                   yaxis = list(title = "Read Count"))
        p$elementId <- NULL
        p
    })
    list( getSelected = isolate(getSelected) )
}
