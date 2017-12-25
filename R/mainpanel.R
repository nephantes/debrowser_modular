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
    list(fluidRow(
         column(12,
         splitLayout(cellWidths = c("50%", "50%"),
                     box(
                         collapsible = TRUE, title = "Main Plot", status = "primary", solidHeader = TRUE, width = NULL,
                         draggable = T, plotlyOutput("vplot1") ),
                     box(
                         collapsible = TRUE, title = "Heatmap", status = "primary", solidHeader = TRUE, width = NULL,
                         draggable = T,  plotlyOutput("vplot2"), 
                         verbatimTextOutput("heatmap_hover"),
                         verbatimTextOutput("heatmap_selected") )) ),
         column(12,
         splitLayout(cellWidths = c("50%", "50%"),
                     box(
                         collapsible = TRUE, title = "Biological Variation", status = "primary", solidHeader = TRUE, width = NULL,
                         draggable = T,plotlyOutput("vplot3")) ,
                     box(
                         collapsible = TRUE, title = "Box Plot", status = "primary", solidHeader = TRUE, width = NULL,
                         draggable = T, plotlyOutput("vplot4") ) ) ) ) )
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
    filt_data$key <-filt_data$ID
    filt_data_rest <- filt_data[ filt_data$Legend!="NS",]
    filt_data_NS <- filt_data[ filt_data$Legend=="NS",]
    datapoints <- as.integer(nrow(filt_data_NS) * input$backperc / 100)
    if (nrow(filt_data_NS) > datapoints)
       filt_data_rand <- filt_data_NS[sample(1:nrow(filt_data_NS), datapoints,
            replace=FALSE),]
    else
       filt_data_rand  <- filt_data_NS
    plot_init_data <- rbind(filt_data_rand, filt_data_rest)
    plot_init_data$Legend  <- factor(plot_init_data$Legend  , levels = unique(plot_init_data$Legend))
    
    plot_data <- plot_init_data
    if (input$mainplot == "volcano") {
        plot_data <- plot_init_data[which(!is.na(plot_init_data$log2FoldChange)
                    & !is.na(plot_init_data$log10padj)
                    & !is.na(plot_init_data$Legend)),]
        plot_data$x <- plot_data$log2FoldChange
        plot_data$y <- plot_data$log10padj
        x <- "log2FC"
        y <- "log10padj"
    } else if (input$mainplot == "scatter") {
        x <-  paste0("Cond", 2*compselect)
        y <-  paste0("Cond", 2*compselect - 1) 
    } else if (input$mainplot == "maplot") {
        plot_data$x <- (plot_init_data$x + plot_init_data$y) / 2
        plot_data$y <- plot_init_data$x - plot_init_data$y
        x <- "A"
        y <- "M"
    }
    
    scatter_plot <- mainScatter(plot_data, x, y)
    
    selectedPoint <- reactive({
        key <- NULL
        if (shg() != "") {
            key <- shg()
        }else{
         eventdata <- event_data("plotly_click", source = "source")
         if (is.null(eventdata)){
             eventdata <- event_data("plotly_hover", source = "source")
             validate(need(!is.null(eventdata), "Hover over the main plots to show. 
             Click on a point to keep the plots. Double click to reset the plots."))
         }
         key <- as.vector(unlist(eventdata$key))
        }
        return(key)
    })
    getVariationData <- reactive({
        # Get point number
        key <- selectedPoint()
        # Pick out the gene with this ID
        vardata <- filt_data[key, ]
        bardata <- as.data.frame(cbind(key, cols,
            t(vardata[, cols]), conds) )
        colnames(bardata) <- c("genename", "libs", "count", "conds")
        bardata$count <- as.numeric(as.character(bardata$count))
        data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
                      bardata[bardata$conds == levels(bardata$conds)[2], ])
        data$conds  <- factor(data$conds  , levels = unique(data$conds))
        data
    })
    getSelected  <- reactive({
        keys <- NULL
        selected <- event_data("plotly_selected", source = "source")
        if (is.null(selected$key)) return (NULL)
        keys <- as.vector(unlist(selected$key))
        filt_data[keys,]
    })
    
    output$vplot1 <- renderPlotly({
        scatter_plot
    })

    output$vplot2 <- renderPlotly({
        dat <- getSelected()

        shinyjs::onevent("mousemove", "vplot2", js$getHoverName())
        shinyjs::onevent("click", "vplot2", js$getSelectedGenes())
        
        validate(need(dim(dat)[1]!=0, "Select an area in the main plot to draw the heatmap. 
                      Use either 'Box Select' or 'Lasso Select' options in 'Main Plot'!"))
       
        p <- runHeatmap(dat[,cols], title = paste("Dataset:", input$dataset),
                   clustering_method = input$clustering_method1,
                   distance_method = input$distance_method1)
        p
    })
    hselGenes <- reactive({
        if (is.null(input$selgenenames)) return("")
        unlist(strsplit(input$selgenenames, split=","))
    })
    
    shg <- reactive({
        if (is.null(input$hoveredgenename)) return("")
        input$hoveredgenename
    })
    output$vplot3 <- renderPlotly({
        vardata <- getVariationData()
        title <- paste(vardata$genename, " variation")
        p <- plot_ly(vardata, x = ~libs, y = ~count, 
           color=~conds, colors=getCondColors(), type = "bar") %>%
           layout(title = title,
           xaxis = list(title = "Samples",categoryorder = "array", 
               categoryarray = vardata$libs),
           yaxis = list(title = "Read Count"),
           margin = list(pad=10))
        p$elementId <- NULL
        p
    })
    output$vplot4 <- renderPlotly({
        vardata <- getVariationData()
        title <- paste(vardata$genename, " variation")
        p <- plot_ly(vardata, x = ~conds, y = ~count, 
            color=~conds, colors=getCondColors(),
            boxpoints = "all", type = "box") %>%
            layout(title = title,
               xaxis = list(title = "Conditions"),
               yaxis = list(title = "Read Count"))
        p$elementId <- NULL
        p
    })
    
    output$heatmap_hover <- renderPrint({
        shg()
    })
    output$heatmap_selected <- renderPrint({
        hselGenes()
    })
    selectedGenes <- reactive({
        ret <- c()
        if (!is.null(isolate(hselGenes())) && isolate(hselGenes()) != "") {
             keys <- isolate(hselGenes())
             ret <-  filt_data[keys,]
        }
        else{
            ret <- isolate(getSelected())
        }
       return(ret)
    })
    selected <- selectedGenes()
    list( getSelected = isolate(selectedGenes()) )
}
