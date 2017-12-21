#' getQCPanel
#'
#' Gathers the conditional panel for QC plots
#'
#' @param input, user input
#' @note \code{getQCSection}
#' @return the panel for QC plots
#'
#' @examples
#'     x <- getQCPanel()
#'
#' @export
#'
getQCPanel <- function(input = NULL) {
    qcPanel <- list(
        wellPanel( conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
        getHelpButton("method", 
        "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#quality-control-plots"))),
        conditionalPanel(condition = "input.qcplot == 'IQR' 
                         || input.qcplot == 'Density'
                         || input.qcplot == 'pca'",
         list(fluidRow(
             column(12,
                box(
                    collapsible = TRUE, title = "Plot1", status = "primary", solidHeader = TRUE, width = NULL,
                    draggable = T, plotlyOutput("qcplot1") ),
                box(
                    collapsible = TRUE, title = "Plot2", status = "primary", solidHeader = TRUE, width = NULL,
                    draggable = T,  plotlyOutput("qcplot2") )) ) ) 
        ),
        conditionalPanel(condition = "(input.qcplot == 'all2all')",
            column(12,  plotlyOutput("plotly_all2all", height= input$all2allheight, width=input$all2allwidth))),
        conditionalPanel(condition = "(input.qcplot == 'heatmap')",
            column(12,  plotlyOutput("plotly_heatmap", height= input$heatmapheight, width=input$heatmapwidth)))
       )
    return(qcPanel)
}

#' getQCPlots
#'
#' Gathers the plot data to be displayed within the
#' quality checks panel.
#'
#' @note \code{getQCPlots}
#' @param dataset, the dataset to use
#' @param input, user input
#' @param metadata, coupled samples and conditions
#' @return the panel for QC plots
#' @examples
#'     x <- getQCPlots()
#'
#' @export
#'
getQCPlots <- function(dataset = NULL, input = NULL,
    metadata = NULL) {
    if (is.null(dataset)) return(NULL)
    qcPlots <- NULL
    if (nrow(dataset) > 0) {
        
        dat <- dataset
        if (input$qcplot == "pca") {
            sc <- getShapeColor(input)
            pcaplot <- plot_pca(dat, input$pcselx, input$pcsely,
                metadata = metadata, color = sc$color,
                size = 5, shape = sc$shape,
                textonoff = sc$textonoff, 
                legendSelect = sc$legendSelect, input = input )
            qcPlots$plot1 <- pcaplot$plot1
            qcPlots$plot2 <- pcaplot$plot2
        } else if (input$qcplot == "IQR" || input$qcplot == "Density" ) {
            qcPlots <- prepAddQCPlots(dataset, input)
        }
    }
    return(qcPlots)
}

#' getShapeColor
#'
#' Generates the fill and shape selection boxes for PCA plots.
#' metadata file has to be loaded in this case
#'
#' @param input, input values
#' @return Color and shape from selection boxes or defaults
#' @examples
#'     x <- getShapeColor()
#' @export
#'
getShapeColor <- function(input = NULL) {
    if (is.null(input)) return (NULL)
    sc <-  c()
    if (!is.null(input$color_pca))
        sc$color <- input$color_pca
    if (!is.null(input$shape_pca))
        sc$shape <- input$shape_pca
    
    sc$textonoff = input$textonoff
    sc$legendSelect = input$legendSelect
    return(sc)
}

#' getQCReplot
#'
#' Prepares QCplots for comparisons and others
#' @note \code{getQCReplot}
#' @param cols, the dataset to use
#' @param conds, the dataset to use
#' @param datasetInput, the dataset to use
#' @param input, user input
#' @return the panel for QC plots
#' @examples
#'     x <- getQCReplot()
#'
#' @export
#'
getQCReplot <- function(cols = NULL, conds = NULL, 
    datasetInput = NULL, input = NULL){
    if (is.null(datasetInput)) return(NULL)
    metadata <- NULL

    samples <- c()
    color <- c()
    shape <- c()
    if (!is.null(cols) && !input$dataset == "comparisons"){
        new_cols <- cols[which(cols %in% input$col_list)]
        new_conds <- conds[which(cols %in% input$col_list)]
        dataset <- datasetInput[, new_cols]
        samples <- new_cols
        color  <- new_cols
        shape <- new_conds

    }else{
        dataset <- datasetInput[,c(input$col_list)]
        samples <- colnames(dataset)
        color  <- colnames(dataset)
        shape <- "Conds"
    }
    if (input$qcplot == "pca") {
    mdata <- readMetaData(input)
    if (!is.null(input$color_pca) && input$color_pca != "None")
        color <- as.character(mdata[samples, input$color_pca])
    if (!is.null(input$shape_pca) && input$shape_pca != "None")
        shape <- as.character(mdata[samples, input$shape_pca])
    
    metadata <- cbind(samples, color, shape)
    }
    if (nrow(dataset)<3) return(NULL)
        getQCPlots(dataset, input, metadata)
}

#' getIQRPlot
#'
#' Makes IQR boxplot plot
#'
#' @param data, count or normalized data
#' @param cols, columns
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getIQRPlot()
#'
getIQRPlot <- function(data=NULL, cols=NULL, title = ""){
    if (is.null(data)) return(NULL)
    data <- as.data.frame(data)
    data[, cols] <- apply(data[, cols], 2,
        function(x) log10(as.integer(x) + 1))
    
    data <- addID(data)
    mdata <- melt(as.data.frame(data[,c("ID", cols)]),"ID")
    colnames(mdata)<-c("ID", "samples", "logcount")
    ypos <- -5 * max(nchar(as.vector(mdata$samples)))
    
    p <- plot_ly(mdata, x = ~samples, y = ~logcount, 
                 color="steelblue", type = "box") %>%
        layout(title = title,
               xaxis = list(title = "samples"),
               yaxis = list(title = "logcount"))
    p$elementId <- NULL
    p
}

#' getDensityPlot
#'
#' Makes Density plots
#'
#' @param data, count or normalized data
#' @param cols, columns
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getDensityPlot()
#'
getDensityPlot <- function(data=NULL, cols=NULL, title = ""){
    if (is.null(data)) return(NULL)
    data <- as.data.frame(data)
    data[, cols] <- apply(data[, cols], 2,
          function(x) log10(as.integer(x) + 1))
    
    data <- addID(data)
    mdata <- melt(as.data.frame(data[,c("ID", cols)]),"ID")
    colnames(mdata)<-c("ID", "samples", "density")
    
    p <- ggplot(data=mdata, aes(x=density)) +
        geom_density(aes(fill = samples), alpha = 0.5) +
        labs(x = "logcount", y = "man/getDensityPlot.Rd") +
        theme_minimal()
    p$elementId <- NULL
    p
}

#' prepAddQCPlots
#'
#' prepares IQR and density plots
#'
#' @param data, barplot data
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     prepAddQCPlots()
#'
#'
prepAddQCPlots <- function(data=NULL, input=NULL){
    if(is.null(data)) return(NULL)
    qcplot <- c()
    if(!is.null(input$qcplot)){
        if (input$qcplot == "IQR"){
            qcplot$plot1 <- getIQRPlot(data, colnames(data), 
                "IQR Plot(Before Normalization)")
            qcplot$plot2 <- getIQRPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "IQR Plot(After Normalization)")
        }
        else if (input$qcplot == "Density"){
            qcplot$plot1 <- getDensityPlot(data, colnames(data), 
                "Density Plot(Before Normalization)") 
            qcplot$plot2 <- getDensityPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "Density Plot(After Normalization)")
        }
    }
    return(qcplot)
}

#' getSelectedCols
#'
#' gets selected columns
#'
#' @param data, all loaded data
#' @param datasetInput, selected dataset
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     getSelectedCols()
#'
#'
getSelectedCols <- function(data = NULL, datasetInput = NULL, input=NULL){
    if(is.null(data) || is.null(datasetInput)) return(NULL)
    selCols <- NULL
    if (!is.null(input$dataset)){
        all <- input$samples
        selection <- input$col_list
        if("All" %in% input$col_list || length(input$col_list) == 0){
            selection <- all
        }else{
            selection <- input$col_list
        }
        if (!is.null(selection))
            selCols <- data[rownames(datasetInput), selection]
    }
    return(selCols)
}