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
                    draggable = TRUE, plotlyOutput("qcplot1", height= input$plotheight, width=input$plotwidth) ),
                box(
                    collapsible = TRUE, title = "Plot2", status = "primary", solidHeader = TRUE, width = NULL,
                    draggable = TRUE,  plotlyOutput("qcplot2", height= input$plotheight, width=input$plotwidth) )) ) ) 
        ),
        conditionalPanel(condition = "(input.qcplot == 'all2all' || input.qcplot == 'heatmap')", box(
            collapsible = TRUE, title = "Plot1", status = "primary", solidHeader = TRUE, width = NULL,
            draggable = TRUE, column(12,  plotlyOutput("plotly_plot", height= input$plotheight, width=input$plotwidth))) )
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
        unitInputs <- reactive({ 
            m <- c(20,20,20,20)
            if(!is.null(input[["left"]]))
                m <- c(input[["left"]],
                       input[["bottom"]],
                       input[["top"]],
                       input[["right"]])
            m
        })
        
        dat <- dataset
        if (input$qcplot == "pca") {
            sc <- getShapeColor(input)
            pcaplot <- plot_pca(dat, input$pcselx, input$pcsely,
                metadata = metadata, color = sc$color,
                size = 5, shape = sc$shape,
                textonoff = sc$textonoff, 
                legendSelect = sc$legendSelect, input = input )
            qcPlots$plot1 <- pcaplot$plot1 +
               theme( plot.margin = unit(unitInputs(), "pt"))
            qcPlots$plot2 <- pcaplot$plot2 +
               theme( plot.margin = unit(unitInputs(), "pt"))
        } else if (input$qcplot == "IQR" || input$qcplot == "Density" ) {
            qcPlots <- prepAddQCPlots(dataset, unitInputs(), input)
        }
    }
    return(qcPlots)
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

#' prepAddQCPlots
#'
#' prepares IQR and density plots
#'
#' @param data, barplot data
#' @param unitInputs, unit Inputs
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     prepAddQCPlots()
#'
#'
prepAddQCPlots <- function(data=NULL, unitInputs = NULL, input=NULL){
    if(is.null(data)) return(NULL)
    qcplot <- c()
    if(!is.null(input$qcplot)){
        if (input$qcplot == "IQR"){
            qcplot$plot1 <- getIQRPlot(data, input, colnames(data), 
                "IQR Plot(Before Normalization)") 
               
            qcplot$plot2 <- getIQRPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "IQR Plot(After Normalization)")
        }
        else if (input$qcplot == "Density"){
            qcplot$plot1 <- getDensityPlot(data, colnames(data), 
                "Density Plot(Before Normalization)") +
                theme( plot.margin = unit(unitInputs, "pt"))
            qcplot$plot2 <- getDensityPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "Density Plot(After Normalization)")  +
                theme( plot.margin = unit(unitInputs, "pt"))
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


#' startQCPlots
#'
#' Gathers the QC plots to be used within the QC panel.
#'
#' @param df_select, given data
#' @param cols, selected columns
#' @param conds, seleced conditions
#' @param input, input from ui
#' @param output, output
#' @return panel
#' @export
#'
#' @export
#'
#' @examples
#'     startQCPlots()
#'
#'

startQCPlots <- function(df_select = NULL, 
    cols = NULL, conds = NULL,
    input = NULL, output = NULL)
{
    
    if (is.null(df_select)) return(NULL)

    qcplots <- reactive({
        qcp <- getQCReplot(cols, conds, 
             df_select, input)
        return(qcp)
    })

    output$qcplot1 <- renderPlotly({
        if (is.null(qcplots()$plot1)) return(plotly_empty(type = "scatter"))

        qcplots()$plot1
            
    })
    output$qcplot2 <- renderPlotly({
        if (is.null(qcplots()$plot2)) return(plotly_empty(type = "scatter"))

        qcplots()$plot2
    })
    

    output$plotly_plot <-renderPlotly({
        if (is.null(df_select)) return(plotly_empty(type = "scatter"))
        p <- c()
        if (input$qcplot == "heatmap")
            p <- runHeatmap(df_select, input, 2)
        else  if (input$qcplot == "all2all")
            p <- all2all(df_select, input)
        p
    })

    output$columnSelForQC <- renderUI({
        existing_cols <- input$samples
        if (!is.null(cols))
            existing_cols <- cols
        wellPanel(id = "tPanel",
                  style = "overflow-y:scroll; max-height: 200px",
                  checkboxGroupInput("col_list", "Select col to include:",
                                     existing_cols, 
                                     selected=existing_cols)
        )
    })
    
}
