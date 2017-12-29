#' runHeatmap
#'
#' Creates a heatmap based on the user selected parameters within shiny.
#'
#' @param data, a matrixthat includes expression values
#' @param  input, input varsiables
#' @return heatmap.2 plot
#'
#' @examples
#'     x <- runHeatmap(mtcars)
#'
#' @export
#' @import gplots
#' @import RColorBrewer
#'
runHeatmap <- function(data = NULL, input){
    if(is.null(data) || nrow(data)<3) return(plotly_empty(type = "scatter"))
    
    cld <- prepHeatData(data)
    
    hclustfun_row <- function(x, ...) hclust(x, method = input$hclustFun_Row)
    hclustfun_col <- function(x, ...) hclust(x, method = input$hclustFun_Col)
    distfun_row <- function(x, ...) {
        if (input$distFun_Row != "cor") {
            return(dist(x, method = input$distFun_Row))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    distfun_col <- function(x, ...) {
        if (input$distFun_Col != "cor") {
            return(dist(x, method = input$distFun_Col))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }

    labRow = input$labRow
    if (labRow == TRUE && nrow(data) > 50)
        labRow = FALSE
    
    if (!input$customColors1 && !input$customColors2)    
        heatmapColors <- eval(parse(text=paste0(input$pal,'(',input$ncol,')')))
    else{
        if (!is.null(input$color1_1))
            heatmapColors <- colorRampPalette(c(input$color1_1, input$color2_1, input$color3_1))(n = 1000)
        #heatmapColors <- colorRampPalette(c("red", "white", "blue"))(n = 1000)
    }
    p <- heatmaply(cld,
        main = input$main,xlab = input$xlab,ylab = input$ylab,
        row_text_angle = input$row_text_angle,
        column_text_angle = input$column_text_angle,
        dendrogram = input$dendrogram,
        branches_lwd = input$branches_lwd,
        seriate = input$seriation,
        colors = heatmapColors,
        distfun_row =  distfun_row,
        hclustfun_row = hclustfun_row,
        distfun_col = distfun_col,
        hclustfun_col = hclustfun_col,
        showticklabels = c(labRow, input$labCol),
        k_col = input$k_Col, 
        k_row = input$k_Row) %>% 
    plotly::layout(margin = list(l = input$left, b = input$bottom))
    
    p$elementId <- NULL
    p
}
#' prepHeatData
#'
#' scales the data
#'
#' @param data, a matrixthat includes expression values
#' @return heatdata
#'
#' @examples
#'     x <- prepHeatData(mtcars)
#'
#' @export
#'
prepHeatData <- function(data) 
{
    if(is.null(data)) return(NULL)
    if (nrow(data)>5000)
        data <- data[1:5000, ]
    ld <- log2(data + 0.1)
    cldt <- scale(t(ld), center = TRUE, scale = TRUE)
    cld <- t(cldt)
    return(cld)
}

#' getIntHeatmap
#'
#' getIntHeatmap
#'
#' @param data, heatData
#' @param input, all input params
#' @return plot
#' @export
#'
#' @examples
#'     getIntHeatmap()
#'
getIntHeatmap <- function(data = NULL,  input = NULL) {
    if(is.null(data)) return(NULL)
    runHeatmap(data, input)
}

#' getSelHeat
#'
#' heatmap selection functionality
#'
#' @param data, selected genes
#' @param input, input params
#' @return plot
#' @export
#'
#' @examples
#'     x <- getSelHeat()
#'
getSelHeat <- function(data=NULL, input = NULL) {
    if (is.null(input)) return(NULL)
    getSelected <- reactive({
        selectedData <- data[unlist(strsplit(input, ",")), ]
    })
    list( getSelected = isolate(getSelected) )
}