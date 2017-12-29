#' runHeatmap
#'
#' Creates a heatmap based on the user selected parameters within shiny.
#'
#' @param data, a matrixthat includes expression values
#' @param input, input varsiables
#' @param num, the heatmap number 
#' @return heatmap.2 plot
#'
#' @examples
#'     x <- runHeatmap(mtcars)
#'
#' @export
#' @import gplots
#' @import RColorBrewer
#'
runHeatmap <- function(data = NULL, input, num = 1){
    if(is.null(data) || nrow(data)<3) return(plotly_empty(type = "scatter"))
    
    cld <- prepHeatData(data)
    
    hclustfun_row <- function(x, ...) hclust(x, method = input[[paste0("hclustFun_Row", num)]])
    hclustfun_col <- function(x, ...) hclust(x, method = input[[paste0("hclustFun_Col", num)]])
    distfun_row <- function(x, ...) {
        if (input[[paste0("distFun_Row",num)]] != "cor") {
            return(dist(x, method = input[[paste0("distFun_Row",num)]]))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    distfun_col <- function(x, ...) {
        if (input[[paste0("distFun_Col",num)]] != "cor") {
            return(dist(x, method = input[[paste0("distFun_Col",num)]]))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }

    if (!input$customColors1 && !input$customColors2)    
        heatmapColors <- eval(parse(text=paste0(input[[paste0("pal", num)]],
            '(',input[[paste0("ncol", num)]],')')))
    else{
        if (!is.null(input[[paste0("color1_", num)]]))
            heatmapColors <- colorRampPalette(c(input[[paste0("color1_",num)]], 
                input[[paste0("color2_",num)]], input[[paste0("color3_",num)]]))(n = 1000)
        #heatmapColors <- colorRampPalette(c("red", "white", "blue"))(n = 1000)
    }
    p <- heatmaply(cld,
        main = input[[paste0("main", num)]],
        xlab = input[[paste0("xlab", num)]],
        ylab = input[[paste0("ylab", num)]],
        row_text_angle = input[[paste0("row_text_angle", num)]],
        column_text_angle = input[[paste0("column_text_angle", num)]],
        dendrogram = input[[paste0("dendrogram", num)]],
        branches_lwd = input[[paste0("branches_lwd", num)]],
        seriate = input[[paste0("seriation", num)]],
        colors = heatmapColors,
        distfun_row =  distfun_row,
        hclustfun_row = hclustfun_row,
        distfun_col = distfun_col,
        hclustfun_col = hclustfun_col,
        showticklabels = c(input[[paste0("labRow", num)]], input[[paste0("labCol", num)]]),
        k_col = input[[paste0("k_Col", num)]], 
        k_row = input[[paste0("k_Row", num)]]) %>% 
    plotly::layout(margin = list(l = input[[paste0("left", num)]],
        b = input[[paste0("bottom", num)]],
        t = input[[paste0("top", num)]],
        r = input[[paste0("right", num)]]
        ))
    
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