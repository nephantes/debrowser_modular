#' mainScatter
#'
#' Creates the main scatter plot to be displayed within the main
#' panel.
#'
#' @param data, dataframe that has log2FoldChange and log10padj values
#' @param x, the name of the x coordinate
#' @param y, the name of the y coordinate
#' @return volcano plot
#'
#' @examples
#'     x <- mainScatter()
#'
#' @export
#'
mainScatter <- function(data = NULL,
    x = NULL, y = NULL) {
    if ( is.null(data) ) return(NULL)
    
    p <- plot_ly(source = "source", data=data, x=~x, y=~y, key=~key,
                 color=~Legend, colors=getColors(getDomains(data)), 
                 type="scatter", mode = "markers",
       text=~paste("<b>", ID, "</b><br>",
       "<br>", "padj=", format.pval(padj, digits = 2), " ",
       "-log10padj=", round(log10padj, digits = 2),
       "<br>", "log2FC=", round(log2FoldChange, digits = 2), " ",
       "foldChange=", round(foldChange, digits = 2),
       "<br>", sep = " ")) %>%
    layout(xaxis = list(title = x),
    yaxis = list(title = y))
    p$elementId <- NULL

    return(p)
}
