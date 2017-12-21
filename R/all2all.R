#' all2all
#'
#' Prepares all2all scatter plots for given datasets. 
#'
#' @param data, data that have the sample names in the header.
#' @param input, input
#' @return all2all scatter plots
#' @examples
#'     input <- c()
#'     input$cex <- 0.7
#'     plot<-all2all(mtcars, input)
#'
#' @export
#'
all2all <- function(data, input) {
    nr <- nrow(data)
    if (nr > 1000)
        nr <- 1000
    dat <-data.frame(log10(data[1:nr,] + 0.1))
    pm <- ggpairs(dat, colour = "Legend", type="scatter",
          lower = list(continuous = wrap("points", alpha = 0.3, size=input$cex)))
    
    return(pm)
}
