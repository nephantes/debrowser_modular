#' getSampleDetails
#' 
#' get sample details
#'
#' @param output, output
#' @param summary, summary output name
#' @param details, details ouput name
#' @param data, data 
#' @return panel
#' @examples
#'     x <- getSampleDetails(output, data)
#'
#' @export
#'
getSampleDetails<- function (output, summary, details, data) {
    if (is.null(data$count)) return(NULL)
    
    output[[summary]]<- renderTable({ 
        countdata <-  data$count
        samplenums <- length(colnames(countdata))
        rownums <- dim(countdata)[1]
        result <- rbind(samplenums, rownums)
        rownames(result) <- c("# of samples", "# of rows (genes/regions)")
        colnames(result) <- "Value"
        result
    },digits=0, rownames = TRUE, align="lc")
    
    output[[details]] <- DT::renderDataTable({ 
        dat <- colSums(data$count)
        dat <- cbind(names(dat), dat)
        dat[, c("dat")] <-  format(
            round( as.numeric( dat[,  c("dat")], digits = 2)),
            big.mark=",",scientific=FALSE)
        
        if (!is.null(data$meta)){
            met <- data$meta
            dat <- cbind(met, dat[,"dat"])
            rownames(dat) <- NULL
            colnames(dat)[ncol(dat)] <- "read counts"
        }else{
            rownames(dat) <- NULL
            colnames(dat) <- c("samples", "read counts")
        }
        dat
    })
}


#' push
#'
#' Push an object to the list.
#'
#' @param l, that are going to push to the list
#' @param ..., list object
#' @return combined list
#'
#' @export
#'
#' @examples
#'     mylist <- list()
#'     newlist <- push ( 1, mylist )
push <- function(l, ...) c(l, list(...))

#' round_vals
#'
#' Plot PCA results.
#'
#' @param l, the value
#' @return round value
#' @export
#'
#' @examples
#'     x<-round_vals(5.1323223)
round_vals <- function(l) {
    l <- round(as.numeric(l), digits = 2)
    parse(text = l)
}



