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

#' selectGroupInfo
#'
#' Group info column selection. This can be used in batch effect
#' or coloring the groups in the plots.
#'
#' @param input, input values
#' @param selectname, name of the select box
#' @param label, label of the select box
#' @note \code{selectGroupInfo}
#' @examples
#'     x <- selectGroupInfo()
#' @export
#'
selectGroupInfo <- function(metadata = NULL, input = NULL,
                              selectname = "groupselect",
                              label = "Group info") {
    if (is.null(metadata)) return (NULL)
    lst.choices <- as.list(c("None", colnames(metadata)))
    selectInput(selectname, label = label,
                choices = lst.choices,
                selected = 1)
}


#' addID
#'
#' Adds an id to the data frame being used.
#'
#' @param data, loaded dataset
#' @return data
#' @export
#'
#' @examples
#'     x <- addID()
#'
addID <- function(data = NULL) {
    if (is.null(data)) return (NULL)
    dat1 <- data.frame(data)
    dat1 <- cbind(rownames(data), data)
    colnames(dat1) <- c("ID", colnames(data))
    dat1
}

#' getVariationData
#'
#' Adds an id to the data frame being used.
#'
#' @param inputdata, dataset 
#' @param cols, columns
#' @param conds, conditions
#' @param key, gene or region name
#' @return plotdata
#' @export
#'
#' @examples
#'     x <- getVariationData()
#'
getVariationData <- function(inputdata = NULL, 
    cols = NULL, conds = NULL, key = NULL) {
    # Pick out the gene with this ID
    vardata <- inputdata[key, ]
    bardata <- as.data.frame(cbind(key, cols,
         t(vardata[, cols]), conds) )
    colnames(bardata) <- c("genename", "libs", "count", "conds")
    bardata$count <- as.numeric(as.character(bardata$count))
    data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
                  bardata[bardata$conds == levels(bardata$conds)[2], ])
    data$conds  <- factor(data$conds, levels = unique(data$conds))
    data
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



