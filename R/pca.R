#' run_pca
#'
#' Runs PCA on the selected dataset.
#'
#' @param x, dataframe with experiment data
#' @param retx, specifies if the data should be returned
#' @param center, center the PCA (Boolean)
#' @param scale, scale the PCA (Boolean)
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda", 
#'         package="debrowser"))
#'     pca_data<-run_pca(getNormalizedMatrix(
#'         demodata[rowSums(demodata[,2:7])>10,2:7]))
#'
#' @export
#'
run_pca <- function(x=NULL, retx = TRUE,
                    center = TRUE, scale = TRUE) {
    if ( is.null(x) || ncol(x) < 2) return (NULL)
    x <- x[rowSums(x)>0, ]
    pca <- prcomp(t(x), retx = retx,
                  center = center, scale. = scale)
    variances <- pca$sdev ^ 2
    explained <- variances / sum(variances)
    
    return(list(PCs = pca$x, explained = explained, pca = pca))
}

#' plot_pca
#'
#' Plots the PCA results for the selected dataset.
#'
#' @param dat, data
#' @param pcx, x axis label
#' @param pcy, y axis label
#' @param metadata, additional data
#' @param color, color for plot
#' @param shape, shape for plot
#' @param size, size of the plot
#' @param textonoff, text on off
#' @param legendSelect, select legend
#' @param input, input param
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda",
#'             package="debrowser"))
#'     metadata<-cbind(colnames(demodata[,2:7]), 
#'             colnames(demodata[,2:7]),
#'             c(rep("Cond1",3), rep("Cond2",3)))
#'     colnames(metadata)<-c("samples", "color", "shape")
#'     
#'     a <- plot_pca(getNormalizedMatrix(
#'             demodata[rowSums(demodata[,2:7])>10,2:7]),
#'             metadata = metadata, color = "samples",
#'             size = 5, shape = "shape")
#'
#' @export
#'
plot_pca <- function(dat = NULL, pcx = 1, pcy = 2,
                     metadata = NULL, color = NULL, shape = NULL,
                     size = NULL, textonoff = "Off", legendSelect = "fill", input = NULL) {
    if ( is.null(dat) || ncol(dat) < 2) return(NULL)
    
    pca_data <- run_pca(dat)
    x <- pca_data$PCs
    explained <- pca_data$explained
    plot_data <- data.frame(x)
    # Prepare data frame to pass to ggplot
    if (!is.null(metadata)) {
        plot_data <- cbind(plot_data, metadata)
    } 
    xaxis <- paste0("PC", pcx)
    yaxis <- paste0("PC", pcy)
    p_data <- plot_data[,c(xaxis, yaxis, "samples", "color", "shape")]
    colnames(p_data) <- c("x", "y", "samples", "color", "shape")
    # Prepare axis labels
    xaxis <- sprintf("PC%d (%.2f%%)", pcx,
                     round(explained[pcx] * 100, 2))
    yaxis <- sprintf("PC%d (%.2f%%)", pcy,
                     round(explained[pcy] * 100, 2))
    
    plot1 <- ggplot(data=p_data, aes(x=x, y=y))
        
    
    if (legendSelect == "color") {
        plot1 <-  plot1 + geom_point(mapping=aes(shape=shape, color=color), size=3 )
    }else{
        plot1 <-  plot1 + geom_point(mapping=aes(shape=shape, color=shape), size=3 )
    }
    if (textonoff == "On")
        plot1 <- plot1 + geom_text(aes(label=samples), vjust = 0, nudge_y = 1)
    plot1 <- plot1 + theme(legend.title = element_blank())
    plot1 <- plot1 +  labs(x = xaxis, y = yaxis)
    plot1$elementId <- NULL
    
    pcaExp <- getPCAexplained(dat, pca_data, input)
    
    plot2 <- drawPCAExplained(pcaExp$plotdata)
   
    return (list(plot1 =  plot1, plot2 =  plot2, pcaset = pcaExp$pcaset))
}

#' getPCAexplained
#'
#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param datasetInput, selected data
#' @param pca_data, from user
#' @param input, input params
#' @return explained plot
#' @examples
#' load(system.file("extdata", "demo", "demodata.Rda", 
#' package="debrowser"))
#' input<-c()
#' input$qcplot<-"pca"
#' input$col_list<-colnames(demodata[,2:7])
#' x <- getPCAexplained(getNormalizedMatrix(demodata[,2:7]), 
#'     input)
#'
#' @export
#'
getPCAexplained <- function(datasetInput = NULL, 
                            pca_data = NULL, input = NULL) {
    if (is.null(datasetInput)) return(NULL)
    datexp <- NULL
    pcaset <- NULL
    
    datexp <- data.frame(cbind(unlist(lapply(
        c(1:length(pca_data$explained)), 
        function(x){paste0("PC", x)})), 
        round(pca_data$explained * 100, 2)))
    colnames(datexp) <- c("PCs", "explained")
    datexp$explained <- as.numeric( as.character(datexp$explained) )
    
    var <- pca_data$pca$sdev^2/sum(pca_data$pca$sdev^2)
    
    ## Select the genes for PCA, removing the least variable 
    
    dThresh.pctile <- 1 - as.numeric(input$pctile)     # distance threshold
    gList.dThresh <- c()
    
    d <- pca_data$pca$rotation[,c(input$pcselx)]
    dThresh<-quantile(d, dThresh.pctile)
    gList.dThresh <- names(which(d>dThresh))
    pcaset <-  datasetInput[gList.dThresh, ]
    return (list(plotdata =  datexp, pcaset = pcaset))
}

#' drawPCAExplained
#'
#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param explainedData, selected data
#' @return explained plot
#' @examples
#'     x <- drawPCAExplained()
#'
#' @export
#'
drawPCAExplained <- function(explainedData = NULL){
    p <- NULL
    if (is.null(explainedData)) return(NULL)
    p <- ggplot(data=explainedData, aes(x=PCs, y=explained)) +
        geom_bar(stat="identity", fill="steelblue") +
        theme_minimal()
    p$elementId <- NULL
    p
}

