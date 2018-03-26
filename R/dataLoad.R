#' debrowserdataload
#'
#' Module to load count data and metadata
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserdataload()
#'
debrowserdataload <- function(input, output, session) {
    loadeddata <- reactiveValues( counttable = "", metadatatable = "")
    dataUpload <- eventReactive(input$dataSubmit, {
        if (is.null(input$countdata)) return (NULL)
        loadeddata$counttable <-as.data.frame(
            try(
                read.delim(input$countdata$datapath, 
                header=T, sep=input$countdataSep, 
            row.names=1 ), T))
        loadeddata$metadatatable <- c()
        if (!is.null(input$metadata$datapath)){
        loadeddata$metadatatable <- as.data.frame(
            try(
                read.delim(input$metadata$datapath, 
                header=T, sep=input$metadataSep), T))
        }
        if (is.null(loadeddata$counttable)) 
            {stop("Please upload the count file!")}
        list(count=loadeddata$counttable, meta=loadeddata$metadatatable)
    })

    output$uploadSummary <- renderTable({ 
    if(input$dataSubmit)
      isolate({
        countdata <- dataUpload()$count
        samplenums <- length(colnames(countdata))
        rownums <- dim(countdata)[1]
        result <- rbind(samplenums, rownums)
        rownames(result) <- c("# of samples", "# of rows (genes/regions)")
        colnames(result) <- "Value"
        result
      })
  },digits=0, rownames = TRUE, align="lc")

  output$sampleGroup <- DT::renderDataTable({ 
    if(input$dataSubmit)
      isolate({
        dat <- colSums(dataUpload()$count)
        dat <- cbind(names(dat), dat)
        dat[, c("dat")] <-  format(
          round( as.numeric( dat[,  c("dat")], digits = 2)),
          big.mark=",",scientific=FALSE)

        if (!is.null(dataUpload()$meta)){
            met <- dataUpload()$meta
            dat <- cbind(met, dat[,"dat"])
            rownames(dat) <- NULL
            colnames(dat)[ncol(dat)] <- "read counts"
        }else{
            rownames(dat) <- NULL
            colnames(dat) <- c("samples", "read counts")
        }
        dat
      })
  })
  loadeddata
}


#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param explainedData, selected data
#' @return explained plot
#' @examples
#'     x <- dataLoadUI()
#'
#' @export
#'
dataLoadUI<- function (id) {
  ns <- NS(id)
  list(fluidRow(
             fileUploadBox(id, "countdata", "Count Data"),
             fileUploadBox(id, "metadata", "Metadata")
        ),
        actionButton(ns("dataSubmit"), label = "Upload"),
  fluidRow(
    shinydashboard::box(title = "Upload Summary",
        solidHeader = T, status = "info",
        width = 12, 
        fluidRow(
          column(12, 
              tableOutput(ns("uploadSummary"))
          )),
        fluidRow(
          column(12,div(style = 'overflow: scroll', 
              DT::dataTableOutput(ns("sampleGroup")))
          )
        )
    )
  ))
}

#' fileUploadBox
#'
#' File upload module
#'
#' @note \code{fileUploadBox}
#' @return radio control
#'
#' @examples
#'    
#'     x <- fileUploadBox("meta", "metadata", "Metadata")
#'
#' @export
#'
fileUploadBox <- function(id = NULL, inputId = NULL, label = NULL) {
  ns <- NS(id)
  
  shinydashboard::box(title = paste0(label, " File"),
                      solidHeader = T, status = "info",
                      width = 6,
                      helpText(paste0("Upload your '", label," File'")),
                      fileInput(inputId=ns(inputId), 
                                label=NULL, 
                                accept=fileTypes()
                      ),
                      sepRadio(id, paste0(inputId, "Sep")))
}

#' sepRadio
#'
#' Radio button for separators
#'
#' @note \code{sepRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- sepRadio("meta", "metadata")
#'
#' @export
#'
sepRadio <- function(id, name) {
  ns <- NS(id)
  radioButtons(inputId=ns(name), 
               label="Separator",
               choices=c(Comma=',',
                         Semicolon=';',
                         Tab='\t'
               ),
               selected='\t'
  )
}

#' fileTypes
#'
#' Returns fileTypes that are going to be used in creating fileUpload UI
#'
#' @note \code{fileTypes}
#' @return file types
#'
#' @examples
#'     x <- fileTypes()
#'
#' @export
#'
fileTypes <- function() {
  c('text/tab-separated-values',
    'text/csv',
    'text/comma-separated-values',
    'text/tab-separated-values',
    '.txt',
    '.csv',
    '.tsv')
}