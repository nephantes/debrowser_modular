#' debrowserbatcheffect
#'
#' Module to correct batch effect
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
#'     x <- debrowserbatcheffect()
#'
debrowserbatcheffect <- function(input, output, session, ldata) {
  
  batchdata <- reactiveValues(count=NULL, meta = NULL)
  
  observeEvent(input$submitBatchEffect, {
    if (is.null(ldata$count)) return (NULL)
    if (input$batchmethod == "Combat"){
       batchdata$count <- correctCombat(input, ldata$count, ldata$meta)
    }
    else{
       batchdata$count <- correctHarman(input, ldata$count, ldata$meta)
    }
    batchdata$meta <- ldata$meta
  })
  
  output$batchfields <- renderUI({
    if (!is.null(ldata$meta))
        list(selectGroupInfo( ldata$meta, input, session$ns("treatment"), "Treatment"),
        selectGroupInfo( ldata$meta, input, session$ns("batch"), "Batch"))
  })
  
  batcheffectdata <- reactive({
    ret <- NULL
    if(!is.null(batchdata$count)){
      ret <- batchdata
    }
    return(ret)
  })
  
  observe({
    getSampleDetails(output, "uploadSummary", "sampleDetails", ldata)
    getSampleDetails(output, "filteredSummary", "filteredDetails", batcheffectdata())
    getTableDetails(output, "loadedtable", session$ns("loadedtable"), ldata$count)
    callModule(debrowserIQRplot, "beforeCorrection",  ldata$count)
    if ( !is.null(batcheffectdata()$count ) && nrow(batcheffectdata()$count)>2 ){
       getTableDetails(output, "batcheffecttable",  session$ns("filteredtable"), data = batcheffectdata()$count)
       callModule(debrowserIQRplot, "afterCorrection",  batcheffectdata()$count)
    }
  })
  
  list(BatchEffect=batcheffectdata)
}


#' batchEffectUI
#' Creates a panel to coorect batch effect
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- batchEffectUI("batcheffect")
#'
#' @export
#'
batchEffectUI<- function (id) {
  ns <- NS(id)
  list(
    fluidRow(
      shinydashboard::box(title = "Batch Effect Correction",
        solidHeader = T, status = "info",  width = 12, 
        fluidRow(
          column(5,div(style = 'overflow: scroll',
                       tableOutput(ns("uploadSummary")),
                       DT::dataTableOutput(ns("sampleDetails"))),
                 uiOutput(ns("loadedtableModal"))
          ),
          column(2,
             shinydashboard::box(title = "Correction Methods",
                 solidHeader = T, status = "info",
                 width = 12, 
                 batchMethodRadio(id),
                 uiOutput(ns("batchfields")),
                 actionButton(ns("submitBatchEffect"), label = "Submit", styleclass = "primary")
           )
          ),
          column(5,div(style = 'overflow: scroll', 
                       
                       tableOutput(ns("filteredSummary")),
                       DT::dataTableOutput(ns("filteredDetails"))),
                 uiOutput(ns("filteredtableModal"))
                 
          )
        ),
        fluidRow(
            column(5,
                   getIQRPlotUI(ns("beforeCorrection"))),
            column(2, div()),
            column(5,
                   getIQRPlotUI(ns("afterCorrection")))
        )
      )
    ))
}

#' batchMethodRadio
#'
#' Radio buttons to sellect batch effect method
#'
#' @note \code{batchMethodRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- batchMethodRadio()
#'
#' @export
#'
batchMethodRadio <- function(id) {
  ns <- NS(id)
  radioButtons(inputId=ns("batchmethod"), 
               label="Correction method:",
               choices=c(Combat='Combat',
                         Harman='Harman'
               ),
               selected='Combat'
  )
}

#' Correct Batch Effect using Combat in sva package
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @return data
#' @export
#'
#' @examples
#'     x<-correctCombat ()
correctCombat <- function (input = NULL, idata = NULL, metadata = NULL) {
  if (is.null(idata) || input$batch == "None") return(NULL)
  batch <- metadata[, input$batch]
  treatment <- metadata[, input$treatment]
  columns <- colnames(idata)
  meta <- data.frame(cbind(columns, treatment, batch))
  datacor <- data.frame(idata[, columns])
  datacor[, columns] <- apply(datacor[, columns], 2,
                              function(x) as.integer(x))
  
  datacor[, columns] <- apply(datacor[, columns], 2,
                              function(x) return(x + runif(1, 0, 0.01)))
  
  modcombat = model.matrix(~1, data = meta)
  
  combat_blind = ComBat(dat=datacor, batch=batch)
  
  a <- cbind(idata[rownames(combat_blind), 2], combat_blind)
  
  a[, columns] <- apply(a[, columns], 2, function(x) ifelse(x<0, 0, x))
  colnames(a[, 1]) <- colnames(idata[, 1])
  a[,columns]
}

#' Correct Batch Effect using Harman
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @return data
#' @export
#'
#' @examples
#'     x<-correctHarman ()
correctHarman <- function (input = NULL, idata = NULL, metadata = NULL) {
  if (is.null(idata)) return(NULL)
  batch.info <- data.frame(metadata[, c(input$treatment, input$batch)])
  rownames(batch.info) <- rownames(metadata)
  colnames(batch.info) <- c("treatment", "batch") 
  
  harman.res <- harman(idata, expt= batch.info$treatment, batch= batch.info$batch, limit=0.95)
  harman.corrected <- reconstructData(harman.res)
  harman.corrected
}