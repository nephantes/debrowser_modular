#' debrowserdownload
#'
#' Module to download given processed data 
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix to be downloaded
#' @return density plot 
#' @export
#'
#' @examples
#'     x <- debrowserdownload(data = data)
#'
debrowserdownload <- function(input, output, session, data = NULL) {
    
    output$downloadData <- downloadHandler(filename = function() {
        "data.csv"
    }, content = function(filename) {
        if(!("ID" %in% names(data)))
            data <- addID(data)
        write.table(data, file, sep = ",", row.names = FALSE)
    })
}


#' downloadSectionUI
#'
#' Returns download button
#'
#' @note \code{downloadSectionUI}
#' @return returns the download button
#' @examples
#'     x <- downloadSectionUI()
#' @export
#'
downloadObjUI <- function(id, ...) {
    ns <- NS(id)
    
    downloadButton(n("data_download"), label = "Download Data")
}
