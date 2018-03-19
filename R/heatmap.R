#' debrowserheatmap
#'
#' Heatmap module to create interactive heatmaps and get selected list from
#' a heatmap
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return heatmapply plot
#'
#' @examples
#'     x <- heatmaply(mtcars)
#'
#' @export
#' @import heatmaply
#'
#'

debrowserheatmap <- function( input, output, session, data){

    output$heatmap <- renderPlotly({
        shinyjs::onevent("mousemove", "heatmap", js$getHoverName(session$ns("hoveredgenename")))
        shinyjs::onevent("click", "heatmap", js$getHoverName(session$ns("hoveredgenename1")))
        #shinyjs::onclick( "heatmap", js$getHoverName(session$ns("hoveredgenename1")))
        runHeatmap(input, data)
    })

    hselGenes <- reactive({
        if (is.null(input$selgenenames)) return("")
        unlist(strsplit(input$selgenenames, split=","))
    })
    shg <- reactive({
        if (is.null(input$hoveredgenename)) return("")
        js$getSelectedGenes(session$ns("heatmap"), session$ns("selgenenames"))
        input$hoveredgenename
    })
    shgClicked <- reactive({
        if (is.null(input$hoveredgenename1)) return("")
        input$hoveredgenename1
    })
    list( shg = (shg), shgClicked=(shgClicked), hselGenes=(hselGenes))
}

#' runHeatmap
#'
#' Creates a heatmap based on the user selected parameters within shiny.#'
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return heatmapply plot
#'
#' @examples
#'     x <- heatmaply(mtcars)
#'
#' @export
#' @import heatmaply
#'
#'
runHeatmap <- function(input, data){
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
    if (is.null(input$customColors1))    
        heatmapColors <- eval(parse(text=paste0(input$pal,
            '(',input$ncol,')')))
    else{
        if (!is.null(input$color1))
            heatmapColors <- colorRampPalette(c(input$color1, 
                input$color2, input$color3))(n = 1000)
        #heatmapColors <- colorRampPalette(c("red", "white", "blue"))(n = 1000)
    }
    p <- heatmaply(cld,
        main = input$main,
        xlab = input$xlab,
        ylab = input$ylab,
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
        showticklabels = c(input$labRow, input$labCol),
        k_col = input$k_Col, 
        k_row = input$k_Row
        ) %>% 
    plotly::layout(
        width=input$width, height=input$height,
        margin = list(l = input$left,
        b = input$bottom,
        t = input$top,
        r = input$right
        ))
    p$elementId <- NULL
    p
}


getHeatmapUI <- function(id) {
    ns <- NS(id)
    fluidRow(column(8,
        plotlyOutput(ns("heatmap"))
    ))
}

#' heatmapControlsUI
#'
#' Generates the left menu to be used for heatmap plots
#'
#' @note \code{heatmapControlsUI}
#' @param id, module ID
#' @return HeatmapControls
#' @examples
#'     x <- heatmapControlsUI(1)
#' @export
#'
heatmapControlsUI <- function(id) {
    ns <- NS(id)
    list(
        dendControlsUI(id, "Row"),
        dendControlsUI(id, "Col"),
        shinydashboard::menuItem("Heatmap Colors",
            conditionalPanel(paste0('!input.', r(ns('customColors'), "-")),
            palUI(id),
            sliderInput(ns("ncol"), "# of Colors", 
                min = 1, max = 256, value = 256)),
                customColorsUI(id)
        ),
        shinydashboard::menuItem("Heatmap Dendrogram",
            selectInput(ns('dendrogram'),'Type',
            choices = c("both", "row", "column", "none"),selected = 'both'),
            selectizeInput(ns("seriation"), "Seriation", 
            c(OLO="OLO", GW="GW", Mean="mean", None="none"),selected = 'OLO'),
            sliderInput(ns('branches_lwd'),'Branch Width',
            value = 0.6,min=0,max=5,step = 0.1)
        ),
        shinydashboard::menuItem("Heatmap Layout",
            textInput(ns('main'),'Title',''),
            textInput(ns('xlab'),'Sample label',''),
            sliderInput(ns('row_text_angle'),'Sample Text Angle',
                value = 0,min=0,max=180),
            textInput(ns('ylab'), 'Gene/Region label',''),
            sliderInput(ns('column_text_angle'),'Gene/Region Text Angle',
                value = 45,min=0,max=180)
    ))
}

#' dendControlsUI
#'
#' get distance metric parameters 
#'
#' @note \code{dendControlsUI}
#' @param id, module ID
#' @param dendtype, Row or Col
#' @return pals
#' @examples
#'     x <- dendControlsUI("heatmap")
#' @export
#'
dendControlsUI <- function(id, dendtype = "Row") {
    ns <- NS(id)
    shinydashboard::menuItem(paste0(dendtype, " dendrogram"),
        selectizeInput(ns(paste0("distFun_", dendtype)), "Dist. method", 
           distFunParamsUI(),
        selected = 'euclidean'),
        selectizeInput(ns(paste0("hclustFun_", dendtype)), "Clustering linkage",
           clustFunParamsUI(), 
        selected = 'complete'),
        sliderInput(ns(paste0("k_", dendtype)), "# of Clusters", 
            min = 1, max = 10, value = 2))
}

#' clustFunParamsUI
#'
#' get cluster function parameter control
#'
#' @note \code{clustFunParamsUI}
#' @return cluster params
#' @examples
#'     x <- clustFunParamsUI()
#' @export
#'
clustFunParamsUI <- function() {
    c(Complete= "complete",Single= "single",Average= "average",
      Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",
      Ward.D= "ward.D",Ward.D2= "ward.D2")
}

#' distFunParamsUI
#'
#' get distance metric parameters 
#'
#' @note \code{distFunParamsUI}
#' @return funParams
#' @examples
#'     x <- distFunParamsUI()
#' @export
#'
distFunParamsUI <- function() {
    c(Cor="cor", Euclidean="euclidean",Maximum='maximum',
      Manhattan='manhattan',Canberra='canberra',
      Binary='binary',Minkowski='minkowski')
}

#' palUI
#'
#' get pallete 
#'
#' @note \code{palUI}
#' @param id, module ID
#' @return pals
#' @examples
#'     x <- palUI("heatmap")
#' @export
#'
palUI <- function(id) {
    ns <- NS(id)
    colSel='RdBu'
    selectizeInput(inputId = ns("pal"), 
        label ="Select Color Palette",
        choices = c('RdBu' = 'RdBu',
        'BlueRed' = 'bluered',
        'RedBlue' = 'redblue',
        'RdYlBu' = 'RdYlBu',
        'RdYlGn' = 'RdYlGn',
        'BrBG' = 'BrBG',
        'Spectral' = 'Spectral',
        'BuGn' = 'BuGn',
        'PuBuGn' = 'PuBuGn',
        'YlOrRd' = 'YlOrRd',
        'Heat' = 'heat.colors',
        'Grey' = 'grey.colors'),
    selected=colSel)
}

#' customColorsUI
#'
#' get Custom Color controls
#'
#' @note \code{getColRng}
#' @param id, module ID
#' @return color range
#' @examples
#'     x <- customColorsUI()
#' @export
#'
customColorsUI <- function(id) {
    ns <- NS(id)
    list(
        checkboxInput(r(ns('customColors'), "-"), 'Custom Colors', value = FALSE),
        conditionalPanel(paste0('input.', r(ns('customColors'), "-")),
        colourpicker::colourInput(ns("color1"), "Choose min colour", "blue"),
        colourpicker::colourInput(ns("color2"), "Choose median colour", "white"),
        colourpicker::colourInput(ns("color3"), "Choose max colour", "red")))
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


#' heatmapJScode
#'
#' heatmap JS code for selection functionality
#'
#' @param id
#' @return JS Code
#' @export
#'
#' @examples
#'     x <- heatmapJScode()
#'
heatmapJScode <- function() {        
        'shinyjs.getHoverName = function(params){
            
           var defaultParams = {
               controlname : "hoveredgenename"
           };
           params = shinyjs.getParams(params, defaultParams);
           console.log(params.controlname)
           var out = ""
         
           if (typeof  document.getElementsByClassName("nums")[0] != "undefined"){
           if (typeof  document.getElementsByClassName("nums")[0].querySelectorAll("tspan.line")[0] != "undefined"){
                out = document.getElementsByClassName("nums")[0].querySelectorAll("tspan.line")[0].innerHTML.match("row: (.*)")[1]
                $("#heatmap-heatmap").attr("gname", out)
           }
           }
           Shiny.onInputChange(params.controlname, $("#heatmap-heatmap").attr("gname"));
        }

        shinyjs.getSelectedGenes = function(params){
            var defaultParams = {
               plotId : "heatmap",
               controlname : "selgenenames"
            };
            params = shinyjs.getParams(params, defaultParams);
            var count = document.getElementById(params.plotId).querySelectorAll("g.y2tick").length
            var start = 0
            var out = ""
            
            for (i = start; i < count; i++)
            {
                if (typeof document.getElementById(params.plotId).querySelectorAll("g.y2tick")[i] != "undefined"){
                  out += document.getElementById(params.plotId).querySelectorAll("g.y2tick")[i].innerHTML.match(">(.*)</text>")[1]  + ","
                }
            }
            Shiny.onInputChange(params.controlname, out);
        }
        '
}

#' getJSLine
#'
#' heatmap JS code for selection functionality
#'
#' @param id
#' @return JS Code
#' @export
#'
#' @examples
#'     x <- getJSLine()
#'
getJSLine <-function()
{        
  list(shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = heatmapJScode(), functions = c("getHoverName", "getSelectedGenes")))
}
