#' deServer
#'
#' Sets up shinyServer to be able to run DEBrowser interactively.
#'
#' @note \code{deServer}
#' @param input, input params from UI
#' @param output, output params to UI
#' @param session, session variable
#' @return the panel for main plots;
#'
#' @examples
#'     deServer
#'
#' @export
#' @importFrom shiny actionButton actionLink addResourcePath column 
#'             conditionalPanel downloadButton downloadHandler 
#'             eventReactive fileInput fluidPage helpText isolate 
#'             mainPanel need numericInput observe observeEvent 
#'             outputOptions parseQueryString plotOutput radioButtons 
#'             reactive reactiveValues renderPlot renderUI runApp 
#'             selectInput shinyApp  shinyServer  shinyUI sidebarLayout 
#'             sidebarPanel sliderInput  stopApp  tabPanel tabsetPanel 
#'             textInput textOutput titlePanel uiOutput tags HTML
#'             h4 img icon updateTabsetPanel updateTextInput  validate 
#'             wellPanel checkboxInput br p checkboxGroupInput onRestore
#'             reactiveValuesToList renderText onBookmark onBookmarked 
#'             updateQueryString callModule enableBookmarking htmlOutput
#'             onRestored NS renderImage h6 selectizeInput div imageOutput
#'             fluidRow splitLayout verbatimTextOutput renderPrint fluidRow
#' @importFrom shinyjs show hide enable disable useShinyjs extendShinyjs
#'             js inlineCSS onclick
#' @importFrom DT datatable dataTableOutput renderDataTable formatStyle
#'             styleInterval formatRound
#' @importFrom ggplot2 aes aes_string geom_bar geom_point ggplot
#'             labs scale_x_discrete scale_y_discrete ylab geom_density
#'             autoplot theme_minimal theme element_blank geom_text
#' @importFrom gplots heatmap.2 redblue
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf colorRampPalette
#' @importFrom graphics barplot hist pairs par rect text plot
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var sd model.matrix
#'             p.adjust runif cov mahalanobis quantile density
#' @importFrom utils read.csv read.table write.table update.packages
#'             download.file
#' @importFrom DOSE enrichDO enrichMap gseaplot dotplot
#' @importMethodsFrom DOSE dotplot summary
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             exists sample subset head mappedkeys ncol nrow subset 
#'             keys mapIds
#' @importMethodsFrom GenomicRanges as.factor setdiff
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#'             as.matrix lapply rev "rownames<-"
#'             gsub
#' @importMethodsFrom S4Vectors eval grep grepl levels sapply t 
#' @importMethodsFrom SummarizedExperiment cbind order rbind
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' @importFrom stringi stri_rand_strings
#' @importFrom annotate geneSymbols
#' @importFrom reshape2 melt
#' @importFrom baySeq getLibsizes getLikelihoods getLikelihoods.NB
#'             getPriors getPriors.NB nbinomDensity
#' @importMethodsFrom baySeq "densityFunction<-" "libsizes<-"
#' @importFrom clusterProfiler compareCluster enrichKEGG enrichGO
#' @importFrom DESeq2 DESeq DESeqDataSetFromMatrix results
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList glmLRT
#'             exactTest estimateCommonDisp glmFit
#' @importFrom limma lmFit voom eBayes topTable
#' @importFrom sva ComBat
#' @importFrom GGally ggpairs wrap
#' @importFrom RCurl getURL
#' @importFrom colourpicker colourInput
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import V8
#' @import shinydashboard
#' @import shinyBS
#' @import plotly
#' @import heatmaply
#' @import googleAuthR
#' @import colourpicker
#' @import pathview

deServer <- function(input, output, session) {
    #library(debrowser)
    #library(googleAuthR)
    enableBookmarking("server")
    options(warn = -1)
    tryCatch(
    {
        if (!interactive()) {
            options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                    shiny.fullstacktrace = FALSE, shiny.trace=FALSE, 
                     shiny.autoreload=TRUE)
            debrowser::loadpack(debrowser)
        }
        
        options(googleAuthR.scopes.selected = 
                    c("https://www.googleapis.com/auth/userinfo.email",
                      "https://www.googleapis.com/auth/userinfo.profile"))
        options("googleAuthR.webapp.client_id" = 
                    "186441708690-n65idoo8t19ghi7ieopat6mlqkht9jts.apps.googleusercontent.com")
        options("googleAuthR.webapp.client_secret" = "ulK-sj8bhvduC9kLU4VQl5ih")
        
        access_token <- callModule(googleAuth, "initial_google_button")
        
        shinyjs::hide("dropdown-toggle")
        shinyjs::js$setButtonHref()
        shinyjs::js$hideDropdown()
        if(exists(".startdebrowser.called")){
            shinyjs::hide("logout")
        }

        # To hide the panels from 1 to 4 and only show Data Prep
        togglePanels(0, c(0), session)


        choicecounter <- reactiveValues(nc = 0, qc = 0, 
            lastselecteddataset = "")
        
       
        observeEvent(input$stopApp, {
            stopApp(returnValue = invisible())
        })
        output$programtitle <- renderUI({
            togglePanels(0, c(0), session)
            getProgramTitle(session)
        })
        output$mainpanel <- renderUI({
            getMainPanel()
        })
        output$qcpanel <- renderUI({
            getQCPanel(input)
        })
        output$gopanel <- renderUI({
            getGoPanel()
        })
        output$cutoffSelection <- renderUI({
            nc <- 1
            if (!is.null(choicecounter$nc)) nc <- choicecounter$nc
            getCutOffSelection(nc)
        })
        output$downloadSection <- renderUI({
            choices <- c("most-varied", "alldetected", "selected")
            defaultchoice <- NULL
            if (buttonValues$startDE)
                choices <- c("up+down", "up", "down",
                             "comparisons", "alldetected",
                             "most-varied", "category", "selected")

            getDownloadSection(TRUE, choices, defaultchoice)
        })
        output$preppanel <- renderUI({
            getDataPrepPanel(!is.null(init_data))
        })
        output$leftMenu  <- renderUI({
            getLeftMenu(input)
        })
        output$initialmenu <-renderUI({
            getInitialMenu(input, output, session)
        })
        output$loading <- renderUI({
            getLoadingMsg()
        })
        output$logo <- renderUI({
            getLogo()
        })
        output$startup <- renderUI({
            getStartupMsg()
        })
        output$afterload <- renderUI({
            getAfterLoadMsg()
        })
        output$mainmsgs <- renderUI({
            if (!is.null(condmsg$text))
                condmsg$text
        })
        buttonValues <- reactiveValues(goQCplots = FALSE, goDE = FALSE,
            startDE = FALSE, gotoanalysis = FALSE)
        output$dataready <- reactive({
            query <- parseQueryString(session$clientData$url_search)
            jsonobj<-query$jsonobject
            if (is.null(jsonobj))
                hide(id = "loading-debrowser", anim = TRUE, animType = "fade")  
            return(!is.null(Dataset()))
        })
        outputOptions(output, "dataready", 
                      suspendWhenHidden = FALSE)
        output$definished <- reactive({
            return(!is.null(filt_data()))
        })
        outputOptions(output, "definished", 
                      suspendWhenHidden = FALSE)
        
        observeEvent(input$gotoanalysis, {
            buttonValues$gotoanalysis <- TRUE
        })
        Dataset <- reactive({
            tmpDataset <- NULL
            query <- parseQueryString(session$clientData$url_search)
            jsonobj<-query$jsonobject
            if (buttonValues$gotoanalysis == TRUE || (!is.null(input$demo) && 
                input$demo == TRUE) || !is.null(jsonobj) ){
                tmpDataset <- load_data(input, session)
                tmpDataset[is.na(tmpDataset)] <- 0
                tmpDataset <-tmpDataset[rowSums(tmpDataset)>0, ] 
                if (!is.null(input$batchselect) && input$batchselect!="None")
                {
                    tmpDataset<-correctBatchEffect(tmpDataset, input)
                }
            }
            if (!is.null(jsonobj))
                hide(id = "loading-debrowser", anim = TRUE, animType = "fade")
            return(tmpDataset)
        })
        observeEvent(input$add_btn, {
            shinyjs::enable("startDE")
            buttonValues$startDE <- FALSE
            choicecounter$nc <- choicecounter$nc + 1
        })
        observeEvent(input$rm_btn, {
            buttonValues$startDE <- FALSE
            if (choicecounter$nc > 0) 
                choicecounter$nc <- choicecounter$nc - 1
            if (choicecounter$nc == 0) 
                shinyjs::disable("startDE")
        })
        observeEvent(input$goDE, {
            shinyjs::disable("startDE")
            hideObj(c("goQCplots", "goDE"))
            showObj(c("add_btn","rm_btn","startDE", "fittype"))
            query <- parseQueryString(session$clientData$url_search)
        })
        observeEvent(input$resetsamples, {
            buttonValues$startDE <- FALSE
            showObj(c("goQCplots", "goDE"))
            hideObj(c("add_btn","rm_btn","startDE"))
            choicecounter$nc <- 0
        })
        samples <- reactive({
            if (is.null(Dataset())) return(NULL)
            getSamples(colnames(Dataset()), index = 1)
        })
        output$restore_DE <- reactive({
            choicecounter$nc
        })
        outputOptions(output, 'restore_DE', suspendWhenHidden = FALSE)

        output$sampleSelector <- renderUI({
            if (is.null(samples())) return(NULL)
            if (is.null(input$samples))
                samp <- samples()
            else
                samp <- input$samples
            tmpSamples <- list(
                selectInput("samples",
                    label = "Samples",
                    choices = samp, multiple = TRUE,
                    selected = samp)
            )
            return(tmpSamples)
        })
        output$batchEffect <- renderUI({
            if(!is.null(input$file2)){
                selectBatchEffect(input)
            }
        })
        output$conditionSelector <- renderUI({
            selectConditions(Dataset(), choicecounter, input, loadingJSON$username)
        })
        dc <- reactive({
            dc <- NULL
            if (buttonValues$startDE == TRUE){
                dc <- prepDataContainer(Dataset(), choicecounter$nc, 
                     isolate(input))
            }
            dc
        })
        observeEvent(input$save_state, {
            shinyjs::hide("save_state")
            shinyjs::show("bookmark_special_name")
            shinyjs::show("name_bookmark")
        })
        observeEvent(input$startDE, {
            buttonValues$startDE <- TRUE
            buttonValues$goQCplots <- FALSE
            init_data <- NULL 
            togglePanels(1, c( 0, 1, 2, 3, 4), session)
            choicecounter$qc <- 0
        })
        observeEvent(input$goQCplots, {
            choicecounter$qc <- 1
            buttonValues$startDE <- FALSE
            buttonValues$goQCplots <- TRUE
            togglePanels(2, c( 0, 2, 4), session)
        })
        comparison <- reactive({
            compselect <- 1
            if (!is.null(input$compselect))
                compselect <- as.integer(input$compselect)
            if (!is.null(dc())){
                if (is.list(dc())){
                    if(length(dc())<compselect)
                        compselect <- 1
                    dc()[[compselect]]
                }
                else
                    dc()
            }
        })
        conds <- reactive({ comparison()$conds })
        cols <- reactive({ comparison()$cols })
        init_data <- reactive({ 
            if (!is.null(comparison()$init_data))
                comparison()$init_data 
            else
                qcdata()
        })
        filt_data <- reactive({
            if (!is.null(comparison()$init_data) &&
                !is.null(input$padjtxt) &&
                !is.null(input$foldChangetxt)){
                applyFilters(init_data(), isolate(cols()), 
                    isolate(conds()), input)
            }
        })
        output$user_name <- renderText({
            if(exists(".startdebrowser.called")){
                return("local")
            }
            loadingJSON$username
        })
        
        loadingJSON <- reactiveValues(username = NULL)
        observe({
            setFilterParams(session, input)
            loadingJSON$username = startBookmark(session, input, Dataset, choicecounter, buttonValues, access_token(), output)
            startPlots()
            startQCPlots(df_select(), cols(), conds(), input, output)
        })
        
        df_select <- reactive({
            dat <- getSelectedCols(Dataset(), datasetInput(), input)
            getNormalizedMatrix(dat, 
                input$norm_method)
        })
        
        condmsg <- reactiveValues(text = NULL)
        selected <- reactiveValues(data = NULL)
        startPlots <- reactive({
            if (is.null(filt_data())) return(NULL)
            compselect <- 1 
            if (!is.null(input$compselect) ) 
                compselect <- as.integer(input$compselect)
            if (!is.null(isolate(filt_data())) && !is.null(input$padjtxt) && 
                !is.null(input$foldChangetxt) && input$dataset != "selected") {
                condmsg$text <- getCondMsg(dc(), input$compselect,
                    cols(), conds())
                selected$data <- getMainPanelPlots(filt_data(), 
                    cols(), conds(), input, compselect, output)
            }
        })
        selectedData  <- reactive({
            selected$data$getSelected()
        })
        
        qcdata <- reactive({
            prepDataForQC(Dataset()[,input$samples], input)
        })
        
        datForTables <- reactive({
            dat <- getDataForTables(input, init_data(),
                filt_data(), selectedData(),
                getMostVaried(), mergedComp())
            return(dat)
        })
        goplots <- reactive({
            dat <- datForTables()
            getGOPlots(dat[[1]][, isolate(cols())], isolate(input))
        })
        inputGOstart <- reactive({
            if (input$startGO){
                isolate(goplots())
            }
        })
        observeEvent(input$startGO, {
            isolate(inputGOstart())
        })
        output$GOPlots1 <- renderPlot({
            if (!is.null(inputGOstart()$p) && input$startGO){
                return(inputGOstart()$p)
            }
        })
        output$KEGGPlot <- renderImage({
            validate(need(!is.null(input$gotable_rows_selected), 
            "Please select a category in the GO/KEGG table to be able 
             to see the pathway diagram"))

            org <- input$organism
            dat <- datForTables()
            genedata <- getEntrezIds(dat[[1]], org)
            i <- input$gotable_rows_selected
            pid <- inputGOstart()$table$ID[i]
            pv.out <- pathview::pathview(gene.data = genedata, 
                      pathway.id = pid,
                      species = substr(inputGOstart()$table$ID[i],0,3), 
                      out.suffix = "b.2layer", kegg.native = TRUE)
            unlink(paste0(pid,".png"))
            unlink(paste0(pid,".xml"))
            list(src = paste0(pid,".b.2layer.png"),
                 contentType = 'image/png')
        }, deleteFile = TRUE)
        
        
        getGOCatGenes <- reactive({
            if(is.null(input$gotable_rows_selected)) return (NULL)
            org <- input$organism
            dat <- tabledat()
            
            i <- input$gotable_rows_selected
            genedata <- getEntrezTable(inputGOstart()$enrich_p$geneID[i], 
                dat[[1]], org)

            #selected$data$getSelected <- reactiveVal(genedata)
            dat[[1]] <- genedata
            dat
        })
        output$GOGeneTable <- DT::renderDataTable({
            validate(need(!is.null(input$gotable_rows_selected), 
            "Please select a category in the GO/KEGG table to be able 
            to see the gene list"))
            dat <- getGOCatGenes()
            if (!is.null(dat)){
                DT::datatable(dat[[1]],
                   list(lengthMenu = list(c(10, 25, 50, 100),
                   c("10", "25", "50", "100")),
                   pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                   getTableStyle(input, dat[[2]], dat[[3]], buttonValues$startDE)
            }
        })
        
        output$getColumnsForTables <-  renderUI({
            if (is.null(table_col_names())) return (NULL)
            selected_list <- table_col_names()
            if (!is.null(input$table_col_list) 
                && all(input$table_col_list %in% colnames(tabledat()[[1]])))
                selected_list <- input$table_col_list
            colsForTable <- list(
                    checkboxGroupInput("table_col_list", "Select col to include:",
                    table_col_names(), 
                    selected=selected_list)
            )
            return(colsForTable)
        })
        table_col_names <- reactive({
            if (is.null(tabledat())) return (NULL)
            colnames(tabledat()[[1]])
        })
        tabledat <- reactive({
            dat <- datForTables()
            if (is.null(dat)) return (NULL)
            dat2 <- removeCols(c("ID", "x", "y","Legend", "Color"), dat[[1]])
            
            pcols <- c(names(dat2)[grep("^padj", names(dat2))], 
                       names(dat2)[grep("pvalue", names(dat2))])
            if (!is.null(pcols) && length(pcols) > 1)
                dat2[,  pcols] <- apply(dat2[,  pcols], 2,
                    function(x) format( as.numeric(x), scientific = TRUE, digits = 3 ))
            else
                dat2[,  pcols] <- format( as.numeric( dat2[,  pcols] ), 
                    scientific = TRUE, digits = 3 )
            rcols <- names(dat2)[!(names(dat2) %in% pcols)]
            dat2[,  rcols] <- apply(dat2[,  rcols], 2,
                function(x) round( as.numeric(x), digits = 2))  
            dat[[1]] <- dat2
            return(dat)
        })
        output$tables <- DT::renderDataTable({
            dat <- tabledat()
            if (is.null(dat) || is.null(table_col_names())
                || is.null(input$table_col_list) || length(input$table_col_list)<1) 
                return (NULL)
            if (!all(input$table_col_list %in% colnames(dat[[1]]), na.rm = FALSE)) 
                return(NULL)
            if (!dat[[2]] %in% input$table_col_list)
                dat[[2]]= ""
            if (!dat[[3]] %in% input$table_col_list)
                dat[[3]]= ""
            
            datDT <- DT::datatable(dat[[1]][, input$table_col_list],
                options = list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input, dat[[2]], dat[[3]], buttonValues$startDE)
            return(datDT)
        })
        getMostVaried <- reactive({
            getMostVariedList(data.frame(init_data()), 
                    c(input$samples), input)
        })
        output$gotable <- DT::renderDataTable({
            if (!is.null(inputGOstart()$table)){
                DT::datatable(inputGOstart()$table,
                    list(lengthMenu = list(c(10, 25, 50, 100),
                    c("10", "25", "50", "100")),
                    pageLength = 25, paging = TRUE, searching = TRUE))
            }
        })
        mergedComp <- reactive({
            dat <- applyFiltersToMergedComparison(
                getMergedComparison(Dataset(), dc(), 
            choicecounter$nc, input), choicecounter$nc, input)
            dat[dat$Legend == "Sig", ]
        })
        

        datasetInput <- function(addIdFlag = FALSE){
            tmpDat <- NULL
            if (choicecounter$qc == 0 ) {
                mergedCompDat <- NULL
                if (input$dataset == "comparisons"){
                    mergedCompDat <- mergedComp()
                }
                tmpDat <- getSelectedDatasetInput(filt_data(), 
                     selectedData(), getMostVaried(),
                     mergedCompDat, input)
            }
            else
                tmpDat <- getSelectedDatasetInput(init_data(), 
                     getSelected = selectedData(),
                     getMostVaried = getMostVaried(),
                     input = input)
            if(addIdFlag)
                tmpDat <- addID(tmpDat)
            choicecounter$lastselecteddataset = input$dataset
            return(tmpDat)
        }
        output$downloadData <- downloadHandler(filename = function() {
            paste(input$dataset, "csv", sep = ".")
        }, content = function(file) {
            dat <- datForTables()
            dat2 <- removeCols(c("x", "y","Legend", "Size"), dat[[1]])
            if(!("ID" %in% names(dat2)))
                dat2 <- addID(dat2)
            write.table(dat2, file, sep = ",", row.names = FALSE)
        })
    },
    err=function(errorCondition) {
        cat("in err handler")
        message(errorCondition)
    },
    warn=function(warningCondition) {
        cat("in warn handler")
        message(warningCondition)
    })
}
