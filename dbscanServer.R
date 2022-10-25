options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(title = "DBSCAN",
                    header=dashboardHeader( # dashboardHeader ####
                                            title = "DBSCAN",
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),
                                             shiny::uiOutput("rfe"),shiny::uiOutput("preprocess")),
                    body=dashboardBody(dataVizUi("overview"), 
                                       shiny::verbatimTextOutput("knnPrint"),shiny::plotOutput("knnPlot"),
                                       shiny::uiOutput("plotUi"), 
                                       shiny::verbatimTextOutput("dbscanPrint"),
                                       shiny::verbatimTextOutput("dbscanTable"),
                                       shiny::uiOutput("eps"), shiny::uiOutput("ellipseType"),
                                       shiny::plotOutput("dbscanPlot"),
                                       shiny::plotOutput("parcoord"), shiny::uiOutput("orderType"),
                                       
                                       shiny::uiOutput("download")
                    )
)

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  # import train sample ####
  data <- datasourceInputServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("overview",data)
  })
  
  output$target <- shiny::renderUI({
    shiny::req(data())
    catVarUi("source")
  })
  shiny::observeEvent(data(),{
    shiny::freezeReactiveValue(input,"source-catVar")
    catVarServer("source",data)
  })
  
  output$rfe <- shiny::renderUI({
    shiny::req(data(),input$`source-catVar`)
    rfeUi("source")
  })
  
  output$preprocess <- shiny::renderUI({
    shiny::req(data(),input$`source-catVar`)
    preProcessUi("source")
  })
  
  knn_fit <- knnServer("source", data)
  shiny::observeEvent(knn_fit(),{
    output$knnPrint <- shiny::renderPrint({knn_fit()})
  })
  output$knnPlot <- shiny::renderPlot({
    shiny::req(knn_fit())
    plot(knn_fit())
  })
  
  output$plotUi <- shiny::renderUI({
    shiny::req(knn_fit())
    shiny::tagList(
      pcaUi(id = "source"),
      RobustPCAUi(id="source"),
      shiny::fluidRow(
        box(dbscanInput("source"),width = 2),
        box(ellipseType("source"),width = 2)
      )
    )
  })
  
  pca <- pcaServer("source", shiny::reactive(knn_fit()$trainingData))
  shiny::observeEvent(pca(),{
    pcaOutput("source", knn_fit()$trainingData, pca)
  })
  
  robPCA <- RobustPCAServer("source", shiny::reactive(knn_fit()$trainingData))

  dbscan <- dbscanServer("source",knn_fit)
  shiny::observeEvent(dbscan(),{
    output$dbscanPrint <- shiny::renderPrint({dbscan()})
    output$dbscanTable <- shiny::renderPrint({table(cluster=dbscan()[["cluster"]],class=knn_fit()$trainingData[[input$`source-catVar`]])})
  })
  
 
  output$dbscanPlot <- shiny::renderPlot({
    shiny::req(input$`source-dim1`, input$`source-dim2`)
    if(input$`source-preprocess`=="TRUE"){
      if(length(colnames(numericDataset(knn_fit()$trainingData)))>3){
        factoextra::fviz_cluster(object = dbscan(),data = numericDataset(knn_fit()$trainingData) %>% scale(center = TRUE,scale = TRUE),
                                 stand = FALSE, ellipse.type = input$`source-ellipse`,
                                 axes = c(as.numeric(input$`source-dim1`),as.numeric(input$`source-dim2`)))
      }else{
        factoextra::fviz_cluster(object = dbscan(),data = numericDataset(knn_fit()$trainingData) %>% scale(center = TRUE,scale = TRUE),
                                 stand = FALSE, ellipse.type = input$`source-ellipse`,
                                 choose.vars = c(as.numeric(input$`source-dim1`),as.numeric(input$`source-dim2`)))
      }
    }else{
      if(length(colnames(numericDataset(knn_fit()$trainingData)))>3){
        factoextra::fviz_cluster(object = dbscan(),data = numericDataset(knn_fit()$trainingData),
                                 stand = FALSE, ellipse.type = input$`source-ellipse`,
                                 axes = c(as.numeric(input$`source-dim1`),as.numeric(input$`source-dim2`)))
      }else{
        factoextra::fviz_cluster(object = dbscan(),data = numericDataset(knn_fit()$trainingData),
                                 stand = FALSE, ellipse.type = input$`source-ellipse`,
                                 choose.vars = c(as.numeric(input$`source-dim1`),as.numeric(input$`source-dim2`)))
      }
      
    }
  }, res = 96)
  
  parcoord <- shiny::reactive({
    shiny::req(knn_fit()$trainingData, dbscan()$cluster)
    dataclust <- shiny::reactive(knn_fit()$trainingData %>% dplyr::bind_cols(cluster=factor(dbscan()$cluster)))
    if(!is.null(input$`source-order`)){
      if(any(dataclust()$cluster==0)){
        if(input$`source-order`==""){
          dataclust() %>% 
            GGally::ggparcoord(columns = numericIndex(dataclust()),
                               groupColumn = categoricIndex(dataclust()))
        }else{
          dataclust() %>% 
            GGally::ggparcoord(columns = numericIndex(dataclust()),
                               groupColumn = categoricIndex(dataclust()), 
                               order = input$`source-order`)
        }
      }else{
        if(input$`source-order`==""){
          dataclust() %>% GGally::ggparcoord(columns = numericIndex(dataclust()),
                                                        groupColumn = 1)
        }else{
          dataclust() %>% GGally::ggparcoord(columns = numericIndex(dataclust()),
                                                        groupColumn = 1,order = input$`source-order`)
        }
      }
    }
  })
  
  output$parcoord <- shiny::renderPlot({
    shiny::req(parcoord())
    parcoord() + geom_text(mapping = aes(x = parcoord()$data$variable,y = round(parcoord()$data$value,2),label=parcoord()$data$.ID))
  }, res = 96)
  output$orderType <- shiny::renderUI({
    shiny::req(dbscan())
    orderType("source")
    })
  
  output$download <- shiny::renderUI({ 
    shiny::req(knn_fit(),dbscan())
    shiny::tagList(
      reportUi("source"),
      downloadUi("source")
    )
  })

  reportServer(id = "source",report_script = "dbscanReport")
  downloadServer("source",list(dbscan=shiny::reactive(dbscan()),
                               trainingSample=shiny::reactive(knn_fit()$trainingData)))
  
}

shinyApp(ui, server)
