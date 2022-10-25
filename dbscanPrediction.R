options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "DBSCAN TEST", 
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id="source"),shiny::uiOutput("preprocess"),shiny::uiOutput("model")),
                    body=dashboardBody(# DBSCAN Outlier Detection ####
                                       dataVizUi("overview"),
                                       shiny::uiOutput("dbscanUI")
                                       
                                       
                    )
)

server <- function(input, output, session){
  
  datatest <- datasourceInputServer(id = "source")
  shiny::observeEvent(datatest(),{
    dataVizOutput("overview",datatest)
  })
  
  output$preprocess <- shiny::renderUI({
    shiny::req(datatest())
    preProcessUi("source")
  })
  
  output$model <- shiny::renderUI({
    shiny::req(datatest(),input$`source-preprocess`)
    shiny::freezeReactiveValue(input,"upload")
    dataFileInput("source")
  })
  
  dbscanData <- dataFileServer("source")

  output$dbscanUI <- shiny::renderUI({
    shiny::req(dbscanData())
    dbscanUi("source")
  })
  datapred <- dbscanPrediction(id = "source",datatest,dbscanData)
  
  dbscanOutput(id = "source", dbscan = dbscanData()$dbscan, data = dbscanData()$trainingSample,
               predicted_scores = datapred$predicted_scores,dataclust_pred = datapred$dataclust_pred)
}

shinyApp(ui, server)
