options(shiny.maxRequestSize = 800 * 1024^2,shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(title = "GRANA CLASSIFIER TRAINING",
                    header=dashboardHeader( # dashboardHeader ####
                                            title = "GRANA CLASSIFIER TRAINING",titleWidth = 350,
                                            disable = FALSE,
                                            notificationUi("source")
                                            ),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),width = 350
                                             # shiny::uiOutput("rfe"),shiny::uiOutput("preprocess")
                                             ),
                    body=dashboardBody(shiny::uiOutput("dataOverview"), shiny::uiOutput("train"))
                    )

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  
  rawdata <- datasourceInputServer(id = "source")
  data <- shiny::reactive({
    if(!is.null(rawdata())){
      if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        rawdata() %>% data_preprocessing()
      }
    }
  })
  shiny::observeEvent(data(),{
    dataOverview <- dataVizUi("overview")
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
  
  # output$rfe <- shiny::renderUI({
  #   shiny::req(data())
  #   rfeUi("source")
  # })
  
  # output$preprocess <- shiny::renderUI({
  #   shiny::req(data())
  #   preProcessUi("source")
  # })

  fitted_model <- rfServer("source",data)
  
  output$train <- shiny::renderUI({
    shiny::req(fitted_model())
    if(inherits(fitted_model(),"train.formula")){
      shiny::tagList(
        trainingUi("source"),
        reportUi("source"),
        downloadUploadUi("source")
      )
    }
  })
  trainingOutput("source",fitted_model)
  downloadUploadServer("source")
  downloadServer("source",fitted_model)
  databaseInputServer("source")
  writeToDBServer("source", fitted_model)
  reportServer(id = "source",report_script = "RandomForest")
  
}

shinyApp(ui, server)
