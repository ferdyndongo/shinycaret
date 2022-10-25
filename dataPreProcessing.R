options(shiny.maxRequestSize = 800 * 1024^2,width = 160,max.print = 99999,shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "DATA PRE-PROCESSING", titleWidth = 280,
                                            disable = FALSE,
                                            notificationUi("source")
                                            ),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),width = 280),
                    body=dashboardBody(shiny::uiOutput("dataViz"))
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
  
  output$dataViz <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
      catVarUi("source"),
      edaUi("source"),
      downloadUploadUi("source")
    )
  })
  dataVizOutput("source",data)
  catVarServer("source",data)
  edaOutput("source",data)
  downloadUploadServer("source")
  downloadServer("source",data)
  # downloadDBtable("source",data)
  databaseInputServer("source")
  writeToDBServer("source",data)
}
shinyApp(ui, server)