options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 800 * 1024^2)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "Principal Component Analysis", 
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target")),
                    body=dashboardBody(tabsetPanel(tabPanel(title = "data",value = "data",dataVizUi("source")),
                                                   tabPanel(title = "pca",value = "pca",pcaUi("source"), shiny::uiOutput("report"))
                                                   )
                                       )
                    )

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  data <- datasourceInputServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("source",data)
    output$target <- shiny::renderUI({
      catVarUi("source")
    })
    catVarServer("source",data)
  })
  
  # output$target <- shiny::renderUI({
  #   shiny::req(data())
  #   catVarUi("source")
  # })
  # shiny::observeEvent(data(),{
  #   shiny::freezeReactiveValue(input,"source-catVar")
  #   catVarServer("source",data)
  # })

  # output$plotUi <- shiny::renderUI({
  #   shiny::req(data())
  #   shiny::tagList(
  #     pcaUi(id = "source"),
  #     # RobustPCAUi(id="source"),
  #     # reportUi("source")
  #   )
  # })
  
  pca <- pcaServer("source",data)
  pcaOutput("source", data, pca)
  
  # RobustPCAServer("source",data)
  output$report <- shiny::renderUI({
    shiny::req(pca())
    reportUi("source")
  })
  reportServer("source","pcaReport")
  
}

shinyApp(ui, server)
