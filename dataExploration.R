options(shiny.maxRequestSize = 200 * 1024^2,shiny.reactlog = TRUE,width = 160)
pkgload::load_all(".")


ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "DATA EXPLORATION",#titleWidth = 250, 
                                            disable = FALSE,
                                            notificationUi("source")
                                            ),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target")),
                    body=dashboardBody(shiny::uiOutput("rawOutput"))
                    )

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  data <- datasourceInputServer(id = "source")
  
  output$rawOutput <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("raw"),
      catVarUi("raw"),
      edaUi("raw"),
      reportUi("raw")
    )
  })
  dataVizOutput("raw",data)
  edaOutput("raw",data)
  catVarServer("raw",data)
  reportServer("raw", "EDA")
  
}
shinyApp(ui, server)
