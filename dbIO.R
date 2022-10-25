options(shiny.maxRequestSize = 800 * 1024^2,shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "Data Collection System", titleWidth = 250,
                                            disable = FALSE,
                                            notificationUi("source")
                                            ),
sidebar=dashboardSidebar(datasourceInputUi(id = "source"),width = 250),
body=dashboardBody(shiny::uiOutput("IO")),
)

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  data <- datasourceInputServer("source")
  
  output$IO <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
      downloadUploadUi("source")
    )
  })
  dataVizOutput(id = "source", data)
  downloadUploadServer("source")
  # downloadDBtable("source", data)
  downloadServer("source",data)
  databaseInputServer("source")
  writeToDBServer("source",data)
}

shinyApp(ui, server)