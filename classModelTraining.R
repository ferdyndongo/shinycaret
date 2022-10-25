options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(title = "MODEL TRAINING",
                    header=dashboardHeader( # dashboardHeader ####
                                            title = "TRAINING",
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target")),
                    body=dashboardBody(dataVizUi("overview"), shiny::uiOutput("models"), shiny::uiOutput("train")
                    )
)

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  
  data <- datasourceInputServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("overview",data)
  })
  
  output$target <- shiny::renderUI({
    shiny::req(data())
    catVarUi("source")
  })
  shiny::observeEvent(data(),input$catVar,{
    shiny::freezeReactiveValue(input,"source-catVar")
    catVarServer("source",data)
  })
  
  output$models <- shiny::renderUI({
    shiny::req(data(), input$`source-catVar`)
    classModelUi("source")
  })

  fitted_model <- classModelServer("source", data)
    
  
  output$train <- shiny::renderUI({
    shiny::req(fitted_model())
    shiny::tagList(
      trainingUi("source"),
      reportUi("source"),
      downloadUi("source")
    )
  })
  trainingOutput("source",fitted_model)
  reportServer(id = "source",report_script = "classReport")
  downloadServer("source",fitted_model)
  
}

shinyApp(ui, server)
