options(shiny.maxRequestSize = 800 * 1024^2,width = 160,max.print = 99999,
        shiny.reactlog = TRUE,dplyr.show_progress = TRUE,show.error.messages = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "EDA",#titleWidth = 250, 
                                            notificationUi("source")
                                            ),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target")),
                    body=dashboardBody(tabsetPanel(id = "dataSwitcher",selected = NULL,type = "tabs",
                                                   # tabPanel("None"),
                                                   tabPanel(title="RAW DATA", value = "source", shiny::uiOutput("rawOutput")),
                                                   tabPanel(title = "PRE-PROCESSED DATA", value = "conv",shiny::uiOutput("convOutput")),
                                                   tabPanel(title = "RANDOM FOREST", value = "rf",shiny::uiOutput("rfOutput"))
                                                   )
                                       )
                    )

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  tryCatch({
    datasourceInputServer("id"="source")
  },warning=function(w){
    notificationServer("source",w)
  })
  
  data <- datasourceInputServer(id = "source")
  data_cleaned <- shiny::reactive({
    if(!is.null(data())){data() %>% data_preprocessing()}
  })
  data <- datasourceInputServer(id = "source")
  output$rawOutput <- shiny::renderUI({
    shiny::req(data())
    if(!is.null(data())){
      shiny::tagList(
        dataVizUi("source"),
        catVarUi("source"),
        edaUi("source"),
        reportUi("source")
      )
    }

  })
  dataVizOutput("source",data)
  edaOutput("source",data)
  reportServer("source", "EDA")

  output$convOutput <- shiny::renderUI({
    shiny::req(data_cleaned())
    shiny::tagList(
      dataVizUi("conv"),
      catVarUi("conv"),
      edaUi("conv"),
      downloadUi("conv")
    )
  })
  dataVizOutput("conv",data_cleaned)
  edaOutput("conv",data_cleaned)
  downloadDBtable("conv", data_cleaned)

  shiny::observeEvent(input$dataSwitcher,{
    if(input$dataSwitcher=="source"){
      catVarServer("source",data)
    }else if(input$dataSwitcher=="conv"){
      catVarServer("conv",data_cleaned)
    }
  })

}

shinyApp(ui, server)
