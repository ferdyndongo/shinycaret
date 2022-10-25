options(shiny.reactlog = TRUE, shiny.maxRequestSize = 400 * 1024^2)
pkgload::load_all(".")

ui <- dashboardPage(title = "Model Test/Predict",
                    header=dashboardHeader( # dashboardHeader ####
                                            title = "MODEL TEST/PREDICT",titleWidth = 350,
                                            disable = FALSE,
                                            notificationUi("source")
                                            ),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source"),width = 350),
                    body=dashboardBody(shiny::uiOutput("model"), shiny::uiOutput("test")
                                       #dataVizUi("overview"), testUi("source"),catVarUi("source"),shiny::uiOutput("test")
                                       )
                    )

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  # import model or test sample ####
  dat <- datasourceInputServer(id = "source")
  shiny::observeEvent(dat(),{
    if(inherits(dat(),"train.formula")){
      model <- shiny::reactive(dat())
      output$model <- shiny::renderUI({
        shiny::req(dat())
        mdbInput("source")
        })
      data <- mdbServer("source")
      output$test <- shiny::renderUI({
        shiny::req(data(),model())
        shiny::tagList(
          dataVizUi("overview"),
          testUi("source"),
          catVarUi("source"),
          reportUi("source"),
        )
      })
      dataVizOutput("overview",data)
      pred <- testOutput("source", model, data)
      catVarServer("source",data)
      downloadServer(id = "source", data = pred)
      # downloadDBtable(id = "source",data = pred)
      reportServer(id = "source",report_script = "TestReport")
    }else if(inherits(dat(),"data.frame") & 
             apply(X = dat(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
      raw_index <- apply(X = dat(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
      model <- shiny::reactive(unserialize(dat()[[raw_index]][[1]]))
      output$model <- shiny::renderUI({
        shiny::req(dat())
        mdbInput("source")
        })
      data <- mdbServer("source")
      output$test <- shiny::renderUI({
        shiny::req(data(),model)
        shiny::tagList(
          dataVizUi("overview"),
          testUi("source"),
          catVarUi("source"),
          reportUi("source"),
        )
      })
      dataVizOutput("overview",data)
      pred <- testOutput("source", model, data)
      catVarServer("source",data)
      downloadServer(id = "source", data = pred)
      # downloadDBtable(id = "source",data = pred)
      reportServer(id = "source",report_script = "TestReport")
    }else{
      output$model <- shiny::renderUI({
        # shiny::req(dat())
        dataFileInput("source")
        })
      model <- dataFileServer("source")
      data <- shiny::reactive(dat())
      output$test <- shiny::renderUI({
        shiny::req(model())
        shiny::tagList(
          dataVizUi("overview"),
          testUi("source"),
          catVarUi("source"),
          reportUi("source"),
        )
      })
      dataVizOutput("overview",data)
      pred <- testOutput("source", model, data)
      catVarServer("source",data)
      downloadServer(id = "source", data = pred)
      # downloadDBtable(id = "source",data = pred)
      reportServer(id = "source",report_script = "TestReport")
    }
  })
  
}

shinyApp(ui, server)
