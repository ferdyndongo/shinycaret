options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                            title = "GEA Dashboard",
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source")),
                    body=dashboardBody(
                      tabsetPanel(
                        tabPanel(title = "parallel plot",value = "par",shiny::uiOutput("data"),shiny::uiOutput("plotPar")),
                        tabPanel(title = "boxpot", value = "box"),
                        tabPanel(title = "stripchart", value = "strip")
                      )
                    )
)


server <- function(input, output, session){
  # thematic::thematic_shiny()
  
  data <- datasourceInputServer(id = "source")
  
  shiny::observeEvent(data(),{
    output$data <- shiny::renderUI({
      shiny::tagList(
        dataVizUi("source"),
        varSelectionUi("source")
      )
    })
    dataVizOutput("source", data)
    varSelectionServer("source",data)
  })
  
  # shiny::observeEvent(input$`source-catVar`,{
    # browser()
    # if(!(is.null(input$`source-catVar`)) | is.null(input$`source-numVar`)){
      
      catVar <- shiny::reactive({
        if(base::all(input$`source-catVar`=="")){
          "noClass"
        }else if(base::all(input$`source-catVar`!="")){
          unique(data()[[input$`source-catVar`[1]]])
        }
      })
      
      output$plotPar <- shiny::renderUI({
        shiny::req(catVar())
        if(length(input$`source-catVar`)==1 | length(input$`source-catVar`)==2){
          purrr::map(catVar(),plotOutput)
        }
      })
      
      observeEvent(catVar(),{
        for(i in 1:length(catVar())){
          base::local({
            var <- shiny::reactive({catVar()[i]})
            output[[var()]] <- shiny::renderPlot({
              if(all(input$`source-catVar`=="") & all(input$`source-numVar`=="")){
                GGally::ggparcoord(data(), columns = numericIndex(data())) + theme(axis.text.x = element_text(angle = 90))
              }else if(all(input$`source-catVar`!="")  & all(input$`source-numVar`=="")){
                # coordcatVar <-shiny::isolate(input$`source-coordcatVar`)
                if(length(input$`source-catVar`)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$`source-catVar`)]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$`source-catVar`)]==var(),]),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$`source-catVar`)==2){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$`source-catVar`[1])]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$`source-catVar`[1])]==var(),]),
                                       groupColumn = input$`source-catVar`[2],
                                       title = paste(var(),"~",input$`source-catVar`[2]))+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }
                
              }else if(all(input$`source-catVar`=="")  & all(input$`source-numVar`!="")){
                if(length(input$`source-numVar`)>1){
                  GGally::ggparcoord(data(), columns = which(colnames(data()) %in% input$`source-numVar`)) + theme(axis.text.x = element_text(angle = 90))
                }
              }else{
                if(length(input$`source-numVar`)>1 & length(input$`source-catVar`)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$`source-catVar`)]==var(),],
                                       columns = which(colnames(data()) %in% input$`source-numVar`),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$`source-numVar`)>1 & length(input$`source-catVar`)==2){
                  GGally::ggparcoord(data()[data()[which(colnames(data())==input$`source-catVar`[1])]==var(),],
                                     columns = which(colnames(data()) %in% input$`source-numVar`),
                                     groupColumn = input$`source-catVar`[2],title = paste(var(),"~",input$`source-catVar`[2]))+
                    theme(axis.text.x = element_text(angle = 90))
                }
              }
            },res = 96)
          })
        }
      })
      
    # }
  # })
  
  
  
  reportServer(id = "par",report_script = "plotVizReport")
  
}

shinyApp(ui, server)