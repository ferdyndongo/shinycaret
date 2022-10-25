options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- dashboardPage(title = "Graphical Exploration Analysis",
                    header=dashboardHeader( # dashboardHeader ####
                                            title = "GEA Dashboard",
                                            disable = FALSE,
                                            dropdownMenu(type = "messages"),
                                            dropdownMenu(type = "notifications"),
                                            dropdownMenu(type = "tasks", badgeStatus = "success")),
                    sidebar=dashboardSidebar(datasourceInputUi(id = "source")),
                    body=dashboardBody(
                      tabsetPanel(
                        tabPanel(title = "parallel plot",value = "par",dataVizUi("par"), plotVizUi("par")),
                        tabPanel(title = "boxpot", value = "box"),
                        tabPanel(title = "stripchart", value = "strip")
                      )
                    )
)

server <- function(input, output, session){
  thematic::thematic_shiny()
  
  data <- datasourceInputServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("par", data)
    plotVizServer("par",data)
    })
  
  
  
}

shinyApp(ui, server)
