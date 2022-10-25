#' a fileInput object selecting data from excel
#'
#' @param id identifier for the datamodule
#' @return a fileInput Ui
#' @export
dataFileInput <- function(id){
  shiny::fileInput(shiny::NS(id,"upload"),label = NULL,accept = ".RDS")
}

#' import data from file excel from the dataFileInput
#'
#' @param id identifier for the datamodule
#' @return a fileInput Ui
#' @export
dataFileServer <- function(id){
  shiny::moduleServer(id,function(input, output, session){
    shiny::reactive({
      shiny::req(input$upload)
      tryCatch({
        load_file(input$upload$name, input$upload$datapath)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
        })
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications",
                                       shinydashboard::notificationItem(text = e$message,
                                                                        icon = shiny::icon("warning"),
                                                                        status = "danger")
          )
        })
      })
      # load_file(input$upload$name, input$upload$datapath)
    })
  })
}
