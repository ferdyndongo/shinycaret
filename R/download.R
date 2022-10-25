#' Ui for the downloadButton
#' @param id module identifier
#' @export
downloadUi <- function(id){
  shiny::downloadButton(shiny::NS(id, "download"))
}

#' A renderUi switching between downloadUi and databaseInputUi allowing the user to upload data from
#' file excel to db and to download a database table to file excel.
#' It is the Ui for the downloadUploadServer and they are linked by id to compose a module.
#' @param id module identifier
#' @export
downloadUploadUi <- function(id){
  shiny::fluidRow(shiny::uiOutput(shiny::NS(id,"upDown")))
}

#' Server function for downloadUploadUi
#' @param id module identifier
#' @export
downloadUploadServer <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$upDown <- shiny::renderUI({
      shiny::req(input$datasource)
      if(input$datasource=="database"){
        downloadUi(id = "source")
      }else{
        shiny::tagList(
          databaseInputUi("source"),
          writeToDBUi("source")
        )
      }
    })
  })
}

#' Function server which downloads data from database table when the downloadUi is clicked
#' @param id module identifier
#' @param data data to be written in an excel file whein the button is clicked
#' @export
downloadDBtable <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    output$download <- shiny::downloadHandler(
      filename = function(){
        # paste0(input$dbtable, ".xls")
        if(is.null(input$datasource)){
          if(!is.null(input$dbtable)){
            paste0(paste(input$dbtable,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }else if(!is.null(input$upload$name)){
            paste0(paste(input$upload$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }else if(!is.null(input$sheet)){
            paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }
        }else{
          if(input$datasource=="database" & !is.null(input$dbtable)){
            paste0(paste(input$dbtable,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }else if(input$datasource=="RDS/model" & !is.null(input$upload$name)){
            paste0(paste(input$upload$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }else if(input$datasource=="Access/Excel" & !is.null(input$sheet)){
            paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
          }
        }
      },
      content = function(file){
        id <- shiny::showNotification("Downloading database table ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        # WriteXLS::WriteXLS(data(),file,input$dbtable)
        if(is.null(input$datasource)){
          if(!is.null(input$dbtable)){
            WriteXLS::WriteXLS(data(),file,input$dbtable)
          }else if(!is.null(input$upload$name)){
            WriteXLS::WriteXLS(data(),file,input$upload$name)
          }else if(!is.null(input$sheet)){
            WriteXLS::WriteXLS(data(),file,input$sheet)
          }
        }else{
          if(input$datasource=="database" & !is.null(input$dbtable)){
            shiny::req(input$schema,input$dbtable)
            WriteXLS::WriteXLS(data(),file,input$dbtable)
          }else if(input$datasource=="RDS/model" & !is.null(input$upload$name)){
            WriteXLS::WriteXLS(data(),file,input$upload$name)
          }else if(input$datasource=="Access/Excel" & !is.null(input$sheet)){
            WriteXLS::WriteXLS(data(),file,input$sheet)
          }
        }
        
      }
    )
  })
}

#' Ui rendering report
#' @param id module identifier
#' @export
reportUi <- function(id){
  shiny::tagList(
    shiny::radioButtons(inputId = shiny::NS(id,"format"),label = "Document format",#choices = c("RNotebook","HTML","PDF")
                        choices = "RNotebook",selected = NULL,inline = TRUE),
    shiny::downloadButton(outputId = shiny::NS(id,"report"),label =  "Generate report")
  )
}

#' Server function for reportUi in charge of generate report
#' @param id module identifier
#' @param report_script file with extension .Rmd in charge of parameterized report
#' @importFrom rmarkdown render
#' @export
reportServer <- function(id, report_script){
  shiny::moduleServer(id, function(input, output, session){
    output$report <- shiny::downloadHandler(
      filename = function(){
        paste(report_script,sep = ".", input$format
              # switch(input$format, RNotebook="html", PDF="pdf",HTLM="html")
        )
      },
      content = function(file){
        # params <- list(input=input)
        # tempReport <- file.path(tempdir(), paste(report_script, "Rmd",sep = "."))
        # file.copy(from = paste(report_script, "Rmd",sep = "."), to = tempReport,overwrite = TRUE)
        
        id <- shiny::showNotification("Rendering report ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        
        render(input = paste(report_script, "Rmd",sep = "."),
               output_format = rmarkdown::html_notebook(),
               # switch(input$format,
               #                      RNotebook=rmarkdown::html_notebook(),
               #                      PDF=rmarkdown::pdf_document(),
               #                      HTML=rmarkdown::html_document()
               #                      ),
               output_file = file,
               envir = new.env(globalenv()),
               params = list(input=input))
      }
    )
  })
}

#' Function server which downloads R objects when the downloadUi is clicked
#' @param id module identifier
#' @param data the R object to be saved in a binary file when the button is clicked
#' @export
downloadServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    Robject <- shiny::reactive({
      shiny::req(data())
      if(inherits(data(), "train.formula")){
        data()
      }else if(inherits(data(),"data.frame") &&
               apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        unserialize(data()[[raw_index]][[1]])
      }else{
        data()
      }
      
    })
    
    output$download <- shiny::downloadHandler(
      filename = function(){
        if(inherits(Robject(),"train.formula")){
          if(!is.null(input$caretModel)){
            if(input$datasource=="database"){
              shiny::req(input$caretModel,input$schema,input$dbtable)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$dbtable,sep = "_"), ".RDS")
            }else if(input$datasource=="Access/Excel"){
              shiny::req(input$caretModel,input$mdb_load, input$sheet)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$sheet,sep = "_"), ".RDS")
            }else if(input$datasource=="RDS/model"){
              shiny::req(input$caretModel,input$upload)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$upload$name,sep = "_"), ".RDS")
            }
          }else{
            if(input$datasource=="database"){
              shiny::req(input$schema,input$dbtable)
              paste0(paste(input$dbtable,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".RDS")
            }else if(input$datasource=="Access/Excel"){
              shiny::req(input$mdb_load, input$sheet)
              paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".RDS")
            }else if(input$datasource=="RDS/model"){
              shiny::req(input$upload)
              paste0(paste(input$upload$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".RDS")
            }

          }
        }else if(inherits(Robject(),"data.frame")){
          if(!is.null(input$caretModel)){
            if(input$datasource=="database"){
              shiny::req(input$caretModel,input$schema,input$dbtable)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$dbtable,sep = "_"), ".xls")
            }else if(input$datasource=="Access/Excel"){
              shiny::req(input$caretModel,input$mdb_load, input$sheet)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$sheet,sep = "_"), ".xls")
            }else if(input$datasource=="RDS/model"){
              shiny::req(input$caretModel,input$upload)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$upload$name,sep = "_"), ".xls")
            }
          }else{
            if(input$datasource=="database"){
              shiny::req(shiny::req(input$schema,input$dbtable))
              paste0(paste(input$dbtable,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
            }else if(input$datasource=="Access/Excel"){
              shiny::req(input$mdb_load, input$sheet)
              paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
            }else if(input$datasource=="RDS/model"){
              shiny::req(input$upload)
              paste0(paste(input$upload$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
            }
          }
        }
      },
      content = function(file){
        if(inherits(Robject(),"train.formula")){
          id <- shiny::showNotification("Saving model ...", duration = NULL, closeButton = FALSE)
          on.exit(shiny::removeNotification(id), add = TRUE)
          # if(shiny::is.reactive(Robject)){
          base::saveRDS(Robject(),file)
          # }else{
          #   base::saveRDS(Robject,file)
          # }
        }else if(inherits(Robject(),"data.frame")){
          id <- shiny::showNotification("Downloading database table ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          WriteXLS::WriteXLS(Robject(),file)
          if(input$datasource=="database" & !is.null(input$dbtable)){
            shiny::req(input$schema,input$dbtable)
            WriteXLS::WriteXLS(Robject(),file,input$dbtable)
          }else if(input$datasource=="RDS/model" & !is.null(input$upload$name)){
            WriteXLS::WriteXLS(Robject(),file,input$upload$name)
          }else if(input$datasource=="Access/Excel" & !is.null(input$sheet)){
            WriteXLS::WriteXLS(Robject(),file,input$sheet)
          }
        }
      }
    )
  })
}
