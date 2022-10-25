#' User interface where for descriptive statistics
#' @param id identifier for the data module
#' @return a tagList object
#' @export
edaUi <- function(id){
  shiny::tagList(
    shiny::h3("Descriptive Stats by variable"),
    shiny::verbatimTextOutput(shiny::NS(id,"stats")),
    shiny::h3("Number of missing values by record"),
    shiny::verbatimTextOutput(shiny::NS(id,"na")),
    DT::dataTableOutput(shiny::NS(id,"na_data")),
    shiny::h3("Number of Below Detection Limit missingness by record"),
    shiny::verbatimTextOutput(shiny::NS(id,"dl")),
    DT::dataTableOutput(shiny::NS(id,"dl_data")),
    shiny::h3("Data with wrong decimal separator"),
    DT::dataTableOutput(shiny::NS(id,"dec_sep")),
    # DT::dataTableOutput(shiny::NS(id,"na_dl_data")),
  )
}

#' Visualize overview of data and descriptive statistics in the edaUi
#' @param id identifier of the data module
#' @param data output of the dataImport
#' @return descriptive statistics and missing values distribution
#' @export
#'@importFrom skimr skim_with sfl
#'@importFrom purrr %>%
edaOutput <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    
    
    n_LoD <- skimr::skim_with(character=skimr::sfl(n_bDL),numeric=skimr::sfl(hist=NULL))
    output$stats <- shiny::renderPrint({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(data())){
          if(!is.null(input$catVar)){
            if(all(input$catVar=="")){
              n_LoD(data())
            }else if(all(input$catVar!="") & length(input$catVar)==1){
              n_LoD(data() %>% dplyr::group_by(.data[[input$catVar[1]]]))
            }
          }
        }
      }
    })
    
    n_mis <- shiny::reactive({
      if(!is.null(data())){
        apply(X = data(),MARGIN = 1,FUN = n_missing)
      }
    })
    output$na <- shiny::renderPrint({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(n_mis())){
          n_mis()
        }
      }
    })
    output$na_data <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(any(n_mis() >= floor(length(numericIndex(data()))/2))){
          data() %>% dplyr::filter(n_mis() >= floor(length(numericIndex(data()))/2))
        }
      }
    })
    
    n_dl <- shiny::reactive({
      if(!is.null(data())){}
      apply(X = data(),MARGIN = 1,FUN = n_bDL)
    })
    output$dl <- shiny::renderPrint({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(n_dl())){
          n_dl()
        }
      }
    })
    output$dl_data <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(any(n_dl() >= floor(length(numericIndex(data()))/2))){
          data() %>% dplyr::filter(n_dl() >= floor(length(numericIndex(data()))/2))
        }
      }
    })
    
    output$dec_sep <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(data())){
          wrong_decimal_separator_data(data())
        }
      }
    })
    
    # n_mis_dl <- shiny::reactive({
    #   shiny::req(data())
    #   apply(X = data(),MARGIN = 1,FUN = n_missing_bDL)
    #   })
    # output$na_dl_data <- shiny::renderTable({
    #   if(any(n_mis_dl() >= floor(length(numericIndex(data()))/2))){
    #     data() %>% dplyr::filter(n_mis_dl() >= floor(length(numericIndex(data()))/2))
    #   }
    # })
    
  })
}
