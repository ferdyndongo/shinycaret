#' Generic container object for plot
#' @param id module identifier
plotUi <- function(id){
  shiny::plotOutput(outputId = shiny::NS(id,"plot"),width = "100%",height = "800px")
}

#' selectInput object for numeric variables. It is the UI for numVarServer and they are linked by id
#' @param id module identifier
numVarUi <- function(id){
  shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE)
}

#' Fills the numVarUi with numeric variables names found in given data. It is linked with numVarUi by id
#' @param id module identifier
#' @param data data where the numeric variables names are found.
numVarServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    num_col <- shiny::reactive({
      if(!is.null(data())){
        names(numericDataset(data()))
      }
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
  })
}

#' SelectInput object with factor or character variables. It is the UI for catVarserver and they are linked by id.
#' @param id module identifier
catVarUi <- function(id){
  shiny::selectInput(shiny::NS(id,"catVar"),label = "", choices = NULL, selected = NULL, selectize = FALSE)
}

#' Server for catVarUi. Fills the catVarUi with the categorical variable names.
#' @param id module identifier
#' @param data data where the categorical variable names are found
catVarServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
  })
}


#' Input module for variable selection in a dataset
#' @param id identifier of module object
#' @return a taglist with 2 selectInput for categorical and numerical variables, and the output container for the plot
varSelectionUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"catVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2)
    )
  )
}

#' Server module for varPlotUi in charge of loading the selectInput with the corresponding variables
#' @param id module identifier
#' @param data dataframe for variable selection
varSelectionServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    num_col <- shiny::reactive({
      if(!is.null(data())){
        names(numericDataset(data()))
      }
    })
    
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
  })
}

#' Server module rendering a boxplot. It can be linked with plotUi by id.
#' @param id module identifier
#' @param data dataframe
boxplotServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    output$plot <- shiny::renderPlot({
      if(!is.null(data())){
        box_plot(data = data(), numVar = input$numVar, catVar = input$catVar)
      }
    },res = 96)
  })
}

#' Server module rendering a parallel plot coordinate. It can be linked with plotUi by id.
#' @param id identifier of the module
#' @param data dataframe
#'@importFrom purrr %>%
parcoordPlotServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- shiny::renderPlot({
      if(!is.null(data())){
        parallel_coordinate_plot(data = data(),numVar = input$numVar,catVar = input$catVar)
      }
    },res = 96)
  })
}

#' Ui container for some plots in Graphical Exploration Analysis
#' @param id module identifier
plotVizUi <- function(id){
  shiny::tagList(
  shiny::fluidRow(
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"catVar"),label = "", multiple = TRUE,
                                           choices = NULL, selected = NULL, selectize = FALSE),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE,
                                           choices = NULL, selected = NULL, selectize = FALSE),width = 2)
  ),
  shiny::h3("parallel coordinates plot"),
    shiny::uiOutput(shiny::NS(id,"plotPar"))
    # shiny::plotOutput(shiny::NS(id,"par_coord_plot")),
    # shiny::fluidRow(
    #   shinydashboard::box(shiny::selectInput(shiny::NS(id,"boxcatVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
    #   shinydashboard::box(shiny::selectInput(shiny::NS(id,"boxnumVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2)
    # ),
    # shiny::h3("Boxplot"),
    # shiny::plotOutput(shiny::NS(id,"boxplot")),
  )
}


#' Server for plotVizUi in charge of fill Ui with plots in the App
#' @param id module identifier
#' @param data data used
#' @importFrom ggplot2 theme element_text
plotVizServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    cat_col <- shiny::reactive({
      shiny::req(data)
      (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
    })
    num_col <- shiny::reactive({
      shiny::req(data())
      names(numericDataset(data()))
    })
    
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
    
    shiny::observe({
      catVar <- ifelse(base::all(input$catVar==""),"noClass",unique(data()[[input$catVar[1]]]))
      output$plotPar <- shiny::renderUI({
        if(length(input$catVar)==1 | length(input$catVar)==2){
          purrr::map(catVar,shiny::plotOutput)
        }
      })
    })
    shiny::observeEvent(input$catVar,{
      browser()
      if(!(is.null(input$catVar)) | is.null(input$numVar)){
        
        # catVar <- shiny::reactive({
        #   if(base::all(input$catVar=="")){
        #     "noClass"
        #   }else if(base::all(input$catVar!="")){
        #     unique(data()[[input$catVar[1]]])
        #   }
        # })
        # output$plotPar <- shiny::renderUI({
        #   shiny::req(catVar())
        #   if(length(input$catVar)==1 | length(input$catVar)==2){
        #     purrr::map(catVar(),plotOutput)
        #   }
        # })
        
        for(i in 1:length(unique(data()[[input$catVar[1]]]))){
          base::local({
            var <- shiny::reactive({unique(data()[[input$catVar[1]]])[i]})
            output[[var()]] <- shiny::renderPlot({
              if(all(input$catVar=="") & all(input$numVar=="")){
                GGally::ggparcoord(data(), columns = numericIndex(data())) + theme(axis.text.x = element_text(angle = 90))
              }else if(all(input$catVar!="")  & all(input$numVar=="")){
                # coordcatVar <-shiny::isolate(input$`source-coordcatVar`)
                if(length(input$catVar)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar)]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$catVar)]==var(),]),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$catVar)==2){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar[1])]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$catVar[1])]==var(),]),
                                       groupColumn = input$catVar[2],
                                       title = paste(var(),"~",input$catVar[2]))+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }
                
              }else if(all(input$catVar=="")  & all(input$numVar!="")){
                if(length(input$numVar)>1){
                  GGally::ggparcoord(data(), columns = which(colnames(data()) %in% input$numVar)) + theme(axis.text.x = element_text(angle = 90))
                }
              }else{
                if(length(input$numVar)>1 & length(input$catVar)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar)]==var(),],
                                       columns = which(colnames(data()) %in% input$numVar),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$numVar)>1 & length(input$catVar)==2){
                  GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar[1])]==var(),],
                                     columns = which(colnames(data()) %in% input$numVar),
                                     groupColumn = input$catVar[2],title = paste(var(),"~",input$catVar[2]))+
                    theme(axis.text.x = element_text(angle = 90))
                }
              }
            },res = 96)
          })
        }
        
      }
    })
    
   
    
    
    
    
    
    # shiny::observeEvent(cat_col(),{
    #   shiny::updateSelectInput(inputId = "boxcatVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    # })
    # shiny::observeEvent(num_col(),{
    #   shiny::updateSelectInput(inputId = "boxnumVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    # })
    
    
    
    
    
    # output$par_coord_plot <- shiny::renderPlot({parallel_coordinate_plot(data(),numVar = input$coordnumVar,catVar = input$coordcatVar)},res = 96)
    
    # output$boxplot <- shiny::renderPlot({box_plot(data(), numVar = input$boxnumVar, catVar = input$boxcatVar)}, res = 96)
    
  })
}
