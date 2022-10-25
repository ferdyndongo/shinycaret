#' User interface where the Principal Component Analysis outputs will be written with variable selection inputs
#' @param id data module identifier
#' @return a tagList object
pcaUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"center"),label = "zero mean",choices = c("","TRUE","FALSE")),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"scale"),label = "unit variance",choices = c("","TRUE","FALSE")),width = 2),
      shiny::actionButton(shiny::NS(id,"run"),label = "run pca", class = "btn-lg")
    ),
    shiny::h3("PRINCIPAL COMPONENTS"),
    shiny::p("Visualize the eigenvalues/variances of Principal Components/dimensions"),
    shiny::verbatimTextOutput(shiny::NS(id,"eig")),
    # shiny::tableOutput(shiny::NS(id,"eig"))
    shiny::selectInput(shiny::NS(id,"ncomp"),label = "", choices = NULL, selected = NULL, selectize = FALSE),
    shiny::h3("SCREE PLOT"),
    shiny::p("Barplot of Eigenvalues/Variances against the number of dimensions"),
    shiny::plotOutput(shiny::NS(id,"screeplot")),
    shiny::h3("CONTRIBUTION OF VARIABLES TO PCs/DIMENSIONS"),
    shiny::verbatimTextOutput(shiny::NS(id,"pcaPrint")),
    shiny::selectInput(shiny::NS(id,"dim"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),
    shiny::plotOutput(shiny::NS(id,"pcaVar")),
    shiny::h3("BIPLOT"),
    shiny::p("Biplot of individuals and variables"),
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim1"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim2"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"biplotVar"),label = "", choices = NULL, selected = NULL, selectize = FALSE), width = 2)
    ),
    shiny::plotOutput(shiny::NS(id,"pca_biplot"))
  )
}

#' Compute Principal Component Analyis with variable selection inputs and outputs summary with plots
#' @param id identifier of data module
#' @param  data a given dataframe whose numeric variables will be extracted
#' @return a pca object
pcaServer <- function(id, data){
  shiny::moduleServer(id,function(input, output, session){

    num_col <- shiny::reactive({
      if(!is.null(data())){
        names(numericDataset(data()))
      }
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "PCA Numeric Variables ",choices = c("",num_col()),selected = "")
    })
    
    zero_mean <- shiny::eventReactive(input$run,{
      shiny::req(input$center)
      input$center=="TRUE"
      })
    unit_variance <- shiny::eventReactive(input$run,{
      shiny::req(input$scale)
      input$scale=="TRUE"
      })
    numVar <- shiny::eventReactive(input$run,{
      input$numVar
    })
    shiny::reactive({
      shiny::req(input$run)
      if(!is.null(data())){
        id <- shiny::showNotification("PCA IS RUNNING ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        if(all(numVar()=="")){
          stats::prcomp(numericDataset(data()),center=zero_mean(),scale.=unit_variance())
        }else if(all(numVar()!="")){
          if(length(numVar())>=3){
            stats::prcomp(numericDataset(data()) %>% dplyr::select(dplyr::all_of(numVar())),center=zero_mean(),scale.=unit_variance())
          }
        }
      }
      
    })
    
    # shiny::reactive({
    #   shiny::req(input$center,input$scale)
    #   zero_mean <- input$center=="TRUE"
    #   unit_variance <- input$scale=="TRUE"
    #   if(all(input$numVar=="")){
    #     stats::prcomp(numericDataset(data()),center=zero_mean,scale.=unit_variance)
    #   }else{
    #     if(length(input$numVar)>=3){
    #       stats::prcomp(numericDataset(data()) %>% dplyr::select(dplyr::all_of(input$numVar)),center=zero_mean,scale.=unit_variance)
    #     }
    #   }
    # })
  })
}

#' output summary of pca
#' @param id identifier of data module
#' @param data data used to fit the pca. It only necessary for biplot in order to classify the graphical visualization of scores.
#' @param  pcaObject a given fitted pca model
pcaOutput <- function(id, data, pcaObject){
  shiny::moduleServer(id,function(input, output, session){
    
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    
    shiny::observeEvent(pcaObject(),{
      shiny::updateSelectInput(inputId = "ncomp",label = "Select the number of PCs to explore:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "2")
      shiny::updateSelectInput(inputId = "dim",label = "Select the PCs to explore:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = c("1","2"))
      shiny::updateSelectInput(inputId = "dim1",label = "Select the PC in x axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "1")
      shiny::updateSelectInput(inputId = "dim2",label = "Select the PC in y axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "2")
      shiny::updateSelectInput(inputId = "biplotVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    
    output$eig <- shiny::renderPrint({
      if(!is.null(pcaObject())){
        shiny::req(input$ncomp)
        factoextra::get_eigenvalue(pcaObject()) %>% utils::head(as.numeric(input$ncomp))
      }
    })
    # output$eig <- shiny::renderTable({factoextra::get_eigenvalue(pcaObject()) %>% utils::head(as.numeric(input$ncomp))})
    output$screeplot <- shiny::renderPlot({
      if(!is.null(pcaObject())){
        shiny::req(input$ncomp)
        factoextra::fviz_eig(X = pcaObject(),addlabels = TRUE,ncp = as.numeric(input$ncomp))
      }
    })
    output$pcaPrint <- shiny::renderPrint({
      if(!is.null(pcaObject())){
        shiny::req(input$dim)
        factoextra::facto_summarize(X = pcaObject(), element = "var", axes = as.numeric(input$dim))
      }
    })
    output$pcaVar <- shiny::renderPlot({
      if(!is.null(pcaObject())){
        shiny::req(input$dim)
        factoextra::fviz_contrib(X = pcaObject(),choice = "var", axes = as.numeric(input$dim))
      }
    })
    output$pca_biplot <- shiny::renderPlot({
      if(!( is.null(pcaObject()) | is.null(data()) )){
        shiny::req(input$dim1, input$dim2)
        if(input$biplotVar==""){
          factoextra::fviz_pca_biplot(X = pcaObject(),addEllipses = TRUE,axes = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }else{
          factoextra::fviz_pca_biplot(X = pcaObject(), habillage = data()[[input$biplotVar]], addEllipses = TRUE, 
                                      axes = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }
      }
    })
  })
}

#' Ui for outlier detection with robust pca plots
#' @param id module object identifier
#' @return a tagList shiny object
RobustPCAUi <- function(id){
  shiny::tagList(
    shiny::plotOutput(shiny::NS(id, "dist")),
    # shiny::plotOutput(shiny::NS(id, "biplot"))
    # shiny::plotOutput(shiny::NS(id, "biplot_class")),
    # shiny::plotOutput(shiny::NS(id, "biplot_rob"))
  )
}

#' build robust pca
#' @param data dataframe
#' @param id module identifier
#' @importFrom rrcov plot PcaHubert PcaClassic biplot
RobustPCAServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    pcaRob <- shiny::reactive({
      shiny::req(input$center,input$scale)
      zero_mean <- input$center=="TRUE"
      unit_variance <- input$scale=="TRUE"
      if(shiny::is.reactive(data)){
        rrcov::PcaHubert(numericDataset(data()) %>% scale(center = zero_mean,scale = unit_variance),
                         scale=FALSE)
      }else{
        rrcov::PcaHubert(numericDataset(data) %>% scale(center = zero_mean,scale = unit_variance),
                         scale=FALSE)
      }
    })
    
    pcaClas <- shiny::reactive({
      shiny::req(input$center,input$scale)
      zero_mean <- input$center=="TRUE"
      unit_variance <- input$scale=="TRUE"
      if(shiny::is.reactive(data)){
        rrcov::PcaClassic(numericDataset(data()) %>% scale(center = zero_mean,scale = unit_variance),
                          scale=FALSE)
      }else{
        rrcov::PcaClassic(numericDataset(data) %>% scale(center = zero_mean,scale = unit_variance),
                          scale=FALSE)
      }
    })
    
    output$dist <- shiny::renderPlot({
      if(!is.null(pcaClas()) & !is.null(pcaRob())){
        shiny::req(pcaClas(),pcaRob())
        graphics::par(mfrow=c(1,2))
        rrcov::plot(x = pcaClas())
        rrcov::plot(x = pcaRob())
      }
    })
    
  })
}