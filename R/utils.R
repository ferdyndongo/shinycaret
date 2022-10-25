#' extract a dataset with only numeric variables from a given dataset
#' @param dataset a dataframe
numericDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.numeric) %>% which())
  }
}

#' extract indexes of the numeric variables from a given dataset
#' @param dataset a dataframe
numericIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.numeric) %>% which()
  }
}

#' extract a dataset with only categorical variables from a given dataset
#' @param dataset a given dataframe
categoricDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.factor) %>% which())
  }
}

#' extract position indexes of the categorical variables in a given dataset
#' @param dataset a given dataset
categoricIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.factor) %>% which()
  }
}

#' extract a dataset with only character variables in a given dataset
#' @param dataset a given dataset
characterDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.character) %>% which())
  }
}

#' extract position indexes of the character variables in a given dataset
#' @param dataset a given dataset
characterIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.character) %>% which()
  }
}

# n_missing <- function(name){sum(is.na(name))}

DL <- function(col){
  base::switch(col,
               Li  = 0.25,
               Na = 50,
               Mn = 20,
               Fe = 150,
               Cu = 30,
               Se = 2,
               Rb = 25,
               Sr = 25,
               Mo = 50,
               Ba = 5,
               Re = 0.07,
               Bi = 0.2,
               U  = 0.1
  )
}

#' Looking for the below Detection Limit sign "<" into a character vector or a dataframe
#' @param name dataframe name or variable name of character vector
#' @return a boolean vector or a dataframe of boolean values
is_bDL <- function(name){stringr::str_starts(string = name,pattern = "<")}

fill_bDL <- function(data){
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")
  
  n_mis_row <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 1,FUN = n_missing)
  na_bDL <- which(n_mis_row > 0 & n_mis_row < floor(length(col_bDL)/2))
  
  
  data <- dplyr::bind_cols(data %>% dplyr::select(-dplyr::all_of(col_bDL)),
                           apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 2,FUN = function(name){
                             if(anyNA(name[na_bDL])){
                               ifelse(is.na(name),"<DL",name)
                             }else{
                               name
                             }
                           }))
  data <- dplyr::bind_cols(data %>% dplyr::select(-dplyr::all_of(col_bDL)),
                           apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),
                                 MARGIN = 2,
                                 FUN = function(name){
                                   if(any(stringr::str_starts(string = name,pattern = "[<-]"))){
                                     ifelse(stringr::str_starts(string = name,pattern = "[<-]"),"<DL",name)
                                   }else{
                                     name
                                   }
                                 }
                           )
  )
  return(data)
}


#' Looking for any "<" Detection Limit sign in a character vector
#' @param name character vector
any_bDL <- function(name){any(stringr::str_starts(string = name,pattern = "<"),na.rm = TRUE)}
# apply(X = is_bDL.dataframe(data),MARGIN = 2,FUN = any)


#' Count the number of values below the limit of detection
#' @param name variable name of the character vector
#' @param name variable name of a character vector
n_bDL <- function(name){is_bDL(name) %>% sum(na.rm = TRUE)}
#n_bDL <- function(name){stringr::str_starts(string = name,pattern = "[<]") %>% sum(na.rm = TRUE)}


#' Give a randomly selected value between 0 and the Detection Limit value
#' @param name a character vector
#' @param LoD Limit Of Detection value
bDL <- function(name, LoD=0){
  set.seed(12345)
  ifelse(test = is_bDL(name),yes = stats::runif(n=n_bDL(name),min = 0,max = LoD), no = name)
}

#' fill <DL position in a dataset with a specific value given randomly between 0 and the LoD
#' @param data a dataframe
impute_bDL <- function(data){
  
  bDL_cols <- apply(X = data,MARGIN = 2,FUN = any_bDL) %>% which()
  
  if(!purrr::is_empty(bDL_cols)){
    for(bDL_col in names(bDL_cols)){
      data[[bDL_col]] <- base::switch(bDL_col,
                                      Li  = bDL(data[[bDL_col]], LoD = 0.25),
                                      Na = bDL(data[[bDL_col]], LoD = 50),
                                      Mn = bDL(data[[bDL_col]], LoD = 20),
                                      Fe = bDL(data[[bDL_col]], LoD = 150),
                                      Cu = bDL(data[[bDL_col]], LoD = 30),
                                      Se = bDL(data[[bDL_col]], LoD = 2),
                                      Rb = bDL(data[[bDL_col]], LoD = 25),
                                      Sr = bDL(data[[bDL_col]], LoD = 25),
                                      Mo = bDL(data[[bDL_col]], LoD = 50),
                                      Ba = bDL(data[[bDL_col]], LoD = 5),
                                      Re = bDL(data[[bDL_col]], LoD = 0.07),
                                      Bi = bDL(data[[bDL_col]], LoD = 0.2),
                                      U  = bDL(data[[bDL_col]], LoD = 0.1)
      )
    }
  }
  return(data)
}


#' Count the number of missing values in a vector
#' @param name variable name of the character vector
n_missing <- function(name){sum(is.na(name))}

#' Count the number of missing values and/or values below the detection limit in a vector
#' @param name variable name of the character vector
n_missing_bDL <- function(name){
  sum(stringr::str_starts(string = name,pattern = "[<]") | is.na(name))
}

is_point_decimal_separator <- function(name){stringr::str_detect(name,"\\.")}

any_comma_decimal_separator <- function(name){any(stringr::str_detect(name,","),na.rm = TRUE)}

replace_decimal_separator <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    if(!purrr::is_empty(cols)){
      for(col in cols){
        data[[col]] <- stringr::str_replace(data[[col]], ",",".")
      }
    }
  }
  return(data)
}

wrong_decimal_separator_data <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    b <- base::data.frame()
    for(col in names(cols)){
      a <- data %>% dplyr::filter(stringr::str_detect(data[[col]],","))
      b <- b %>%  dplyr::bind_rows(a) 
    }
    return(b)
  }
}

rename_data <- function(data){
  data %>% dplyr::rename(anno=colnames(data[1]), numero=colnames(data[2]), matricola=colnames(data[3]),
                         class=colnames(data[4]), "d13C"=colnames(data[5]), "d15N"=colnames(data[6]), 
                         "d2H"=colnames(data[7]),"Li"=colnames(data[8]),  "Na"=colnames(data[9]), 
                         "Mn"=colnames(data[10]), "Fe"=colnames(data[11]), "Cu"=colnames(data[12]), 
                         "Se"=colnames(data[13]), "Rb"=colnames(data[14]), "Sr"=colnames(data[15]), 
                         "Mo"=colnames(data[16]), "Ba"=colnames(data[17]), "Re"=colnames(data[18]), 
                         "Bi"=colnames(data[19]), "U"=colnames(data[20]))
}

#' Convert data variables to the right data type
#' @param data a dataframe
convert_data_type <- function(data){
  
  for(col in colnames(data)){
    data[[col]] <- base::switch(col,
                                anno = as.factor(data[[col]]),
                                numero = as.factor(data[[col]]),
                                matricola = as.factor(data[[col]]),
                                class = as.factor(data[[col]]),
                                d13C = as.numeric(data[[col]]),
                                d15N = as.numeric(data[[col]]),
                                d2H = as.numeric(data[[col]]),
                                Li  = as.numeric(data[[col]]),
                                Na = as.numeric(data[[col]]),
                                Mn = as.numeric(data[[col]]),
                                Fe = as.numeric(data[[col]]),
                                Cu = as.numeric(data[[col]]),
                                Se = as.numeric(data[[col]]),
                                Rb = as.numeric(data[[col]]),
                                Sr = as.numeric(data[[col]]),
                                Mo = as.numeric(data[[col]]),
                                Ba = as.numeric(data[[col]]),
                                Re = as.numeric(data[[col]]),
                                Bi = as.numeric(data[[col]]),
                                U  = as.numeric(data[[col]])
    )
  }
  return(data)
}


#' Delete samples with high percentage of missing values. The threshold is given by more than 50% of the numeric predictors.
#' @param data dataframe
rm_na <- function(data){
  
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")
  if(any(data %>% dplyr::select(dplyr::all_of(col_bDL)) %>% is.na() %>% apply(MARGIN = 1,FUN = all))){
    na_bDL <- data %>% dplyr::select(dplyr::all_of(col_bDL)) %>% is.na() %>% apply(MARGIN = 1,FUN = all) %>% which()
    data <- data %>% dplyr::slice(-na_bDL)
  }
  
  col_iso <- c("d13C","d15N","d2H")
  if(any(data %>% dplyr::select(dplyr::all_of(col_iso)) %>% is.na() %>% apply(MARGIN = 1,FUN = all))){
    na_iso <- data %>% dplyr::select(dplyr::all_of(col_iso)) %>% is.na() %>% apply(MARGIN = 1,FUN = all) %>% which()
    data <- data %>% dplyr::slice(-na_iso)
  }
  
  n_mis_row <- apply(X = data %>% dplyr::select(dplyr::all_of(c(col_bDL,col_iso))),MARGIN = 1,FUN = n_missing)
  if(any(n_mis_row >= floor((length(col_bDL)+length(col_iso))/2))){
    data <- data %>% dplyr::filter(n_mis_row < floor((length(col_bDL)+length(col_iso))/2))
  }
  
  n_mis_column <- apply(X = numericDataset(data),MARGIN = 2,FUN = n_missing)
  if(any(n_mis_column >= floor(nrow(data)/2))){
    data <- data %>% dplyr::select(-dplyr::all_of(names(which(n_mis_column >= floor(nrow(data)/2)))))
  }
  
  return(data)
}

#' Delete samples with high percentage of varialbes below de Detection Limit. 
#' The threshold is given by more than 50% of the numeric predictors.
#' @param data dataframe
rm_bDL <- function(data){
  
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")

  n_dl_row <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 1,FUN = n_bDL)
  if(any(n_dl_row >= floor(length(col_bDL)/2))){
    data <- data %>% dplyr::filter(n_dl_row < floor(length(col_bDL)/2))
  }
  
  n_dl_column <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 2,FUN = n_bDL)
  if(any(n_dl_column >= floor(nrow(data)/2))){
    data <- data %>% dplyr::select(-dplyr::all_of(names(which(n_dl_column >= floor(nrow(data)/2)))))
  }
  
  return(data)
}

#' Compute some data preprocessing steps such as replace the under detection limit value wiht a random variable between 0 and the detecttion limit,
#' convert data variables and remove records with high number of missing values
#' @param data dataframe
data_preprocessing <- function(data){
  data %>% rename_data() %>% rm_na() %>% fill_bDL()  %>% rm_bDL() %>% 
    impute_bDL() %>% replace_decimal_separator() %>% convert_data_type()
}

#' load data from files with extension xls, xlsx, csv, txt and mdb
#' @param name the name of the file
#' @param path full path of the file
#' @param sheet_name use only for specifying the excel sheetname or the mdb table name
load_file <- function(name, path, sheet_name=NULL) {
  ext <- tools::file_ext(name)
  base::switch(ext,
               xls = readxl::read_excel(path = path,sheet = sheet_name),
               xlsx = readxl::read_excel(path = path,sheet = sheet_name),
               csv = readr::read_csv(file = path),
               txt = utils::read.table(file = path, quote="\"", comment.char=""),
               tsv = utils::read.delim(file = path),
               mdb = Hmisc::mdb.get(file = path,tables = sheet_name),
               RDS = base::readRDS(file = path),
               # RData = base::load(file = name, envir = base::new.env()),
               shiny::validate("Invalid file; Please upload a .csv, .tsv, .txt, .xlsx, .xls, .mdb or .RDS file")
  )
}

#' extract the name of sheets (workbook) or tables (mdb) in a given file .xls or .mdb
#' @param name the file name
#' @param path the full path of the file 
sheet_list <- function(name, path){
  ext <- tools::file_ext(name)
  base::switch(ext,
               xls = readxl::excel_sheets(path = path),
               xlsx = readxl::excel_sheets(path = path),
               mdb = Hmisc::mdb.get(file = path,tables = TRUE),
               shiny::validate("Invalid file; Please upload a .mdb, .xlsx or .xls file")
  )
}

#' variable selection through recursive feature elimination based on random forest
#' @param data a dataset with response variable and quantitative regressors. 
#' If there are categorical variable they have to be transformed in dummy variables
#' prior to use rfe function for variable selection
#' @param catVar the response variable
rfeVar <- function(data, catVar){
  set.seed(1000)
  rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                          rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
  return(data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar))))
}

#' pre-processing steps used for regressors: zero-variance elimination, near zero-value elimination,
#' highly correlated variable elimination, centering, scaling and bagImpute imputation for missing values.
#' @param data a numerical dataset containing predictor variables.
preprocess <- function(data){
  preProc <- caret::preProcess(data, method=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
  return(stats::predict(preProc, data))
}

#' Train and fit predictive models of classification and regression from the caret package.
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param model A string specifying which classification or regression model to use. 
#' Possible values are found using names(getModelInfo())
#' @param preprocess a character vector specifying the pre-processing methods to be used.
#' @return a fitted knn model
trainModel <- function(data, catVar, model, preprocess=NULL){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    
    ## train and fit the model
    ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
    if(base::is.null(preprocess) | purrr::is_empty(preprocess)){
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbosity=0)
    }else{
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit, verbosity=0,
                   preProcess=preprocess)
    }
  }
}

#' Fit and train some classification models
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param rfe logical value whether to apply variable selection with recursive feature elimination algorythm
#' @param model string specifiying which classification model will be used.
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a trained and fitted classification model
fit_class <- function(data, catVar, model, rfe=FALSE, preprocess=FALSE){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    if(catVar %in% colnames(data)){
      if(base::isTRUE(rfe)){
        data <- rfeVar(data, catVar)
      }else{
        data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
      }
      ## fitting and tuning the model
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
      if(base::isTRUE(preprocess)){
        if(model=="gbm"){
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit,verbose=0,
                       verbose=0,preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
        }else{
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit,verbosity=0,
                       verbosity=0,preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
        }
      }else{
        if(model=="gbm"){
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbose=0)
        }else{
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbosity=0)
        }
        
      }
    }
  }
}

#' Fit and train a k nearest neighbors model
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param rfe logical value whether to apply variable selection with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a fitted knn model
fit_knn <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    ## fitting and tuning the knn
    ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data,  method="knn", trControl=ctrl_fit,
                   preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
    }else{
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data, method="knn", trControl=ctrl_fit)
    }
  }
  
}

#' Train a Random Forest model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return an Random Forest fitted model
fit_rf <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      # ## recursive feature elimination as feature selection method
      set.seed(1000)
      rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                              rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                method="rf",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , method="rf", 
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
      
    }
    
    return(fitted_rf)
  }
}

#' Train a XGBTREE Model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return an Random Forest fitted model
fit_xgbTree <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    if(base::isTRUE(rfe)){
      # ## recursive feature elimination as feature selection method
      set.seed(1000)
      rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                              rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , 
                                preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                method="xgbTree", verbosity=FALSE,
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , verbosity=FALSE, method="xgbTree",
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
      
    }
    
    return(fitted_rf)
  }
}

#' Train a Linear Discriminant Analysis model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a linear discriminant analysis fitted model
fit_lda <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    if(base::isTRUE(rfe)){
      set.seed(1000)
      ldaProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                               rfeControl=caret::rfeControl(functions = caret::ldaFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(ldaProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_lda <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="lda",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_lda <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="lda",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    
    return(fitted_lda)
  }
}

#' Train a Decision Tree model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a decision tree fitted model
fit_cart <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      # data <- rfeVar(data, catVar)
      set.seed(1000)
      cartProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                                rfeControl=caret::rfeControl(functions = caret::treebagFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(cartProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_cart <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                  method="rpart",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_cart <- caret::train(formula(paste(catVar,"~ .")), data ,
                                  method="rpart",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_cart)
  }
}

#' Train a Support Vector Machine model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a support vector machine fitted model
fit_svm <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_svm <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="svmRadial",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_svm <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="svmRadial",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_svm)
  }
}

#' Train a Partial Least Squares model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a partial least squares fitted model
fit_pls <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_pls <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="pls",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_pls <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="pls",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_pls)
  }
}


#' Outlier identification with boxplot
#' @param dataset a dataframe of numerical variables
#' @return dataframe with a variable `all` specifying if the observation is outlier for all variables
boxOut <- function(dataset){
  outbox <- list()
  vars <- colnames(dataset)
  for(var in vars) outbox[[var]] <- data.frame(outlier = dataset[[var]] %in% grDevices::boxplot.stats(dataset[[var]])$out)
  if(length(vars)>1) outbox$all <- data.frame(outlier=apply(X = data.frame(outbox),MARGIN = 1,FUN = all))
  # index_out <- which(apply(X = data.frame(outbox),MARGIN = 1,FUN = all))
  # data <- dplyr::bind_cols(dataset,out=outbox$all)
  return (outbox)
}

#' Local Outlier Factor identification
#' @param X A dataset 
#' @param minPts minimum number of points to be considered given by a knn
#' @return dataframe with local outlier factor scores
lofOut <- function(X, minPts){
  
  lofscores <- data.frame(scores=DDoutlier::LOF(dataset = X, k = minPts))
  return(lofscores)
}

#' plot outlier if there is some.
#' @param rawdata the raw dataset in input.
#' @param outdata the dataset with logical variable
#' @return a parallel coordinates 
#' @importFrom purrr is_empty
#' @importFrom stats formula
outplot <- function(rawdata, outdata){
  
  dataset <- dplyr::bind_cols(rawdata, outdata)
  # lapply(X = dataset,FUN = is.numeric) %>% data.frame() %>% apply(MARGIN = 2,FUN = isTRUE)
  num_col <- sapply(X = dataset,FUN = is.numeric) %>% which()
  cat_col <- sapply(X = dataset,FUN = is.factor) %>% which()
  bool_col <- sapply(X = dataset,FUN = is.logical) %>% which()
  if(!(is_empty(dataset[bool_col]) & is_empty(dataset[cat_col]))){
    
    if(!(is_empty(dataset[bool_col]) | is_empty(dataset[cat_col]))){
      
      if (!(is_empty(which(dataset[[names(bool_col)]])) | length(unique(dataset[[names(cat_col)]]))<2)){
        GGally::ggparcoord(data = dataset, columns = num_col, groupColumn = names(bool_col)) + 
          ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
      }else if(is_empty(which(dataset[[names(bool_col)]])) & !length(unique(dataset[[names(cat_col)]]))<2){
        # GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(cat_col))
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2) +
          ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
      }else if(!is_empty(which(dataset[[names(bool_col)]])) & length(unique(dataset[[names(cat_col)]]))<2){
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(bool_col))
      }else if(is_empty(which(dataset[[names(bool_col)]])) & length(unique(dataset[[names(cat_col)]]))<2){
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2)
      }
      
    }else if(is_empty(dataset[bool_col]) | !is_empty(dataset[cat_col])){
      GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(cat_col)) +
        ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
    }else if(!is_empty(dataset[bool_col]) | is_empty(dataset[cat_col])){
      if(!is_empty(which(dataset[[names(bool_col)]]))){
        GGally::ggparcoord(data = dataset, columns = num_col, groupColumn = names(bool_col)) 
        # +facet_wrap(formula(paste("~dataset$",names(bool_col))))
      }
    }
    
  }else{
    GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2)
  }
}

clustViz <- function(dbscanObject, dbscanData,comp=c(1,2)){
  if(length(colnames(dbscanData))>3){
    factoextra::fviz_cluster(object = dbscanObject, data = dbscanData, stand = FALSE, axes = comp)
  }else{
    factoextra::fviz_cluster(object = dbscanObject, data = dbscanData,stand = FALSE,choose.vars=comp)
  }
}

#' visualizzation on the predicted cluster points onto the fitted cluster space given by output of the function clustViz
#' @param fviz_clust ggplot object given by the clustViz function
#' @param predicted_scores dataframe with data used to predict and the predicted cluster column
#' @param comp the chosen axes or dimensions
#' @return a plot with a projection of predicted cluster points into a given cluster space
#' @importFrom ggplot2 ggplot aes geom_point geom_text xlab ylab
#' @importFrom rlang .data
predViz <- function(fviz_clust, predicted_scores,comp=c(1,2)){
  if(any(predicted_scores$cluster==0)){
    outlier.data <- predicted_scores %>% dplyr::filter(predicted_scores$cluster==0)
    ggplot(data = fviz_clust$layers[[1]]$data, mapping = aes(fviz_clust$layers[[1]]$data$x,fviz_clust$layers[[1]]$data$y)) + 
      fviz_clust$layers[[2]] + 
      geom_point(data = predicted_scores, 
                 mapping = aes(.data[[colnames(predicted_scores)[comp[1]]]], 
                               .data[[colnames(predicted_scores)[comp[2]]]], 
                               colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]])) + 
      geom_text(data = predicted_scores,
                mapping = aes(x = .data[[colnames(predicted_scores)[comp[1]]]],
                              y = .data[[colnames(predicted_scores)[comp[2]]]]+0.15,
                              colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]],
                              label=rownames(predicted_scores))) +
      # geom_text(data = outlier.data,
      #           mapping = aes(x = .data[[colnames(outlier.data)[comp[1]]]],
      #                         y = .data[[colnames(outlier.data)[comp[2]]]]+0.15,
      #                         colour=.data[[colnames(outlier.data)[length(outlier.data)]]],
      #                         label=rownames(outlier.data))) +
      xlab(fviz_clust$labels$x) + 
      ylab(fviz_clust$labels$y)
  }else{
    ggplot(data = fviz_clust$layers[[1]]$data, mapping = aes(fviz_clust$layers[[1]]$data$x,fviz_clust$layers[[1]]$data$y)) +
      fviz_clust$layers[[2]] + 
      geom_point(data = predicted_scores, 
                 mapping = aes(.data[[colnames(predicted_scores)[comp[1]]]], 
                               .data[[colnames(predicted_scores)[comp[2]]]], 
                               colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]])) + 
      xlab(fviz_clust$labels$x) + 
      ylab(fviz_clust$labels$y)
  }
}


#' DBSCAN outlier detection
#' @param datatrain dataset with numerical inputs
#' @param eps distance to be considered
#' @param minPts minimum number of points within the considered distance eps
#' @return a dbscan object
dbscan_outlier_detection <- function(datatrain, eps, minPts){
  if(!(is.null(datatrain))){
    X <- datatrain %>% scale()
    ds <- dbscan::dbscan(x = X,eps, minPts)
    return(ds)
  }
  
}

#' parallel coordinate plot of data with a column for cluster membership
#' @param dataclust dataframe with a column for cluster membership
#' @param varOrder method used to order variables in the horizontal axis
#' @return a dbscan object
parcoord_plot <- function(dataclust, varOrder=NULL){
  if(!is.null(varOrder)){
    if(any(dataclust$cluster==0)){
      if(varOrder!=""){
        dataclust %>% 
          GGally::ggparcoord(columns = numericIndex(dataclust),
                             groupColumn = categoricIndex(dataclust), 
                             order = varOrder)
      }else{
        dataclust %>% 
          GGally::ggparcoord(columns = numericIndex(dataclust),
                             groupColumn = categoricIndex(dataclust))
      }
    }else{
      if(varOrder!=""){
        dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1,order = varOrder)
      }else{
        dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1)
      }
    }
  }else{
    if(any(dataclust$cluster==0)){
      dataclust %>% 
        GGally::ggparcoord(columns = numericIndex(dataclust),
                           groupColumn = categoricIndex(dataclust))
    }else{
      dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1)
    }
  }
  
}

box_plot <- function(data, numVar=NULL, catVar=NULL){
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="") & all(numVar!="")){
      if(length(numVar)==1){
        graphics::boxplot(data[[numVar]],horizontal=TRUE)
      }else if(length(numVar)>1){
        graphics::boxplot(data %>% dplyr::select(dplyr::all_of(numVar)) %>% scale(),horizontal = TRUE)
      }
    }else if(all(catVar!="") & all(numVar!="")){
      if(length(catVar)==1){
        graphics::par(mfrow=c(length(numVar),1))
        for(var in numVar){
          graphics::boxplot(formula(paste(var,"~",catVar)),data=data,horizontal=TRUE,las=1)
        }
      }
    }
  }
}

#' write to database from a file excel
#' @param dsn the datasource name connected to database
#' @param filepath the file excel full path
#' @param schemaname the schema's name where the table is stored
#' @param tablename the table's name which will be updated. if not selected a new table will be created in the selected schema
#' @param overwrite logical value: when FALSE and append is FALSE with not selected table, new one is created
#' @param append logical value: if TRUE and overwrite is FALSE, the selected table will be updated
#' @export
write_to_db <- function(dsn, filepath, schemaname, tablename,overwrite=FALSE,append=TRUE){
  db <- DBI::dbConnect(odbc::odbc(), dsn)
  if (is.character(filepath) && (basename(filepath) %>% stringr::str_ends(pattern = ".xls") |
                                 basename(filepath) %>% stringr::str_ends(pattern = ".xlsx"))){
    data <- readxl::read_excel(path = filepath)
  }else if(is.character(filepath) && basename(filepath) %>% stringr::str_ends(pattern = ".csv")){
    data <- readr::read_csv(file = filepath)
  }else{
    data <- filepath
  }
  destination <- paste(schemaname, tablename, sep = ".")
  odbc::dbWriteTable(conn = db, name =  DBI::SQL(destination), value = data,overwrite=overwrite,append=append)
  odbc::dbDisconnect(db)
}

parallel_coordinate_plot <- function(data, numVar=NULL, catVar=NULL){
  
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="") & all(numVar=="")){
      GGally::ggparcoord(data = data, columns = numericIndex(data)) +
        theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar!="") & all(numVar=="")){
      if(length(catVar)==1){
        # browser()
        # p1<- GGally::ggparcoord(data = data, columns = numericIndex(data), groupColumn = catVar) +
        #   theme(axis.text.x = element_text(angle = 90))
        GGally::ggparcoord(data = data,columns = numericIndex(data)) +
          ggplot2::facet_grid(data[[catVar]] ~ .,scales = "free") +
          # ggplot2::facet_wrap(~data[[catVar]],scales = "free") +
          theme(axis.text.x = element_text(angle = 90))
        # gridExtra::grid.arrange(p1, p2)
      }else if(length(catVar)==2){
        GGally::ggparcoord(data = data, columns = numericIndex(data), groupColumn = catVar[1]) +
          ggplot2::facet_wrap(~data[[catVar[2]]],scales = "free") + theme(axis.text.x = element_text(angle = 90))
      }
    }else if(all(catVar=="") & all(numVar!="")){
      if(length(numVar)>1){GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar))} + 
        theme(axis.text.x = element_text(angle = 90))
    }else{
      if(length(numVar)>1 & length(catVar)==1){
        p1 <- GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar), groupColumn = catVar) + 
          theme(axis.text.x = element_text(angle = 90))
        p2 <- GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar)) + 
          ggplot2::facet_wrap(~data[[catVar]],scales = "free") + 
          theme(axis.text.x = element_text(angle = 90))
        gridExtra::grid.arrange(p1, p2)
      }else if(length(numVar)>1 & length(catVar)==2){
        GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar), groupColumn = catVar[1]) + 
          ggplot2::facet_wrap(~data[[catVar[2]]],scales = "free") + theme(axis.text.x = element_text(angle = 90))
      }
    }
  }
}

parallel_coordinate_plot2 <- function(data, numVar=NULL, catVar=NULL){
  # lev <- ifelse(base::all(catVar==""),"noClass",unique(data[[catVar[1]]]))
  # list_catVar <- purrr::map(lev,list)
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="") & all(numVar=="")){
      GGally::ggparcoord(data = data, columns = numericIndex(data)) + 
        theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar!="") & all(numVar=="")){
      if(length(catVar)==1){
        graphics::par(mfrow=c(length(unique(data[[catVar]])),1))
        for(var in unique(data[[catVar]])){
          GGally::ggparcoord(data[data[which(colnames(data)==catVar)]==var,],
                             columns = numericIndex(data[data[which(colnames(data)==catVar)]==var,]))
          + theme(axis.text.x = element_text(angle = 90))
        }
      }else if(length(catVar)==2){
        graphics::par(mfrow=c(length(unique(data[[catVar[1]]])),1))
        for(var in unique(data[[catVar[1]]])){
          GGally::ggparcoord(data[data[which(colnames(data)==catVar[1])]==var,],
                             columns = numericIndex(data[data[which(colnames(data)==catVar)]==var,]),
                             groupColumn = catVar[2]) + theme(axis.text.x = element_text(angle = 90))
        }
      }
    }else if(all(catVar=="") & all(numVar!="")){
      if(length(numVar)>1){GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar))} + 
        theme(axis.text.x = element_text(angle = 90))
    }else{
      if(length(numVar)>1 & length(catVar)==1){
        graphics::par(mfrow=c(length(unique(data[[catVar]])),1))
        for(var in unique(data[[catVar]])){
          GGally::ggparcoord(data = data[data[which(colnames(data)==catVar)]==var,], 
                             columns = which(colnames(data[data[which(colnames(data)==catVar)]==var,]) %in% numVar)) + 
            theme(axis.text.x = element_text(angle = 90))
        }
      }else if(length(numVar)>1 & length(catVar)==2){
        graphics::par(mfrow=c(length(unique(data[[catVar[1]]])),1))
        for(var in unique(data[[catVar[1]]])){
          GGally::ggparcoord(data = data[data[which(colnames(data)==catVar[1])]==var,], 
                             columns = which(colnames(data[data[which(colnames(data)==catVar)]==var,]) %in% numVar), 
                             groupColumn = catVar[2]) + theme(axis.text.x = element_text(angle = 90))
        }
      }
    }
  }
}

notify <- function(msg, id = NULL) {
  shiny::showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
}
