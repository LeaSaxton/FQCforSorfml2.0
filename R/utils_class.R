# default machine learning parametedef.proportion<-0.7
defCostRange<-2^seq(-15,3,2)
defGammaRange<-2^seq(-5,15,2)
defEpsilonRange<-c(seq(0,0.1,0.03), 0.2, seq(0.3,0.9,0.3))
maxK <-20
defNtree<-1000
defNumberOfIterations <- 80
defPercentageForTrainingSet <- 0.75

#' Accuracy
#' @description generates Accuracy performance metric from predicted and
#' actual values
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param true
#' @param predicted
#' @return Accuracy performance metric
#'
#' @examples
#' \dontrun{Accuracy(true, predicted)}

Accuracy <- function(true, predicted) {
  correct <- sum(true == predicted)
  total <- length(true)
  accuracy <- correct / total
  return(accuracy)
}


#' evalMetrics
#' @description generates RSQUARE and RMSE performance metric from predicted and
#' actual values
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param true
#' @param predicted
#' @return list containing RSquare and RMSE performance metric
#'
#' @examples
#' \dontrun{evalMetrics(true, predicted)}

evalMetrics <- function(true, predicted) {
  RSquare <- RSQUARE(true, predicted)
  RMSE = RMSE(true, predicted)
  return(list("RMSE" = RMSE, "RSquare" = RSquare))
  
}

#' getPretreatmentVectorClass
#' @description converts string of pretreatment parameter to vector to be used
#' in data scaling before machine learning modeling
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param pretreatment pretreatment string object  provided by config file
#' @return vector
#'
#' @examples
#' \dontrun{gePretreatmentVectorClass(pretreatment)}
#'
getPretreatmentVectorClass <- function(pretreatment){
  # Pretreatment parameter is changed to vector as it is required by
  # caret::preProcess method.
  if (pretreatment == "normalise"){
    return(c("norma"))
  }
  if(pretreatment == "auto-scale"){
    return(c("center", "scale"))
  }
  if(pretreatment == "range-scale"){
    return(c("range"))
  }
  if(pretreatment == "meam"){
    return(c("center"))
  }
  if(pretreatment=="pareto-scale"){
    return(c("pareto"))
  }
  if(pretreatment =="vast-scaling"){
    return(c("vast"))
  }
  if(pretreatment=="level-scaling"){
    return(c("level"))
  }
}

#' getClassificationParameters
#' @description creates  classificationParameterList which contains all relevant
#' information to be used by machine learning models. classificationParameterList
#' is used as an internal object passed through different functions in machine
#' learning modeling
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param mlm  machine learning model list
#' @param dataSet  dataFrame produced by analytical platforms
#' @param metaDataName metadata Name 
#' @return list of classification parameters
#'
#' @examples
#' \dontrun{getClassificationParameters(mlm, dataSet, platform, metaDataName )}

getClassificationParameters <- function(mlm, dataSet, platform, metaDataName){
  mlmParams <- strsplit(mlm, ":")[[1]]
  print(mlmParams)
  if(mlmParams[2] == "" || is.na(mlmParams[2]))
    mlmParams[2] <- "mean-center"
  if(mlmParams[3] == "" || is.na(mlmParams[3]))
    mlmParams[3] <- defNumberOfIterations
  if(mlmParams[4] == ""  || is.na(mlmParams[4]))
    mlmParams[4] <- defPercentageForTrainingSet
  if(mlmParams[5] == ""  || is.na(mlmParams[5]))
    mlmParams[5] <- "repeatedcv"
  if(mlmParams[6] == ""  || is.na(mlmParams[6]))
    mlmParams[6] <- 10
  
  # classificationParameterList object contains all necessary information for a machine learning model to run
  classificationParameterList <- list("method" = mlmParams[1], "pretreatment" =  mlmParams[2], "numberOfIterations" = as.numeric(mlmParams[3]),
                                  "percentageForTrainingSet" = as.numeric(mlmParams[4]), dataSet = dataSet,
                                  "platform" = platform, "resampling" = mlmParams[5], "metaDataName" = metaDataName, "tuneLength" = mlmParams[6])
  
  return(classificationParameterList)
  
}

#' plotPrediction
#' @description draws plot of predicted values against actual values with RMSE value
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param model  machine learning model
#' @param testSet  dataFrame used for  machine learning model validation

#'
#' @examples
#' \dontrun{plotPrediction(model, testSet )}


plotPrediction <- function(model, testSet) {
  predicted <- predict(model, testSet)
  RMSE <- RMSE(testSet$TVC, predicted)
  plot(predicted, testSet$TVC, xlab='Predicted log10 TVC',
       ylab='Actual log10 TVC', main=paste('RMSE:', RMSE))
}

#' readConfigFileClass
#' @description reads, parses and performs some validity checks on the configuration data
#' supplied in json format as config file
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param configFile  configFile
#' @import configr
#' @return list of config object containing machineLearningModels, outputDirectory,
#' createStatisticsFile, createPerformancePlots, createPCAPlots
#'
#' @examples
#' \dontrun{readConfigFile(configFile)}

readConfigFileClass<-function(configFile){
  
  fileExtension <- tolower(file_ext(configFile))
  if(fileExtension != "json")
    stop("Input parameters validation error: Configuration file format is not supported, json is valid!")
  
  config <-read.config(file = configFile)
  
  # checks if the output directory exists
  # Output directory will contain statistics report, pca plots and performance plots
  # generated by the program
  if(!is.null(config$outputDirectory) && !file.exists(config$outputDirectory))
    stop("Input parameters validation error: Output directory does not exist!")
  
  #if output directory is not provided, working directory is used as an outputDirectory
  if(is.null(config$outputDirectory) || is.na(config$outputDirectory))
    config$outputDirectory = getwd()
  
  #config$outputDirectory = paste0(config$outputDirectory, "/FoodQualityController-", Sys.time())
  
  # machinelearningmodels are parsed and a vector of machineLearningModels is created in different format
  # each machine leaarning model in the vector becomes string in the format of
  # shortName:pretreatment:numberOfIterations:proportionOfTrainingSet
  mlmConfig <-  config$machineLearningModels

  mlmList <-c()
  for(i in 1:length(mlmConfig$shortName)){
    if( (mlmConfig$shortName[i] == "RT" || mlmConfig$shortName[i] == "RF")  && (!is.null(mlmConfig$direction[i]) && !is.na(mlmConfig$direction[i])))
      warning("Data pretreatmet is not performed in Ramdom Forests and Regression Trees, parameter is ignored!")
    if(mlmConfig$shortName[i] != "SR" &&  !is.null(mlmConfig$direction[i]) && !is.na(mlmConfig$direction[i]))
      warning("direction paremeter is only used in Stepwise Regression, in other regression models it is ignored!")
    
    if(is.null(mlmConfig$pretreatment[i]) || is.na(mlmConfig$pretreatment[i]))
      mlmConfig$pretreatment[i] <- "mean-center"
    if(is.null(mlmConfig$numberOfIterations[i]) || is.na(mlmConfig$numberOfIterations[i]))
      mlmConfig$numberOfIterations[i] <- defNumberOfIterations
    if(is.null(mlmConfig$proportionOfTrainingSet[i]) || is.na(mlmConfig$proportionOfTrainingSet[i]))
      mlmConfig$proportionOfTrainingSet[i] <- defPercentageForTrainingSet

    
    
    mlmList <- c(mlmList, paste0(mlmConfig$shortName[i], ":" , mlmConfig$pretreatment[i], ":",
                                 mlmConfig$numberOfIterations[i], ":" ,mlmConfig$proportionOfTrainingSet[i], ":",
                                 mlmConfig$resampling[i],":", mlmConfig$tuneLength[i]))
  }
  
  config$machineLearningModels <- mlmList
  
  return(config)
  
}


#' readDatasetClass
#' @description reads, parses and performs some validity checks on the dataset.
#' Data manipulation is done if it is needed. For example if the number of features exceeds
#' 200, it is reduced by feature selection. Any name of feature is numeric, it is converted to
#' character value as it is required in some machine learning models.
#' supplied in json format as config file
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param dataFileName  dataFileName
#' @import openxlsx caret tools
#' @return dataFrame
#'
#' @examples
#' \dontrun{readConfigFileClass(configFile)}
#'

# Modified by Shintaro Kinoshita : add "metaFileName" and "bacterialName" arguments to "readData" function
readDatasetClass<-function(dataFileName, metaFileName, metaDataName){
  cat("readDataSet function is starting \n")
  fileExtension <- tolower(file_ext(dataFileName))
  # According to file extension different method is used to read data
  if(fileExtension == "xlsx")
    dataSet <- openxlsx::read.xlsx(dataFileName, sheet = 1, rowNames=TRUE, colNames = TRUE,)
  if(fileExtension == "csv")
    dataSet <- as.data.frame(read.csv(dataFileName, sep=",", header=TRUE, row.names=NULL))
  #modify column names
  colnames(dataSet) <- colnames(dataSet)[2:ncol(dataSet)]
  dataSet <- dataSet[,-ncol(dataSet)]
  # Make row names unique
  row_names <- make.unique(rownames(dataSet))
  rownames(dataSet) <- row_names
  emptyColumns <- colSums(is.na(dataSet) | dataSet == "") == nrow(dataSet)
  dataSet <- dataSet[, !emptyColumns]
  dataSet <- na.omit(dataSet)
  colnames(dataSet) <- gsub("X","", colnames(dataSet))
  
  dataSet <- dataSet[,2:ncol(dataSet)]

  #colnames(dataSet)=as.character(colnames(dataSet))
  
  # Modified by Lea Saxton : Read metadata
  rawMetaData <- read.csv( metaFileName, header = TRUE )

  # Define the regular expression pattern to match sample column names
  pattern <- "^[Ss]ample.*"
  
  # Identify columns that match the pattern in the metadata
  sampleColumn <- grepl(pattern, colnames(rawMetaData))
  # Get the column name(s) where sampleColumn is TRUE
  sampleColumnNames <- colnames(rawMetaData)[sampleColumn]
  # Remove the sampleColumn(s) from the data frame
  rawMetaData <- as.data.frame(rawMetaData[, !sampleColumn])
  colnames(rawMetaData) <- metaDataName
    # Make the sampleColumn the row names
    #rownames(rawMetaData) <- rawMetaData[[sampleColumn]]
    row_names2 <- make.unique(rownames(rawMetaData))
    rownames(rawMetaData) <- row_names2
    
  # Find common row names
  common_rows <- intersect(row.names(dataSet), row.names(rawMetaData))
  
  # Filter dataSet_removed to include only common rows
  dataSet <- dataSet[row.names(dataSet) %in% common_rows, ]
  
  # Filter rawMetaData to include only common rows
  rawMetaData <- as.data.frame(rawMetaData[row.names(rawMetaData) %in% common_rows, ])
  colnames(rawMetaData) <- metaDataName
  
  # Combine the datasets using cbind
  dataSet <- cbind(dataSet, rawMetaData)
  
  # Number of features is reduced by feature selection,
  # import in processing FTIR data
  if (ncol(dataSet) > 200) {
    cat("number Features > 200 \n")
    features <- selectFeaturesClass(dataSet, metaDataName)
    features <- as.data.frame(features)
    sensory <- dataSet[,metaDataName]
    dataSet <- dataSet[, colnames(dataSet) %in% colnames(features)]
    dataSet <- cbind(dataSet,sensory)
    cat("done")
  }
  
  # Check if the metadata contains the data named 'metaDataName'
  cat( paste0( "\nmetaDataName : ", metaDataName, "\n" ) )
  print(colnames(rawMetaData))
  if ( !any( names( rawMetaData ) == metaDataName ) ) {
    cat( "WARNING : ", "The data column named '", metaDataName, "' was not found in the metadata." )
    cat( "\nCheck the metadata content again.\n" )
    cat( paste0( "'", names( rawMetaData )[ length( rawMetaData ) ], "' in the metadata are used instead. \n\n" ) )
    metaDataName <- names( rawMetaData )[ length( rawMetaData ) ]
  }
  
  # Combine Dataset and metadata if not found
  if( !length( grep( metaDataName, names( dataSet ) ) ) > 0 && !is.null( metaFileName ) ) {
    metaData    <- data.frame( sensory = rawMetaData[ , metaDataName ] ) #; print( metaData )
    min_nrow    <- min( nrow( dataSet ), nrow( metaData ) ) #; print( min_nrow )
    dataSet     <- cbind( dataSet[ 1:min_nrow, ], sensory= metaData[ 1:min_nrow, ] )#; print( dataSet_mod )
    cat( paste0( "NOTE : NO '", metaDataName, "' data found in the original data. the first ", min_nrow, " rows in the metadata were combined to the original dataset." ) )
  }
  
  
  #cat( "\n" )
  #print( dataSet )
  
  return(dataSet)
  
}

createPerformanceStatisticsClass <- function(performanceResults, classificationParameterList){
  # AccList contains list of Accuracy for each iteration
  AccList <- vector(mode="list", length = classificationParameterList$numberOfIterations)
  
  AccList <- unlist(lapply(performanceResults, function(x) x$Accuracy))
  meanAcc <- round(mean(AccList), 4)
  cumulativeMeanAccList <- cumsum(AccList) / seq_along(AccList)
  names(cumulativeMeanAccList) <- seq_along(AccList)

  bestHyperParamsList <- NULL
  if(!is.null(performanceResults[[1]]$bestHyperParams))
    bestHyperParamsList <- unlist(lapply(performanceResults, function(x) x$bestHyperParams))
  
  classificationParameterList$methodWithDataScaling <- paste0(classificationParameterList$method, "(", classificationParameterList$pretreatment,  ")")
  
  cat(paste0(classificationParameterList$method, "(", classificationParameterList$pretreatment,  ") mean Accuracy: ", meanAcc, '\n'))
  
  # Result object is returned to run.regression function in regression.R, which contains whole performance information for the machine learning model
  result <- list("AccList"= AccList, "cumulativeMeanAccList" = cumulativeMeanAccList, "Accuracy" = meanAcc,
                 "bestHyperParamsList" = bestHyperParamsList, method = classificationParameterList$method, platform = classificationParameterList$platform,
                 "pretreatment" = classificationParameterList$pretreatment)
}

#' selectFeaturesClass
#' @description reduces the number of features in the dataset
#' by selecting the more important fautures
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param  dataSet  dataFrame object
#' @import Boruta
#' @return dataFrame
#'
#' @examples new dataset with reduced number of features
#' \dontrun{selectFeatures(dataSet, metaDataName)}

selectFeaturesClass <- function(dataSet, metaDataName) {
  cat('selectFeatures function is starting \n')
  cat("Dependent variable: ", metaDataName, "\n")
  
  # Check for missing data in the dependent variable
  if (any(is.na(dataSet[[metaDataName]]))) {
    cat("Missing data found in the dependent variable. Removing rows with missing values.")
    dataSet <- dataSet[complete.cases(dataSet), ]
  }
  
  # Perform Boruta search
  # It uses Random Forest model behind,
  # search top-down and eliminates the irrelevant features step-by-step progressively.
  
  # Perform Boruta search
  boruta_output <- Boruta(as.formula(paste(metaDataName, "~ .")), data = na.omit(dataSet), doTrace = 0)
  boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
  
  
  cat("Selected attributes:\n")
  print(boruta_signif)
  
  
  cat("Hello, the selectFeatures function is finished \n")
  
  return(dataSet)
}

#' removeRedundantFeatures
#' @description reduces the number of features in the dataset by
#' removing the reduntant during the variable selection process of data modeling.
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param  dataSet  dataFrame object
#' @import caret corrplot plyr
#' @return
#'
#' @examples new dataset with reduced number of features
#' \dontrun{removeRedundantFeatures(selectFeatures)}

removeRedundantFeatures<-function(dataSet){
  # Remove non-informative  features
  nzv <- caret::nearZeroVar(dataSet, saveMetrics= TRUE)
  dataSet <- dataSet[, !nzv$nzv]
  
  # Calculate correlation matrix
  descrCor <- cor(dataSet)
  
  # Finds higly correlated features
  highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)
  highlyCorCol <- colnames(dataSet)[highlyCorrelated]
  
  # Removes highly correlated features
  dat <- dataSet[, -which(colnames(dataSet) %in% highlyCorCol)]
  
  return(dat)
  
}

#' statsClassification
#' @description calculates basic statistics of a predicted model
#' by comparing to the observed data.
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param  predicted
#' @param  observed
#' @return data frame containing Accuracy, Bf, Af and Worst
#'
#' @examples
#' \dontrun{statsClassification(predicted, ovserved)}

statsClassification <- function(predicted, observed) {
  cat("statsClassification function is starting \n")
  
  # Calculate confusion matrix
  cm <- table(predicted, observed)
  
  # Calculate metrics
  accuracy <- sum(diag(cm)) / sum(cm)
  precision <- ifelse(colSums(cm) == 0, 0, diag(cm) / colSums(cm))
  recall <- ifelse(rowSums(cm) == 0, 0, diag(cm) / rowSums(cm))
  
  # Handle NaN values for precision and recall
  precision[is.nan(precision)] <- 0
  recall[is.nan(recall)] <- 0
  
  # Calculate F1 Score
  f1_score <- 2 * precision * recall / (precision + recall)
  
  # Handle NaN values for F1 Score
  f1_score[is.nan(f1_score)] <- 0
  
  # Create a data frame with the metrics
  metrics <- data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)
  return(round(metrics, 3))
}




#' saveResultClass
#' @description generates 'result.csv' contains the best statistics values in each models
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param  statsClass
#' @param  outputDir
#' @return null, generates 'result.csv' contains the best statistics values in each models
#'
#' @examples
#' \dontrun{saveResultClass(statsClass, outDir)}

saveResultClass <- function(statsClass, method, outputDir, platform) {
  cat("saveresultsClass function is starting \n")
  print(statsClass)
  # Calculate the means of each metric
  means <- colMeans(statsClass[, c("Accuracy", "Precision", "Recall", "F1_Score")])
  
  # Create a new data frame StatsClass with the means
  statsClass <- data.frame(Accuracy = means["Accuracy"],
                           Precision = means["Precision"],
                           Recall = means["Recall"],
                           F1_Score = means["F1_Score"])
  print(statsClass)
  # make a data frame which is added to 'result.csv'
  df <- data.frame(
    method = method,
    platform = platform,
    Acc = statsClass$Accuracy,
    Precision = statsClass$Precision,
    Recall = statsClass$Recall,
    F1 = statsClass$F1_Score
  )
  
  # Define filepath: Modify outputDir if required
  filepath <- file.path(outputDir, "result.csv")
  
  # Create 'result.csv' or append to it
  if (!file.exists(filepath)) {
    cat("\nNOTE: 'result.csv' does not exist, so it's newly created.\n")
    write.table(
      df,
      file = filepath,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = ","
    )
  } else {
    filedata <- read.table(filepath, header = TRUE, sep = ",")
    filedata <- as.data.frame(filedata)
    
    # Check if the same row already exists
    duplicate <- identical(filedata[nrow(filedata), ], df)
    
    if (!duplicate) {
      newdata <- rbind(filedata, df)
      write.table(
        newdata,
        file = filepath,
        append = FALSE,
        quote = FALSE,
        row.names = FALSE,
        col.names = TRUE,
        sep = ","
      )
    } else {
      cat("\nNOTE: The same row already exists in 'result.csv'. Skipping duplicate.\n")
    }
  }
}