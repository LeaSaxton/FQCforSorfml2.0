#' run.analysis
#' @description calls each machine learning run methods according to
#' the method parameter in regressionParameterList
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param configParams  list containing parameters provided in config json file
#' by the user
#' @import foreach R.utils
#'
#' @examples
#' \dontrun{run.analysis(configParams)}

run.analysis.class <- function(configParams){
  fileList <- configParams$platformList$dataFileName
  metaList  <- configParams$platformList$metaFileName
  metaDataNameList <- configParams$platformList$typeMetaData
  platformList <- configParams$platformList$platformName
  mlmList <- configParams$machineLearningModels
  cat("########################\n#### START ANALYSIS ####\n########################\n\n")
  
  # Initialization of platformPerformanceResults
  platformPerformanceResults <- vector(mode="list", length = length(platformList))
  
  # platformPerformanceResults <- foreach(i=seq(1:length(platformList))) %dopar% {
  metaDataNameList <- metaDataNameList[!is.na(metaDataNameList)]
  for(i in 1:length(platformList)) {
    # arguments to readDataset function
    dataSet = readDatasetClass(fileList[i], metaList[i], metaDataNameList[i])
    print(metaDataNameList)
    bestMLM <- ""
    bestAcc <- 0

    if(configParams$createPCAPlots == TRUE)
      generatePCAPlotsClass(dataSet, configParams$outputDirectory, platformList[i])
    
    mlmPerformanceResults <- vector(mode="list", length = length(mlmList))
    
    # For each platforms and machine learning models following code is executed
    for(j in 1:length(mlmList)) {
      mlm <- mlmList[j]
      print(mlm)
      # classificationParameterList is creates as list object which carries necessary parameters for machine learning
      # models to run.
      # elements of classificationParameterList are set to default values if they are not supplied by the user.
      # default values are defined in utils.R
      classificationParameterList <- getClassificationParameters( mlm, dataSet, platformList[i], metaDataNameList[i])
      dataSet <- classificationParameterList$dataSet
      
      #Add configParams$outputDirectory to regressionParameterList
      classificationParameterList$outputDir <- configParams$outputDirectory
      
      mlmPerformanceResult <- run.classification(classificationParameterList)
      if(is.null(mlmPerformanceResult)){
        cat("For ", platformList[i], " Timeout Exception in ", classificationParameterList$method , "(",
            classificationParameterList$pretreatment, ")\n")
        
        mlmPerformanceResult <- list("Accuracy" = NA,method = classificationParameterList$method,
                                     pretreatment = classificationParameterList$pretreatment, platform = classificationParameterList$platform)
        mlmPerformanceResults[[j]]  <- mlmPerformanceResult
        next
        
      }
      
      # Through the loop best machine learning model is found according to Accuracy performance metric
      if(!is.null(mlmPerformanceResult) && mlmPerformanceResult$Accuracy > bestAcc){
        bestAcc <- mlmPerformanceResult$Accuracy
        bestMLM <- mlmPerformanceResult$method
      }
      # Machine learning model list updated
      # Each platform has got different mlmPerformanceResults list
      mlmPerformanceResults[[j]]  <- mlmPerformanceResult
    }
    
    
    # Best machine learning model for the platform is printed
    cat("For ", platformList[i], " best model is ", bestMLM , " with Accuracy: " , bestAcc,"\n")
    
    # Each item in platformPerformanceResults corresponds to one platform,
    # Associated mlmPerformanceResults(performance results of machine learning model list), bestMLM, bestRMSE, bestRSquare which have been
    # created with the previous loop are appended to platformPerformanceResults list.
    platformPerformanceResults[[i]] <- list("platform" = platformList[i], "bestMLM" = bestMLM, "bestAcc" = bestAcc,
                                            "mlmPerformanceResults" = mlmPerformanceResults )
    
  }
  # Accuracy_Statistics.csv file are created if createStatisticsFile parameter is set as TRUE in config file
  generateStatisticsClass(platformPerformanceResults, configParams$outputDirectory, configParams$createStatisticsFile)
  # Performance plots which shows RSquare and  RMSE means through number of iterations are created for each platform
  if(configParams$createPerformancePlots)
    generatePerformancePlotsClass(platformPerformanceResults, configParams$outputDirectory)
  
}

#' run.classification
#' @description calls each machine learning run methods according to
#' the method parameter in classificationParameterList
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param classificationParameterList  list containing parameters needed by machine
#' learning models to run
#'
#' @return a list which contains performance results for machine learning
#' model
#' @examples
#' \dontrun{run.classification(classficationParameterList)}

run.classification <- function(classificationParameterList){
  method<-classificationParameterList$method
  cat('run.classification is starting \n')
  cat(paste0("method name : ", method, "\n"))
  result<-NULL
  if(method == "SVR-Radial"){
    cat('svr.run is starting \n')
    regressionParameterList$kernel <- "radial"
    result<-svr.run(regressionParameterList)
    regressionParameterList<-within(regressionParameterList, rm(kernel))
  }
  if(method == "SVR-Polynomial"){
    cat('svr.run is starting \n')
    regressionParameterList$kernel <- "polynomial"
    result<-svr.run(regressionParameterList)
    regressionParameterList<-within(regressionParameterList, rm(kernel))
    
  }
  if(method == "KNN"){
    cat('knnClass.run is starting \n')
    result<-knnClass.run(classificationParameterList)
  }
  if(method == "RF"){
    cat('randomForestClass.run is starting \n')
    result<-randomForestClass.run(classificationParameterList)
  }
  if(method == "PLS" || method == "PCR"){
    cat(paste0("pls.pcr.run is starting for ", method ,"\n"))
    result<-pls.pcr.run(regressionParameterList)
  }
  if(method == "RT"){
    cat('regressionTree.run is starting \n')
    result<-regressionTree.run(regressionParameterList)
  }
  if(method == "NN"){
    cat('neuralNetwork.run is starting \n')
    result<-neuralNetwork.run(regressionParameterList)
  }
  
  # Linear regression models
  # Ordinary Least Squares Regression (OLS) or Stepwise Regression (SR)
  if(method == "OLS" || method == "SR"){
    cat('linearRegression.run is starting \n')
    result<-linearRegression.run(regressionParameterList)
    
  }
  # Regularized regression models() Ridge Regression, Lasso Regression
  if(method == "RR" || method == "LR" ){
    cat('regularizedRegression.run is starting \n')
    result<-regularizedRegression.run(regressionParameterList)
  }
  
  # Elastic Net Regression
  if(method == "ER" ){
    cat('elasticRegression.run is starting \n')
    result<-elasticRegression.run(regressionParameterList)
  }
  
  # XGBoost
  if(method == "XGBoost" ){
    cat('XGBoost.run is starting \n')
    result<-XGBoost.run(regressionParameterList)
  }
  
  return(result)
  
}

#' makeRankAccuracy
#' @description make rankAccuracy.csv in /HEATMAPS/ dir.
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param outputDirectory  output directory
#' @return
#' @export
#'
#' @examples
#' \dontrun{makeRankAccuracy(outputDirectory)}

makeRankAccuracy <- function(configParams) {
  # Define output directory, platform name, and bacterial name
  outDir <- configParams$outputDirectory
  platformName <- configParams$platformList$platformName
  
  # Define dirpath: Modify outputDir if required
  dirpath <- ifelse(substr(outDir, nchar(outDir), nchar(outDir)) != "/", paste0(outDir, "/"), outDir)
  
  # Create HEATMAP dir
  dirpath_heatmaps <- paste0(dirpath, "HEATMAPS", "/")
  if (!dir.exists(dirpath_heatmaps)) {
    cat("\nNOTE: The directory '", dirpath_heatmaps, "' does not exist.\n")
    cat("      So it was newly created.\n\n")
    dir.create(dirpath_heatmaps)
  }
  
  # Get result data from 'result.csv'
  dirpath_result <- paste0(dirpath, "result.csv")
  result_data <- read.table(dirpath_result, header = TRUE, sep = ",")

  # Sort descending by Accuracy values
  result_data <- result_data[order(-result_data$Acc), ]
  
  # Create rank column
  result_data <- transform(result_data, rank = 1:nrow(result_data))
  
  # Reorder columns with rank as the first column
  result_data <- result_data[, c("rank", names(result_data)[-ncol(result_data)])]
  
  # Create full path of rankRmse.csv
  filepath_heatmaps <- paste0(dirpath_heatmaps, "rankAccuracy.csv")
  
  # Save rankRmse.csv
  cat("\nNOTE: 'rankAccuracy.csv' is being created or overwritten.\n")
  write.table(
    result_data,
    file = filepath_heatmaps,
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    sep = ","
  )
}


#' assess.quality.class
#' @description assess.quality.class
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param configFile  configFile
#' @import caret doParallel
#' @return
#' @export
#'
#' @examples
#' \dontrun{assess.quality(configFile)}

assess.quality.class <- function(configFile=configFile){
  cat("assess quality function for classification  is starting \n")
  # config file should be provided by the use
  if(is.null(configFile))
    stop("ERROR configuration file should be defined!")
  
  # config file which contains user-defined parameters for the application is parsed.
  # After parsing, configParams object is created as a list of following elements
  # platform list, machine learning models and their parameters, output directory, createStatisticsFile,
  # createPerformancePlots, createPCAPlots
  
  configParams = readConfigFileClass(configFile)

  # in run.analysis foreach method is called as parallel
  # registerDoParallel(cores=4)
  
  run.analysis.class(configParams)
  
  # Create rank Accuracy file
  makeRankAccuracy(configParams)
  
}