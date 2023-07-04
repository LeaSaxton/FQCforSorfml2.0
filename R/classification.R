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
  print(fileList)
  print(metaList)
  print(metaDataNameList)
  print(platformList)
  print(mlmList)
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

    if(configParams$createPCAPlots == TRUE)
      generatePCAPlots(dataSet, configParams$outputDirectory, platformList[i], bacterialNameList[i])
    
    mlmPerformanceResults <- vector(mode="list", length = length(mlmList))
    
    # For each platforms and machine learning models following code is executed
    for(j in 1:length(mlmList)) {
      mlm <- mlmList[j]
      print(mlm)
      # RegressionParameterList is creates as list object which carries necessary parameters for machine learning
      # models to run.
      # elements of RegressionParameterList are set to default values if they are not supplied by the user.
      # default values are defined in utils.R
      classificationParameterList <- getClassificationParameters( mlm, dataSet, platformList[i], metaDataNameList[i])
      dataSet <- classificationParameterList$dataSet
      
      #Add configParams$outputDirectory to regressionParameterList
      classificationParameterList$outputDir <- configParams$outputDirectory
      print(classificationParameterList$outputDir)
      #cat( str( regressionParameterList ) )
      
      # according to method parameter of regressionParameterList different machine learning model is executed in  run.regression
      # mlmPerformanceResult <- tryCatch(
      #                 {
      #                         #withTimeout(run.regression(regressionParameterList), timeout = 10000, onTimeout = "silent")
      #                         run.regression(regressionParameterList)
      #                 },
      #                 error=function(cond) {
      #                         message(cond)
      #                         print(sys.calls())
      #                         return(NULL)
      #                 }
      #         )
      
      mlmPerformanceResult <- run.classification(classificationParameterList)
      stop()
      if(is.null(mlmPerformanceResult)){
        cat("For ", platformList[i], " Timeout Exception in ", regressionParameterList$method , "(",
            regressionParameterList$pretreatment, ")\n")
        
        mlmPerformanceResult <- list("RMSE" = NA,"RSquare" = NA, method = regressionParameterList$method,
                                     pretreatment = regressionParameterList$pretreatment, platform = regressionParameterList$platform)
        mlmPerformanceResults[[j]]  <- mlmPerformanceResult
        next
        
      }
      
      # Through the loop best machine learning model is found according to RMSE performance metric
      if(!is.null(mlmPerformanceResult) && mlmPerformanceResult$RMSE < bestRMSE){
        bestRSquare <- mlmPerformanceResult$RSquare
        bestRMSE <- mlmPerformanceResult$RMSE
        bestMLM <- mlmPerformanceResult$method
      }
      # Machine learning model list updated
      # Each platform has got different mlmPerformanceResults list
      mlmPerformanceResults[[j]]  <- mlmPerformanceResult
    }
    
    
    # Best machine learning model for the platform is printed
    cat("For ", platformList[i], " best model is ", bestMLM , " with RMSE: " , bestRMSE,  " and R-squared: ", bestRSquare, "\n")
    
    # Each item in platformPerformanceResults corresponds to one platform,
    # Associated mlmPerformanceResults(performance results of machine learning model list), bestMLM, bestRMSE, bestRSquare which have been
    # created with the previous loop are appended to platformPerformanceResults list.
    platformPerformanceResults[[i]] <- list("platform" = platformList[i], "bestMLM" = bestMLM, "bestRMSE" = bestRMSE, "bestRSquare" = bestRSquare,
                                            "mlmPerformanceResults" = mlmPerformanceResults )
    
  }
  # RSquare_Statistics.csv and RMSE_Statistics.csv files are created if createStatisticsFile parameter is set as TRUE in config file
  generateStatistics(platformPerformanceResults, configParams$outputDirectory, configParams$createStatisticsFile, bacterialNameList)
  
  # Performance plots which shows RSquare and  RMSE means through number of iterations are created for each platform
  if(configParams$createPerformancePlots)
    generatePerformancePlots(platformPerformanceResults, configParams$outputDirectory)
  
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
    cat('knn.run is starting \n')
    result<-knn.run(regressionParameterList)
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

#' makeRankRmse
#' @description make rankRmse.csv in /HEATMAPS/ dir.
#' @author Shintaro Kinoshita \email{shintaro.kinoshita.584@@cranfield.ac.uk}
#' @param outputDirectory  output directory
#' @return
#' @export
#'
#' @examples
#' \dontrun{makeRankRmse(outputDirectory)}

#Modified by Lea Saxton : Changing the structure of the rankRmse.csv file
makeRankRmse <- function(configParams) {
  # Define output directory, platform name, and bacterial name
  outDir <- configParams$outputDirectory
  platformName <- configParams$platformList$platformName
  bacterialName <- configParams$platformList$bacterialName
  
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
  result_data <- as.data.frame(result_data)
  
  # Sort ascending in RMSE values
  result_data <- result_data[order(result_data$RMSE), ]
  
  # Create full path of rankRmse.csv
  filepath_heatmaps <- paste0(dirpath_heatmaps, "rankRmse.csv")
  
  # Create lines for each rank
  rank_lines <- character(nrow(result_data))
  for (i in 1:nrow(result_data)) {
    rank_line <- paste("rank", i,
                       "\\",
                       "\\shortstack{",
                       platformName,
                       " / ",
                       bacterialName,
                       "}",
                       "\\",
                       "RMSE: \\",
                       result_data[i, 3],
                       "\\",
                       "Acc: \\",
                       result_data[i, 4],
                       "%",
                       "\\",
                       "$\\Delta_{max}$: \\",
                       result_data[i, 5],
                       "\\",
                       "$A_{f}$: \\",
                       result_data[i, 6],
                       "\\",
                       "$B_{f}$: \\",
                       result_data[i, 7],
                       "\\",
                       "\"",
                       sep = "")
    rank_lines[i] <- rank_line
  }
  
  # Save rank lines into "rankRmse.csv"
  cat(paste(rank_lines, collapse = "\n"), file = filepath_heatmaps)
  cat("\n", file = filepath_heatmaps, append = TRUE)
  
  # Print rank lines
  print(rank_lines)
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
  
  # Modified by Shintaro Kinoshita : Create rank RMSE file
  makeRankRmse(configParams)
  
}