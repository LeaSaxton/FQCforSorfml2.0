#' Cost-complexity post pruning of regression treed
#' @description This method post-prunes the regression tree by cost-complexiy
#' to overcome issue of over-fitting to training set
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param modelTree regression tree model created on training set before pruning
#' @param testSet validation dataset
#' @return a list which contains
#' RMSEList:  a list of RMSE values calculated for each cost-complexity
#' bestPrunedTree: the best pruned tree model found according to RMSE metric
#' RMSE: RMSE value of the best pruned tree
#' RSquare: RSquare value of the best pruned tree
#' bestCp: cost comlexity of the best pruned tree
#' @import rpart
#'
#' @examples
#' \dontrun{pruneTree(modelTree, testSet )}

pruneTree <- function(modelTree, testSet) {
  cat("pruneTree function is starting \n")
  RMSEListForCpList <- c()
  bestRMSE <- Inf  # Set initial value to positive infinity
  bestRSquare <- 0
  cpSequence <- seq(from = 0.01, to = 0.1, by = 0.01)
  bestPrunedTree <- NULL
  bestCp <- 0
  statsReg <- NULL
  for (cp in cpSequence) {
    prunedTree <- prune(modelTree, cp = cp)
    predicted <- predict(prunedTree, testSet, type = "vector")
    modelRMSE <- RMSE(testSet$TVC, predicted)
    modelRSquare <- RSQUARE(testSet$TVC, predicted)
    RMSEListForCpList <- c(RMSEListForCpList, modelRMSE)

    if (modelRMSE < bestRMSE) {
      cat("We are in the loop \n")
      bestRMSE <- modelRMSE
      bestPrunedTree <- prunedTree
      bestRSquare <- modelRSquare
      bestCp <- cp
      statsReg <- statsRegression(predicted, testSet$TVC)
    }
  }
  print(modelRMSE)
  print(bestRMSE)
  print(statsReg)
  names(RMSEListForCpList) <- cpSequence
  return(list(
    "RMSEList" = RMSEListForCpList,
    "RMSE" = bestRMSE,
    "bestPrunedTree" = bestPrunedTree,
    "RSquare" = bestRSquare,
    "bestCp" = bestCp,
    "statsReg" = statsReg
  ))
}
#' @description This method creates combined names for the column of the results RMSE and RMSEsquared
#' to overcome issue of duplicated column name
#' @author Saxton Lea \email{lea.saxton.831@@cranfield.ac.uk}
#' @param nameList platform names
#' @param testSet validation dataset
#' @return the new name
#' @import rpart
#' @examples
#' \dontrun{makeUniqueNames(nameList, bacterialName )}
makeUniqueNames <- function(namesList, bacterialName) {
  uniqueNames <- paste(namesList, "_", bacterialName, sep = "")
  return(uniqueNames)
}

#' Main function to calculate  performance of Regression tree using bagging
#' @description This function calculates performance of Regression tree through
#' iterations and returns performance metrics.In each iteration different
#' partitioning is done on dataset to create training and validation datasets.
#' The model tree is post-prunes by cost-complexiy to overcome issue of
#' over-fitting to training set in each iteration.
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' RMSEList: a list which contains RMSE of each iteration
#' cumulativeRMSEList: a list which contains cumulative RMSE mean in
#' each iteration
#' RMSE: mean RMSE of all iterations
#' RSquareList: a list which contains RSquare of each iteration
#' cumulativeRSquareList : a list which contains cumulative RSquare mean in
#' each iteration
#' RSquare: mean RSquare of all iterations
#' @import rpart foreach
#'
#' @examples
#' \dontrun{regressionTree.run(regressionParameterList)}
#'
regressionTree.run <- function(regressionParameterList){

        cat('regressionTree.run \n')
        cat(regressionParameterList$pretreatment, '\n')
        # pretreatment method is overriden to no_pretreatment in case it has been set by user
        # there is no need for data pretreatment  before regression tree
        bacterialName <- regressionParameterList$bacterialName
        dataSet_removed <- regressionParameterList$dataSet
        platformName <- regressionParameterList$platform
        cat(regressionParameterList$pretreatment)
        if (bacterialName %in% colnames(dataSet_removed)) {
          dataSet_TVC <- data.frame(TVC = dataSet_removed[, bacterialName])
          rownames(dataSet_TVC) <- row.names(dataSet_removed)
          dataSet_removed <- dataSet_removed[, !(colnames(dataSet_removed) == bacterialName)]
        } else {
          cat("The bacterialName column does not exist in the dataSet_removed data frame.\n")
        }
        # Find common row names
        common_rows <- intersect(row.names(dataSet_removed), row.names(dataSet_TVC))
        # Filter dataSet_removed to include only common rows
        dataSet_removed <- dataSet_removed[row.names(dataSet_removed) %in% common_rows, ]
        if (regressionParameterList$pretreatment == "raw") {
          dataSet <- cbind(dataSet_removed, dataSet_TVC)
        }else{
          preProcValues <- preProcess(dataSet_removed, method = gePretreatmentVector(regressionParameterList$pretreatment))
          dataSet <- cbind(dataSet_removed, dataSet_TVC)
          regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
          #dataSet <- regressionParameterList$dataSet
        }
        set.seed(1821)
        print(dataSet)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # Modified by Shintaro Kinoshita : List of models for RDS
        #all_models <- list()

        # Modified by Lea Saxton : Define variants for the best models
        bestRMSE  <- Inf
        bestModel <- NULL

        # Modified by Shintaro Kinoshita : Define the statistics regression list
        statsReg <- NULL

        for(i in 1:regressionParameterList$numberOfIterations) {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]
                # Check if there are two columns named "TVC" in trainSet
                if (sum(colnames(trainSet) == "TVC") == 2) {
                  cat("there are 2 columns 'TVC' in trainSet \n")
                  # Remove one of the "TVC" columns
                  trainSet <- trainSet[, -which(colnames(trainSet) == "TVC")[1]]
                }
                # Check if there are two columns named "TVC" in testSet
                if (sum(colnames(testSet) == "TVC") == 2) {
                  cat("there are 2 columns 'TVC' in testSet \n")
                  # Remove one of the "TVC" columns
                  testSet <- testSet[, -which(colnames(testSet) == "TVC")[1]]
                }
                ##Decision trees
                modelTree <- rpart(trainSet$TVC ~ ., data=trainSet)

                #function to check if any rows have missing values and remove them if necessary
                testSet <- testSet[complete.cases(testSet), ]
                # cost-complexity post-pruning
                pruneResult <- pruneTree(modelTree,testSet)

                RMSE <- pruneResult$RMSE
                RSquare <- pruneResult$RSquare

                print(RMSE)
                print(RSquare)

                # Check if this model has the best RMSE so far
                if (RMSE < bestRMSE) {
                        bestRMSE  <- RMSE
                        bestModel <- modelTree
                        bestHyperParams <- list("k"=modelTree$bestTune[1,1])
                        #statsReg <- statsRegression( predictedValues, testSet$TVC )
                        statsReg <- pruneResult$statsReg
                }

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)

                # Modified by Shintaro Kinoshita : append model to the list
                #all_models[[i]] <- modelTree
        }

        # Modified by Shintaro Kinoshita : Make "reg" dir to save RDS files
        name_path <- regressionParameterList$outputDir
        # Modified by Lea Saxton : Extract the desired part of the path and define a new path to save the model
        extracted_path <- sub("/analysis/.*", "", name_path)
        # Defining a new parameter with the name of the desired folder where the models will be saved
        folder_models <- "models"
        # Changing the path
        name_path <- file.path(extracted_path, folder_models)
        cat("New path :", name_path, "\n")
        if ( substr( name_path, nchar( name_path ), nchar( name_path ) ) == "/" ) {
                name_path <- paste0( name_path, "reg" )
        } else {
                name_path <- paste0( name_path, "/reg" )
        }
        #cat( paste0( name_path, "\n" ) )
        # Modified by Shintaro kinoshita : check if the "reg" file exists, if not, create
        if ( dir.exists( name_path ) == FALSE ) {
                cat( "\n\nNOTE : The dir 'reg' does not exist so it was newly created.\n" )
                dir.create( name_path, showWarnings = FALSE )
        }

        # Modified by Lea Saxton : Save the best model and its hyperparameters
        name_platform <- regressionParameterList$platform
        name_model    <- regressionParameterList$method
        name_bacteria <- regressionParameterList$bacterialName
        name_file     <- paste0( name_platform, "_",name_bacteria,"_", name_model, ".rda" )
        name_path_rds <- paste0( name_path, "/", name_file )
        #saveRDS( bestModel, file = name_file )
        save( bestModel, file = name_path_rds )
        # Modified by Lea Saxton : Save the associated RMSE in a file
        name_file     <- paste0( name_platform, "_",name_bacteria,"_", name_model, ".txt" )
        name_path_txt <- paste0( name_path, "/", name_file )
        #write.table( bestRMSE, file = name_file, row.names = FALSE, col.names = FALSE )
        write.table( bestRMSE, file = name_path_txt, row.names = FALSE, col.names = FALSE )
        # Modified by Shintaro Kinoshita : create RDS file
        #name_platform <- regressionParameterList$platform
        #name_model    <- regressionParameterList$method
        #name_file     <- paste0( name_platform, "_", name_model, ".rds" )
        #name_path     <- paste0( "machineLearning/models/", name_file )
        #saveRDS( all_models, file = name_path )
        #saveRDS( all_models, file = name_file )

        # Modified by Shinaro Kinoshita : Add statistics values into result.csv
        bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
        cat('statsReg \n')
        print(statsReg)
        cat("bestHyperParams \n")
        print(bestHyperParams)
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir, platformName, bacterialName)
        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
