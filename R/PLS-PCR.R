#' Main function to calculate  performance of Partial least squares regression
#' and PCA regression
#' @description This function calculates performance of Partial least squares
#' regression and PCA regression through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets.
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' method: method name as PLS or PCA
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' RMSEList: a list which contains RMSE of each iteration
#' cumulativeRMSEList: a list which contains cumulative RMSE mean in
#' each iteration#'
#' RMSE: mean RMSE of all iterations
#' RSquareList: a list which contains RSquare of each iteration
#' cumulativeRSquareList : a list which contains cumulative RSquare mean in
#' each iteration
#' RSquare: mean RSquare of all iterations
#' @import caret pls foreach
#'
#' @examples
#' \dontrun{pls.pcr.run(regressionParameterList)}

pls.pcr.run<- function(regressionParameterList){
        cat('pls.pcr.run \n')
        bacterialName <- regressionParameterList$bacterialName
        dataSet_removed <- regressionParameterList$dataSet
        platformName <- regressionParameterList$platform
        cat(regressionParameterList$pretreatment, "\n")
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
        } else {
        #preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
          preProcValues <- preProcess(dataSet_removed, method = gePretreatmentVector(regressionParameterList$pretreatment))
          dataSet <- cbind(dataSet_removed, dataSet_TVC)
          regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
          #dataSet <- regressionParameterList$dataSet
        }
        # Function to replace 0 values with column median
        #replace_zeros_with_median <- function(x) {
                #if (any(x == 0)) {
                        #median_value <- median(x[x != 0])
                        #x[x == 0] <- median_value
                #}
                #return(x)
        #}
        # Apply the function to each column of trainSet
        #dataSet <- as.data.frame(lapply(dataSet, replace_zeros_with_median))
        set.seed(1821)
        # Partition data into training and test set
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
                # training set and test set are created
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
                if (nrow(trainSet) > 0) {
                # Create model using PCR or PLS
                  if (regressionParameterList$method=="PCR"){
                          modelFit <- pcr(TVC~., data=trainSet)
                  }else if (regressionParameterList$method=="PLS"){
                          modelFit <- plsr(TVC ~ . , data=trainSet, scale=TRUE, validation="CV")
                  }
                }
                # Using testSet pls or pcr model predicts TVC values
                predictedValues <- predict(modelFit, testSet, ncomp=2)
                #print(predictedValues)
                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)
                # Check if this model has the best RMSE so far
                if (!is.nan(RMSE) && !is.nan(RSquare)) {
                        # Check if this model has the best RMSE so far
                        if (RMSE < bestRMSE) {
                                bestRMSE <- RMSE
                                bestModel <- modelFit
                                bestHyperParams <- list("k" = modelFit$bestTune[1, 1])
                                statsReg <- statsRegression(predictedValues, testSet$TVC)
                        }

                # pls or pcr model model with the performance metrics for the current iteration is appended to the  model list
                # the model list contains all models for all iterations
                        performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)
                } else {
                        cat("NaN values found in RMSE or RSquare calculation. Skipping model for iteration ", i, "\n")
                }
                # Modified by Shintaro Kinoshita : append model to the list
                #modelFit$call$formula <- as.character(modelFit$call$formula)
                #all_models[[i]] <- modelFit
        }
        # Modified by Shintaro Kinoshita : Make "reg" dir to save RDS files
        name_path <- regressionParameterList$outputDir
        # Modified by Lea Saxton : Extract the desired part of the path and define a new path to save the models
        extracted_path <- sub("/analysis/.*", "", name_path)
        # Create a new parameter with the name of the folder where the models will be saved
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
        # statsReg will contains 'k value'
        bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir, platformName, bacterialName )
        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
