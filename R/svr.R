#' Main function to calculate performance of svr model
#' @description This function calculates performance of svr model through iterations
#' and returns performance metrics.In each iteration different partitioning is done
#' on dataset to create training and validation datasets, cross-validation tuning is
#' done on training dataset to find optimum cost,gamma, epsilon values
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of Iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' kernel: svr kernel methods as radial or polynomial
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
#' bestHyperParamsList: a list containing best cost, gamma and epsilon values
#' @import caret foreach e1071
#'
#' @examples
#' \dontrun{svr.run(regressionParameterList)}

svr.run <- function(regressionParameterList){
        cat('svr.run \n')
        dataSet_removed <- regressionParameterList$dataSet
        bacterialName <- regressionParameterList$bacterialName
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
        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # List of models for RDS
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
                # This generic function tunes hyperparameters of statistical methods using a grid search over supplied parameter ranges.
                # tune function uses tune.control object created with fix sampling
                tuningResult <- e1071::tune(svm, trainSet, trainSet$TVC,
                                            ranges = list(cost = defCostRange, gamma = defGammaRange, epsilon = defEpsilonRange),
                                            tunecontrol = tune.control(sampling = "fix")
                )
                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("cost" = tuningResult$best.parameters["cost"][1,1],
                                        "gamma" = tuningResult$best.parameters["gamma"][1,1],
                                        "epsilon" = tuningResult$best.parameters["epsilon"][1,1])

                # svr model is created with the best hyperparameters for the current iteration
                modelFit <- svm(trainSet, trainSet$TVC, type="eps-regression",
                                kernel=regressionParameterList$kernel, cost=bestHyperParams$cost, gamma =bestHyperParams$gamma,
                                epsilon =bestHyperParams$epsilon)
                # Using testSet svr model predicts TVC values
                predictedValues <- predict(modelFit , testSet)
                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # svr model with the performance metrics for the current iteration is appended to the svr model list
                # svr model list contains all svr models for all iterations
                performanceResults[[i]] <- list( "RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)
                modelFit$call$formula <- as.character( modelFit$call$formula )
                if (!is.null(modelFit$x.scale$"scaled:center")) {
                  names_original <- attr(modelFit$x.scale$"scaled:center", "names")
                  names_scaled <- gsub('X', '', names_original)
                  attr(modelFit$x.scale$"scaled:center", "names") <- names_scaled
                }
                #cat( str( attr( modelFit$x.scale$"scaled:center", "names" ) ) )


                if (!is.null(modelFit$x.scale$"scaled:scale")) {
                  names_original <- attr(modelFit$x.scale$"scaled:scale", "names")
                  names_scaled <- gsub('X', '', names_original)
                  attr(modelFit$x.scale$"scaled:scale", "names") <- names_scaled
                }
                #cat( str( attr( modelFit$x.scale$"scaled:center", "names" ) ) )

                                # Check if this model has the best RMSE so far
                if (RMSE < bestRMSE) {
                        bestRMSE  <- RMSE
                        bestModel <- modelFit
                        bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                        statsReg <- statsRegression( predictedValues, testSet$TVC )
                }

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
        name_file     <- paste0( name_platform, "_", name_model, ".rda" )
        name_path_rds <- paste0( name_path, "/", name_file )
        #saveRDS( bestModel, file = name_file )
        save( bestModel, file = name_path_rds )

        # Modified by Lea Saxton : Save the associated RMSE in a file
        name_file     <- paste0( name_platform, "_",name_bacteria,"_", name_model, ".txt" )
        name_path_txt <- paste0( name_path, "/", name_file )
        #write.table( bestRMSE, file = name_file, row.names = FALSE, col.names = FALSE )
        write.table( bestRMSE, file = name_path_txt, row.names = FALSE, col.names = FALSE )

        # Modified by Shintaro Kinoshita : Create RDS file
        #name_platform <- regressionParameterList$platform
        #name_model    <- regressionParameterList$method
        #name_file     <- paste0( name_platform, "_", name_model, ".rds" )
        #name_path     <- paste0( "machineLearning/models/", name_file )
        #saveRDS( all_models, file = name_path )
        #saveRDS( all_models, file = name_file )

        # Modified by Shinaro Kinoshita : Add statistics values into result.csv
        bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}

