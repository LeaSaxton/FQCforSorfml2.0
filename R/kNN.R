#' Main function to calculate performance of k-nearest neighbors model
#' @description After pretreatment on dataset this function calculates performance
#' of k-nearest neighbors model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum k-value
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
#' bestHyperParamsList: a list containing best k-value for each iteration
#' @import caret
#'
#' @examples
#' \dontrun{knn.run(regressionParameterList)}

knn.run <- function(regressionParameterList){
        cat('knn.run \n')

        #print( regressionParameterList )

        dataSet_removed <- regressionParameterList$dataSet
        dataSet_TVC     <- data.frame( TVC = dataSet_removed$TVC )
        dataSet_removed <- dataSet_removed[ ,colnames( dataSet_removed ) != "TVC" ]

        # knn as a distance based algorithm is affected by the scale of the variables.
        # Scaling type is supplied by the user
        #preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        preProcValues <- preProcess(dataSet_removed, method = gePretreatmentVector(regressionParameterList$pretreatment))
        dataSet <- cbind(dataSet_removed, dataSet_TVC)
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

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
        statsReg <- NULL #statsReg <- statsRegression( predictedValues, testSet$TVC )

        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                # Before training resampling method is set as 5 fold cross validation
                trControl <- trainControl(method = "cv", number = 5)

                # model is trained with trainSet using 5 fold cross validation
                # as tuneGrid parameter possible k values is supplied,  train function finds the optimum k-value.
                modelFit <- caret::train(TVC ~ . , method='knn', data=trainSet,
                                         tuneGrid=expand.grid(k=1:maxK), trControl=trControl)

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("k"=modelFit$bestTune[1,1])

                # Using testSet knn model predicts TVC values
                predictedValues <- predict(modelFit, testSet)

                #cat( paste0( "\n\ntestSet     : ", str( testSet ), "\n" ) )
                #cat( paste0( "testSet$TVC : ", str( testSet$TVC ), "\n" ) )
                #cat( paste0( "is.null(testSet$TVC) : ", is.null( testSet$TVC ), "\n\n" ) )

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE <- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # Check if this model has the best RMSE so far
                if (RMSE < bestRMSE) {
                        bestRMSE  <- RMSE
                        bestModel <- modelFit
                        bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                        statsReg <- statsRegression( predictedValues, testSet$TVC )
                }

                performanceResults[[i]] <- list( "RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)

                # Modified by Shintaro Kinoshita : append model to the list
                #modelFit$call$formula <- as.character(modelFit$call$formula)
                #all_models[[i]] <- modelFit
        }

        # Modified by Shintaro Kinoshita : Make "temp" dir to save RDS files
        name_path <- regressionParameterList$outputDir
        if ( substr( name_path, nchar( name_path ), nchar( name_path ) ) == "/" ) {
                name_path <- paste0( name_path, "temp" )
        } else {
                name_path <- paste0( name_path, "/temp" )
        }
        #cat( paste0( name_path, "\n" ) )

        # Modified by Shintaro kinoshita : check if the "temp" file exists, if not, create
        if ( dir.exists( name_path ) == FALSE ) {
                cat( "\n\nNOTE : The dir 'temp' does not exist so it was newly created.\n" )
                dir.create( name_path, showWarnings = FALSE )
        }

        # Modified by Lea Saxton : Save the best model and its hyperparameters
        name_platform <- regressionParameterList$platform
        name_model    <- regressionParameterList$method
        name_bacteria <- regressionParameterList$bacterialName
        name_file     <- paste0( name_platform, "_", name_bacteria, "_", name_model, ".rda" ) # 'platform'_'model'_reg.rda
        name_path_rds <- paste0( name_path, "/", name_file )
        #saveRDS( bestModel, file = name_file )
        save( bestModel, file = name_path_rds )

        # Modified by Lea Saxton : Save the associated RMSE in a file
        name_file     <- paste0( name_platform, "_", name_bacteria, "_", name_model, ".txt" ) # 'platform'_'model'_RMSE.rds
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
        bestHyperParams <- data.frame( bestK = bestHyperParams$k ) # Make a dataframe for 'k value'
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
