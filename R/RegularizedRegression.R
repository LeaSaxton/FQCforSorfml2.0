#' Main function to calculate performance of regularized regression model
#' (lasso or ridge regressions)
#' @description After pretreatment on dataset this function calculates performance
#' of regularized regression models (lasso or ridge) through iterations and returns
#' performance metrics.In each iteration different partitioning is done on dataset
#' to create training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum alpha and lambda values
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
#' bestHyperParamsList: a list containing best alpha and lambda values for each iteration
#' @import caret glmnet foreach
#'
#' @examples
#' \dontrun{regularizedRegression.run(regressionParameterList)}
#'
regularizedRegression.run <- function(regressionParameterList){

        # In regression, it is often recommended to scale the features to make it easier to interpret the intercept term.
        # Scaling type is supplied by the user
        cat(regressionParameterList$pretreatment)
        if(regressionParameterList$pretreatment =="raw"){
          dataSet <- regressionParameterList$dataSet
        }else{
          preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
          regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
          dataSet <- regressionParameterList$dataSet
        }
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
                dummies <- dummyVars(TVC ~ ., data = dataSet)

                trainDummies = predict(dummies, newdata = trainSet)
                testDummies = predict(dummies, newdata = testSet)

                trainMx = as.matrix(trainDummies)
                trainTarget = trainSet$TVC

                testMx = as.matrix(testDummies)
                testTarget = testSet$TVC

                alpha = -1
                if(regressionParameterList$method == "RR" )
                        alpha = 0
                if(regressionParameterList$method == "LR" )
                        alpha = 1

                set.seed(123)

                cvFit <- cv.glmnet(trainMx, trainTarget, alpha = alpha)

                # Fit the final model on the training data
                modelFit <- glmnet(trainMx, trainTarget, alpha = alpha, lambda = cvFit$lambda.min)

                bestHyperParams <- list(lambda=cvFit$lambda.min)

                # Using testSet the model predicts TVC values
                predictedValues <- predict(modelFit,  newx = testMx)

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

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)

                # Modified by Shintaro Kinoshita : append model to the list
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
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
