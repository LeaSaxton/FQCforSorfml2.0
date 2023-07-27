#' Main function to calculate performance of Random forest model
#' @description this function calculates performance of Random forest model
#' through iterations and returns performance metrics.In each iteration different
#' partitioning is done on dataset to create training and validation datasets,
#' tuning is done on training dataset to find optimum mtry-value
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of Iterations to calculate performance
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
#' bestHyperParamsList: a list containing best mtry value and default number of trees
#' @import randomForest caret foreach
#'
#' @examples
#' \dontrun{randomForest.run(regressionParameterList)}
#dataset <- "/Users/shintarokinoshita/Desktop/FQC/FQC/input/config_air_test.js"
randomForest.run <- function(regressionParameterList){
        cat('randomForest.run \n')
        cat(regressionParameterList$pretreatment, '\n') # ;cat( str( regressionParameterList ) )
        dataSet_removed <- regressionParameterList$dataSet
        bacterialName <- regressionParameterList$bacterialName
        platformName <- regressionParameterList$platform
        #Ensuring the dataset does not containg NaN and missing values
        dataSet_removed <- na.omit(dataSet_removed)
        # Iterate over each element in the list
        for (i in seq_along(dataSet_removed)) {
          if (is.numeric(dataSet_removed[[i]])) {
            # Check for NaN values in numeric elements
            dataSet_removed[[i]] <- dataSet_removed[[i]][!is.nan(dataSet_removed[[i]])]
          }
        }
        # Create dataSet_TVC with the same row names as dataSet_removed
        dataSet_TVC <- data.frame(TVC = dataSet_removed[[bacterialName]])
        rownames(dataSet_TVC) <- row.names(dataSet_removed) # ;cat( str( dataSet_removed ) )
        dataSet_removed <- dataSet_removed[ ,colnames( dataSet_removed ) != "TVC" ] # ;cat( str( dataSet_TVC ) )
        # Find common row names
        common_rows <- intersect(row.names(dataSet_removed), row.names(dataSet_TVC))
        # Filter dataSet_removed to include only common rows
        dataSet_removed <- dataSet_removed[row.names(dataSet_removed) %in% common_rows, ]

        #Modified by Léa Saxton :
        if (regressionParameterList$pretreatment == "raw") {
          dataSet <- cbind(dataSet_removed, dataSet_TVC)
        } else {
          preProcValues <- preProcess(dataSet_removed, method = gePretreatmentVector(regressionParameterList$pretreatment))
          #preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
          dataSet <- cbind(dataSet_removed, dataSet_TVC) # ;cat( str( dataSet ) )
          regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
          #dataSet <- regressionParameterList$dataSet
        }
        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

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
                trainSet_y<-trainSet[,names(trainSet)=="TVC"]
                trainSet_x<-trainSet[,names(trainSet)!="TVC"]
                testSet <- dataSet[-trainIndexList[,i],]
                testSet_y<-testSet[,names(testSet)=="TVC"]
                testSet_x<-testSet[,names(testSet)!="TVC"]

                # Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
                tuningResult <- tuneRF(trainSet_x, trainSet_y, , ntreeTry=5000, stepFactor=1.1, improve=0.0000001,
                                       trace=TRUE, plot=TRUE, doBest=TRUE)

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("mtry"=tuningResult$mtry,"ntree"=tuningResult$ntree)

                # RandomForest model is created with the best hyperparameters for the current iterations
                modelFit <- randomForest( x = trainSet_x, y = trainSet_y, xtest = testSet_x, ytest = testSet_y,
                                          ntree = bestHyperParams$ntree, mtry = bestHyperParams$mtry, keep.forest = TRUE)

                # Using testSet svm model predicts TVC values
                predictedValues <- modelFit$test$predicted

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

        # Modified by Shinaro Kinoshita : Add statistics values into result.csv
        # statsReg will contains 'k value'
        bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
        statsReg <- cbind( statsReg, bestHyperParams ) # Then, combine 2 dataframes
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir, platformName, bacterialName)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}

randomforest.run.alternative <- function(dataAll){
        require(mlr)
        cat('run.rfr.1 \n')

        set.seed(90)
        trainIndex <- createDataPartition(dataAll$TVC, p = defProportion,
                                          list = FALSE, times = 1)
        trainSet <- dataAll[trainIndex,]
        testSet <- dataAll[-trainIndex,]

        #Create task
        rtask <- makeRegrTask(id = "TVC", data = trainSet, target = "TVC")

        #create RF learner
        learner <- makeLearner("regr.randomForest")
        #Tune the model

        mtry_lower <- sqrt(ncol(trainSet) -1)
        mtry_upper <- ncol(trainSet) -1

        forestParamSpace <- makeParamSet(
                makeIntegerParam("ntree", lower = 1000, upper = 1000), # Define hyperparameter space
                makeIntegerParam("mtry", lower = mtry_lower, upper = 12),
                makeIntegerParam("nodesize", lower = 1, upper = 5),
                makeIntegerParam("maxnodes", lower = 5, upper = 20))

        randSearch <- makeTuneControlRandom(maxit = 100)       # Define a random search method with 100 iterations
        #gridSearch <- makeTuneControlGrid()

        cvForTuning <- makeResampleDesc("CV", iters = 5)       # Define a 5-fold cross-validation strategy

        #Tune the hyperparameters
        tunedForestPars <- tuneParams(learner, task = rtask,
                                      resampling = cvForTuning,
                                      par.set = forestParamSpace,
                                      control = randSearch)
        tunedForestPars    # Print tuning results

        # train a final model to make a learner with the tuned hyperparameters
        tunedForest <- setHyperPars(learner, par.vals = tunedForestPars$x)
        tunedForestModel <- mlr::train(tunedForest, rtask)

        predicted <- predict(tunedForestModel, newdata = testSet )

        modelRMSE<-sqrt(mean((predicted$data[,"response"] - predicted$data[,"truth"])^2))
        modelRSquare <- RSquare(predicted$data[,"truth"], predicted$data[,"response"])

        return(list("RMSE" = modelRMSE, "RSquare" = modelRSquare))

}
