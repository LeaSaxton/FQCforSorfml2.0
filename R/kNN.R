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
        bacterialName <- regressionParameterList$bacterialName
        platformName <- regressionParameterList$platform
        # Modified by Lea Saxton : Ensuring the dataset does not containg NaN and missing values
        dataSet_removed <- na.omit(dataSet_removed)
        emptyColumns <- colSums(is.na(dataSet_removed) | dataSet_removed == "") == nrow(dataSet_removed)
        dataSet_removed <- dataSet_removed[, !emptyColumns]
        # Iterate over each element in the list
        # Check if column name is "NA" and remove it if exists
        if ("NA" %in% colnames(dataSet_removed)) {
          dataSet_removed <- dataSet_removed[, colnames(dataSet_removed) != "NA"]
        }
        for (i in seq_along(dataSet_removed)) {
          if (is.numeric(dataSet_removed[[i]])) {
            # Check for NaN values in numeric elements
            dataSet_removed[[i]] <- dataSet_removed[[i]][!is.nan(dataSet_removed[[i]])]
          }
        }
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

        # knn as a distance based algorithm is affected by the scale of the variables.
        # Scaling type is supplied by the user
        #preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        if (regressionParameterList$pretreatment == "raw") {
          dataSet <- cbind(dataSet_removed, dataSet_TVC)
        }else{
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
                #print the last ten columns of the dataset
                num_names <- 10

                last_column_names <- tail(colnames(testSet), num_names)
                print(last_column_names)

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
                # Before training resampling method is set as 5 fold cross validation
                trControl <- trainControl(method = "cv", number = 5)
                # model is trained with trainSet using 5 fold cross validation
                # as tuneGrid parameter possible k values is supplied,  train function finds the optimum k-value.
                modelFit <- caret::train(TVC ~ . , method='knn', data=trainSet,
                                         tuneGrid=expand.grid(k=1:maxK), trControl=trControl)
                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                # Get column names of testSet and trainSet
                testSet_columns <- colnames(testSet)
                trainSet_columns <- colnames(trainSet)

                # Display the last 10 rows of the last 10 columns
                num_rows <- 10
                num_cols <- 10

                last_rows <- tail(testSet, num_rows)
                last_cols <- tail(testSet, num_cols)
                print(last_rows[, tail(colnames(last_rows), num_cols)])

                last_rows2 <- tail(trainSet, num_rows)
                last_cols2 <- tail(trainSet, num_cols)
                print(last_rows2[, tail(colnames(last_rows2), num_cols)])

                cat("dim testSet: \n")
                print(dim(testSet))
                cat("dim trainSet: \n")
                print(dim(trainSet))

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

        # Modified by Shintaro kinoshita : check if the "red" file exists, if not, create
        if ( dir.exists( name_path ) == FALSE ) {
                cat( "\n\nNOTE : The dir 'reg' does not exist so it was newly created.\n" )
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
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir,platformName, bacterialName)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
