#' Main function to calculate performance of neural network model
#' @description After pretreatment on dataset this function calculates performance
#' of neural network model
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance metrics
#' RMSE: mean RMSE of all iterations
#' RSquare: mean RSquare of all iterations
#' @import caret neuralnet
#'
#' @examples
#' \dontrun{neuralNetwork.run(regressionParameterList)}

# Identify and remove outliers using Tukey's fences
remove_outliers <- function(dataSet, bacterialName, multiplier = 1.5) {
        variable <- dataSet[[bacterialName]]
        q1 <- quantile(variable, 0.25)
        q3 <- quantile(variable, 0.75)
        iqr <- q3 - q1

        fence_low <- q1 - multiplier * iqr
        fence_high <- q3 + multiplier * iqr
        print(fence_high)
        print(fence_low)

        data_cleaned <- dataSet[!(variable < fence_low | variable > fence_high), ]

        return(data_cleaned)
}

neuralNetwork.run <- function(regressionParameterList){
        cat(regressionParameterList$pretreatment)
        cat(gePretreatmentVector(regressionParameterList$pretreatment), "\n")
        dataSet_removed <- regressionParameterList$dataSet
        bacterialName <- regressionParameterList$bacterialName
        platformName <- regressionParameterList$platform
        # Modified by Lea Saxton : Ensuring the dataset does not containg NaN and missing values
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
        if (regressionParameterList$pretreatment == "raw") {
                dataSet <- cbind(dataSet_removed, dataSet_TVC)
        } else {
                preProcValues <- preProcess(dataSet_removed, method = gePretreatmentVector(regressionParameterList$pretreatment))
                dataSet <- cbind(dataSet_removed, dataSet_TVC)
                #regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        }
        set.seed(90)
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
                # Impute missing values using k-nearest neighbor imputation
                preProcValues <- preProcess(trainSet, method = "knnImpute")
                trainSet <- predict(preProcValues, trainSet)
                testSet <- predict(preProcValues, testSet)
                #Remove outliers from a specific variable in your dataset
                trainSet <- remove_outliers(trainSet, "TVC", 1.5)
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
                if ("TVC" %in% colnames(trainSet)) {
                         print("Column named TVC exists in trainSet")
                } else {
                        print("Column named TVC does not exist in trainSet")
                }
                modelFit <- neuralnet(TVC ~ . ,
                               data = as.data.frame(trainSet),
                               hidden=10, threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7)
                predictedValues <- predict(modelFit, as.matrix(testSet))

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # Check if this model has the best RMSE so far
                if (RMSE < bestRMSE) {
                        bestRMSE  <- RMSE
                        bestModel <- modelFit
                        bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                        statsReg <- statsRegression( predictedValues, testSet$TVC )
                }

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)

                # Modified by Shintaro Kinoshita : append model to the list
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
        saveResult(statsReg, regressionParameterList$method, regressionParameterList$outputDir, platformName, bacterialName)

        return(createPerformanceStatistics(performanceResults, regressionParameterList))

}
