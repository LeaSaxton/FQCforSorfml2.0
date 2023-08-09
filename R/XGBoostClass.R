#' Main function to calculate performance of XGBoost model for classification tasks
#' @description This function calculates performance for XGBoost(gradient boosting)
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param classificationParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance metrics
#' AccList: a list which contains RMSE of each iteration
#' cumulativeAccuracyList: a list which contains cumulative Accuracy mean in
#' each iteration
#' Accuracy: mean accuracy of all iterations
#' @import caret xgboost DiagrammeR
#'
#' @examples
#' \dontrun{XGBoostClass.run(classificationParameterList)}

XGBoostClass.run <- function(classificationParameterList){
        cat('XGBoostClass.run \n')
        platformName <- classificationParameterList$platform
        metaDataType <- classificationParameterList$metaDataName
        outDir <- classificationParameterList$outputDir
        dataSet_removed <- classificationParameterList$dataSet
        dataSet_removed$sensory <- factor(dataSet_removed$sensory)
        print(length(levels(dataSet_removed$sensory)))
        # Impute missing values using k-nearest neighbor imputation
        preProcValues <- preProcess(dataSet_removed, method = "knnImpute")
        dataSet_removed <- predict(preProcValues, dataSet_removed)
        # Iterate over each element in the list
        for (i in seq_along(dataSet_removed)) {
                if (is.numeric(dataSet_removed[[i]])) {
                        # Check for NaN values in numeric elements
                        dataSet_removed[[i]] <- dataSet_removed[[i]][!is.nan(dataSet_removed[[i]])]
                }
        }
        # Create dataSet_sensory with the same row names as dataSet_removed
        dataSet_sensory <- data.frame(sensory = dataSet_removed[, metaDataType])
        rownames(dataSet_sensory) <- row.names(dataSet_removed)
        dataSet_removed <- dataSet_removed[ ,colnames( dataSet_removed ) != "sensory" ]
        # Find common row names
        common_rows <- intersect(row.names(dataSet_removed), row.names(dataSet_sensory))
        # Filter dataSet_removed to include only common rows
        dataSet_removed <- dataSet_removed[row.names(dataSet_removed) %in% common_rows, ]
        #Applying the pretreatment
        if (classificationParameterList$pretreatment == "raw") {
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        } else if (classificationParameterList$pretreatment == "pareto"){
                dataSet_removed<-apply(dataSet_removed, 2, function(y) ( (y -mean(y)) / ( sqrt(sd(y)) ) ) )
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        }else if (classificationParameterList$pretreatment == "center") {
                dataSet_removed <- apply(dataSet_removed, 2, function(y) (y - mean(y)))
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        }else if (classificationParameterList$pretreatment == "vast") {
                dataSet_removed<-apply(dataSet_removed, 2, function(y) ( ( (y- mean(y))*mean(y)) / ((sd(y))^2) ) )
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        }else if (classificationParameterList$pretreatment == "level"){
                dataSet_removed<-apply(dataSet_removed, 2, function(y) ( (y -mean(y)) / (mean(y)) ) )
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        } else if (classificationParameterList$pretreatment == "norm"){
                dataSet_removed<-apply(dataSet_removed, 2, function(y) ((y - min(y))/ (max(y)-min(y))))
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
        }else {
                preProcValues <- preProcess(dataSet_removed, method = getPretreatmentVectorClass(classificationParameterList$pretreatment))
                dataSet <- cbind(dataSet_removed, sensory = dataSet_sensory)
                classificationParameterList$dataSet <- predict(preProcValues, classificationParameterList$dataSet)
        }
        dataSet$sensory <- factor(dataSet$sensory)
        #Converting the sensory column as factor
        set.seed(90)
        trainIndex <- createDataPartition(dataSet$sensory, p = classificationParameterList$percentageForTrainingSet,
                                          list = FALSE, times = 1)

        train = dataSet[trainIndex, ]
        test = dataSet[-trainIndex, ]
        #define predictor and response variables in training set
        train_x = data.matrix(train[, 1:length(train)-1])
        train_y = train[,length(train)]

        #define predictor and response variables in testing set
        test_x = data.matrix(test[, 1:length(test)-1])
        test_y = test[, length(test)]

        #define final training and testing sets
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)

        #defining a watchlist
        watchlist = list(train=xgb_train, test=xgb_test)

        # Fit XGBoost model and display training and testing data at each iteration
        model = xgb.train(
                data = xgb_train, watchlist = watchlist,
                max.depth = 3,
                nrounds = classificationParameterList$numberOfIterations,
                eval_metric = "logloss"
        )
        print(model)

        # Get the best number of rounds based on the evaluation metric (logloss in this case)
        best_nrounds <- which.min(model$evaluation_log$test_logloss)

        # Define final model with the best number of rounds
        model_xgboost = xgboost(
                data = xgb_train,
                max.depth = 3,
                nrounds = best_nrounds,
                verbose = 0
        )

        # Use model to make predictions on test data
        pred_test <- predict(model_xgboost, xgb_test)

        # Clip predicted values to the range [1, 3]
        pred_test <- pmin(pmax(pred_test, 1), 3)

        # Round predicted values to the nearest integers (since XGBoost returns continuous values)
        pred_test <- round(pred_test)

        # Create a factor variable for predictions with levels 1, 2, and 3
        pred_y <- as.factor(pred_test)
        levels(pred_y) <- c("1", "2", "3")  # Manually set the levels to match the class labels

        print(pred_y)

        Accuracy <- Accuracy(test_y, pred_y)
        print(Accuracy)

        Accuracy <- round(Accuracy, 4)
        # Calculate the confusion matrix
        conf_matrix <- confusionMatrix(pred_y, test$sensory)
        #plot the confusion matrix
        confusion_matrix <- confusion_matrix(conf_matrix, platformName, outDir, "XGBoost" )

        statsClass <- statsClassification( pred_y, test_y )

        result <- list("Accuracy" = Accuracy, method = classificationParameterList$method, platform = classificationParameterList$platform,
                       "pretreatment" = classificationParameterList$pretreatment)

        #Make "class" dir to save RDS files
        name_path <- classificationParameterList$outputDir
        #Extract the desired part of the path and define a new path to save the models
        extracted_path <- sub("/analysis/.*", "", name_path)
        # Create a new parameter with the name of the folder where the models will be saved
        folder_models <- "models"
        # Changing the path
        name_path <- file.path(extracted_path, folder_models)
        cat("New path :", name_path, "\n")
        if ( substr( name_path, nchar( name_path ), nchar( name_path ) ) == "/" ) {
                name_path <- paste0( name_path, "class" )
        } else {
                name_path <- paste0( name_path, "/class" )
        }

        #Check if the "class" file exists, if not, create
        if ( dir.exists( name_path ) == FALSE ) {
                cat( "\n\nNOTE : The dir 'class' does not exist so it was newly created.\n" )
                dir.create( name_path, showWarnings = FALSE )
        }

        #Save the best model and its hyperparameters
        name_platform <- classificationParameterList$platform
        name_model    <- classificationParameterList$method
        name_file     <- paste0( name_platform, "_",name_model, ".rda" )
        name_path_rds <- paste0( name_path, "/", name_file )
        save( model_xgboost, file = name_path_rds )

        #Save the associated Accuracy in a file
        name_file     <- paste0( name_platform,"_", name_model, ".txt" )
        name_path_txt <- paste0( name_path, "/", name_file )
        write.table( Accuracy, file = name_path_txt, row.names = FALSE, col.names = FALSE )

        #Add statistics values into result.csv
        bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
        statsClass <- cbind( statsClass, bestHyperParams ) # Then, combine 2 dataframes
        saveResultClass(statsClass, classificationParameterList$method, classificationParameterList$outputDir, platformName)

        return (result)
}
