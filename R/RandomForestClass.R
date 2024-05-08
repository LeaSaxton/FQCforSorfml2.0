#' Main function to calculate performance of Random forest model
#' @description this function calculates performance of Random forest model
#' through iterations and returns performance metrics.In each iteration different
#' partitioning is done on dataset to create training and validation datasets,
#' tuning is done on training dataset to find optimum mtry-value
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param classificationParameterList  a list which contains
#' number_of_iterations: number of Iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' RMSEList: a list which contains RMSE of each iteration
#' Accuracy: mean accuracy of all iterations
#' bestHyperParamsList: a list containing best mtry value and default number of trees
#' @import randomForest caret foreach
#'
#' @examples
#' \dontrun{randomForestClass.run(classificationParameterList)}

randomForestClass.run <- function(classificationParameterList){
    cat('randomForestClass.run \n')
    cat(classificationParameterList$pretreatment, '\n')
    dataSet_removed <- classificationParameterList$dataSet
    metaDataName <- classificationParameterList$metaDataName
    platformName <- classificationParameterList$platform
    outDir <- classificationParameterList$outputDir
    # Ensuring the dataset does not containg NaN and missing values
    dataSet_removed <- na.omit(dataSet_removed)
    # Iterate over each element in the list
    for (i in seq_along(dataSet_removed)) {
      if (is.numeric(dataSet_removed[[i]])) {
        # Check for NaN values in numeric elements
        dataSet_removed[[i]] <- dataSet_removed[[i]][!is.nan(dataSet_removed[[i]])]
      }
    }
    # Create dataSet_sensory with the same row names as dataSet_removed
    dataSet_sensory <- data.frame(sensory = dataSet_removed[[metaDataName]])
    rownames(dataSet_sensory) <- row.names(dataSet_removed)
    dataSet_removed <- dataSet_removed[ ,colnames( dataSet_removed ) != "sensory" ]
    # Find common row names
    common_rows <- intersect(row.names(dataSet_removed), row.names(dataSet_sensory))
    # Filter dataSet_removed to include only common rows
    dataSet_removed <- dataSet_removed[row.names(dataSet_removed) %in% common_rows, ]

    #Applying the pretreatment
    if (classificationParameterList$pretreatment == "raw") {
      dataSet <- cbind(dataSet_removed, dataSet_sensory)
      } else if (classificationParameterList$pretreatment == "pareto"){
        dataSet_removed<-apply(dataSet_removed, 2, function(y) ( (y -mean(y)) / ( sqrt(sd(y)) ) ) )
        dataSet <- cbind(dataSet_removed, dataSet_sensory)
      }else if (classificationParameterList$pretreatment == "vast") {
        dataSet_removed<-apply(dataSet_removed, 2, function(y) ( ( (y- mean(y))*mean(y)) / ((sd(y))^2) ) )
        dataSet <- cbind(dataSet_removed, dataSet_sensory)
      }else if (classificationParameterList$pretreatment == "level"){
        dataSet_removed<-apply(dataSet_removed, 2, function(y) ( (y -mean(y)) / (mean(y)) ) )
        dataSet <- cbind(dataSet_removed, dataSet_sensory)
      } else if (classificationParameterList$pretreatment == "norm"){
        dataSet_removed<-apply(dataSet_removed, 2, function(y) ((y - min(y))/ (max(y)-min(y))))
        dataSet <- cbind(dataSet_removed, dataSet_sensory)
        }else {
      preProcValues <- preProcess(dataSet_removed, method = getPretreatmentVectorClass(classificationParameterList$pretreatment))
      dataSet <- cbind(dataSet_removed, dataSet_sensory)
      classificationParameterList$dataSet <- predict(preProcValues, classificationParameterList$dataSet)
    }
    performanceResults <- vector(mode="list", length = classificationParameterList$numberOfIterations)

    set.seed(1821)

    # Convert the target variable to a factor
    dataSet$sensory <- as.factor(dataSet$sensory)
    # Partition data into training and test set
    trainIndexList <- createDataPartition(dataSet$sensory, p = classificationParameterList$percentageForTrainingSet,
                                          list = FALSE, times = classificationParameterList$numberOfIterations)

    #Define variants for the best models
    bestAcc <- 0
    bestModel <- NULL

    #Define the statistics regression list
    statsClass <- NULL

    # Define an empty data frame for bestHyperParams
    bestHyperParams <- data.frame(bestK = numeric(0))

    # Define empty vectors to store mtry and ntree values
    mtry_values <- c()
    ntree_values <- c()

    for(i in 1:classificationParameterList$numberOfIterations) {
      # training set and test set are created
      trainSet <- dataSet[trainIndexList[,i],]
      trainSet_y <- trainSet[,names(trainSet)=="sensory"]
      trainSet_x <- trainSet[,names(trainSet)!="sensory"]
      testSet <- dataSet[-trainIndexList[,i],]
      testSet_y <- testSet[,names(testSet)=="sensory"]
      testSet_x <- testSet[,names(testSet)!="sensory"]

      # Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
      tuningResult <- tuneRF(trainSet_x, trainSet_y, , ntreeTry=5000, stepFactor=1.1, improve=0.0000001,
                             trace=TRUE, plot=TRUE, doBest=TRUE)#5000 very high number for tuning, same concerns as with the regression counterpart

      # list of bestHyperParams is created with best hyperparameters
      bestHyperParams <- list("mtry"=tuningResult$mtry,"ntree"=tuningResult$ntree)


      # RandomForest model is created with the best hyperparameters for the current iterations
      modelFit <- randomForest(x = trainSet_x, y = trainSet_y, xtest = testSet_x, ytest = testSet_y,
                               ntree = bestHyperParams$ntree, mtry = bestHyperParams$mtry, keep.forest = TRUE)

      # Using testSet, the random forest model predicts class labels
      predictedValues <- predict(modelFit, testSet_x)

      # Calculate accuracy of the predictions
      Accuracy <- Accuracy(testSet$sensory, predictedValues)

      if (Accuracy > bestAcc) {
                bestAcc  <- Accuracy
                bestModel <- modelFit
                # Add the bestK value to the bestHyperParams data frame
                bestHyperParams <- rbind(bestHyperParams, data.frame(bestK = modelFit$bestTune[1, 1]))
                # Store mtry and ntree values from tuning into the vectors
                mtry_values <- tuningResult$mtry
                ntree_values <- tuningResult$ntree
                # Calculate the confusion matrix
                conf_matrix <- confusionMatrix(predictedValues, testSet$sensory)
                #plot the confusion matrix
                confusion_matrix <- confusion_matrix(conf_matrix, platformName, outDir, "RF" )
                statsClass <- statsClassification( predictedValues, testSet$sensory )
      }

        performanceResults[[i]] <- list("Accuracy" = Accuracy, "bestHyperParams" = bestHyperParams)
      }
      # Make "class" dir to save RDA files
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
      #cat( paste0( name_path, "\n" ) )

      # Check if the "class" file exists, if not, create
      if ( dir.exists( name_path ) == FALSE ) {
        cat( "\n\nNOTE : The dir 'class' does not exist so it was newly created.\n" )
        dir.create( name_path, showWarnings = FALSE )
      }

      #Save the best model and its hyperparameters
      name_platform <- classificationParameterList$platform
      name_model    <- classificationParameterList$method
      name_file     <- paste0( name_platform,'_', name_model, ".rda" )
      name_path_rds <- paste0( name_path, "/", name_file )
      #saveRDS( bestModel, file = name_file )
      save( bestModel, file = name_path_rds )

      # Save the associated Accuracy in a file
      name_file     <- paste0( name_platform, "_", name_model, ".txt" )
      name_path_txt <- paste0( name_path, "/", name_file )
      #write.table( bestAcc, file = name_file, row.names = FALSE, col.names = FALSE )
      write.table( bestAcc, file = name_path_txt, row.names = FALSE, col.names = FALSE )

      # Add statistics values into result.csv
      # statsClass will contains 'k value'
      #bestHyperParams <- data.frame( bestK = c( 0 ) ) # Dummy dataframe for 'k value'
      statsClass <- cbind(statsClass, mtry = mtry_values, ntree = ntree_values)
      saveResultClass(statsClass, classificationParameterList$method, classificationParameterList$outputDir, platformName)

    return(createPerformanceStatisticsClass(performanceResults, classificationParameterList ))

}

