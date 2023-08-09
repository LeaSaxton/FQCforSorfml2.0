#' Main function to calculate performance of Linear Discriminant Analysis model for classification
#' @description After pretreatment on dataset this function calculates performance
#' of PLSDA model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets.
#' @author Lea saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param classificationParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' AccList: a list which contains RMSE of each iteration
#' cumulativeAccuracyList: a list which contains cumulative RMSE mean in
#' each iteration
#' Accuracy: mean accuracy of all iterations
#' @import caret
#'
#' @examples
#' \dontrun{LDA.run(classificationParameterList)}
LDA.run <- function(classificationParameterList){
          cat('LDA.run \n')
          dataSet_removed <- classificationParameterList$dataSet
          platformName <- classificationParameterList$platform
          metaDataType <- classificationParameterList$metaDataName
          outDir <- classificationParameterList$outputDir
          #Ensuring the dataset does not containg NaN and missing values
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
          if (metaDataType %in% colnames(dataSet_removed)) {
            dataSet_sensory <- data.frame(sensory = dataSet_removed[, metaDataType])
            rownames(dataSet_sensory) <- row.names(dataSet_removed)
            dataSet_removed <- dataSet_removed[, !(colnames(dataSet_removed) == metaDataType)]
          } else {
            cat("The metaDatType column does not exist in the dataSet_removed data frame.\n")
          }
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
          #Converting the sensory column as factor
          dataSet$sensory <- factor(dataSet$sensory)
          set.seed(1821)
          # Partition data into training and test set
          trainIndexList <- createDataPartition(dataSet$sensory, p = classificationParameterList$percentageForTrainingSet,
                                                list = FALSE, times = classificationParameterList$numberOfIterations)
          
          performanceResults <- vector(mode="list", length = classificationParameterList$numberOfIterations)
          
          # List of models for RDS
          #Define variants for the best models
          bestAcc  <- 0
          bestModel <- NULL
          
          #Define the statistics classification list
          statsClass <- NULL
          for(i in 1:classificationParameterList$numberOfIterations) {
            # training set and test set are created
            trainSet <- dataSet[trainIndexList[,i],]
            testSet <- dataSet[-trainIndexList[,i],]
            
            # Check if there are two columns named "TVC" in trainSet
            if (sum(colnames(trainSet) == "sensory") == 2) {
              cat("there are 2 columns 'sensory' in trainSet \n")
              # Remove one of the "sensory" columns
              trainSet <- trainSet[, -which(colnames(trainSet) == "sensory")[1]]
            }
            
            # Check if there are two columns named "sensory" in testSet
            if (sum(colnames(testSet) == "sensory") == 2) {
              cat("there are 2 columns 'sensory' in testSet \n")
              # Remove one of the "sensory" columns
              testSet <- testSet[, -which(colnames(testSet) == "sensory")[1]]
            }
            # Before training resampling method is set as 5 fold cross validation
            trControl <- trainControl(method = "cv", number = 5)
            # Train the Linear Discriminant Analysis (LDA) model
            modelFit <- caret::train(sensory ~ ., method = 'lda', data = trainSet, trControl = trControl)
            
            # Using testSet knn model predicts sensory values
            predictedValues <- predict(modelFit, testSet)
            
            # Calculate accuracy of the predictions
            Accuracy <- Accuracy(testSet$sensory, predictedValues)
            
            # Check if this model has the best Accuracy so far
            if (Accuracy > bestAcc) {
              bestAcc <- Accuracy
              bestModel <- modelFit
              # Calculate the confusion matrix
              conf_matrix <- confusionMatrix(predictedValues, testSet$sensory)
              #plot the confusion matrix
              confusion_matrix <- confusion_matrix(conf_matrix, platformName, outDir, "LDA" )
              statsClass <- statsClassification( predictedValues, testSet$sensory )
            }
            
            performanceResults[[i]] <- list("Accuracy" = Accuracy)
            
          }
          
          # Make "class" dir to save RDS files
          name_path <- classificationParameterList$outputDir
          #Extract the desired part of the path and define a new path to save the models
          extracted_path <- sub("/analysis/.*", "", name_path)
          # Create a new parameter with the name of the folder where the models will be saved
          folder_models <- "models"
          # Changing the path
          name_path <- file.path(extracted_path, folder_models)
          cat("New path :", name_path, "\n")
          if ( substr( name_path, nchar( name_path ), nchar( name_path ) ) == "/" ) {
            name_path <- paste0( name_path, "/class" )
          } else {
            name_path <- paste0( name_path, "/class" )
          }
          
          #Check if the "class" directory exists, if not, create
          if ( dir.exists( name_path ) == FALSE ) {
            cat( "\n\nNOTE : The dir 'class' does not exist so it was newly created.\n" )
            dir.create( name_path, showWarnings = FALSE )
          }
          #Save the best model and its hyperparameters
          name_platform <- classificationParameterList$platform
          name_model    <- classificationParameterList$method
          name_file     <- paste0( name_platform, "_", name_model, ".rda" )
          name_path_rds <- paste0( name_path, "/", name_file )
          save( bestModel, file = name_path_rds )
          
          # Save the associated Accuracy in a file
          name_file     <- paste0( name_platform, "_", name_model, ".txt" )
          name_path_txt <- paste0( name_path, "/", name_file )
          write.table( bestAcc, file = name_path_txt, row.names = FALSE, col.names = FALSE )
          
          #Add statistics values into result.csv
          cat("statsClass \n")
          print(statsClass)
          saveResultClass(statsClass, classificationParameterList$method, classificationParameterList$outputDir, platformName)
          
          return(createPerformanceStatisticsClass(performanceResults, classificationParameterList))
}