#' Main function to calculate performance of XGBoost model
#' @description This function calculates performance XGBoost(gradient boosting)
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
#' @import caret xgboost DiagrammeR
#'
#' @examples
#' \dontrun{XGBoost.run(regressionParameterList)}

XGBoost.run <- function(regressionParameterList){
        cat('run.XGBoost \n')
        if (regressionParameterList$pretreatment == "raw") {
            dataSet <- regressionParameterList$dataSet
        }else{
            preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
            regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
            dataSet <- regressionParameterList$dataSet
        }
        set.seed(90)
        trainIndex <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                          list = FALSE, times = 1)

        train = dataSet[trainIndex, ]
        test = dataSet[-trainIndex, ]

        #define predictor and response variables in training set
        train_x = data.matrix(train[, -1])
        train_y = train[,1]

        #define predictor and response variables in testing set
        test_x = data.matrix(test[, -1])
        test_y = test[, 1]

        #define final training and testing sets
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)

        #defining a watchlist
        watchlist = list(train=xgb_train, test=xgb_test)

        # Modified by Shintaro Kinoshita : List of models for RDS
        #all_models <- list()

        # Modified by Lea Saxton : Define variants for the best models
        #bestRMSE  <- Inf
        #bestModel <- NULL

        #fit XGBoost model and display training and testing data at each iteration
        model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

        min<- min(model$evaluation_log$train_rmse)
        best_nrounds<-min(which(model$evaluation_log$train_rmse==min))

        #define final model
        model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 500, verbose = 0)

        #use model to make predictions on test data
        pred_y = predict(model_xgboost, xgb_test)

        modelRMSE <- RMSE(test_y, pred_y)
        modelRMSE <- round(modelRMSE, 4)
        modelRSquare <- RSQUARE(test_y, pred_y)
        modelRSquare <- round(modelRSquare, 4)
        statsReg <- statsRegression( pred_y, test_y )

        result <- list("RMSE" = modelRMSE, "RSquare" = modelRSquare, method = regressionParameterList$method, platform = regressionParameterList$platform,
                       "pretreatment" = regressionParameterList$pretreatment)

        # Modified by Shintaro Kinoshita : append model to the list
        #all_models[[0]] <- model

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
        save( model_xgboost, file = name_path_rds )

        # Modified by Lea Saxton : Save the associated RMSE in a file
        name_file     <- paste0( name_platform, "_",name_bacteria,"_", name_model, ".txt" )
        name_path_txt <- paste0( name_path, "/", name_file )
        #write.table( modelRMSE, file = name_file, row.names = FALSE, col.names = FALSE )
        write.table( modelRMSE, file = name_path_txt, row.names = FALSE, col.names = FALSE )

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

        return (result)
}


run.XGBoost_v2 <- function(method,dataAll){
        require(xgboost)
        require(DiagrammeR)
        cat('run.XGBoost \n')

        set.seed(90)
        trainIndex <- createDataPartition(dataAll$TVC, p = 0.3,
                                          list = FALSE, times = 1)

        # one hot encode all categorical variables
        dummy <- dummyVars(" ~ .", data = dataAll)
        need_data <- data.frame(predict(dummy, newdata = dataAll))
        y_label <- need_data$TVC
        need_data <- need_data %>% select(-TVC)
        need_data <- data.frame(TVC = y_label, need_data)

        train_data <- need_data[trainIndex,]
        val_data <- need_data[-trainIndex,]

        # create tuning grid
        grid_default <- expand.grid(nrounds = c(50, 75, 100, 150, 200, 250),
                                    max_depth = c(2, 3, 4, 5),
                                    eta = c(0.05, 0.1, 0.15),
                                    gamma = c(0),
                                    colsample_bytree = c(0.7),
                                    min_child_weight = c(5),
                                    subsample = c(0.6))

        # set random seed
        set.seed(1234)
        # train XGBoost model
        xgb_model <- caret::train(formula(train_data),
                           data=train_data,
                           tuneGrid = grid_default,
                           method = "xgbTree",
                           metric = "RMSE",
                           verbosity = 0)

        xgb.plot.tree(model = xgb_model$finalModel, trees = 1)



        predictedValues <- predict(xgb_model, newdata=val_data)

        result <- evalMetrics(val_data$TVC, predictedValues)


        return (result)

}
