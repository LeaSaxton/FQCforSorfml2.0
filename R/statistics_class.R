#' generateStatisticsClass
#' @description generates Accuracy statistics report for each platform
#' and machine learning models
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param platformPerformanceResults list of machine learning performance results
#' for each platform
#' @param outputDir output directory name provided by the use in config json file
#' @param createStatisticsFile boolean value indicating whether to create statistics
#' file or not
#' @import gplots dplyr
#'
#' @examples
#' \dontrun{generateStatisticsClass(platformPerformanceResults, outputDir, createStatisticsFile)}
#'
generateStatisticsClass <- function(platformPerformanceResults, outputDir, createStatisticsFile){

  mlmLongDesc = list("NN" = "Neural Network", "SVR-Radial" = "SVR-Radial", "SVR-Polynomial" = "SVR-Polynomial",
                     "KNN" = "k-nearest neighbors", "RF" = "Random Forest",
                     "PLS" = "Partial least squares Regression", "PCR" = "PCA Regression", "OLS"= "Ordinary Least Squares Regression" ,
                     "SR" = "Stepwise Regression", "RR" ="Ridge Regression","LR" = "Lasso Regression", "ER" = "Elastic Regression",
                     "XGBoost" = "XGBoost","RT"= "Regression Tree"
  )

  cat("########################\n####  ML PERFORMANCE RESULT####\n########################\n\n")

  mlmShortNameList <- unlist(lapply(platformPerformanceResults[[1]]$mlmPerformanceResults, function(x) x$method))
  pretreatmentList <- unlist(lapply(platformPerformanceResults[[1]]$mlmPerformanceResults, function(x) x$pretreatment))
  mlmLongDescList <- unlist(mlmLongDesc[mlmShortNameList])
  methodNameWithDataPretreatment <- paste0(mlmShortNameList, "(", pretreatmentList, ")")
  platformList <- unlist(lapply(platformPerformanceResults, function(x) x$platform))
  Accdf <- data.frame(mlmShortNameList)

  for(platformPerformanceResult in platformPerformanceResults){

    AccListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$Accuracy))
    Accdf <- cbind(Accdf, AccListForMLM)

  }

  if(nrow(Accdf) == 0)
    return()

  cat("\n\nAccuracy FOR ML METHODS\n\n")
  Accdf <- as.data.frame(Accdf)
  print( "Accdf_01:" ); print( Accdf )
  print( "methodNameWithDataPretreatment:" );
  print( methodNameWithDataPretreatment )
  if (any(duplicated(methodNameWithDataPretreatment))) {
          print("Duplicate method names found, adjusting...")
          methodNameWithDataPretreatment <- make.unique(methodNameWithDataPretreatment)
  }
  # Remove missing values from methodNameWithDataPretreatment
  methodNameWithDataPretreatment <- methodNameWithDataPretreatment[!is.na(methodNameWithDataPretreatment)]
  rownames(Accdf) <- methodNameWithDataPretreatment
  colnames(Accdf) <- c("methodName",platformList)
  print(Accdf)
  Accdf$ML_Means <- round(rowMeans(Accdf[2:ncol(Accdf)], na.rm=TRUE), 4)
  Accdf$ML_Means <- round(rowMeans(Accdf[2:ncol(Accdf)], na.rm = TRUE), 4)
  Accdf <- rbind(Accdf, c(NA, round(colMeans(Accdf[2:ncol(Accdf)], na.rm = TRUE), 4)))
  rownames(Accdf)[nrow(Accdf)] <- "Platform_Means"
  # Calculate Accdf_ForHeatMap with unique ML methods and maximum Accuracy for each
  Accdf_ForHeatMap <- as.data.frame(Accdf %>% group_by(methodName) %>% filter(ML_Means == max(ML_Means)))
  Accdf <- Accdf[, -1]
  Accdf <- format(Accdf, nsmall = 4)
  Accdf[nrow(Accdf), ncol(Accdf)] <- ""
  Accdf_ForHeatMap <- head(Accdf_ForHeatMap[, -ncol(Accdf_ForHeatMap), drop = FALSE], -1)
  print(Accdf_ForHeatMap)
  rownames(Accdf_ForHeatMap) <- Accdf_ForHeatMap$methodName
  Accdf_ForHeatMap <- Accdf_ForHeatMap[, -1]

  if(createStatisticsFile == TRUE){
    dir.create(path = outputDir, showWarnings = FALSE)
    AccuracyFile <- paste0(outputDir, "/Accuracy_Statistics.csv")
    print("Accuracy_Statistics.csv has been created or overwritten")
    write.csv(Accdf, file = AccuracyFile)
  }
  cat("About to generate heatmaps \n")
  if (!is.null(Accdf_ForHeatMap)) {
    print("Accdf_ForHeatMap:")
    print(Accdf_ForHeatMap)
  } else {
    print("Accdf_ForHeatMap is NULL")
  }



  if (!is.null(nrow(Accdf_ForHeatMap)) && !is.null(ncol(Accdf_ForHeatMap)) && nrow(Accdf_ForHeatMap) >= 2 && ncol(Accdf_ForHeatMap) >= 2){
    # Plot best prediction method for each technique and medium according to rmse
    pdf(paste0(outputDir, "/Heatmap_ML_methods.pdf"),width = 8, height = 10)
    par(c(5.1,4.1,4.1,2.1))

    heatmap.2(as.matrix(Accdf_ForHeatMap), Rowv=FALSE, key=TRUE,
              cexCol = 1, cexRow=1,
              margins=c(8,14),
              cellnote = as.matrix(Accdf_ForHeatMap), notecol="black", notecex=0.8,
              col=colorRampPalette(c("green", "red")), breaks = seq(0.3, 1.3, 0.1),
              main = paste0("ML methods by Accuracy"),
              scale = "none", density.info="none", trace="none", dendrogram="none", Colv="NA"
    )
    dev.off()
  }
  cat("generateStatisticsClass function is finished \n")
}
