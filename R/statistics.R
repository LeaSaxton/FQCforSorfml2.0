#' generateStatistics
#' @description generates RMSE and RSQUARE statistics report for each platform
#' and machine learning models
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformPerformanceResults list of machine learning performance results
#' for each platform
#' @param outputDir output directory name provided by the use in config json file
#' @param createStatisticsFile boolean value indicating whether to create statistics
#' file or not
#' @import gplots dplyr
#'
#' @examples
#' \dontrun{generateStatistics(platformPerformanceResults, outputDir, createStatisticsFile)}
#'
generateStatistics <- function(platformPerformanceResults, outputDir, createStatisticsFile, bacterialName){

        mlmLongDesc = list("NN" = "Neural Network", "SVR-Radial" = "SVR-Radial", "SVR-Polynomial" = "SVR-Polynomial",
                           "KNN" = "k-nearest neighbors", "RFR" = "Random Forest",
                           "PLS" = "Partial least squares Regression", "PCR" = "PCA Regression", "OLS"= "Ordinary Least Squares Regression" ,
                           "SR" = "Stepwise Regression", "RR" ="Ridge Regression","LR" = "Lasso Regression", "ER" = "Elastic Regression",
                           "XGBoost" = "XGBoost","RT"= "Regression Tree"
        )

        cat("########################\n####  ML PERFORMANCE RESULT####\n########################\n\n")

        mlmShortNameList <- unlist(lapply(platformPerformanceResults[[1]]$mlmPerformanceResults, function(x) x$method))
        pretreatmentList <- unlist(lapply(platformPerformanceResults[[1]]$mlmPerformanceResults, function(x) x$pretreatment))
        mlmLongDescList <- unlist(mlmLongDesc[mlmShortNameList])
        methodNameWithDataPretreatment <- paste0(mlmLongDescList , "(", pretreatmentList , ")")
        platformList <- unlist(lapply(platformPerformanceResults, function(x) x$platform))
        Rmsedf <- data.frame(mlmShortNameList)
        RSquaredf <- data.frame()

        for(platformPerformanceResult in platformPerformanceResults){
                RmseListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$RMSE))
                RSquaredListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$RSquare))
                Rmsedf <- cbind(Rmsedf, RmseListForMLM)
                RSquaredListForMLM <- na.omit(RSquaredListForMLM)
                print("RSquaredListForMLM")
                print(RSquaredListForMLM)
                print("RSquaredf after cbind")
                print(RSquaredf)
                if(nrow(RSquaredf) == 0){
                        RSquaredf <- cbind(RSquaredListForMLM)
                }
                else{
                        RSquaredf <- cbind(RSquaredf, RSquaredListForMLM)
                }

        }

        if(nrow(Rmsedf) == 0)
                return()

        cat("\n\nRMSE FOR ML METHODS\n\n")
        Rmsedf <- as.data.frame(Rmsedf)
        print( "Rmsedf_01:" ); print( Rmsedf )
        print( "methodNameWithDataPretreatment:" ); print( methodNameWithDataPretreatment )

        rownames(Rmsedf) <- methodNameWithDataPretreatment
        #print( "Rmsedf_02:" ); print( Rmsedf )

        #cat( rownames( Rmsedf ) )
        colnames(Rmsedf) <- c("methodName", makeUniqueNames(platformList, bacterialName))
        #print( "Rmsedf_03:" ); print( Rmsedf )

        Rmsedf$ML_Means <- round(rowMeans(Rmsedf[2:ncol(Rmsedf)], na.rm=TRUE), 4)
        Rmsedf$ML_Means <- round(rowMeans(Rmsedf[2:ncol(Rmsedf)], na.rm = TRUE), 4)
        Rmsedf <- rbind(Rmsedf, c(NA, round(colMeans(Rmsedf[2:ncol(Rmsedf)], na.rm = TRUE), 4)))
        rownames(Rmsedf)[nrow(Rmsedf)] <- "Platform_Means"

        # Calculate Rmsedf_ForHeatMap with unique ML methods and minimum RMSE for each
        Rmsedf_ForHeatMap <- as.data.frame(Rmsedf %>% group_by(methodName) %>% filter(ML_Means == min(ML_Means)))
        Rmsedf <- Rmsedf[, -1]
        Rmsedf <- format(Rmsedf, nsmall = 4)
        Rmsedf[nrow(Rmsedf), ncol(Rmsedf)] <- ""
        print(Rmsedf)
        Rmsedf_ForHeatMap <- head(Rmsedf_ForHeatMap[, -ncol(Rmsedf_ForHeatMap), drop = FALSE], -1)
        rownames(Rmsedf_ForHeatMap) <- mlmLongDescList[Rmsedf_ForHeatMap$methodName]
        Rmsedf_ForHeatMap <- Rmsedf_ForHeatMap[, -1]

        cat("\n\nR-squared FOR ML METHODS\n\n")
        RSquaredf<- as.data.frame(RSquaredf)
        rownames(RSquaredf) <- methodNameWithDataPretreatment
        colnames(RSquaredf) <- platformList
        cat("issue is here \n")
        RSquaredf$ML_Means <- round(rowMeans(RSquaredf, na.rm=TRUE),4)
        RSquaredf <- rbind(RSquaredf, round(colMeans(RSquaredf, na.rm=TRUE),4))
        rownames(RSquaredf)[nrow(RSquaredf)] <- "Platform_Means"
        RSquaredf <- format(RSquaredf, nsmall = 4)
        RSquaredf[nrow(RSquaredf), ncol(RSquaredf)] <- ""
        RSquaredf <- RSquaredf[, -1]
        print("Rmsedf")
        print(Rmsedf)
        print("Rsquaredf")
        print(RSquaredf)


        if(createStatisticsFile == TRUE){
                dir.create(path = outputDir, showWarnings = FALSE)
                RMSEFile <- paste0(outputDir, "/RMSE_Statistics.csv")
                write.csv(Rmsedf, file = RMSEFile)
                RSquareFile <- paste0(outputDir, "/RSquare_Statistics.csv")
                write.csv(RSquaredf, file = RSquareFile)
        }
        cat("About to generate heatmaps \n")
        if (!is.null(Rmsedf_ForHeatMap)) {
          print("Rmsedf_ForHeatMap:")
          print(Rmsedf_ForHeatMap)
        } else {
          print("Rmsedf_ForHeatMap is NULL")
        }
        if (!is.null(nrow(Rmsedf_ForHeatMap)) && !is.null(ncol(Rmsedf_ForHeatMap)) && nrow(Rmsedf_ForHeatMap) >= 2 && ncol(Rmsedf_ForHeatMap) >= 2){
                # Plot best prediction method for each technique and medium according to rmse
                pdf(paste0(outputDir, "/Heatmap_ML_methods.pdf"))
                par(c(5.1,4.1,4.1,2.1))

                heatmap.2(as.matrix(Rmsedf_ForHeatMap), Rowv=FALSE, key=TRUE,
                          cexCol = 1, cexRow=1,
                          margins=c(8,14),
                          cellnote = as.matrix(Rmsedf_ForHeatMap), notecol="black", notecex=0.8,
                          col=colorRampPalette(c("green", "red")), breaks = seq(0.3, 1.3, 0.1),
                          main = paste0("ML methods by RMSE"),
                          scale = "none", density.info="none", trace="none", dendrogram="none", Colv="NA",
                )
                dev.off()
        }
}
