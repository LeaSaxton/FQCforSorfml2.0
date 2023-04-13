#' generatePerformancePlot
#' @description generates performance plot for each analytical platform
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param data cumulative performance metrics results
#' @param plotName RMSE Means OR rsquare Means
#' @param dataNames machine learning model names
#' @param file output pdf file name
#' @import colorRamps ggplot2
#'
#' @examples
#' \dontrun{generatePerformancePlot(data, plotName, dataNames, file)}

generatePerformancePlot <- function(data, plotName, dataNames, file) {

        # Modified by Shintaro Kinoshita : Preview of the dataset
        #cat( "str( data )\n" );      print( data )
        #cat( "str( dataNames )\n" ); print( dataNames )
        #cat( "cat( file )\n" );      print( file )
        #cat( "cat( plotName )\n" );  print( plotName )
        #cat( "\n" )

        if ( 0 ) { # Modified by Shintaro Kinoshita : COMMENTOUT

        # generate different colors for every data entry
        colors <- primary.colors(nrow(data) + 1, steps = 3, no.white = TRUE)

        # create pdf file
        pdf(file)

        # prepare plot drawing area
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        xAxisPreset <- c(0, ncol(data))
        yAxisPreset <- c(min(data), max(data))
        plot(xAxisPreset, yAxisPreset, xlab = "Iterations",
             ylab = "Score", main = plotName, type = "n")


        for(i in 1:nrow(data)) {
                x<-data[i,]
                n <- length(x[x!=0])
                lines(1:n, data[i, 1:n], col = colors[i])
        }

        # generate legend for plot
       legend("left", inset=c(1.02 , 1.1 ), dataNames, lty=c(1), col=colors, cex = 0.8)

        # close file
        dev.off()

        } # Modified by Shintaro Kinoshita : COMMENTOUT

        # Create dataframe for ggplot
        data_frame <- NULL
        for ( i in 1 : nrow( data ) ) {
                # Make vector for label
                label <- rep( NA, ncol( data ) )
                label[ as.integer( ncol( data ) * 0.8 ) ] <- dataNames[ i ]
                # Make dataframe element being combined to the original dataframe
                df_elem <- data.frame( Score      = as.numeric( data[ i, ] ),
                                       Iterations = seq( ncol( data ) ),
                                       Method     = rep( dataNames[ i ], ncol( data ) ),
                                       Label      = label )
                data_frame <- rbind( data_frame, df_elem )
        }
        #print( data_frame )

        plot <- ggplot( data = data_frame, aes( x = Iterations, y = Score, colour = Method, label = Label ) )
        plot <- plot +
                theme_light() +
                ggtitle( plotName ) +
                xlim( 0, ncol( data ) + 3 ) +
                xlab( "Iterations" ) +
                theme( legend.position = "none",
                       plot.title = element_text( size = 16, face = "bold" ),
                       axis.title = element_text( size = 12, face = "bold" ) ) +
                scale_x_continuous( breaks = seq( 0, ncol( data ) + 1, by = 2 ) ) +
                geom_line( size = 1.5 ) +
                geom_label( angle = 30 )

        ggsave( plot = plot, filename = file )

}


#' generatePerformancePlots
#' @description generates performance plots for each platform. Plots show
#' RMSE and RSquare change in different number of iterations
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformPerformanceResults
#' @param outputDir
#'
#' @examples
#' \dontrun{generatePerformancePlot(data, plotName, dataNames, file)}

generatePerformancePlots <- function(platformPerformanceResults, outputDir){

        if(file.exists(outputDir) == FALSE)
                dir.create(path = outputDir, showWarnings = FALSE)

        outputDir = paste0(outputDir, "/PerformancePlots")
        dir.create(path = outputDir, showWarnings = FALSE)

        # for each platform different folder is created
        for(platformPerformanceResult in platformPerformanceResults) {

                mergedCumulativeRMSEList<-NULL
                mergedCumulativeRSquareList <- NULL

                generatePlot <- FALSE

                RmseListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$RMSE))
                # if the same ML method is called more than once, the list contains same ML method more than once
                # (same ML method could called with different data pretreatment method or number of iteration)
                # minRmseforMethod is defined below which contains unique ML methods with minimum RMSE for each
                # performance plots same as heatmap should give unique ML with best performance
                methodList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$method))
                data <- list(RMSE = RmseListForMLM, method = methodList)
                minRmseforMethod <- aggregate(RMSE ~ method, data, function(x) min(x))

                for(i in 1:length(platformPerformanceResult$mlmPerformanceResults)) {
                        mlmPerformanceResult <- platformPerformanceResult$mlmPerformanceResults[[i]]
                        minRMSE <- minRmseforMethod[minRmseforMethod$method == mlmPerformanceResult$method,]$RMSE
                        mlmPerformanceResult$min <- FALSE
                        platformPerformanceResult$mlmPerformanceResults[[i]]$min<-FALSE
                        if(minRMSE == mlmPerformanceResult$RMSE){
                                platformPerformanceResult$mlmPerformanceResults[[i]]$min<-TRUE
                                if(is.null(mlmPerformanceResult$cumulativeMeanRMSEList) == FALSE){
                                        if(generatePlot == FALSE){
                                                mergedCumulativeRMSEList <- dplyr::bind_rows(mlmPerformanceResult$cumulativeMeanRMSEList)
                                                mergedCumulativeRSquareList <- dplyr::bind_rows(mlmPerformanceResult$cumulativeMeanRSquareList)
                                                generatePlot <- TRUE
                                        }
                                        else{
                                                mergedCumulativeRMSEList <- dplyr::bind_rows(mergedCumulativeRMSEList, mlmPerformanceResult$cumulativeMeanRMSEList)
                                                mergedCumulativeRSquareList <- dplyr::bind_rows(mergedCumulativeRSquareList, mlmPerformanceResult$cumulativeMeanRSquareList)
                                        }
                                }
                        }
                }


                if(generatePlot == TRUE){

                        cat("Creating performance plots for ", platformPerformanceResult$platform,"\n"  )

                        mergedCumulativeRSquareList[is.na(mergedCumulativeRSquareList)] = 0
                        mergedCumulativeRMSEList[is.na(mergedCumulativeRMSEList)] = 0

                        mlmList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults,
                                                 function(x){
                                                         if(!is.null(x$cumulativeMeanRMSEList) && x$min == TRUE)
                                                                 return(x$method)
                                                 }))

                        generatePerformancePlot(data = mergedCumulativeRMSEList*100, plotName = paste0("RMSE Means for ", platformPerformanceResult$platform),
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/", platformPerformanceResult$platform, "_RMSE_Means.pdf", sep = ""))


                        generatePerformancePlot(data = mergedCumulativeRSquareList*100, plotName = paste0("RSquare Means for ", platformPerformanceResult$platform),
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/", platformPerformanceResult$platform, "_RSquare_Means.pdf", sep = ""))

                }
        }


}



