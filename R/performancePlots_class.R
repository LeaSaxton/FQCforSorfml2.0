#' generatePerformancePlotClass
#' @description generates performance plot for each analytical platform
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param data cumulative performance metrics results
#' @param plotName Accuracy Means
#' @param dataNames machine learning model names
#' @param file output pdf file name
#' @import colorRamps ggplot2
#'
#' @examples
#' \dontrun{generatePerformancePloClass(data, plotName, dataNames, file)}

generatePerformancePlotClass <- function(data, plotName, dataNames, file) {

        cat("generatePerformancePlotClass function is starting \n ")
        if ( 0 ) {
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

        }

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
                geom_label()

        ggsave( plot = plot, filename = file )

}


#' generatePerformancePlotsClass
#' @description generates performance plots for each platform. Plots show
#' Accuracy change in different number of iterations
#' @author Lea Saxton \email{lea.saxton.831@@cranfield.ac.uk}
#' @param platformPerformanceResults
#' @param outputDir
#'
#' @examples
#' \dontrun{generatePerformancePlotClass(data, plotName, dataNames, file)}

generatePerformancePlotsClass <- function(platformPerformanceResults, outputDir){

        cat("generatePerformancePlotsClass function is starting \n")

        if(file.exists(outputDir) == FALSE)
                dir.create(path = outputDir, showWarnings = FALSE)

        outputDir = paste0(outputDir, "/PerformancePlots")
        dir.create(path = outputDir, showWarnings = FALSE)

        # for each platform different folder is created
        for(platformPerformanceResult in platformPerformanceResults) {

                mergedCumulativeAccuracyList<-NULL

                generatePlot <- FALSE

                AccuracyListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$Accuracy))

                # if the same ML method is called more than once, the list contains same ML method more than once
                # (same ML method could called with different data pretreatment method or number of iteration)
                # maxAccuracyforMethod is defined below which contains unique ML methods with maximum Accuracy for each
                # performance plots same as heatmap should give unique ML with best performance
                methodList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$method))
                data <- list(Accuracy = AccuracyListForMLM, method = methodList)
                maxAccuracyforMethod <- aggregate(Accuracy ~ method, data, function(x) max(x))

                for(i in 1:length(platformPerformanceResult$mlmPerformanceResults)) {
                        mlmPerformanceResult <- platformPerformanceResult$mlmPerformanceResults[[i]]
                        maxAccuracy <- maxAccuracyforMethod[maxAccuracyforMethod$method == mlmPerformanceResult$method,]$Accuracy
                        mlmPerformanceResult$max <- FALSE
                        platformPerformanceResult$mlmPerformanceResults[[i]]$max<-FALSE
                        if(maxAccuracy == mlmPerformanceResult$Accuracy){
                                platformPerformanceResult$mlmPerformanceResults[[i]]$max<-TRUE
                                if(is.null(mlmPerformanceResult$cumulativeMeanAccList) == FALSE){
                                        if(generatePlot == FALSE){
                                                mergedCumulativeAccList <- dplyr::bind_rows(mlmPerformanceResult$cumulativeMeanAccList)
                                                generatePlot <- TRUE
                                        }
                                        else{
                                                mergedCumulativeAccList <- dplyr::bind_rows(mergedCumulativeAccList, mlmPerformanceResult$cumulativeMeanAccList)
                                        }
                                }
                        }
                }


                if(generatePlot == TRUE){

                        cat("Creating performance plots for ", platformPerformanceResult$platform,"\n"  )

                        mergedCumulativeAccList[is.na(mergedCumulativeAccList)] = 0

                        mlmList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults,
                                                 function(x){
                                                         if(!is.null(x$cumulativeMeanAccList) && x$max == TRUE)
                                                                 return(x$method)
                                                 }))
                        generatePerformancePlotClass(data = mergedCumulativeAccList*100, plotName = paste0("Accuracy Means for ", platformPerformanceResult$platform),
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/", platformPerformanceResult$platform, "_Accuracy_Means.pdf", sep = ""))


                }
        }

        cat("generatePerformancePlotsClass function is finished, plots were generated \n")
}

library(ggplot2)

confusion_matrix <- function(conf_matrix, platform_name, outDir, method){
        cat("confusion_matrix function is starting \n")
        if (!file.exists(outDir))
                dir.create(path = outDir, showWarnings = FALSE)

        outputDir <- paste0(outDir, "/ConfusionMatrix")

        if (!file.exists(outputDir))
                dir.create(path = outputDir, showWarnings = FALSE)

        outputFile <- paste0(outputDir, "/", platform_name,"_", method, "_ConfMatrix.pdf")

        # Plot the confusion matrix
        conf_matrix_plot <- ggplot(data = as.data.frame(conf_matrix$table),
                                   aes(x = Reference, y = Prediction, fill = Freq)) +
                geom_tile() +
                scale_fill_gradient(low = "yellow", high = "orange") +
                theme_minimal() +
                labs(title = paste0("Confusion Matrix for ", platform_name," ", method ),
                     x = "Reference",
                     y = "Prediction")

        # Save the confusion matrix plot as a PDF
        ggsave(filename = outputFile, plot = conf_matrix_plot, width = 8, height = 6)

        # Close the PDF device
        dev.off()
}




