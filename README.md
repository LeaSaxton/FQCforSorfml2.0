# Summary


FoodQualityController is a flexible and user-friendly R package that is used to identify, validate, and optimize the most suitable machine learning platform for predicting microbial/sensorial quality of beef products using data from a wide range of analytical platforms.

# Table of Contents

-   [Summary](#summary)
-   [FoodQualityController workflow](#foodQualityController-workflow)
-   [Installation](#installation)
    -   [Dependencies](#dependencies)
    -   [Install FoodQualityController from source](#install-foodQualityController-from-source)
-   [Quick Start](#quick-start)
    -   [Load FoodQualityController](#load-foodQualityController)
    -   [Reading application parameters from configuration file](#reading-application-parameters-from-configuration-file)
    -   [Reading data from input files](#reading-data-from-input-files)
    -   [Creating output files](#creating-output-files)
-   [Acessing help](#accessing-help)
-   [Questions, bug reports or issues](#questions-bug-reports-or-issues)

# FoodQualityController workflow

The typical workflow of FoodQualityController is outlined below: <img src="images/Flowchart.png" width="600"/>

# Installation

## Dependencies

FoodQualityController needs the following:

-   **R** (tested on version 4.2.2)
-   **The following R libraries:** (The number is the version tested during development)

<!-- -->

       caret (6.0.91)             neuralnet (1.44.2)
       colorRamps (2.3.1)         openxlsx (4.2.5)
       Boruta (7.0.0)             pls (2.8.0)
       configr (0.3.5)            plyr (1.8.7) 
       corrplot (0.92)            randomForest (4.7.1) 
       doParallel (1.0.17)        rpart (4.1.16)
       e1071 (1.7.9)              shape (1.4.6)
       foreach (1.5.2)            tools (4.1.1)
       glmnet (4.1.3)             viridis (0.6.2)
       mixOmics (6.16.3)          xgboost (1.5.2.1)
       

**Note:** The package is platform-independent; it was developed and runs on multiple operating systems (Windows, MacOS).

All dependencies should be installed together with the FoodQualityController package, however, they can be installed separately. To install all required CRAN dependencies of FoodQualityController, type the following in R:

```{r}
> install.packages(c("caret", "colorRamps", "Boruta", "corrplot", "doParallel", "e1071", "foreach", "glmnet", "mixOmics", "neuralnet" ,
"openxlsx", "pls", "plyr", "randomForest", "rpart", "shape", "tools", "viridis", "xgboost"))
```

## Install FoodQualityController from source

You can download the latest source from the [FQCforSorfml GitHub repository page](https://github.com/LeaSaxton/FQCforSorfml2.0.git).

An example installation of FQC onto your environment using `git` command.

Type the command below in your terminal:

```
% git clone https://github.com/LeaSaxton/FQCforSorfml2.0.git
```

Then, go to R console, and install the source package:

```{r}
> library(utils)
> install.packages("FQCforSorfml", repos = NULL, type = "source", dependencies=TRUE)
```

# Quick Start

## Load FoodQualityController

Once the package is installed, to start using FoodQualityController simply load the FoodQualityController package in R:

```{r}
> library(FoodQualityController)
```

## Reading application parameters from configuration file

FoodQualityController can read configuration file in json format. 
`assess.quality` is the main function of FoodQualityController for regression, and takes name of the configuration file as parameter.
`assess.quality.class` which is the main function of FoodQualityController for classification, and takes name of the configuration file as parameter.
Configuration file contains user-defined parameters which are required by the application.

Example of a input files is as following:

 <img src="images/input_file_example.png" alt="example input file" width="600"/>


## Reading data from input files

Input data files from different analytical platforms contain microbial data or sensory scores on which machine learning models run.
Name of the datafiles with absolute directory path should be provided with `dataFileName` tag under
`platformList` tag in the configuration file, for more details see in *Input configuration file format* section.

## How to run FoodQualityController

The mains functions that are exported to the user in FoodQualityController package are assess.quality fore regression and assess.quality.class for classification.

It is called as following with configuration file name as method parameter.

```{r}
> assess.quality("/Users/lea/Cranfield/FoodQualityController/input/config.json")
```

```{r}
> assess.quality.class("/Users/lea/Cranfield/FoodQualityController/input/config.json")
```

## Creating output files

Output directory is provided in outputDirectory section of configuration file by the user. If it is not provided, the output directory becomes the current working directory. 

The general structure of the output directory for regression and files are below:

 <img src="images/output_dir_structure.png" alt="example output dir" width="600"/>

-   `rankRmse.csv` : List of ML models which are sorted based on the RMSEs. ✅Note that the format of this data are optimised for LaTeX document preparation.
-   `<PlatformName>_PCA.pdf` : PCA plot of analytical data.
-   `<PlatformName>_RMSE_Means.pdf` : Line plot of the RMSE scores in each iteration between the ML models.
-    `<PlatformName>_RSquare_Means.pdf` : Line plot of the RSquare scores in each iteration between the ML models.
-    `result.csv` : CSV data recording results of the evaluation Statistics in each ML model.
-    `RSME_Statistics.csv` : CSV data recording RMSEs in each model.
-    `RSquare_Statistics.csv` : CSV data recording RSquares in each model.
-    `<PlatformName>_<BacterialName>_<MLmodelName>.rda` : RDA data containing the model information which aqcuired the lowest RMSE throughout the iterations.
-    `<PlatformName>_<BacterialName>_<MLmodelName>.txt` : Text data containing the lowest RMSE in the ML model.

The general structure of the output directory for classification and files are below:

-   `rankAccuracy.csv` : List of ML models which are sorted based on the Accuracies. ✅Note that the format of this data are optimised for LaTeX document preparation.
-   `<PlatformName>_PCA.pdf` : PCA plot of analytical data.
-   `<PlatformName>_Accuracy_Means.pdf` : Line plot of the RMSE scores in each iteration between the ML models.
-   `<PlatformName>_<MethodName>_ConfMatrix.pdf` : Confusion matrix for each ML method/analytical platform combination.
-   `Heatmap_ML_methods.pdf` : Heatmap showing the best analytical platform/ ML method based on their accuracy.
-    `result.csv` : CSV data recording results of the evaluation Statistics in each ML model.
-    `Accuracy_Statistics.csv` : CSV data recording Accuracies in each model.
-    `<PlatformName>_<MLmodelName>.rda` : RDA data containing the model information which aqcuired the highest Accuracy throughout the iterations.
-    `<PlatformName>_<MLmodelName>.txt` : Text data containing the highest accuracy in the ML model.

# Accessing help

To access help pages for any of the functions or built-in data provided by FoodQualityController, prefix the name of the function or data set with a question mark, e.g. to get additional information on the `assess.quality` function, type the following in R:

```{r}
?assess.quality
```
**For the moment, the PLS and NN machine learning algorithms are not working**
