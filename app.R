#
# title: "R Shiny App for Supervised Learning on imbalanced data"
# authors: "Lounes Mechouek", "Mohammed Nassim Sehdi", "Mohamed Amine Raiah"
# date: "October 15, 2022"
#
options(encoding = "UTF-8")

if (!require(shinydashboard)) install.packages('shinydashboard')
if (!require(shiny)) install.packages('shiny')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(DT)) install.packages('DT')
if (!require(corrplot)) install.packages("corrplot")
if (!require(DescTools)) install.packages("DescTools")
if (!require(FactoMineR)) install.packages("FactoMineR")
if(!require(devtools)) install.packages("devtools")
if(!require(imputeTS)) install.packages("imputeTS")
if(!require(caTools)) install.packages("caTools") 
if(!require(rpart)) install.packages("rpart")
if(!require(caret)) install.packages("caret")
if(!require(partykit)) install.packages("partykit")
if(!require(nnet)) install.packages("nnet")
if(!require(e1071)) install.packages('e1071')
if(!require(pROC)) install.packages('pROC')
if(!require(kernlab)) install.packages('kernlab')
if(!require(ROSE)) install.packages('ROSE')
if(!require(UBL)) install.packages("UBL")
 
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(corrplot)
library(DescTools)
library(FactoMineR)
library(devtools)
library("factoextra")
library(imputeTS)
library(caTools)
library(rpart)
library(caret)
library(partykit)
library(nnet)
library(e1071)
library(pROC)
library(kernlab)
library(ROSE)
library(smotefamily)
library(UBL)

devtools::install_github("kassambara/factoextra")

source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)

