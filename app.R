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

devtools::install_github("kassambara/factoextra")

source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)

