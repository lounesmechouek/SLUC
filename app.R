#
# title: "R Shiny App for Supervised Learning on imbalanced data"
# authors: "Lounes Mechouek", "Mohammed Nassim Sehdi"
# date: "October 15, 2022"
#
options(encoding = "UTF-8")

if (!require(shinydashboard)) install.packages('shinydashboard')
if (!require(shiny)) install.packages('shiny')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(DT)) install.packages('DT')

library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)
