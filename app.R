#
# title: "R Shiny App for Supervised Learning on imbalanced data"
# authors: "Lounes Mechouek", "Mohammed Nassim Sehdi"
# date: "October 15, 2022"
#

# Packages
library(shinydashboard)
library(shiny)
library(dplyr)

# Fonctions
percentageOutliers <- function(df){
  res = 0
  lgt = 0
  for (n in colnames(df)) {
    if(class(df[,n]) == "integer" | class(df[,n]) == "numeric"){
      res = res + length(which(df[,n] >  mean(df[,n]) + 3 * sd(df[,n]) | x < mean(df[,n]) - 3 * sd(df[,n])))
      lgt = lgt+length(df[,n])
    }
  }
  return(format(round((res/lgt)*100, 2), nsmall = 2))
}

ui <- dashboardPage(
  dashboardHeader(title = "ASCD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement", tabName = "chargement", icon = icon("fa-light fa-file-import", verify_fa = FALSE)),
      menuItem("Exploration", tabName = "exploration", icon = icon("fa-solid fa-chart-line", verify_fa = FALSE)),
      menuItem("Decision Tree", tabName = "decision-tree", icon = icon("fa-light fa-network-wired", verify_fa = FALSE)),
      menuItem("Logistic Regression", tabName = "logistic-regression", icon = icon("fa-light fa-gears", verify_fa = FALSE)),
      menuItem("SVM", tabName = "svm", icon = icon("project-diagram", verify_fa = FALSE)),
      menuItem("Sampling", tabName = "sampling", icon = icon("fa-light fa-vials", verify_fa = FALSE))
    )
  ),
  dashboardBody(
    # On définit des styles css pour centrer les éléments dans les box
    tags$style(HTML('
            .col-sm-12 div.box{
              display: flex;
              align-items: center;
              justify-content: center;
              align-content: center;
              overflow-x : scroll;
              overflow-y : scroll;
            }
            .unique{
              flex-grow: 1;
              margin: 0 5% 0 5%;
            }
            .shiny-input-container > .control-label{
              margin-bottom: 2%;
            }
            .dataTables_wrapper{
              position:absolute;
              margin:0;
              top: 10px;
            }
    ')),
    
    
    tabItems(
      
      # Chargement des données
      tabItem(tabName = "chargement",
        fluidRow(
          box(
            fileInput("uploaded_data", "Choisissez un fichier (CSV/txt)...", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            align="center", height = 300, width = 12, class="unique"
          ),
        ),
        
        fluidRow(
          box(
            DT::dataTableOutput("table_display"),
            height = 500, width = 12, class="unique"
          )
        )
      ),
      
      # Exploration des données
      tabItem(tabName = "exploration",
        fluidRow(
          column(width = 4,
            box(title = "Dimensions", width = NULL, solidHeader = TRUE, status = "primary",
              box(title = "Variables", width = NULL, infoBoxOutput("statDimVar")),
              box(title = "Individus", width = NULL, infoBoxOutput("statDimInd")),
              align="center"
          )),
          
          column(width = 4,
             box(title = "Type de variables", width = NULL, solidHeader = TRUE, status = "primary",
                 box(title = "Quantitatives", width = NULL, infoBoxOutput("statVarQuan")),
                 box(title = "Catégorielles", width = NULL, infoBoxOutput("statVarQual")),
                 align="center"
          )),
          
          column(width = 4,
             box(title = "Généralités", width = NULL, solidHeader = TRUE, status = "primary",
                 box(title = "Données manquantes", width = NULL, infoBoxOutput("statNA")),
                 box(title = "Outliers", width = NULL,  infoBoxOutput("statOutliers")),
                 align="center"
          ))
        )
              
      ),
      
      # Classification par arbre de décision
      tabItem(tabName = "decision-tree"
              
      ),
      
      # Classification par LR
      tabItem(tabName = "logistic-regression"
              
      ),
      
      # Classification par SVM
      tabItem(tabName = "svm"
              
      ),
      
      # Échantillonage
      tabItem(tabName = "sampling"
              
      )
      
    )
  )
)

server <- function(input, output) {
  # On peut charger des fichiers jusqu'à 500Mo
  options(shiny.maxRequestSize=500*1024^2)
  
  # Récupération du dataset avec gestion des changements
  data <- reactive({
    # On vérifie que la variable n'est pas null
    if (is.null(input$uploaded_data)) {
      return()
    } else {
      read.table(input$uploaded_data$datapath, header = TRUE, sep = ",")
      #read.csv(input$uploaded_data$datapath, header = TRUE, sep = ",")
    }
  })
  
  # Section "Chargement"
  # Affichage du tableau de données 
  output$table_display <- DT::renderDT({
    validate(need(!is.null(data()) , "Veuillez sélectionner un dataset valide"))
    DT::datatable(data()) %>%  DT::formatStyle(0:nrow(data()))
  })
  
  # Section "Exploration"
  cleanData <- reactive({
    # On pré-traite les données selon les critères de l'utilisateur
    data()
  })
  
  
  # Statistiques générales
  output$statDimVar <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    res <- dim(df)[2]
    infoBox("Variables", res,  
      color = "light-blue",
      icon = icon("square-root-alt")
    )
  })
  
  output$statDimInd <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    res <- dim(df)[1]
    infoBox("Individus", res,  
        color = "light-blue",
        icon = icon("ellipsis-h")
    )
  })
  
  
  output$statVarQuan <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    res <- 0
    for (n in colnames(df)) {
      if(class(df[,n]) == "integer" | class(df[,n]) == "numeric"){
        res<-res+1
      }
    }
    infoBox("Quantitatives", res,  
        color = "light-blue",
        icon = icon("sort-numeric-asc")
    )
  })
  
  output$statVarQual <-  renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    res <- 0
    for (n in colnames(df)) {
      if(class(df[,n]) == "factor" | class(df[,n]) == "character"){
        res<-res+1
      }
    }
    infoBox("Categorielles", res,  
        color = "light-blue",
        icon = icon("pen-alt")
    )
  })
    
  output$statNA <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    res <- 0
    for (n in colnames(df)) {
      res <- res+sum(is.na(df[,n]))
    }
    res = format(round((res/(dim(df)[2]*dim(df)[1]))*100, 2), nsmall = 2)
    infoBox("Missing Values", paste0(res,"%"),  
        color = "light-blue",
        icon = icon("times-circle")
    )
  })
    
    
  output$statOutliers <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    infoBox("Outliers", paste0(percentageOutliers(df),"%"),  
        color = "light-blue",
        icon = icon("chain")
    )
  })
  
  # Récapitulatif des métriques par variable
  
  
  # Analyse Univariée
  
  
  
  # Analyse Multivariée
  
  # Visualisation par réduction de dimensionnaltié
  
  # Prétraitements
  
}

# Run the application 
shinyApp(ui = ui, server = server)
