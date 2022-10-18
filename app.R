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
# Renvoie le pourcentage d'outliers dans un jeu de données (ou un vecteur) df
percentageOutliers <- function(df){
  res <- 0
  lgt <- 0
  if(!is.null(colnames(df))){
    for (n in colnames(df)) {
      if(class(df[,n]) == "integer" | class(df[,n]) == "numeric"){
        res <- res + length(which(df[,n] >  mean(df[,n]) + 3 * sd(df[,n]) | x < mean(df[,n]) - 3 * sd(df[,n])))
        lgt <- lgt+length(df[,n])
      }
    }
    return(format(round((res/lgt)*100, 2), nsmall = 2))
  } else{
    if(class(df) == "integer" | class(df) == "numeric"){
      return(length(which(df >  mean(df) + 3 * sd(df) | x < mean(df) - 3 * sd(df))) / length(df))
    } else {
      return(NULL)
    }
  }
}

# Renvoie le pourcentage de valeurs manquantes dans un jeu de données (ou un vecteur) df
percentageNA <- function(df){
  res <- 0
  if(!is.null(colnames(df))){
    for (n in colnames(df)) {
      res <- res+sum(is.na(df[,n]))
    }
    res <- format(round((res/(dim(df)[2]*dim(df)[1]))*100, 2), nsmall = 2)
  } else{
      res <- format(round((sum(is.na(df))/length(df))*100,2), nsmall = 2)
  }
  return(res)
}

# Renvoie True si la variable représentée par le vecteur x est qualitative
isQualitative <- function(x){
  if(class(x) == "factor" | class(x) == "character" | class(x) == "logical") #(length(unique(x))<=n & class(x)=="integer")
    return(TRUE)
  else 
    return(FALSE)
}

# Renvoie True si la variable est constante 
isConstant <- function(v){
  return(length(unique(v)) == 1)
}

# Renvoie le nombre de classes de la variable x
nbClass <- function(x){
  if(isQualitative(x)){
    return(length(unique(x))) 
  } else{
    return(NULL)
  }
}


# Partie Graphique
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
                 box(title = "Qualitatives", width = NULL, infoBoxOutput("statVarQual")),
                 align="center"
          )),
          
          column(width = 4,
             box(title = "Généralités", width = NULL, solidHeader = TRUE, status = "primary",
                 box(title = "Données manquantes", width = NULL, infoBoxOutput("statNA")),
                 box(title = "Outliers", width = NULL,  infoBoxOutput("statOutliers")),
                 align="center"
          ))
        ),
        
        fluidRow(
          column(title = "Récapitulatif", width = 12,
            box(width = NULL, height=400, class="unique", DT::dataTableOutput("table_recapitulatif"))
          )
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
      if(!isQualitative(df[,n])){
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
      if(isQualitative(df[,n])){
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
    res <- percentageNA(df)
    infoBox("Missing Values", paste0(res,"%"),  
        color = "light-blue",
        icon = icon("times-circle")
    )
  })
    
  output$statOutliers <- renderInfoBox({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    po <- percentageOutliers(df)
    if(!is.null(po)){
      infoBox("Outliers", paste0(po,"%"),  
          color = "light-blue",
          icon = icon("chain")
      )
    }
  })
  
  # Récapitulatif des métriques par variable
  output$table_recapitulatif <- DT::renderDT({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    
    varNames <- c(colnames(df))
    rawNames <- c("Type",  "Moy", "Med", "Nb Class", "Cst?", "% NA", "% Out")
    
    resDf <- data.frame(matrix(0, nrow=length(rawNames), ncol=length(varNames)), row.names=rawNames)
    
    # On calcule les statistiques relatives à chaque variable
    i <- 1
    type <- "-"
    med <- "-"
    moy <- "-"
    nbclass <- "-"
    cst <- "-"
    perna <- "-"
    perout <- "-"
    
    for(vn in varNames){
      # type de la variable
      if(!isQualitative(df[,vn])) 
        type <- "Qtt"
      else 
        type <- "Cat"
      
      # moyenne
      if(!isQualitative(df[,vn])){
        moy <- mean(df[,vn])
      } else 
        moy <- "-"
      
      # mediane
      if(!isQualitative(df[,vn])){
        med <- median(df[,vn])
      } else
        med <- "-"
      
      # nombre de classes
      nbclass <- nbClass(df[,vn])
      if(is.null(nbclass)) nbclass <- "-"
       
      # variable constante ?
      cst <- isConstant(df[,vn])
      if(is.null(cst)) cst <- "-"
      
      # pourcentage de NA
      perna <- percentageNA(df[,vn])
      
      # pourcentage d'outliers
      perout <- percentageOutliers(df[,vn])
      if(is.null(cst)) perout <- "-"
      
      resDf[,i] <- c(type, moy, med, nbclass, cst, perna, perout)
    
      i <- i+1
    }
    colnames(resDf) <- varNames  
    DT::datatable(resDf) %>%  DT::formatStyle(0:nrow(resDf))
  })
  
  # Analyse Univariée
  
  
  
  # Analyse Multivariée
  
  # Visualisation par réduction de dimensionnaltié
  
  # Prétraitements
  
}

# Run the application 
shinyApp(ui = ui, server = server)
