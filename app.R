#
# title: "R Shiny App for Supervised Learning on imbalanced data"
# authors: "Lounes Mechouek", "Mohammed Nassim Sehdi"
# date: "October 15, 2022"
#
options(encoding = "UTF-8")
# Packages
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


# Fonctions
# Renvoie le pourcentage d'outliers dans un jeu de données (ou un vecteur) df
percentageOutliers <- function(df){
  res <- 0
  lgt <- 0
  if(!is.null(colnames(df))){
    for (n in colnames(df)) {
      if(class(df[,n]) == "integer" | class(df[,n]) == "numeric"){
        res <- res + length(which(df[,n] >  mean(df[,n], na.rm=TRUE) + 3 * sd(df[,n], na.rm=TRUE) | df[,n] < mean(df[,n], na.rm=TRUE) - 3 * sd(df[,n], na.rm=TRUE)))
        lgt <- lgt+length(df[,n])
      }
    }
    return(format(round((res/lgt)*100, 2), nsmall = 2))
  } else{
    if(class(df) == "integer" | class(df) == "numeric"){
      return(length(which(df >  mean(df, na.rm=TRUE) + 3 * sd(df, na.rm=TRUE) | df < mean(df, na.rm=TRUE) - 3 * sd(df, na.rm=TRUE))) / length(df))
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

# Renvoie un dataframe avec la fréquence de chaque modalité de la variable y
getFrequences <- function(y){
  df <- as.data.frame(table(y))
  res <- data.frame(
    classe = c(as.factor(df[,1])),
    frequence = c(df[,2])
  )
  return(res)
}

# Renvoie un dataframe avec la fréquence de chaque couple (x,y) où x est une variable et y l'outcome
getFrequenceByVar <- function(df, x, y){
  res = df %>% count(df[,x], df[,y], sort = TRUE)
  colnames(res) = c("modalite", "classe", "frequence")
  return(res)
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
    tags$style(HTML('Z
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
              
              overflow-x : scroll
              overflow-y : scroll;
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
          div(h2("Récapitulatif"), style="margin-left:2%;"),
          column(width = 12,
            box(height=450, width = NULL, class="unique", DT::dataTableOutput("table_recapitulatif"))
          )
        ),
        
        div(h2("Exploration des données"), style="margin-left:1%; margin-bottom:3%;"),
        
        fluidRow(
          column(title = "Proportions des classes", width = 6,
            selectInput("outcomeVariable", label="Veuillez sélectionner l'outcome", choices=c("-")),
            box(width = NULL, plotOutput("repartitionClasses"))
          ),
          column(title = "Proportions des classes selon une variable", width = 6,
             fluidRow(
              column(width = 6,
                selectInput("repartitionParVar", label="Répartition du churn selon une variable", choices=c("-"))
              ),
              column(width = 6,
                selectInput("nbIntervalles", label="Nombre d'intervalles pour la variable", choices=c("-")),
              ),
             ),
             box(width = NULL, plotOutput("repartitionClassVar"))
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
      return(read.table(input$uploaded_data$datapath, header = TRUE, sep = ","))
      #read.csv(input$uploaded_data$datapath, header = TRUE, sep = ",")
    }
  })
  
  observeEvent(data(), {
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    nms <- colnames(cleanData())
    
    #tdisplay_proxy <- DT::dataTableProxy("table_display")
    #DT::replaceData(tdisplay_proxy, df, resetPaging = FALSE)
    
    choix <- c("-")
    updateSelectInput(inputId = "outcomeVariable", choices = c(choix, nms))
  })
  
  observeEvent(outcomeVar(), {
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    nms <- colnames(cleanData())
    ov <- outcomeVar()
    choix <- c("-")
    
    if (is.null(ov) | ov == "-") {
      updateSelectInput(inputId = "repartitionParVar", choices = choix)
    } else {
      updateSelectInput(inputId = "repartitionParVar", choices = c(choix, nms[nms!=ov]))
    }
    
  })
  
  observeEvent(classPerVar(), {
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    nms <- colnames(cleanData())
    choix <- c("-")
    cpv <- classPerVar()
    
    if (is.null(cpv) | cpv == "-") {
      updateSelectInput(inputId = "nbIntervalles", choices = choix)
    } else{
      if(isQualitative(df[, cpv])){
        updateSelectInput(inputId = "nbIntervalles", choices = choix)
      } else {
        updateSelectInput(inputId = "nbIntervalles", choices = c(choix, c(2:15)))
      }
      
    }
    
  })
  
  
  # Section "Chargement"
  # Affichage du tableau de données 
  output$table_display <- DT::renderDataTable({
    validate(need(!is.null(data()) , "Veuillez sélectionner un dataset valide"))
    DT::datatable(data())
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
        type <- "Qual"
      
      # moyenne
      if(!isQualitative(df[,vn])){
        moy <- mean(df[,vn], na.rm=TRUE)
      } else 
        moy <- "-"
      
      # mediane
      if(!isQualitative(df[,vn])){
        med <- median(df[,vn], na.rm=TRUE)
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
      if(is.null(perna)) perna <- "-"
      
      # pourcentage d'outliers
      perout <- percentageOutliers(df[,vn])
      if(is.null(perout)) perout <- "-"
      
      resDf[,i] <- c(type, moy, med, nbclass, cst, perna, perout)
    
      i <- i+1
    }
    colnames(resDf) <- varNames  
    DT::datatable(resDf) %>%  DT::formatStyle(0:nrow(resDf))
  })
  
  # Analyse Univariée
  # L'utilisateur sélectionne l'outcome
  outcomeVar <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    nms <- colnames(cleanData())
    choix <- c("-")
    
    # On vérifie que la variable n'est pas null
    if (is.null(input$outcomeVariable) | input$outcomeVariable == "-") {
      updateSelectInput(inputId = "repartitionParVar", choices = choix)
      return("-")
    } else {
      updateSelectInput(inputId = "repartitionParVar", choices = c(choix, nms[nms!=input$outcomeVariable]))
      return(input$outcomeVariable)
    }
  })
  
  output$repartitionClasses <- renderPlot({
    if(outcomeVar() == "-"){validate(need(!is.null(NULL) , "Sélectionnez d'abord la variable de sortie"))}
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    df <- cleanData()
    ov <- outcomeVar()
    
    res <- getFrequences(df[, ov])
    
    ggplot(res, aes(x="", y=frequence, fill=classe))+geom_bar(width = .75, stat = "identity")+coord_polar("y", start=0)
  })
  
  classPerVar <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- cleanData()
    nms <- colnames(cleanData())
    choix <- c("-")
    
    # On vérifie que la variable n'est pas null
    if (is.null(input$repartitionParVar) | input$repartitionParVar == "-") {
      updateSelectInput(inputId = "nbIntervalles", choices = choix)
      return("-")
    } else {
      if(!isQualitative(df[, input$repartitionParVar])){
        updateSelectInput(inputId = "nbIntervalles", choices = c(choix, c(2:15)))
      }
      return(input$repartitionParVar)
    }
  })
  
  output$repartitionClassVar <- renderPlot({
    if(classPerVar() == "-"){validate(need(!is.null(NULL) , "Sélectionnez une variable à étudier"))}
    if(outcomeVar() == "-"){validate(need(!is.null(NULL) , "Sélectionnez d'abord la variable de sortie"))}
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    df <- cleanData()
    cpv <- classPerVar()
    ov <- outcomeVar()
    
    if(isQualitative(df[,cpv])){
      res = getFrequenceByVar(df, cpv, ov)
      ggplot(res, aes(x=modalite, y=frequence, fill=classe))+geom_bar(width = .5, stat = "identity")
    } 
    # G�rer variable continue
    
  })
  
  # Analyse Multivariée
  
  # Visualisation par réduction de dimensionnaltié
  
  # Prétraitements
  
}

# Run the application 
shinyApp(ui = ui, server = server)
