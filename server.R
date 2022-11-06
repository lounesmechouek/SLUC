options(encoding = "UTF-8")

# Packages
library(dplyr)
library(ggplot2)
library(DT)
library(corrplot)
library(DescTools)
library(FactoMineR)
library("factoextra")
library(imputeTS)


################################################
#                 Fonctions                    #
################################################

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

# Renvoie un dataframe avec la fréquence de chaque modalitÃ© de la variable y
getFrequences <- function(y){
  df <- as.data.frame(table(y))
  res <- data.frame(
    classe = c(factor(df[,1])),
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

# Renvoie le nom des variables qualitatives de df
# Pour avoir le nom des variables quantitatives : 1/ nms <- colnames(df) 2/ nms[!(nms %in% getQualitatives(df))]
getQualitatives <- function(df){
  res <- c()
  for(col in colnames(df)){
    if(isQualitative(df[,col])) res <- c(res, col)
  }
  return(res)
}


######################################################
#                 DÉBUT SERVEUR                      #
######################################################

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
    updateSelectInput(inputId = "boxRepart", choices = c(choix, nms))
    updateSelectInput(inputId = "corrContVar", choices = c(nms[!(nms %in% getQualitatives(df))]))
    updateSelectInput(inputId = "corrMixVar", choices = c(nms))
    updateSelectInput(inputId = "pcaVars", choices = c(nms[!(nms %in% getQualitatives(df))]))
    updateSelectInput(inputId = "cp1", choices = c(1:length(nms)))
    updateSelectInput(inputId = "excludedVars", choices = c(nms))
  })
  
  observeEvent(cleanData(), {
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
      updateSelectInput(inputId = "boxRepart", choices = c(choix, nms[nms!=ov]))
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
  # Affichage du tableau de donnÃ©es 
  output$table_display <- DT::renderDataTable({
    validate(need(!is.null(data()) , "Veuillez sélectionner un dataset valide"))
    DT::datatable(data(), options = list(scrollX = TRUE))
  })
  
  # Section "Exploration"
  cleanData <- reactive({
    df <- data()

    if(naPolicy() == "Supprimer les individus"){
      df1 <- na.omit(df)
    }
    else if(naPolicy() == "Remplacer par la moyenne de la colonne"){
      df1 <- na_mean(df)
    }else{
      df1 <- df
    }
    
    if(!(is.null(exdVars()))){
      df2 <- df1[,!( names(df1) %in% c(exdVars()) )]
    } else {
      df2 <- df1
    }
    
    print(percentageNA(df))
    print(percentageNA(df2))
    return(df2)
  })
  
  # Statistiques gÃ©nÃ©rales
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
    DT::datatable(resDf, options = list(scrollX = TRUE)) %>%  DT::formatStyle(0:nrow(resDf))
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
    res$classe <- factor(res$classe)
    
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
      res$classe = factor(res$classe)
      ggplot(res, aes(x=modalite, y=frequence, fill=classe))+geom_bar(width = .5, stat = "identity")
    } else {
      validate(need(!is.null(nombreInterv()) & nombreInterv()!='-' , "Veuillez sélectionner un nombre d'intervalles à construire"))
      interv <- cut(df[,cpv],  as.numeric(nombreInterv()))
      df1 <- data.frame(df, interv)
      res <- getFrequenceByVar(df1, "interv", ov)
      res$classe = factor(res$classe)
      ggplot(res, aes(x=modalite, y=frequence, fill=classe))+geom_bar(width = .5, stat = "identity")
    }
  })
  
  nombreInterv <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    return(input$nbIntervalles)
  })
  
  varBoxRepart <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))

    # On vérifie que la variable n'est pas null
    if (is.null(input$boxRepart) | input$boxRepart == "-") {
      return("-")
    } else {
      return(input$boxRepart)
    }
  })
  
  output$boxPlotRepart <- renderPlot({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(outcomeVar()) & outcomeVar()!='-' , "Veuillez d'abord sélectionner une variable de sortie"))
    validate(need(!is.null(varBoxRepart()) & varBoxRepart()!='-' , "Veuillez sélectionner une variable à afficher"))
    
    df <- cleanData()
    
    validate(need(!isQualitative(df[,varBoxRepart()]) , "Veuillez sélectionner une variable QUANTITATIVE"))
    
    varBox <- varBoxRepart()
    ov <- outcomeVar()
    
    df1 <- data.frame(df[,varBox], factor(df[,ov]))
    colnames(df1) <- c("Variable", "Classes")

    ggplot(df1, aes(x=Variable, y=Classes)) + geom_boxplot()+ coord_flip() #outlier.colour="red", outlier.shape=8, outlier.size=4
    
  })
  
  corrContinue <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    return(input$corrContVar)
  })
  
  output$corrContPlotVar <- renderPlot({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(corrContinue()) & corrContinue()!='-' & length(corrContinue()>1), "Veuillez sélectionner au moins deux variables à étudier"))
    
    df <- cleanData()
    
    df1 <- cor(na.omit(df[c(corrContinue())]))
    
    corrplot(df1, addgrid.col = "darkgray", type = "upper", method = "color", addCoef.col = "gray")
  })
  
  corrMixte <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    return(input$corrMixVar)
  })
  
  output$corrMixPlotVar <- renderPlot({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(corrMixte()) & corrMixte()!='-' & length(corrMixte())>1 , "Veuillez sélectionner au moins deux variables à étudier"))
    
    df <- na.omit(cleanData())
    tabMixt <- corrMixte()
    res <- matrix(nrow = length(tabMixt), ncol = length(tabMixt))
    
    for(i in 1:length(tabMixt)){
      var1 <- tabMixt[i]
      coefs <- c()
      for(j in 1:length(tabMixt)){
        var2 <- tabMixt[j]
        names <- c(var1, var2)
        if(j<i){
          coefs <- c(coefs, res[i, j])
        } else if(j==i){
          coefs <- c(coefs, 1)
        } else{
          if(isQualitative(df[, var1]) & isQualitative(df[, var2])){
            coefs <- c(coefs, DescTools::PairApply(df[names], DescTools::CramerV))
          } else if(isQualitative(df[, var1]) & !isQualitative(df[, var2])){
            aov1 <- aov(df[, var2] ~ df[, var1])
            coefs <- c(coefs, unlist(summary(aov1))["Pr(>F)1"])
          } else if(isQualitative(df[, var2]) & !isQualitative(df[, var1])){
            aov1 <- aov(df[, var1] ~ df[, var2])
            coefs <- c(coefs, unlist(summary(aov1))["Pr(>F)1"])
          } else{
            coefs <- c(coefs, NA)
          }
        }
      }
      res[, i] <- coefs
    }
    df1 <- as.data.frame(res)
    colnames(res) <- c(tabMixt)
    rownames(res) <- c(tabMixt) 
 
    corrplot(res, addgrid.col = "darkgray", type = "upper", method = "color", addCoef.col = "gray")
  })
  
  
  # Visualisation par réduction de dimensionnalitié
  varACP <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    return(input$pcaVars)
  }) 
  
  varSuppACP <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    return(input$pcaSupp)
  })
  
  observeEvent(varACP(), {
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    va <- varACP()
    print(va)
    choix <- c("-")
    
    if (is.null(va)) {
      updateSelectInput(inputId = "pcaSupp", choices = choix)
    } else {
      updateSelectInput(inputId = "pcaSupp", choices = c(va))
    }
  })
  
  pcaRes <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    df <- na.omit(cleanData())
    nms <- c(colnames(df))
    nms <- nms[!(nms %in% getQualitatives(df))]
    
    df1 <- df[, c(names(df) %in% nms)]

    if(is.null(varACP())){
      return(PCA(df1, graph=F))
    } else if( !is.null(varACP()) &  is.null(varSuppACP()) ){
      return(PCA(df1[,!(names(df1) %in% c(varACP()))], graph=F))
    } else if(!is.null(varACP()) &  !is.null(varSuppACP()) ){
      indx <- c()
      for(name in varSuppACP()){
        indx <- c(indx, grep(name, colnames(df1)))
      }
      return(PCA(df1[,!(names(df1) %in% c(varACP()))], graph=F, quanti.sup=indx))
    }
  })
  
  
  numCP1 <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    return(input$cp1)
  })
  
  numCP2 <- reactive({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    
    return(input$cp2)
  })
  
  observeEvent(numCP1(), {
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    num <- c(1:length(colnames(cleanData())))
  
    if(is.null(numCP1())){
      updateSelectInput(inputId = "cp2", choices =  )
    } else {
      selection <- strtoi(numCP1())
      num <- num[num!=selection]
      updateSelectInput(inputId = "cp2", choices =  num)
    }
  })
  
  output$pcaVarPlot <- renderPlot({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(pcaRes()) , "Une erreur a eu lieu lors de l'ACP"))
    
    if(is.null(numCP1()) | is.null(numCP2())){
      fviz_pca_var(pcaRes(), axes = c(1,2)) + labs(title ="Cercle de corrélation", x = paste0("PC", 1), y = paste0("PC", 2))
    } else {
      fviz_pca_var(pcaRes(), axes = c(strtoi(numCP1()), strtoi(numCP2()))) + labs(title ="Cercle de corrélation", x = paste0("PC", strtoi(numCP1())), y = paste0("PC",  strtoi(numCP2())))
    }
  }) 
  
  output$pcaIndPlot <- renderPlot({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(pcaRes()) , "Une erreur a eu lieu lors de l'ACP"))
    
    if(is.null(numCP1()) | is.null(numCP2())){
      fviz_pca_ind(pcaRes(), axes = c(1,2)) + labs(title ="Répartition des individus", x = paste0("PC", 1), y = paste0("PC", 2))
    } else {
      fviz_pca_ind(pcaRes(), axes = c(strtoi(numCP1()), strtoi(numCP2()))) + labs(title ="Répartition des individus", x = paste0("PC", strtoi(numCP1())), y = paste0("PC",  strtoi(numCP2())))
    }
  })
  
  
  output$tableACP <- DT::renderDT({
    validate(need(!is.null(cleanData()) , "Veuillez sélectionner un dataset valide"))
    validate(need(!is.null(pcaRes()) , "Une erreur a eu lieu lors de l'ACP"))
    df <- cleanData()
    acp <- pcaRes()
    nbDim <- length(colnames(acp$var$contrib))
        
    
    nameContib <- c()
    nameSqrCos <- c()
    for(i in 1:nbDim){
      nameContib <- c(nameContib, paste0("contrib",i))
      nameSqrCos <- c(nameSqrCos, paste0("sqrCos",i))
    }
    
    colnames(acp$var$contrib) <- nameContib
    colnames(acp$var$cos2) <- nameSqrCos
    
    data <- data.frame(acp$var$contrib, acp$var$cos2)
    
    DT::datatable(data, options = list(scrollX = TRUE))
  })
  
  # Prétraitements
  exdVars <- reactive({
    validate(need(!is.null(data()) , "Veuillez sélectionner un dataset valide"))
    return(input$excludedVars)
  })
  
  naPolicy <- reactive({
    validate(need(!is.null(data()) , "Veuillez sélectionner un dataset valide"))
    return(input$naHandling)
  })
}
