options(encoding = "UTF-8")

# Packages
library(shiny)
library(shinydashboard)

# Partie Graphique
ui <- dashboardPage(
  dashboardHeader(title = "ASCD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement", tabName = "chargement", icon = icon("arrow-right-to-file", verify_fa = FALSE)),
      menuItem("Exploration", tabName = "exploration", icon = icon("pie-chart", verify_fa = FALSE)),
      menuItem("Decision Tree", tabName = "decision-tree", icon = icon("project-diagram", verify_fa = FALSE)),
      menuItem("Logistic Regression", tabName = "logistic-regression", icon = icon("cogs", verify_fa = FALSE)),
      menuItem("SVM", tabName = "svm", icon = icon("line-chart", verify_fa = FALSE)),
      menuItem("Sampling", tabName = "sampling", icon = icon("gauge-simple-med", verify_fa = FALSE))
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
            .datatables {
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
                  height = 550, width = 12, class="unique"
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
              
              div(h2("Pré-traitements"), style="margin-left:1%; margin-bottom:3%;"),
              fluidRow(
                column(title = "Variables à exclure de l'étude", width = 6, selectInput("excludedVars", label="Variables à exclure de l'étude", choices=c(), multiple=TRUE)),
                
                column(title = "Gestion des valeurs manquantes", width = 6, selectInput("naHandling", label="Gestion des valeurs manquantes", choices=c("Ignorer", "Supprimer les individus", "Remplacer par la moyenne de la colonne (qtt)"))),
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
              ),
              
              fluidRow(
                column(title = "Distribution de l'attribut selon les classes", width = 12,
                       selectInput("boxRepart", label="Répartition de la variable", choices=c("-")),
                       box(width = NULL, plotOutput("boxPlotRepart"))
                )
              ),
              fluidRow(
                column(title = "Coefficient de Pearson", width = 6,
                       selectInput("corrContVar", label="Coefficients de corrélation entre variables quantitatives", choices=c("-"), multiple=TRUE),
                       box(width = NULL, plotOutput("corrContPlotVar"))
                ),
                column(title = "V de Cramer (Qualitatives) / Test ANOVA (Mixtes)", width = 6,
                       selectInput("corrMixVar", label="Test statistique sur variables qualitatives ou mixtes", choices=c("-"), multiple=TRUE),
                       box(width = NULL, plotOutput("corrMixPlotVar"))
                )
              ),
              
              div(h2("Réduction de la dimensionnalité"), style="margin-left:1%; margin-bottom:3%;"),
              
              fluidRow(
                column(width = 6, selectInput("pcaVars", label="Variables à exclure (ACP)", choices=c(), multiple=TRUE)),
                column(width = 6, selectInput("pcaSupp", label="Variables supplémentaires", choices=c(), multiple=TRUE))
              ),
              
              fluidRow(
                column(width = 6, selectInput("cp1", label="Première composante principale", choices=c())),
                column(width = 6, selectInput("cp2", label="Seconde composante principale", choices=c()))
              ),
              
              fluidRow(
                column(title = "Cercle de corrélation", width = 6, box(width = NULL, plotOutput("pcaVarPlot"))),
                column(title = "Représentation des individus", width = 6, box(width = NULL, plotOutput("pcaIndPlot")))
              ),
              
              fluidRow(
                column(title = "Contributions et qualité de représentation", width = 12, 
                       box(height=550, width = NULL, class="unique", DT::dataTableOutput("tableACP")))
              ),
      
      ),
      
      # Classification par arbre de décision
      tabItem(tabName = "decision-tree",
          div(h2("Paramètres par défaut"), style="margin-left:2%;"),
          fluidRow(
            column(title = "Variables à exclure", width = 6, selectInput("dtVars", label="Variables à exclure (DT)", choices=c(), multiple=TRUE))
          ),
          fluidRow(
            column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracyDT"), align="center")),
            column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionDT"), align="center")),
            column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallDT"), align="center")),
            column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1DT"), align="center"))
          ),
          fluidRow(
            column(title = "Arbre", width = 6, box(width = NULL, plotOutput("plotDT"))),
            column(title = "Courbe ROC", width = 6, box(width = NULL, plotOutput("aucDT")))
          ),
          
          fluidRow(
            column(width = 6, div(h2("Grid Search"), style="margin-left:2%;")),
            column(width = 6, infoBoxOutput("dtParam"))
          ),
          fluidRow(
            column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracyDTGS"), align="center")),
            column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionDTGS"), align="center")),
            column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallDTGS"), align="center")),
            column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1DTGS"), align="center"))
          ),
          fluidRow(
            column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucDTGS"))),
          )
      ),
      
      # Classification par LR
      tabItem(tabName = "logistic-regression",
        div(h2("Paramètres par défaut"), style="margin-left:2%;"),
        fluidRow(
          column(title = "Variables à exclure", width = 6, selectInput("lrVars", label="Variables à exclure (LR)", choices=c(), multiple=TRUE))
        ),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracyLR"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionLR"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallLR"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1LR"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucLR"))),
        ),
        fluidRow(
          column(width = 6, div(h2("Grid Search"), style="margin-left:2%;")),
          column(width = 6, infoBoxOutput("lrParam"))
        ),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracyLRGS"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionLRGS"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallLRGS"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1LRGS"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucLRGS"))),
        )     
      ),
      
      # Classification par SVM
      tabItem(tabName = "svm",
        div(h2("Paramètres par défaut"), style="margin-left:2%;"),
        fluidRow(
          column(title = "Variables à exclure", width = 6, selectInput("svmVars", label="Variables à exclure (SVM)", choices=c(), multiple=TRUE))
        ),
        
        div(h3("Sans kernel"), style="margin-left:2%;"),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracySVM"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionSVM"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallSVM"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1SVM"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucSVM"))),
        ),
        
        
        div(h3("Avec kernel (radial)"), style="margin-left:2%;"),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracySVMK"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionSVMK"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallSVMK"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1SVMK"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucSVMK"))),
        ),
        
        div(h2("Grid Search"), style="margin-left:2%;"),
        fluidRow(
          column(width = 6, div(h3("Sans kernel"), style="margin-left:2%;")),
          column(width = 6, infoBoxOutput("svmParam"))
        ),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracySVMGS"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionSVMGS"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallSVMGS"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1SVMGS"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucSVMGS"))),
        ),
        
        fluidRow(
          column(width = 6, div(h3("Avec kernel (Radial)"), style="margin-left:2%;")),
          column(width = 6, infoBoxOutput("svmKParam"))
        ),
        fluidRow(
          column(width = 3, box(title = "Accuracy", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("accuracySVMKGS"), align="center")),
          column(width = 3, box(title = "Precision", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("precisionSVMKGS"), align="center")),
          column(width = 3, box(title = "Recall", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("recallSVMKGS"), align="center")),
          column(width = 3, box(title = "F-measure", width = NULL, solidHeader = TRUE, status = "primary", infoBoxOutput("f1SVMKGS"), align="center"))
        ),
        fluidRow(
          column(title = "Courbe ROC", width = 12, box(width = NULL, plotOutput("aucSVMKGS"))),
        ),
      ),
      
      # Echantillonage
      tabItem(tabName = "sampling",
          fluidRow(
            column(width = 12, box(width = NULL, selectInput("samplMethod", label="Veuillez choisir la méthode de sampling à appliquer...", choices=c("", "Undersampling", "Oversampling")))),
          )    
      )
      
    )
  )
)