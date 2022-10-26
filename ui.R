options(encoding = "UTF-8")

# Packages
library(shiny)
library(shinydashboard)

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