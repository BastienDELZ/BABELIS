#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)


ui <- dashboardPage(
  dashboardHeader(title = "Application"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Données nationales", tabName = "national"),
      menuItem("Données régionales", tabName = "regional"),
      menuItem("Données départementales", tabName = "departmental"),
      menuItem("Info",tabName = "info")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet "Données nationales"
      tabItem(tabName = "national",
              fluidPage(
                headerPanel("Données nationales"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("annee", label = "Sélectionnez une année",
                                min = 2010, max = 2021, value = c(2010, 2021)),
                    selectInput("profession_national", label = "Profession :",
                                choices = c("Dentiste", "Rhumatologue"), multiple = FALSE)
                  ),
                  mainPanel(
                    # Ajoutez ici le contenu spécifique à cet onglet
                  )
                )
              )
      ),
      
      # Onglet "Données régionales"
      tabItem(tabName = "regional",
              fluidPage(
                headerPanel("Données régionales"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("region_regional", label = "Sélectionnez une région :",
                                choices = c("Bretagne", "Grand Est"), multiple = FALSE),
                    selectInput("profession_regional", label = "Profession :",
                                choices = c("Dentiste", "Rhumatologue"), multiple = FALSE)
                  ),
                  mainPanel(
                    # Ajoutez ici le contenu principal pour "Données régionales"
                  )
                )
              )
      ),
      
      # Onglet "Données départementales"
      tabItem(tabName = "departmental",
              fluidPage(
                headerPanel("Données départementales"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               h3("Critères", align ="center"),
                               tags$hr(),
                               sliderInput(
                                 inputId ="periode_dep", # se sera input$periode_dep dans le serveur
                                 label = "Choisissez une période",
                                 value = c(2010, 2021),#valeur de base affichée par défaut
                                 min = min(data_effectif[, annee]),
                                 max = max(data_effectif[, annee]),
                                 step = 1,
                                 #Utiliser pour ne pas avoir de virgule en séparateur de millier
                                 sep =""
                               ),
                               selectInput(
                                 inputId = "profession_dep",
                                 label = "Profession(s) libérale(s) :",
                                 #a remplacer par levels(data_effectif[, profession_sante])
                                 choices = levels(data_effectif[, profession_sante]),
                                 #Pemert le choix de plusieurs professions <- a discuter
                                 multiple = F
                               ),
                               selectInput(
                                 inputId = "departement",
                                 label = "Département(s) à rechercher :",
                                 #a remplacer par levels(data_effectif[, libelle_departement])
                                 choices = levels(data_effectif[, libelle_departement]),
                                 #Pemert le choix de plusieurs departement <- a discuter
                                 multiple = F
                               ),
                               checkboxGroupInput(
                                 inputId = "sexe_dep", 
                                 label = "Please select", 
                                 #a remplacer par levels(test[, libelle_sexe])
                                 selected = "F",
                                 #a remplacer par levels(test[, libelle_sexe])
                                 choices = levels(data_effectif[, libelle_sexe])
                               ),
                               div(actionButton(
                                 inputId = "go_dep",
                                 label = "MAJ",
                                 icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(#partie droite
                    #decomposition en onglet dans l'affichage principal
                    tabsetPanel(

                      tabPanel("Carte"
                      ),
                      tabPanel("Graphique",
                               tags$hr(),
                               plotOutput("comb_plot_dep")
                      ),
                      tabPanel("Data",
                               dataTableOutput(outputId = "datatable_dep")
                      )
                    )
                  )
                )
              )
      ),
      
      # Onglet "Info"
      tabItem(tabName = "info",
              fluidPage(
                headerPanel("Résumé de votre requête - Info"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               selectInput("profession_info", label = "Profession :",
                                           choices = levels(data_effectif[, profession_sante]), multiple = FALSE),
                               selectInput("region_info", label = "Région :",
                                           choices = levels(data_effectif[, libelle_region]), multiple = FALSE),
                               selectInput("departement_info", label = "Département :",
                                           choices = levels(data_effectif[, libelle_departement]), multiple = FALSE),
                               checkboxGroupInput("sexe_info", label = "Sélectionnez le sexe :",
                                                  choices = levels(data_effectif[, libelle_sexe])),
                               div(actionButton(inputId = "go_info",
                                                label = "MAJ",
                                                icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(
                    fluidRow(
                      box(title = "Région", width = 12, solidHeader = TRUE, status = "primary", color = "#286192",
                          fluidRow(
                            column(width = 4,
                                   box(title = "Nombre de dentistes", width = NULL, solidHeader = TRUE, status = "primary",
                                       box(title = "Femme(s) : ", width = NULL, status = "primary", infoBoxOutput("customerCountNoFilter")),
                                       box(title = "Homme(s) : ", width = NULL, status = "primary", infoBoxOutput("percentTotalNoFilter"))
                                   )),
                            column(width = 4,
                                   box(title = "Et en ramenant à la population ?", width = NULL, solidHeader = TRUE, status = "success",
                                       box(title = "Number of Customers", width = NULL, status = "success", textOutput(outputId = "texte_info")),

                                       #box(title = "Percent of Total Customers", width = NULL, status = "success", infoBoxOutput("percentTotalStayed"))
                                   )),
                            column(width = 4,
                                   box(title = "Comparaison", width = NULL, solidHeader = TRUE, status = "warning",
                                       box(title = "Ile de France : ", width = NULL, status = "warning", infoBoxOutput("customerCountChurn")),
                                       box(title = "Mayotte : ", width = NULL, status = "warning", infoBoxOutput("percentTotalChurn"))
                                   ))
                          )
                          
                      )),
                    
                  )
                )
              )
      )
    )
  )
)


