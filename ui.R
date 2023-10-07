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
      menuItem("Données départementales", tabName = "departemental"),
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
      tabItem(tabName = "departemental",
              fluidPage(
                headerPanel("Données départementales"),
                sidebarLayout(
                  sidebarPanel(width = 2,
                               h3("Critères", align ="center"),
                               tags$hr(),
                               radioButtons(
                                 inputId = "echelle",
                                 label = "Echelle géographique",
                                 selected = 5,
                                 choices = c("Régionale" = 4, "Départementale" = 5)
                               ),
                               sliderInput(
                                 inputId ="periode", # se sera input$periode_dep dans le serveur
                                 label = "Choisissez une période",
                                 value = c(2010, 2021),#valeur de base affichée par défaut
                                 min = min(data_effectif[, annee]),
                                 max = max(data_effectif[, annee]),
                                 step = 1,
                                 #Utiliser pour ne pas avoir de virgule en séparateur de millier
                                 sep =""
                               ),
                               conditionalPanel(
                                 condition = "input.echelle == '4'",
                                 selectInput(
                                   inputId = "region",
                                   label = "Région à rechercher :",
                                   #a remplacer par levels(data_effectif[, libelle_departement])
                                   choices = levels(data_effectif[, libelle_region]),
                                   #Pemert le choix de plusieurs departement <- a discuter
                                   multiple = F,
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.echelle == '5'",
                                 selectInput(
                                   inputId = "departement",
                                   label = "Département à rechercher :",
                                   #a remplacer par levels(data_effectif[, libelle_departement])
                                   choices = levels(data_effectif[, libelle_departement]),
                                   #Pemert le choix de plusieurs departement <- a discuter
                                   multiple = F,
                                   selected = "Hérault"
                                 )
                               ),
                               selectInput(
                                 inputId = "profession",
                                 label = "Profession libérale d'intérêt:",
                                 #a remplacer par levels(data_effectif[, profession_sante])
                                 choices = levels(data_effectif[, profession_sante]),
                                 #Pemert le choix de plusieurs professions <- a discuter
                                 multiple = F,
                                 selected = "Chirurgiens"
                               ),
                               selectInput(
                                 inputId = "profession_comp",
                                 label = "Profession libérale à comparer:",
                                 #a remplacer par levels(data_effectif[, profession_sante])
                                 choices = levels(data_effectif[, profession_sante]),
                                 #Pemert le choix de plusieurs professions <- a discuter
                                 multiple = T
                               ),
                               checkboxGroupInput(
                                 inputId = "sexe", 
                                 label = "Please select", 
                                 #a remplacer par levels(test[, libelle_sexe])
                                 selected = "F",
                                 #a remplacer par levels(test[, libelle_sexe])
                                 choices = levels(data_effectif[, libelle_sexe])
                               ),
                               div(actionButton(
                                 inputId = "go",
                                 label = "MAJ",
                                 icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(width =10,#partie droit
                            conditionalPanel(condition = "input.echelle = '5'",
                                             tabsetPanel(
                                               tabPanel("Carte",
                                                        leafletOutput("carte_region"),
                                                        textOutput("scale")
                                               ),
                                               tabPanel("Graphique",
                                                        tags$hr(),
                                                        fluidRow(
                                                          box(title = "A adapter avec un textOutput",
                                                              width = 12,
                                                              #plotOutput("comb_plot")
                                                              highchartOutput("comb_plot"))),
                                                        fluidRow(
                                                          box(title = "A adapter avec un textOutput",
                                                              width = 6,
                                                              plotOutput("pyr")),
                                                          box(title = "A adapter avec un textOutput",
                                                              width = 6,
                                                              highchartOutput("comp_pro"))
                                                        ),
                                                        fluidRow(
                                                          box(title = "A adapter avec un textOutput",
                                                              width = 12,
                                                              #plotOutput("comb_plot")
                                                              highchartOutput("hono_patien")))
                                                        
                                               ),
                                               tabPanel("Data",
                                                        dataTableOutput(outputId = "datatable")
                                               )
                                             )
                                             ),
                            conditionalPanel(condition = "input.echelle == '4'",
                                             h4("Éléments pour l'Option 2"))
                            #decomposition en onglet dans l'affichage principal
                            
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
                                           choices = levels(data_effectif[, libelle_departement]), multiple = FALSE, selected = "Hérault"),
                               
                               div(actionButton(inputId = "go_info",
                                                label = "MAJ",
                                                icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(
                    fluidRow(
                      box(title = textOutput(outputId = "region_info"), width = 12, solidHeader = TRUE, status = "primary", color = "#286192",
                          fluidRow(
                            
                            column(width = 4,
                                   box(title = textOutput(outputId = "nb_info"), width = NULL, solidHeader = TRUE, status = "success", textOutput(outputId = "texte_info"),
                                       
                                       #box(title = "Number of Customers", width = NULL, status = "success", textOutput(outputId = "texte_info")),
                                       
                                       #box(title = "Percent of Total Customers", width = NULL, status = "success", infoBoxOutput("percentTotalStayed"))
                                   )),
                            column(width = 8,
                                   box(title = "Comparaison", width = NULL, solidHeader = TRUE, status = "warning",
                                       box(title = "Autres départements de la région : ", width = NULL, status = "warning", htmlOutput(outputId = "comparaison_region")),
                                       
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


