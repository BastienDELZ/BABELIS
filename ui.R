
library(shiny)


ui <- dashboardPage(
  dashboardHeader(title = "BABELIS"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Exploration", 
               tabName = "exploration"),
      menuItem("Info",
               tabName = "info")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet "Exploration des données"
      tabItem(tabName = "exploration",
              fluidPage(
                headerPanel(textOutput("header_name")),
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
                                 multiple = F,
                                 selected = "Chirurgiens"
                               ),
                               selectInput(
                                 inputId = "profession_comp",
                                 label = "Profession libérale à comparer:",
                                 #a remplacer par levels(data_effectif[, profession_sante])
                                 choices = levels(data_effectif[, profession_sante]),
                                 multiple = T
                               ),
                               div(actionButton(
                                 inputId = "go",
                                 label = "MAJ",
                                 icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(width =10,#partie droit

                                             tabsetPanel(
                                               tabPanel("Carte",
                                                        withSpinner(type =5 ,
                                                                    leafletOutput("carte_region")
                                                                    )
                                               ),
                                               tabPanel("Graphique",
                                                        tags$hr(),
                                                        fluidRow(
                                                          box(title = "Effectif de professionnels de santé par année",
                                                              width = 12,
                                                              #plotOutput("comb_plot")
                                                              withSpinner(type =5 ,
                                                                          highchartOutput("comb_plot")
                                                                          )
                                                              )
                                                          ),
                                                        fluidRow(
                                                          box(title = "Pyramide des âges au sein de la profession",
                                                              width = 6,
                                                              withSpinner(type =5, 
                                                                          plotOutput("pyr")
                                                              )
                                                              ),
                                                          box(title = "Nombre d'habitants par praticien par année",
                                                              width = 6,
                                                              withSpinner(type =5,
                                                                          highchartOutput("comp_pro")
                                                              )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(title = "Honoraires annuels et patientèle par profession",
                                                              width = 12,
                                                              #plotOutput("comb_plot")
                                                              withSpinner(type =5,
                                                                          highchartOutput("hono_patien"))))
                                                        
                                               ),
                                               tabPanel("Data",
                                                        dataTableOutput(outputId = "datatable")
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
                  sidebarPanel(width = 2,
                               selectInput("region_info",
                                           label = "Région :",
                                           choices = levels(data_effectif[, libelle_region]), 
                                           multiple = FALSE),
                               selectInput("departement_info",
                                           label = "Département :",
                                           choices = levels(data_effectif[, libelle_departement]), 
                                           multiple = FALSE, 
                                           selected = "Hérault"),
                               selectInput("profession_info", 
                                           label = "Profession :",
                                           choices = levels(data_effectif[, profession_sante]), 
                                           multiple = FALSE),
                               div(actionButton(inputId = "go_info",
                                                label = "MAJ",
                                                icon = icon("rotate")
                               ),
                               align = "center")
                  ),
                  mainPanel(width = 10,
                    fluidRow(
                      box(title = textOutput(outputId = "region_info"), 
                          width = 12, 
                          solidHeader = TRUE, 
                          status = "primary", 
                          color = "#286192",
                          fluidRow(
                            column(width = 4,
                                   box(title = textOutput(outputId = "nb_info"),
                                       width = NULL, 
                                       solidHeader = TRUE, 
                                       status = "success", 
                                       textOutput(outputId = "texte_info")
                                       
                                       
                                       #box(title = "Percent of Total Customers", width = NULL, status = "success", infoBoxOutput("percentTotalStayed"))
                                   ))
                            # column(width = 4,
                            #        box(title = "Comparaison national", width = NULL, solidHeader = TRUE, status = "success", htmlOutput(outputId = "comp_hono" ))
                            #            
                            #            
                            #            #box(title = "Percent of Total Customers", width = NULL, status = "success", infoBoxOutput("percentTotalStayed"))
                            #        )
                            
                          ),
                          "Comparaison du nombre d'habitant pour 1 praticien avec les autres départements de la région :",
                          tags$br(),
                          tags$br(),
                          htmlOutput(outputId = "comparaison_region"),
                          tags$br(),
                          htmlOutput(outputId = "hono_info"),
                          tags$br(),
                          htmlOutput(outputId = "comparaison_hono")
                      )),
                    
                  )
                )
              )
      )
    )
  )
)


