#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

shinyUI(
  navbarPage("Application",
             #tabPanel = Création des onglet de navigation de la page dans son ensemble
             tabPanel("Données nationales"),
             tabPanel("Données régionales",
                      sidebarLayout(
                        #Creation de l'espace interactif avec curseur, bouton...
                        sidebarPanel(width = 3,
                                     h3("Critères", align ="center"),
                                     tags$hr(),#saut de ligne
                          sliderInput("periode_reg", 
                                      label = "Choisissez une période",
                                      value = c(2010, 2021),#valeur de base affichée par défaut
                                      min = 2010,
                                      max = 2021,
                                      step = 1,
                                      #Utiliser pour ne pas avoir de virgule en séparateur de millier
                                      sep =""
                                      ),
                          selectInput(
                            inputId = "profession_reg",
                            label = "Profession(s) libérale(s) :",
                            #a remplacer par levels(data_effectif[, profession_sante])
                            choices = c("dentistes", "rumatologues"),
                            #Pemert le choix de plusieurs professions <- a discuter
                            multiple =F
                          ),
                          selectInput(
                            inputId = "region",
                            label = "Région à rechercher :",
                            #a remplacer par levels(data_effectif[, libelle_departement])
                            choices = c("Bretagne", "Grand Est"),
                            #Pemert le choix de plusieurs departement <- a discuter
                            multiple = F
                          ),
                          checkboxGroupInput(inputId = "idCheckGroup", 
                                             label = "Please select", 
                                             #a remplacer par levels(test[, libelle_sexe])
                                             selected = "F",
                                             #a remplacer par levels(test[, libelle_sexe])
                                             choices = c("F","H")),
                          div(actionButton(inputId = "go",
                                       label = "MAJ",
                                       icon = icon("rotate")
                                       ),
                              align = "center")
                        ),
                        # Espace affichage principal dans chaque onglet
                        mainPanel(#partie droite
                          #decomposition en onglet dans l'affichage principal
                          tabsetPanel(
                            tabPanel("Carte"),
                            tabPanel("Graphique",
                                     plotOutput("comb_plot_reg")
                                     ),
                            tabPanel("Data")
                          )
                        )
                      )),
             
             tabPanel("Données départementales",
                      sidebarLayout(
                        #Creation de l'espace interactif avec curseur, bouton...
                        sidebarPanel(width = 3,
                                     h3("Critères", align ="center"),
                                     tags$hr(),#saut de ligne
                          sliderInput("periode_dep", # se sera input$periode_dep dans le serveur
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
                          checkboxGroupInput(inputId = "sexe_dep", 
                                             label = "Please select", 
                                             #a remplacer par levels(test[, libelle_sexe])
                                             selected = "F",
                                             #a remplacer par levels(test[, libelle_sexe])
                                             choices = levels(data_effectif[, libelle_sexe])
                                             ),
                          div(actionButton(inputId = "go",
                                       label = "MAJ",
                                       icon = icon("rotate")
                                       ),
                              align = "center")
                        ),
                        # Espace affichage principal dans chaque onglet
                        mainPanel(#partie droite
                          #decomposition en onglet dans l'affichage principal
                          tabsetPanel(
                            tabPanel("Carte",
                                     plotOutput("plot1")
                                     ),
                            tabPanel("Graphique",
                                     plotOutput("comb_plot_dep")
                                     ),
                            tabPanel("Data",
                                     dataTableOutput(outputId = "datatable_dep")
                                     )
                          )
                        )
                      )
             ),
             tabPanel("Info",
                      sidebarLayout(
                        #Creation de l'espace interactif avec curseur, bouton...
                        sidebarPanel(width = 3,
                                     h3("Critères", align ="center"),
                                     tags$hr(),#saut de ligne
                                     selectInput(
                                       inputId = "profession_res",
                                       label = "Profession(s) libérale(s) :",
                                       #a remplacer par levels(data_effectif[, profession_sante])
                                       choices = c("dentiste", "j'ai pas d'inspi"),
                                       #Pemert le choix de plusieurs professions <- a discuter
                                       multiple =F
                                     ),
                                     selectInput(
                                       inputId = "region_res",
                                       label = "Region :",
                                       #a remplacer par levels(data_effectif[, profession_sante])
                                       choices = c("bretagne", "j'ai pas d'inspi"),
                                       #Pemert le choix de plusieurs professions <- a discuter
                                       multiple =F
                                     ),
                                     selectInput(
                                       inputId = "departement_res",
                                       label = "Département :",
                                       #a remplacer par levels(data_effectif[, libelle_departement])
                                       choices = c("Hérault", "j'ai pas d'inspi"),
                                       #Pemert le choix de plusieurs departement <- a discuter
                                       multiple =F
                                     ),
                                     
                                     checkboxGroupInput(inputId = "sexe_res", 
                                                        label = "Please select", 
                                                        #a remplacer par levels(test[, libelle_sexe])
                                                        selected = "F",
                                                        #a remplacer par levels(test[, libelle_sexe])
                                                        choices = c("F","H")),
                                     div(actionButton(inputId = "go",
                                                      label = "MAJ",
                                                      icon = icon("rotate")
                                     ),
                                     align = "center")
                        ),
                        # Espace affichage principal dans chaque onglet
                        mainPanel(#partie droite
                          fluidPage(
                            headerPanel("Résumé de votre requête"),
                          fluidRow(
                            column(
                              width = 4,
                              div(
                                h4("Nombre de dentistes"),
                                p("Contenu de la boîte 1..."),

                            )
                                
                              )
                                
                               
                          )
                          )
                                   
  )
)
)
)
)







