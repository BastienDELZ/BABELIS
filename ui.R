#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Application"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Données nationales", tabName = "national"),
      menuItem("Données régionales", tabName = "regional"),
      menuItem("Données départementales", tabName = "departmental"),
      menuItem("Info", tabName = "info")
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
                  sidebarPanel(
                    selectInput("departement_departmental", label = "Sélectionnez un département :",
                                choices = c("Hérault", "J'ai pas d'inspi"), multiple = FALSE),
                    selectInput("profession_departmental", label = "Profession :",
                                choices = c("Dentiste", "Rhumatologue"), multiple = FALSE)
                  ),
                  mainPanel(
                    # Ajoutez ici le contenu principal pour "Données départementales"
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
                                           choices = c("Dentiste", "Rhumatologue"), multiple = FALSE),
                               selectInput("region_info", label = "Région :",
                                           choices = c("Bretagne", "J'ai pas d'inspi"), multiple = FALSE),
                               selectInput("departement_info", label = "Département :",
                                           choices = c("Hérault", "J'ai pas d'inspi"), multiple = FALSE),
                               checkboxGroupInput("sexe_info", label = "Sélectionnez le sexe :",
                                                  choices = c("Femme", "Homme"), selected = "Femme")
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
                                       #box(title = "Number of Customers", width = NULL, status = "success", infoBoxOutput("customerCountStayed")),
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


