#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  #Utilisation de la fonction réactive pour conserver les paramètres

  data_dep <- reactive({
    input$go_dep
    isolate({
      data_effectif[(profession_sante == input$profession_dep & 
                       annee >= input$periode_dep[1] & 
                       annee <= input$periode_dep[2] & 
                       libelle_departement == input$departement &
                       classe_age != "tout_age"), ]
    })
  })
  
  output$datatable_dep <- renderDataTable({
    data_dep()
  })

  output$comb_plot_dep <- renderPlot({
      tot_dep <- data_dep()[, .(effectif_tot = sum(effectif)), by = .(annee)]
      # draw the histogram with the specified number of bins
      plot1 <- ggplot(data_dep(), aes(x = as.factor(annee), y = effectif, fill = factor(classe_age))) +

        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        labs(x = "Année", y = "Effectif par classe d'âge", fill = "Classe d'âge") +
        theme_minimal() +
        theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
      
      plot2 <- ggplot(tot_dep, aes(x = as.factor(annee), y = effectif_tot)) +
        geom_point() +
        labs(x = "Année", y = "Effectif total") +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
      # faire un autre graphique avec les indicateurs de population (taux)
      combined_plot <- plot2 / plot1 #plot 2 au dessus du plot 1
      combined_plot
      })
  
  # data_reg <- reactive({data_effectif[(profession_sante %in% input$profession_reg & annee %in% range(input$periode_reg[1]:input$periode_reg[2])), ]})
  # 
  # output$comb_plot_reg <- renderPlot({
  #   input$go #
  #   isolate({
  #     # draw the histogram with the specified number of bins
  #     plot1 <- ggplot(data_reg(), aes(x = as.factor(annee), y = effectif, fill = factor(libelle_regartement))) +
  #       geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  #       labs(x = "Année", y = "Effectif par regartement", fill = "regartement") +
  #       theme_minimal() +
  #       theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
  #     effectif_reg <- data_effectif[(libelle_sexe =="tout sexe" & classe_age=="tout_age"), list(effectif = sum(effectif)), by=list(profession_sante= profession_sante, annee=annee, region=libelle_region)]
  #     plot2 <- ggplot((effectif_reg$effectif), aes(x=as.factor(annee), y=effectif)) +
  #       geom_point() +
  #       labs(x = "Année", y = "Effectif total") +
  #       theme_minimal() +
  #       theme(axis.text.x = element_blank(),
  #             axis.ticks.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
  #     # faire un autre graphique avec les indicateurs de population (taux)
  #     combined_plot <- plot2 / plot1 #plot 2 au dessus du plot 1
  #   })
  # })
  data_info <- reactive({
    input$go_info
    isolate({
      data_effectif[(profession_sante == input$profession_info & 
                       annee == max(annee) &
                       # annee >= input$periode_info[1] & 
                       # annee <= input$periode_info[2] & 
                       libelle_region == input$region_info &
                       libelle_departement == input$departement &
                       classe_age != "tout_age" & 
                       libelle_sexe == input$sexe_info), ]
    })
  })

  newdta_info <- newdta[(libelle_sexe =="tout sexe" & classe_age=="tout_age"), list(effectif = round(1/(effectif/Effectif))), by=list(profession_sante== input$profession_info, annee=max(annee), dep=input$departement_info)]
  output$texte_info <- renderText({
    paste(1, input$profession_info, "/", input$newdta_info)
  })
}




  
