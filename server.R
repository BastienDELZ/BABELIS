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
      newdta[(profession_sante %in% c(input$profession_dep) & 
                annee >= input$periode_dep[1] & 
                annee <= input$periode_dep[2] & 
                libelle_departement == input$departement &
                classe_age != "tout_age"), ]
    })
  })
  
  data_comp_dep <- reactive({
    input$go_dep
    isolate({
      newdta[(libelle_sexe == "tout sexe" & 
                profession_sante %in% c(input$profession_dep, input$profession_dep_comp) &
                annee >= input$periode_dep[1] & 
                annee <= input$periode_dep[2] &
                libelle_departement == input$departement &
                classe_age == "tout_age"), ]
    })
  })
  
  observe({
    updateSelectInput(session,
                      inputId = "profession_dep_comp",
                      label = "Profession libérale d'intérêt:",
                      #a remplacer par levels(data_effectif[, profession_sante])
                      choices = setdiff(levels(data_effectif$profession_sante), input$profession_dep),
                      #Pemert le choix de plusieurs professions <- a discuter
    )
  })
  
  output$datatable_dep <- renderDataTable({
    data_dep()
  })
  
  # output$comb_plot_dep <- renderPlot({
  #   tot_dep <- data_dep()[, .(effectif_tot = sum(effectif)), by = .(annee)]
  #   # draw the histogram with the specified number of bins
  #   plot1 <- ggplot(data_dep(), aes(x = as.factor(annee), y = effectif, fill = factor(classe_age))) +
  #     
  #     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  #     labs(x = "Année", y = "Effectif par classe d'âge", fill = "Classe d'âge") +
  #     theme_minimal() +
  #     theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
  #   
  #   plot2 <- ggplot(tot_dep, aes(x = as.factor(annee), y = effectif_tot)) +
  #     geom_point() +
  #     labs(x = "Année", y = "Effectif total") +
  #     theme_minimal() +
  #     theme(axis.text.x = element_blank(),
  #           axis.ticks.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))
  #   # faire un autre graphique avec les indicateurs de population (taux)
  #   combined_plot <- plot2 / plot1 #plot 2 au dessus du plot 1
  #   combined_plot
  # })
  
  
  output$comb_plot_dep <- renderHighchart({
    tot_dep <- data_dep()[, .(effectif_tot = sum(effectif)), by = .(annee)]
    input$go_dep
    isolate({
    highchart() %>%
      hc_add_series(data_dep(), "column", hcaes(x = annee, y = effectif, group = classe_age)) %>% 
      hc_add_series(tot_dep, "line",hcaes(x = annee, y = effectif_tot), yAxis=1, name = paste("Effectif",input$profession_dep)) %>%
      hc_yAxis_multiples(
        list(
          title = list(text = paste("Effectif de", input$profession_dep, "par classe d'âge")), 
          opposite = F,
          showLastLabel = FALSE),
        list(
          title = list(text = paste("Effectif de ", input$profession_dep,"en", input$departement)), 
          opposite = T,
          showLastLabel = FALSE)
      ) %>%
      hc_tooltip(shared = TRUE)
    })
  })
  
  newdta_info <- reactive({
    input$go_info
    isolate({
      newdta[(profession_sante == input$profession_info & 
                
                annee == max(annee) &
                libelle_region == input$region_info &
                libelle_departement == input$departement_info &
                classe_age == "tout_age" & 
                libelle_sexe == "tout sexe"), list(effectif = round(1/(effectif/Effectif)))]
    })
  })
  
  output$texte_info <- renderText({
    paste(1, input$profession_info, "pour", newdta_info(), "habitants")
  })
  
  newdta_comp_region <- reactive({
    input$go_info
    isolate({
      newdta[(profession_sante == input$profession_info & 
                annee == max(annee) &
                libelle_region == input$region_info &
                libelle_departement != input$departement_info &
                classe_age == "tout_age" & 
                libelle_sexe == "tout sexe"), list(effectif = round(1/(effectif/Effectif)), by = libelle_departement)]
    })
  })
  
  output$pyr_dep <- renderPlot({
    pyr_dep_dta <- data_dep()[libelle_sexe %in% c("femmes", "hommes") & annee == max(annee),]
    pyr_dep_dta$classe_age <- droplevels(pyr_dep_dta$classe_age)
    pyr_dep_dta$libelle_sexe <- droplevels(pyr_dep_dta$libelle_sexe)
    pyr_dep_dta <- pyr_dep_dta[libelle_sexe =="femmes", effectif := -effectif]
    pyr_dep_dta <- nvx_classe_age(pyr_dep_dta)
    
    
    # range_pyr_dep <- seq(round(min(pyr_dep_dta$effectif),-(10^(floor(log10(min(pyr_dep_dta$effectif)))+1))), 
    #                      max(pyr_dep_dta$effectif),
    #                      by = 10^(floor(log10(max(abs(min(pyr_dep_dta$effectif)), max(pyr_dep_dta$effectif))))-1)
    #                      )
    
    pyr_dep <- ggplot(pyr_dep_dta, aes(x = class_age_num, y = effectif, fill = libelle_sexe)) + 
      geom_bar(data = pyr_dep_dta[libelle_sexe =="femmes",], stat = "identity") +
      geom_bar(data = pyr_dep_dta[libelle_sexe =="hommes",], stat = "identity") +
      scale_x_continuous(breaks = seq(1,nlevels(pyr_dep_dta$classe_age), by =1),
                         labels = levels(pyr_dep_dta$classe_age)) +
      coord_flip()
    # scale_y_continuous(breaks  = range_pyr_dep,
    #                    labels = abs(range_pyr_dep))
    pyr_dep
  })
  
  output$comp_pro_dep <- renderHighchart({
    hchart(data_comp_dep()[order(annee)] , "line", hcaes(x = annee, y = effectif, group = profession_sante))
  })
  
  output$hono_patien <- renderHighchart({
    highchart() %>%
      hc_add_series(data_comp_dep(), "line", hcaes(annee, hono_sans_depassement_moyens, group = profession_sante), yAxis=1) %>% 
      hc_add_series(data_comp_dep(), "line", hcaes(x = annee, y = nombre_patients_uniques , group = profession_sante)) %>%
      hc_yAxis_multiples(
        list(
          title = list(text = "Patientele"), 
          opposite = F),
        list(
          title = list(text = "Salaire moyen hors depassement"), 
          opposite = T)
      ) %>%
      hc_tooltip(shared = TRUE)
  })
  
  # output$comparaison_region <- renderText({
  #   texte <- ""
  #   for(i in 1:nrow(newdta_comp_region())){
  #     ntext <- paste(newdta_comp_region()[[i,2]], ":", newdta_comp_region()[[i,1]])
  #     texte <- paste(texte, ntext, sep = '<br/>')
  #   }
  #   HTML(texte)
  # })
  
  output$comparaison_region <- renderText({
    if (is.infinite(newdta_comp_region()[[1,1]])){
      texte <- paste(newdta_comp_region()[[1,2]], " : Données manquantes pour les",input$profession_info)
    } else{
      texte <- paste("<b>",newdta_comp_region()[[1,2]], " :" ,1, input$profession_info, "pour", newdta_comp_region()[[1,1]] , "hab", "</b>")
    }
    for(i in 2:nrow(newdta_comp_region())){
      if (is.infinite(newdta_comp_region()[[i,1]])){
        ntext <- (paste(newdta_comp_region()[[i,2]]," : Données manquantes pour les",input$profession_info))
      } else{
        ntext <- paste("<b>",newdta_comp_region()[[i,2]], " :" ,1, input$profession_info, "pour", newdta_comp_region()[[i,1]] , "hab","</b>")
      }
      texte <- paste(texte,ntext, sep = '<br/>')
    }
    HTML(texte)
    
  })
  
  output$region_info <- renderText({
    input$go_info
    isolate({
      input$region_info
    })
  })
  
  output$nb_info <- renderText({
    input$go_info
    isolate({
      paste("Nombre de ", input$profession_info, "ramené à la population")
    })
  })
  
  # 
  # data_carte_region <- reactive({
  #   
  #   isolate({
  #     newdta[(profession_sante == input$profession_dep & 
  #               annee == max(annee) &
  #               classe_age == "tout_age"), list(effectif = round(1/(effectif/Effectif))), by= libelle_departement]
  #   })
  # })
  # 
  # output$carte_region <- renderLeaflet({
  #   qpal <- colorQuantile(palette = "YlGnBu",n=5,domain = data_carte_region()$effectif[!is.infinite(data_carte_region()$effectif)])
  #   leaflet(data_carte) %>% addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity=0.5,fillColor = ~qpal(data_carte_region()$effectif)) 
  #   #addProviderTiles
  # })
  
  
  data_carte_region <- reactive({
    input$go_dep
    isolate({
      a <- newdta[(profession_sante == input$profession_dep &
                     annee == max(annee) &
                     classe_age == "tout_age" & libelle_sexe=="tout sexe"), list(eff = round(1/(s_par_region/S_EFF_region))), by= libelle_departement]
      indices_tri <- match(data_carte$NAME_2, a$libelle_departement)
      a <- a[indices_tri, ]
    })
  })
  
  output$carte_region <- renderLeaflet({
    #qpal <- colorNumeric(palette = "YlGnBu", domain = data_carte_region()$eff[!is.infinite(data_carte_region()$eff)])
    #qpal <- colorBin(palette = "YlGnBu", domain = data_carte_region()$eff[!is.infinite(data_carte_region()$eff)], bins = 5)
    qpal <- colorQuantile(palette = "YlGnBu",n=5,domain = data_carte_region()$eff[!is.infinite(data_carte_region()$eff)])
    leaflet(data_carte) %>% addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity=0.5,fillColor = ~qpal(data_carte_region()$eff)) %>% 
      addLegend(pal = qpal, values = ~data_carte_region()$eff, opacity = 1)
    #addProviderTiles
  })
  
}
  