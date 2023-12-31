################################
# A.HUSSON, K.LENOTTE, B.DELEUZE
################################

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  #Utilisation de la fonction réactive pour conserver les paramètres
  observeEvent(input$disparitus, {
      shinyjs::toggle(id = "side")
    })
  
  
  output$header_name <- renderText({
    input$go
    isolate({
      ifelse(input$echelle == '5', "Données Départementales", "Données Régionales")
    })
  })
  
  observe({
    updateSelectInput(session,
                      inputId = "profession_comp",
                      label = "Profession libérale à comparer:",
                      #a remplacer par levels(data_effectif[, profession_sante])
                      choices = setdiff(levels(data_effectif$profession_sante), 
                                        input$profession),
                      #Pemert le choix de plusieurs professions <- a discuter
    )
  })
  
  observeEvent(input$departement,
               {
                 updateSelectInput(session,
                                   inputId = "profession",
                                   #a remplacer par levels(data_effectif[, profession_sante])
                                   choices = levels(droplevels(data_effectif[(libelle_departement == input$departement & libelle_sexe == "tout sexe" & classe_age == "tout_age" & effectif > 0 & annee == max(annee)),])$profession)
                                   #Pemert le choix de plusieurs professions <- a discuter
                 )
               })
  
  observeEvent(input$region,
               {
                 updateSelectInput(session,
                                   inputId = "profession",
                                   choices = levels(droplevels(data_effectif[(libelle_region == input$region & libelle_sexe == "tout sexe" & classe_age == "tout_age" & effectif > 0 & annee == max(annee)),])$profession)

                 )
               })
  
  observeEvent(input$echelle,
               {
                 if(input$echelle == "5"){
                   data_dep <- reactive({
                     input$go
                     isolate({
                       newdta[(profession_sante %in% c(input$profession) & 
                                 annee >= input$periode[1] & 
                                 annee <= input$periode[2] & 
                                 libelle_departement == input$departement &
                                 classe_age != "tout_age"), ]
                     })
                   })
                   
                   data_comp_dep <- reactive({
                     input$go
                     isolate({
                       newdta[(libelle_sexe == "tout sexe" & 
                                 profession_sante %in% c(input$profession, input$profession_comp) &
                                 annee >= input$periode[1] & 
                                 annee <= input$periode[2] &
                                 libelle_departement == input$departement &
                                 classe_age == "tout_age"), ]
                     })
                   })
                   
                   output$comb_plot <- renderHighchart({
                     tot_dep <- data_dep()[libelle_sexe == "tout sexe", .(effectif_tot = sum(effectif)), by = .(annee)]
                     input$go
                     isolate({
                       highchart() %>%
                         hc_add_series(data_dep()[libelle_sexe == "tout sexe",],
                                       "column",
                                       hcaes(x = annee, y = effectif, group = classe_age)) %>% 
                         hc_add_series(tot_dep,
                                       "line",
                                       hcaes(x = annee, y = effectif_tot), 
                                       yAxis=1, 
                                       name = paste("Effectif",input$profession)) %>%
                         hc_yAxis_multiples(
                           list(
                             title = list(text = paste("Effectif de", input$profession, "par classe d'âge")), 
                             opposite = F,
                             showLastLabel = FALSE),
                           list(
                             title = list(text = paste("Effectif de ", input$profession,"en", input$departement)), 
                             opposite = T,
                             showLastLabel = FALSE)
                         ) %>%
                         hc_tooltip(shared = TRUE)
                     })
                   })
                   
                   output$pyr <- renderPlot({
                     pyr_dta <- data_dep()[libelle_sexe %in% c("femmes", "hommes") & annee == max(annee),]
                     pyr_dta$classe_age <- droplevels(pyr_dta$classe_age)
                     pyr_dta$libelle_sexe <- droplevels(pyr_dta$libelle_sexe)
                     pyr_dta <- pyr_dta[libelle_sexe =="femmes", effectif := -effectif]
                     pyr_dta <- nvx_classe_age(pyr_dta)
                     
                     pyr <- ggplot(pyr_dta, aes(x = class_age_num, y = effectif, fill = libelle_sexe)) + 
                       geom_bar(data = pyr_dta[libelle_sexe =="femmes",], stat = "identity") +
                       geom_bar(data = pyr_dta[libelle_sexe =="hommes",], stat = "identity") +
                       scale_x_continuous(breaks = seq(1,nlevels(pyr_dta$classe_age), by =1),
                                          labels = levels(pyr_dta$classe_age)) +
                       xlab("Classe d'âge")+
                       ylab("Effectif")+
                       coord_flip()
                     
                     pyr
                   })
                   
                   output$comp_pro <- renderHighchart({
                     highchart() %>%
                       hc_add_series(data_comp_dep()[order(annee)],
                                     "line",
                                     hcaes(x = annee, 
                                           y = effectif, 
                                           group = profession_sante),
                                     marker = list(symbol = "square"),
                                     tooltip = list(pointFormat = "<b>Effectif praticien :</b> {point.y}<br/>")) %>%
                       hc_add_series(data_comp_dep()[,.(ratio = round(1/(effectif/Effectif))), by =.(annee, profession_sante)][order(annee)],
                                     "line",
                                     hcaes(x = annee, 
                                           y = ratio, 
                                           group = profession_sante),
                                     yAxis =1,
                                     marker = list(symbol = "circle"),
                                     tooltip = list(pointFormat = "<b>Nombre d'habitant par praticien :</b> {point.y}<br/>")) %>%
                       hc_yAxis_multiples(
                         list(
                           title = list(text = paste("Effectif de praticien en", data_comp_dep()$libelle_departement[1])), 
                           opposite = F),
                         list(
                           title = list(text = "Nombre d'habitant pour 1 praticien"), 
                           opposite = T)
                       )
                   })
                   
                   output$hono_patien <- renderHighchart({
                     highchart() %>%
                       hc_add_series(data_comp_dep(), 
                                     "line", 
                                     hcaes(annee, 
                                           hono_sans_depassement_moyens, 
                                           group = profession_sante), 
                                     yAxis=1,marker = list(symbol = "circle"), 
                                     tooltip = list(pointFormat = "<b>Honoraire annuel moyen :</b> {point.y} € <br/>")) %>% 
                       hc_add_series(data_comp_dep(), 
                                     "line", 
                                     hcaes(x = annee,
                                           y = nombre_patients_uniques ,
                                           group = profession_sante), 
                                     marker = list(symbol = "square"), 
                                     tooltip = list(pointFormat = "<b>Nombre de patient :</b> {point.y}<br/>")) %>%
                       hc_yAxis_multiples(
                         list(
                           title = list(text = "Patientele"), 
                           opposite = F),
                         list(
                           title = list(text = "Honoraire mensuel moyen hors depassement"), 
                           opposite = T)
                       ) %>%
                       hc_tooltip(shared = F)
                   })
                   
                   output$datatable <- renderDataTable({
                     input$go
                     isolate({
                       dtatable <- data_dep()[, .(annee, profession_sante, libelle_departement, classe_age, libelle_sexe, effectif, hono_sans_depassement_moyens, nombre_patients_uniques)]
                       colnames(dtatable) <- c("Année", "Profession", "Département", "Classe d'âge", "Sexe", "Effectif", "Honoraire moyen", "Nombre de patient")
                       dtatable
                     })
                     })
                   
                   data_carte_region <- reactive({
                     input$go
                     isolate({
                       newdta[(profession_sante == input$profession &
                                 annee == max(annee) &
                                 classe_age == "tout_age"), .(effectif = round(1/(effectif/Effectif))), by= libelle_departement]
                     })
                   })
                   
                   output$carte_region <- renderLeaflet({
                     qpal <- colorBin(palette = "YlGnBu",
                                      bins=5,
                                      domain = data_carte_region()$effectif[!is.infinite(data_carte_region()$effectif)])
                     leaflet(data_carte) %>% addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity=0.5,fillColor = ~qpal(data_carte_region()$effectif)) %>%
                       addLegend(title = "Nombre d'habitant par praticien", pal = qpal, values = ~data_carte_region()$effectif, opacity = 1)
                     #addProviderTiles
                   })
                   
                 }
                 else if(input$echelle == "4"){
                   data_reg <- reactive({
                     input$go
                     isolate({
                       newdta[(profession_sante %in% c(input$profession) & 
                                 annee >= input$periode[1] & 
                                 annee <= input$periode[2] & 
                                 libelle_region == input$region &
                                 classe_age != "tout_age"), ]
                     })
                   })
                   
                   data_comp_reg <- reactive({
                     input$go
                     isolate({
                       newdta[(libelle_sexe == "tout sexe" & 
                                 profession_sante %in% c(input$profession, input$profession_comp) &
                                 annee >= input$periode[1] & 
                                 annee <= input$periode[2] &
                                 libelle_region == input$region &
                                 classe_age == "tout_age"),
                              .(effectif = sum(effectif),
                                Effectif = sum(Effectif),
                                hono_sans_depassement_moyens = sum(hono_sans_depassement_moyens)/nrow(newdta[libelle_region == input$region,.(libelle_departement), by = libelle_departement][,1]),
                                nombre_patients_uniques = sum(nombre_patients_uniques)/nrow(newdta[libelle_region == input$region,.(libelle_departement), by = libelle_departement][,1])),
                              by =.(annee, profession_sante, libelle_region)]
                     })
                   })
                   
                   output$comb_plot <- renderHighchart({
                     tot_reg <- data_reg()[libelle_sexe == "tout sexe", .(effectif_tot = sum(effectif)), by = .(annee, libelle_region)]
                     input$go
                     isolate({
                       highchart() %>%
                         hc_add_series(data_reg()[libelle_sexe == "tout sexe",.(effectif = sum(effectif)) ,by = .(annee, libelle_region, classe_age)], "column", hcaes(x = annee, y = effectif, group = classe_age)) %>% 
                         hc_add_series(tot_reg, "line",hcaes(x = annee, y = effectif_tot), yAxis=1, name = paste("Effectif",input$profession)) %>%
                         hc_yAxis_multiples(
                           list(
                             title = list(text = paste("Effectif de", input$profession, "par classe d'âge")), 
                             opposite = F,
                             showLastLabel = FALSE),
                           list(
                             title = list(text = paste("Effectif de ", input$profession,"en", input$region)), 
                             opposite = T,
                             showLastLabel = FALSE)
                         ) %>%
                         hc_tooltip(shared = TRUE)
                     })
                   })
                   
                   output$pyr <- renderPlot({
                     pyr_dta <- data_reg()[libelle_sexe %in% c("femmes", "hommes") & annee == max(annee), effectif, by = .(annee, libelle_region, classe_age, libelle_sexe)]
                     pyr_dta$classe_age <- droplevels(pyr_dta$classe_age)
                     pyr_dta$libelle_sexe <- droplevels(pyr_dta$libelle_sexe)
                     pyr_dta <- pyr_dta[libelle_sexe =="femmes", effectif := -effectif]
                     pyr_dta <- nvx_classe_age(pyr_dta)
                     
                     
                     # range_pyr_dep <- seq(round(min(pyr_dep_dta$effectif),-(10^(floor(log10(min(pyr_dep_dta$effectif)))+1))), 
                     #                      max(pyr_dep_dta$effectif),
                     #                      by = 10^(floor(log10(max(abs(min(pyr_dep_dta$effectif)), max(pyr_dep_dta$effectif))))-1)
                     #                      )
                     
                     pyr <- ggplot(pyr_dta, aes(x = class_age_num, y = effectif, fill = libelle_sexe)) + 
                       geom_bar(data = pyr_dta[libelle_sexe =="femmes",], 
                                stat = "identity") +
                       geom_bar(data = pyr_dta[libelle_sexe =="hommes",],
                                stat = "identity") +
                       scale_x_continuous(breaks = seq(1,nlevels(pyr_dta$classe_age), by =1),
                                          labels = levels(pyr_dta$classe_age)) +
                       coord_flip()
                     # scale_y_continuous(breaks  = range_pyr_dep,
                     #                    labels = abs(range_pyr_dep))
                     pyr
                   })
                   
                   output$comp_pro <- renderHighchart({
                     highchart() %>%
                       hc_add_series(data_comp_reg()[order(annee)],
                                     "line",
                                     hcaes(x = annee, y = effectif, group = profession_sante),
                                     marker = list(symbol = "square"), 
                                     tooltip = list(pointFormat = "<b>Nombre de praticien :</b> {point.y}<br/>")) %>%
                       hc_add_series(data_comp_reg()[,.(ratio = round(1/(effectif/Effectif))), by =.(annee, profession_sante)][order(annee)],
                                     "line",
                                     hcaes(x = annee, y = ratio, group = profession_sante),
                                     yAxis =1,
                                     marker = list(symbol = "circle"),
                                     tooltip = list(pointFormat = "<b>Nombre d'habitant par praticien :</b> {point.y}<br/>")) %>%
                       hc_yAxis_multiples(
                         list(
                           title = list(text = paste("Effectif de praticien en", input$departement)), 
                           opposite = F),
                         list(
                           title = list(text = "Nombre d'habitant pour 1 praticien"), 
                           opposite = T)
                       )
                   })
                   
                   output$hono_patien <- renderHighchart({
                     highchart() %>%
                       hc_add_series(data_comp_reg(), "line", hcaes(annee, 
                                                                    hono_sans_depassement_moyens, 
                                                                    group = profession_sante), 
                                     yAxis=1, 
                                     tooltip = list(pointFormat = "<b>Honoraire annuel moyen :</b> {point.y} € <br/>")) %>% 
                       hc_add_series(data_comp_reg(), 
                                     "line",
                                     hcaes(x = annee, y = nombre_patients_uniques , group = profession_sante), 
                                     tooltip = list(pointFormat = "<b>Nombre de patient :</b> {point.y}")) %>%
                       hc_yAxis_multiples(
                         list(
                           title = list(text = "Patientele"), 
                           opposite = F),
                         list(
                           title = list(text = "Honoraire moyen hors depassement"), 
                           opposite = T)
                       ) %>%
                       hc_tooltip(shared = TRUE)
                   })
                   
                   output$datatable <- renderDataTable({
                     input$go
                     isolate({
                       dtatable <- data_reg()[, .(effectif = sum(effectif),
                                      hono_sans_depassement_moyens = sum(hono_sans_depassement_moyens)/nrow(newdta[libelle_region == input$region,.(libelle_departement), by = libelle_departement][,1]),
                                      nombre_patients_uniques = sum(nombre_patients_uniques)/nrow(newdta[libelle_region == input$region,.(libelle_departement), by = libelle_departement][,1])),
                                  by =.(annee, profession_sante, libelle_region, classe_age, libelle_sexe)]
                       colnames(dtatable) <- c("Année", "Profession", "Région", "Classe d'âge", "Sexe", "Effectif", "Honoraire moyen", "Nombre de patient")
                       dtatable
                     })

                   })
                   
                   data_carte_region <- reactive({
                     input$go
                     isolate({
                       a <- newdta[(profession_sante == input$profession &
                                      annee == max(annee) &
                                      classe_age == "tout_age" & libelle_sexe=="tout sexe"), 
                                   list(eff = round(1/(s_par_region/S_EFF_region))), by= libelle_departement]
                       indices_tri <- match(data_carte$NAME_2, a$libelle_departement)
                       a <- a[indices_tri, ]
                     })
                   })
                   
                   output$carte_region <- renderLeaflet({
                     qpal <- colorBin(palette = "YlGnBu",bins =5,domain = data_carte_region()$eff[!is.infinite(data_carte_region()$eff)])
                     leaflet(data_carte) %>% addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity=0.5,fillColor = ~qpal(data_carte_region()$eff)) %>% 
                       addLegend(title ="Nombre d'habitant par praticien", pal = qpal, values = ~data_carte_region()$eff, opacity = 1)
                     #addProviderTiles
                   })
                 }
               })
  # observeEvent(input$region_info,
  #               {
  #                 updateSelectInput(session,
  #                                   inputId = "profession_info",
  #                                   choices = levels(droplevels(data_effectif[(libelle_region == input$region_info & libelle_sexe == "tout sexe" & classe_age == "tout_age" & effectif > 0 & annee == max(annee)),])$profession)
  # 
  #                 )
  #               })
  
  observeEvent(input$region_info,
               {
                 updateSelectInput(session,
                                   inputId = "departement_info",
                                   choices = levels(droplevels(data_effectif[libelle_region == input$region_info,])$libelle_departement)
                 )
               })
  
  newdta_info <- reactive({
    input$go_info
    isolate({
      newdta[(profession_sante == input$profession_info & 
                annee == max(annee) &
                libelle_region == input$region_info &
                libelle_departement == input$departement_info &
                classe_age == "tout_age" & 
                libelle_sexe == "tout sexe"), .(ratio = round(1/(effectif/Effectif)), hono_sans_depassement_moyens, nombre_patients_uniques)]
    })
  })
  
  output$nb_info <- renderText({
    input$go_info
    isolate({
      paste("Nombre de ", input$profession_info, "ramené à la population")
    })
  })
  
  output$texte_info <- renderText({
    paste(1, substr(input$profession_info, 1, nchar(input$profession_info) - 1), "pour", newdta_info()[,.(ratio)], "habitants")
  })
  
  newdta_comp_region <- reactive({
    input$go_info
    isolate({
      newdta[(profession_sante == input$profession_info & 
                annee == max(annee) &
                libelle_region == input$region_info &
                libelle_departement != input$departement_info &
                classe_age == "tout_age" & 
                libelle_sexe == "tout sexe"), 
             .(ratio = round(1/(effectif/Effectif)), hono_sans_depassement_moyens, nombre_patients_uniques), by = libelle_departement]
    })
  })

  info <- reactive({
    input$go_info
    isolate({
      newdta[(profession_sante == input$profession_info & 
                annee == max(annee)  &
                libelle_departement != input$departement_info &
                classe_age == "tout_age" & 
                libelle_sexe == "tout sexe"), 
             .(ratio = round(1/(effectif/Effectif)), hono_sans_depassement_moyens, nombre_patients_uniques), by = libelle_departement]
    })
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
    if (is.infinite(newdta_comp_region()[[1,2]]) | newdta_comp_region()[[1,2]] == 0 | is.na(newdta_comp_region()[[1,2]])){
      texte <- paste("-",newdta_comp_region()[[1,1]], " : Données manquantes pour les",input$profession_info)
    } else{
      texte <- paste("<b>","-",newdta_comp_region()[[1,1]], " :" ,1, substr(input$profession_info, 1, nchar(input$profession_info) - 1), "pour", newdta_comp_region()[[1,2]] , "hab", "</b>")
    }
    for(i in 2:nrow(newdta_comp_region())){
      if (is.infinite(newdta_comp_region()[[i,2]]) | newdta_comp_region()[[i,2]] == 0 | is.na(newdta_comp_region()[[i,2]])){
        ntext <- (paste("-",newdta_comp_region()[[i,1]]," : Données manquantes pour les",input$profession_info))
      } else{
        ntext <- paste("<b>","-",newdta_comp_region()[[i,1]], " :" ,1, substr(input$profession_info, 1, nchar(input$profession_info) - 1), "pour", newdta_comp_region()[[i,2]] , "hab","</b>")
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
  
  output$hono_info <- renderText({
    input$go_info
    isolate({
      if(newdta_info()[, hono_sans_depassement_moyens] == 0 | is.na(newdta_info()[, hono_sans_depassement_moyens])){
        paste("Données manquantes pour", input$profession_info, "en", input$departement_info)
      }else{
        paste(newdta_info()[, hono_sans_depassement_moyens], "€ par praticien")
      }
    })
  })
  
  output$comparaison_hono <- renderText({
    if (is.infinite(newdta_comp_region()[[1,3]]) | newdta_comp_region()[[1,3]] == 0 | is.na(newdta_comp_region()[[1,3]])){
      texte <- paste("-",newdta_comp_region()[[1,1]], " : Données manquantes pour les",input$profession_info)
    } else{
      texte <- paste("<b>","-",newdta_comp_region()[[1,1]], " :" ,newdta_comp_region()[[1,3]], "€ d'honoraire en moyenne par praticien", "</b>")
    }
    for(i in 2:nrow(newdta_comp_region())){
      if (is.infinite(newdta_comp_region()[[i,3]]) | newdta_comp_region()[[i,3]] == 0 | is.na(newdta_comp_region()[[i,3]])){
        ntext <- (paste("-",newdta_comp_region()[[i,1]]," : Données manquantes pour les",input$profession_info))
      } else{
        ntext <- paste("<b>","-",newdta_comp_region()[[i,1]], " :" ,newdta_comp_region()[[i,3]], "€ d'honoraire en moyenne par praticien","</b>")
      }
      texte <- paste(texte,ntext, sep = '<br/>')
    }
    HTML(texte)
    
  })
  
  output$comp_hono <- renderText({
    input$go_info
    isolate({
      paste("Département avec le meilleur ratio :", '<br/>',info()[ratio ==max(ratio), .(libelle_departement, ratio)][[1,1]], "avec", info()[ratio ==max(ratio),.(libelle_departement, ratio)][[1,2]], "habitants par praticiens", '<br/>',"Département avec le moins bon ratio :" ,'<br/>' ,info()[ratio == min(ratio),.(libelle_departement, ratio)][[1,1]], "avec", info()[ratio == min(ratio),.(libelle_departement, ratio)][[1,2]], "habitants par praticiens")
    })
  })
}
  