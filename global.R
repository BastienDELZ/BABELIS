# install.packages("shiny")
library(shiny)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("patchwork")
library(patchwork) 
# install.packages("data.table")
library(data.table)
#install.packages("leaflet")
library(leaflet)
#install.packages("MASS")
#install.packages("sf")
library(sf)
library(MASS)
library(shinydashboard)
library(tidyr)
library(highcharter)


# chargement des fonctions
creat_dta <- function(){
  dta_1 <- fread("dta_1.csv")
  dta_2 <- fread("dta_2.csv")
  dta_3 <- fread("dta_3.csv")
  dta_4 <- fread("dta_4.csv")
  dta_5 <- fread("dta_5.csv")
  dta_6 <- fread("dta_6.csv")
  dta_7 <- fread("dta_7.csv")
  data <- rbind(dta_1, dta_2, dta_3, dta_4,dta_5, dta_6, dta_7 )
  data[, (2:8) := lapply(.SD, factor), .SDcols = 2:8]
  return(data)
} 

nvx_classe_age <- function(data.table){
  data.table[,class_age_num := .GRP, by = classe_age]
  return(data.table)
}


# chargement des donnees globales
# options(timeout = max(1000, getOption("timeout")))
# 
# demo <- fread(file ="/Users/bastiendeleuze/Desktop/Archive projet shiny/shiny/Proj_PrograShiny/estimation_pop.csv", header = T, stringsAsFactors = T)
# demo_piv <- pivot_longer(demo, cols = starts_with("E"), names_to = "Annee", values_to = "Effectif")
# demo_piv$Effectif <-gsub("_", "", demo_piv$Effectif)
# demo_piv$Annee <- substr(demo_piv$Annee, nchar(demo_piv$Annee)-3, nchar(demo_piv$Annee))
# demo_piv <-  as.data.table(demo_piv)
# demo_piv <- setnames(demo_piv, old = c("Annee"), new = c("annee"))
# demo_piv <- demo_piv[, ':=' (annee = factor(annee))]
# demo_piv <- demo_piv[, Departement := NULL]
# 
# write.csv2(demo_piv, file = "demo_piv.csv", row.names =F)

data_carte <- readRDS("gadm36_FRA_2_sf.rds")

# data_effectif <- fread("https://data.opendatasoft.com/api/explore/v2.1/catalog/datasets/demographie-effectifs-et-les-densites@observatoirepathologies-cnam/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B")
# data_effectif <- data_effectif[libelle_departement!="Tout département"]
# data_effectif <- data_effectif[libelle_departement!="FRANCE"]
# data_effectif <- setnames(data_effectif, old = c(1), new = c("annee"))
# data_effectif <- setnames(data_effectif, old = c("departement"), new = c("Num_dep"))
# data_effectif[,  ':=' (annee = factor(annee), Num_dep = factor(Num_dep))]
# data_effectif <- data_effectif[, c(3,12:14) := NULL]
# data_effectif[, (2:8) := lapply(.SD, factor), .SDcols = 2:8]
# 
# write.csv2(data_effectif, file = "dta.csv", row.names =F)
# data_effectif <-fread("dta.csv")
# split_1 <- round(nrow(data_effectif)/7)
# split_2 <- split_1*2
# split_3 <-split_1*3
# split_4 <- split_1*4
# split_5 <- split_1*5
# split_6 <- split_1*6
# dta_1 <- data_effectif[1:split_1,]
# write.csv2(dta_1, file = "dta_1.csv", row.names =F)
# dta_2 <- data_effectif[(split_1+1):split_2,]
# write.csv2(dta_2, file = "dta_2.csv", row.names =F)
# dta_3 <- data_effectif[(split_2+1):split_3,]
# write.csv2(dta_3, file = "dta_3.csv", row.names =F)
# dta_4 <- data_effectif[(split_3+1):split_4 ,]
# write.csv2(dta_4, file = "dta_4.csv", row.names =F)
# dta_5 <- data_effectif[(split_4+1):split_5,]
# write.csv2(dta_5, file = "dta_5.csv", row.names =F)
# dta_6 <- data_effectif[(split_5+1):split_6,]
# write.csv2(dta_6, file = "dta_6.csv", row.names =F)
# dta_7 <- data_effectif[(split_6+1):nrow(data_effectif),]
# write.csv2(dta_7, file = "dta_7.csv", row.names =F)

# honoraires <- fread("https://data.opendatasoft.com/api/explore/v2.1/catalog/datasets/honoraires@observatoirepathologies-cnam/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B")
# honoraires <- honoraires[libelle_departement!="Tout département"]
# honoraires <- honoraires[libelle_departement!="FRANCE"]
# honoraires <- setnames(honoraires, old = c(1), new = c("annee"))
# honoraires <- setnames(honoraires, old = c("departement"), new = c("Num_dep"))
# honoraires[,  ':=' (annee = factor(annee), Num_dep = factor(Num_dep))]
# honoraires <- honoraires[, c(3:4,6:8,11:26) := NULL]
# write.csv2(honoraires, file = "dta_hono.csv", row.names =F)

# patient <- fread("https://data.opendatasoft.com/api/explore/v2.1/catalog/datasets/patientele@observatoirepathologies-cnam/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B")
# patient <- patient[libelle_departement!="Tout département"]
# patient <- patient[libelle_departement!="FRANCE"]
# patient <- setnames(patient, old = c(1), new = c("annee"))
# patient <- setnames(patient, old = c("departement"), new = c("Num_dep"))
# patient <- patient[, c(3:4,6,8:13) := NULL]
# write.csv2(patient, file = "dta_pat.csv", row.names =F)


data_effectif <-creat_dta()
dta_hono <- fread("dta_hono.csv",
                  na.string = c("NS", "NA", "na"))
dta_hono[,':=' (annee = as.integer(annee), 
                Num_dep = as.character(Num_dep), 
                profession_sante =factor(profession_sante),
                hono_sans_depassement_moyens = as.numeric(hono_sans_depassement_moyens), 
                depassements_moyens = as.numeric(depassements_moyens))]
dta_pat <- fread("dta_pat.csv",
                 na.string = c("NS", "NA", "na"))
dta_pat[,':=' (Num_dep = as.character(Num_dep),
               nombre_patients_uniques = as.numeric(nombre_patients_uniques))]
demo_piv <- fread("demo_piv.csv")
newdata <- merge(data_effectif, demo_piv, by = c("annee","Num_dep" ),  all.x = TRUE)
newdata2 <- merge(newdata, dta_hono, by = c("annee","profession_sante","Num_dep" ),  all.x = TRUE)
newdta <- merge(newdata2, dta_pat, by = c("annee","profession_sante","Num_dep" ),  all.x = TRUE)
newdta <-newdta[, ':=' (s_par_region =sum(effectif), S_EFF_region = sum(Effectif)), by = list(libelle_region, profession_sante, classe_age, annee, libelle_sexe)]


# texte <- ""
# for(i in 1:nrow(test)){
#   texte <- paste(texte, test[[i,1]], newdata_comp_region[[i,2]], "\n")
# }
# cat(texte)



#dta <- merge(data_effectif, demo_piv, by = c("annee","Num_dep"), all.x =T)

# library(highcharter)
# 
# # Données
# # Données
# categories <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
#                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# precipitation <- c(27.6, 28.8, 21.7, 34.1, 29.0, 28.4, 45.6, 51.7, 39.0,
#                    60.0, 28.6, 32.1)
# temperature <- c(-13.6, -14.9, -5.8, -0.7, 3.1, 13.0, 14.5, 10.8, 5.8,
#                  -0.7, -11.0, -16.4)
# 
# # Création du graphique
# highchart() %>%
#   hc_chart(type = "column") %>%
#   hc_title(text = "Karasjok weather, 2021", align = "left") %>%
#   hc_subtitle(text = 'Source: <a href="https://www.yr.no/nb/historikk/graf/5-97251/Norge/Troms%20og%20Finnmark/Karasjok/Karasjok?q=2021" target="_blank">YR</a>', align = "left") %>%
#   hc_xAxis(categories = list(categories = categories, crosshair = TRUE)) %>%
#   hc_yAxis(
#     list(
#       title = list(text = "Precipitation"),
#       labels = list(format = "{value} mm"),
#       opposite = TRUE
#     ),
#     list(
#       title = list(text = "Temperature"),
#       labels = list(format = "{value}°C"),
#       opposite = FALSE
#     )
#   ) %>%
#   hc_tooltip(shared = TRUE) %>%
#   hc_legend(
#     align = "left",
#     x = 80,
#     verticalAlign = "top",
#     y = 60,
#     floating = TRUE,
#     backgroundColor = "rgba(255,255,255,0.25)"
#   ) %>%
#   hc_add_series(
#     list(
#       name = "Precipitation",
#       type = "column",
#       yAxis = 1,
#       data = precipitation,
#       tooltip = list(valueSuffix = " mm")
#     )
#   ) %>%
#   hc_add_series(
#     list(
#       name = "Temperature",
#       type = "spline",
#       data = temperature,
#       tooltip = list(valueSuffix = "°C")
#     )
#   )


