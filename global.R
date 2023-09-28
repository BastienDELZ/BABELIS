# install.packages("shiny")
library(shiny)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("patchwork")
library(patchwork) 
# install.packages("data.table")
library(data.table)
install.packages("MASS")
library(MASS)

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


# chargement des donnees globales
# options(timeout = max(1000, getOption("timeout")))
# 
# demo <- fread(file ="estimation_pop.csv", header = T, stringsAsFactors = T)
# demo_piv <- pivot_longer(demo, cols = starts_with("E"), names_to = "Annee", values_to = "Effectif")
# demo_piv$Effectif <-gsub("_", "", demo_piv$Effectif)
# demo_piv$Annee <- substr(demo_piv$Annee, nchar(demo_piv$Annee)-3, nchar(demo_piv$Annee))
# demo_piv <-  as.data.table(demo_piv)
# demo_piv <- setnames(demo_piv, old = c("Annee"), new = c("annee"))
# demo_piv <- demo_piv[, ':=' (annee = factor(annee))]
# demo_piv <- demo_piv[, Departement := NULL]
# 
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


data_effectif <-creat_dta()
newdta <- merge(data_effectif, demo_piv, by = c("Num_dep", "annee"),  all.x = TRUE)
#une ligne c'est pas un medecin


#dta <- merge(data_effectif, demo_piv, by = c("annee","Num_dep"), all.x =T)