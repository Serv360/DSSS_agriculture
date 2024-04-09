rm(list=ls())
gc()

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(tibble)
library(ggdist)
library(sf)
library(tidyverse)

#a changer pour lolo l'asticot
setwd("..")


chemin <- "/data/"

base <- list()

for (annee in c(2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) {
  donnees_annee <- list()
  for (region in c(11,24,27,28,32,44,52,53,75,76,84,93,94)) {
    chemin_fichier <- paste0(chemin, "merged_tibble_", annee, "_", region, ".csv")
    donnees_region <- read.csv(chemin_fichier, sep = ";")
    
    # Convertir les colonnes nécessaires en caractères si elles ne le sont pas déjà
    colonnes_a_convertir <- c("Departement") # Ajoutez d'autres colonnes au besoin
    for (colonne in colonnes_a_convertir) {
      if (!is.character(donnees_region[[colonne]])) {
        donnees_region[[colonne]] <- as.character(donnees_region[[colonne]])
      }
    }
    
    donnees_annee[[region]] <- donnees_region
  }
  
  # Assemblage des données pour l'année donnée
  donnees_assemblees <- do.call(rbind, donnees_annee)
  base[[as.character(annee)]] <- donnees_assemblees
}

base <- as.data.frame(do.call(rbind, base))


## définir les cycles de croissance du blé et du mais
## proposition ble : octobre -> avril / mars -> juillet
## proposition mais : avril -> aout / juillet -> octobre

##########
## Mais ##
##########
base <-base %>%filter(Culture =="14 - MaÃ¯s grain irriguÃ©" |Culture == "15 - MaÃ¯s grain non irriguÃ©" )

# Fonction pour calculer la moyenne des températures pour une période spécifiée
calculer_moyenne_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  moyenne <- mean(valeurs_numeriques, na.rm = TRUE)
  return(moyenne)
}

# Fonction pour calculer le maximum des températures pour une période spécifiée
calculer_max_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  maximum <- max(valeurs_numeriques, na.rm = TRUE)
  return(maximum)
}

# Fonction pour calculer le minimum des températures pour une période spécifiée
calculer_min_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  minimum <- min(valeurs_numeriques, na.rm = TRUE)
  return(minimum)
}

# Ajouter les colonnes pour avril-aout
base$temp_moy_avr_aout <- sapply(base$temperature_moyenne24h, calculer_moyenne_periode, debut = 90, fin = 245)
base$temp_max_avr_aout <- sapply(base$temperature_moyenne24h, calculer_max_periode, debut = 90, fin = 245)
base$temp_min_avr_aout <- sapply(base$temperature_moyenne24h, calculer_min_periode, debut = 90, fin = 245)
base$prec_moy_avr_aout <- sapply(base$precipitation_somme24h, calculer_moyenne_periode, debut = 90, fin = 245)
base$radia_moy_avr_aout <- sapply(base$radiation_somme24h, calculer_moyenne_periode, debut = 90, fin = 245)
# Ajouter les colonnes pour juillet-octobre
base$temp_moy_juil_oct <- sapply(base$temperature_moyenne24h, calculer_moyenne_periode, debut = 185, fin = 295)
base$temp_max_juil_oct <- sapply(base$temperature_moyenne24h, calculer_max_periode, debut = 185, fin = 295)
base$temp_min_juil_oct <- sapply(base$temperature_moyenne24h, calculer_min_periode, debut = 185, fin = 295)
base$prec_moy_juil_oct <- sapply(base$precipitation_somme24h, calculer_moyenne_periode, debut = 185, fin = 295)
base$radia_moy_juil_oct <- sapply(base$radiation_somme24h, calculer_moyenne_periode, debut = 185, fin = 295)

####################
#Random forest# 
####################

library(caret)
library(randomForest)

train_data <- base[, c("rend","Departement", "Annee", "temp_moy_juil_oct", "temp_max_juil_oct", "temp_min_juil_oct", 
                       "prec_moy_juil_oct", "radia_moy_juil_oct", "temp_moy_avr_aout", 
                       "prec_moy_avr_aout", "radia_moy_avr_aout", "temp_max_avr_aout", "temp_min_avr_aout")]


# Supprimer les lignes avec des valeurs NA dans les ensembles d'entraînement 
train_data <- train_data[complete.cases(train_data), ]


# Définir la méthode de cross-validation
# Créer un index de groupe pour chaque année
group_index <- groupKFold(train_data$Annee, k = 3)

# Définir la méthode de cross-validation avec timeslice et groupKFold : prise en compte du caractère temporel des données
train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE,
                              index = group_index)
# Entraînement du modèle de forêt aléatoire avec cross-validation
model <- train(rend ~ Departement + temp_moy_juil_oct + temp_max_juil_oct + temp_min_juil_oct +
prec_moy_juil_oct + radia_moy_juil_oct + temp_moy_avr_aout + prec_moy_avr_aout
+ radia_moy_avr_aout + temp_max_avr_aout + temp_min_avr_aout,
               data = train_data,
               method = "rf",
               trControl = train_control)

print(model)


# Tracer l'importance des prédicteurs
importance <- varImp(model)
print(importance)


variables_a_analyser <- c("temp_moy_juil_oct", "temp_max_juil_oct", "temp_min_juil_oct", 
                          "prec_moy_juil_oct", "radia_moy_juil_oct", "temp_moy_avr_aout", 
                          "prec_moy_avr_aout", "radia_moy_avr_aout", "temp_max_avr_aout", "temp_min_avr_aout")
pdf("effets_partiels_mais.pdf")  # Ouvre un fichier PDF pour sauvegarder les graphiques

par(mfrow = c(3, 4)) 

# Boucle pour tracer l'effet partiel pour chaque variable
for (variable in variables_a_analyser) {
  effet_partiel <- pdp::partial(object = model, pred.var = variable, train = train_data)
  plot(effet_partiel, main = paste("Effet partiel de", variable, "sur le rendement"))
}

dev.off() 
## j'essaie de déterminer les "seuils critiques" pour chaque variable grâce à 
# ces effets partiels




###############################REPRENDRE ICI#################"










# #"temp_moy_juil_oct" : seuils 288 et 290
# "temp_max_juil_oct" : linéaire
# "temp_min_juil_oct" : seuil 283
# "prec_moy_juil_oct": seuil 4
# "radia_moy_juil_oct": seuils 1.6e+07 et 1.9e+07
# "temp_moy_avr_aout" : seuil 292
# "prec_moy_avr_aout" : seuils 2.5 et 4
# "radia_moy_avr_aout" : seuil 1.8e+07
# "temp_max_avr_aout" : seuil 296
# "temp_min_avr_aout" : seuils 276 et 281

###############
#### Within ####
###############

# Création des variables seuils (dummies)

# temp_moy_juil_oct
train_data$temp_moy_juil_oct_seuil288 <- as.numeric(train_data$temp_moy_juil_oct < 288)
train_data$temp_moy_juil_oct_seuil288_290 <- as.numeric(train_data$temp_moy_juil_oct >= 288 & train_data$temp_moy_juil_oct < 290)

# temp_max_juil_oct : linéaire, pas de seuil spécifique

# temp_min_juil_oct
train_data$temp_min_juil_oct_seuil283 <- as.numeric(train_data$temp_min_juil_oct <= 283)

# prec_moy_juil_oct
train_data$prec_moy_juil_oct_seuil4 <- as.numeric(train_data$prec_moy_juil_oct <= 4)

# radia_moy_juil_oct
train_data$radia_moy_juil_oct_seuil1 <- as.numeric(train_data$radia_moy_juil_oct <= 1.6e+07)
train_data$radia_moy_juil_oct_seuil1_9 <- as.numeric(train_data$radia_moy_juil_oct > 1.6e+07 & train_data$radia_moy_juil_oct <= 1.9e+07)

# temp_moy_avr_aout
train_data$temp_moy_avr_aout_seuil292 <- as.numeric(train_data$temp_moy_avr_aout < 292)

# prec_moy_avr_aout
train_data$prec_moy_avr_aout_seuil2_5 <- as.numeric(train_data$prec_moy_avr_aout <= 2.5)
train_data$prec_moy_avr_aout_seuil2_5_4 <- as.numeric(train_data$prec_moy_avr_aout > 2.5 & train_data$prec_moy_avr_aout <= 4)

# radia_moy_avr_aout
train_data$radia_moy_avr_aout_seuil1_8 <- as.numeric(train_data$radia_moy_avr_aout <= 1.8e+07)

# temp_max_avr_aout
train_data$temp_max_avr_aout_seuil296 <- as.numeric(train_data$temp_max_avr_aout <= 296)

# temp_min_avr_aout
train_data$temp_min_avr_aout_seuil276 <- as.numeric(train_data$temp_min_avr_aout <= 276)
train_data$temp_min_avr_aout_seuil281 <- as.numeric(train_data$temp_min_avr_aout > 276 & train_data$temp_min_avr_aout <= 281)



library(plm)
# 
# modele_regression_within <- plm(rend ~ 
#                                   temp_moy_juil_oct_seuil288 + temp_moy_juil_oct + temp_moy_juil_oct_seuil288_290 +
#                                   temp_max_juil_oct + temp_min_juil_oct_seuil283 + temp_min_juil_oct +
#                                   prec_moy_juil_oct_seuil4 + prec_moy_juil_oct +
#                                   radia_moy_juil_oct + radia_moy_juil_oct_seuil1 + radia_moy_juil_oct_seuil1_9 +
#                                   temp_moy_avr_aout_seuil292 + temp_moy_avr_aout + 
#                                   prec_moy_avr_aout_seuil2_5 + prec_moy_avr_aout_seuil2_5_4 + prec_moy_avr_aout +
#                                   radia_moy_avr_aout_seuil1_8 + radia_moy_avr_aout +
#                                   temp_max_avr_aout_seuil296 + temp_max_avr_aout +
#                                   temp_min_avr_aout_seuil276 + temp_min_avr_aout + 
#                                   temp_min_avr_aout_seuil281,
#                                 data = train_data,
#                                 index = c("Annee", "Departement"),
#                                 model = "within")
# 
# summary(modele_regression_within)


# Je ne garde que les variables significatives

modele_regression_within <- plm(rend ~ temp_moy_juil_oct_seuil288_290 +
                                   temp_min_juil_oct_seuil283 + 
                                   prec_moy_juil_oct +
                                  radia_moy_juil_oct +  
                                  temp_moy_avr_aout_seuil292  +
                                  radia_moy_avr_aout_seuil1_8 + radia_moy_avr_aout +
                                  temp_min_avr_aout_seuil276 + temp_min_avr_aout ,
                                data = train_data,
                                index = c("Annee", "Departement"),
                                model = "within")

summary(modele_regression_within)



###########
#Long Dif ## 
#############

donnees_2010 <- subset(base, Annee == 2010)
donnees_2010 <- na.omit(donnees_2010)

donnees_2011 <- subset(base, Annee == 2011)
donnees_2011 <- na.omit(donnees_2011)

donnees_2021 <- subset(base, Annee == 2021)
donnees_2021 <- na.omit(donnees_2021)

donnees_2022 <- subset(base, Annee == 2022)
donnees_2022 <- na.omit(donnees_2022)


# Calculer les rendements moyens pour chaque département et chaque période, pour chaque année
rendements_moyens_2010 <- donnees_2010 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2010 = mean(rend),
    temp_moy_juil_oct_2010 = mean(temp_moy_juil_oct),
    temp_max_juil_oct_2010 = mean(temp_max_juil_oct),
    temp_min_juil_oct_2010 = mean(temp_min_juil_oct),
    prec_moy_juil_oct_2010 = mean(prec_moy_juil_oct),
    radia_moy_juil_oct_2010 = mean(radia_moy_juil_oct),
    temp_moy_avr_aout_2010 = mean(temp_moy_avr_aout),
    prec_moy_avr_aout_2010 = mean(prec_moy_avr_aout),
    radia_moy_avr_aout_2010 = mean(radia_moy_avr_aout),
    temp_max_avr_aout_2010 = mean(temp_max_avr_aout),
    temp_min_avr_aout_2010 = mean(temp_min_avr_aout)
  )

rendements_moyens_2011 <- donnees_2011 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2011 = mean(rend),
    temp_moy_juil_oct_2011 = mean(temp_moy_juil_oct),
    temp_max_juil_oct_2011 = mean(temp_max_juil_oct),
    temp_min_juil_oct_2011 = mean(temp_min_juil_oct),
    prec_moy_juil_oct_2011 = mean(prec_moy_juil_oct),
    radia_moy_juil_oct_2011 = mean(radia_moy_juil_oct),
    temp_moy_avr_aout_2011 = mean(temp_moy_avr_aout),
    prec_moy_avr_aout_2011 = mean(prec_moy_avr_aout),
    radia_moy_avr_aout_2011 = mean(radia_moy_avr_aout),
    temp_max_avr_aout_2011 = mean(temp_max_avr_aout),
    temp_min_avr_aout_2011 = mean(temp_min_avr_aout)
  )

rendements_moyens_2021 <- donnees_2021 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2021 = mean(rend),
    temp_moy_juil_oct_2021 = mean(temp_moy_juil_oct),
    temp_max_juil_oct_2021 = mean(temp_max_juil_oct),
    temp_min_juil_oct_2021 = mean(temp_min_juil_oct),
    prec_moy_juil_oct_2021 = mean(prec_moy_juil_oct),
    radia_moy_juil_oct_2021 = mean(radia_moy_juil_oct),
    temp_moy_avr_aout_2021 = mean(temp_moy_avr_aout),
    prec_moy_avr_aout_2021 = mean(prec_moy_avr_aout),
    radia_moy_avr_aout_2021 = mean(radia_moy_avr_aout),
    temp_max_avr_aout_2021 = mean(temp_max_avr_aout),
    temp_min_avr_aout_2021 = mean(temp_min_avr_aout)
  )

rendements_moyens_2022 <- donnees_2022 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2022 = mean(rend),
    temp_moy_juil_oct_2022 = mean(temp_moy_juil_oct),
    temp_max_juil_oct_2022 = mean(temp_max_juil_oct),
    temp_min_juil_oct_2022 = mean(temp_min_juil_oct),
    prec_moy_juil_oct_2022 = mean(prec_moy_juil_oct),
    radia_moy_juil_oct_2022 = mean(radia_moy_juil_oct),
    temp_moy_avr_aout_2022 = mean(temp_moy_avr_aout),
    prec_moy_avr_aout_2022 = mean(prec_moy_avr_aout),
    radia_moy_avr_aout_2022 = mean(radia_moy_avr_aout),
    temp_max_avr_aout_2022 = mean(temp_max_avr_aout),
    temp_min_avr_aout_2022 = mean(temp_min_avr_aout)
  )

# Fusionner les jeux de données pour calculer la différence des rendements
donnees_diff <- merge(merge(merge(rendements_moyens_2010, rendements_moyens_2021, by = c("Departement", "Culture")), rendements_moyens_2022, by = c("Departement", "Culture")), rendements_moyens_2011, by = c("Departement", "Culture"))
donnees_diff[is.na(donnees_diff)] <- 0
donnees_diff <- donnees_diff %>%
  mutate(moyenne_rendement_2021_2022 = rowMeans(select(., c(rendement_moyen_2022, rendement_moyen_2021)), na.rm = TRUE),
         moyenne_rendement_2010_2011 = rowMeans(select(., c(rendement_moyen_2010, rendement_moyen_2011)), na.rm = TRUE),
         moyenne_temp_moy_avr_aout_2021_2022 = rowMeans(select(., c(temp_moy_avr_aout_2021, temp_moy_avr_aout_2022)), na.rm = TRUE),
         moyenne_temp_max_avr_aout_2021_2022 = rowMeans(select(., c(temp_max_avr_aout_2021, temp_max_avr_aout_2022)), na.rm = TRUE),
         moyenne_temp_min_avr_aout_2021_2022 = rowMeans(select(., c(temp_min_avr_aout_2021, temp_min_avr_aout_2022)), na.rm = TRUE),
         moyenne_prec_moy_avr_aout_2021_2022 = rowMeans(select(., c(prec_moy_avr_aout_2021, prec_moy_avr_aout_2022)), na.rm = TRUE),
         moyenne_radia_moy_avr_aout_2021_2022 = rowMeans(select(., c(radia_moy_avr_aout_2021, radia_moy_avr_aout_2022)), na.rm = TRUE),
         moyenne_temp_moy_juil_oct_2021_2022 = rowMeans(select(., c(temp_moy_juil_oct_2021, temp_moy_juil_oct_2022)), na.rm = TRUE),
         moyenne_prec_moy_juil_oct_2021_2022 = rowMeans(select(., c(prec_moy_juil_oct_2021, prec_moy_juil_oct_2022)), na.rm = TRUE),
         moyenne_radia_moy_juil_oct_2021_2022 = rowMeans(select(., c(radia_moy_juil_oct_2021, radia_moy_juil_oct_2022)), na.rm = TRUE),
         moyenne_temp_max_juil_oct_2021_2022 = rowMeans(select(., c(temp_max_juil_oct_2021, temp_max_juil_oct_2022)), na.rm = TRUE),
         moyenne_temp_min_juil_oct_2021_2022 = rowMeans(select(., c(temp_min_juil_oct_2021, temp_min_juil_oct_2022)), na.rm = TRUE),
         
         moyenne_temp_moy_avr_aout_2010_2011 = rowMeans(select(., c(temp_moy_avr_aout_2010, temp_moy_avr_aout_2011)), na.rm = TRUE),
         moyenne_temp_max_avr_aout_2010_2011 = rowMeans(select(., c(temp_max_avr_aout_2010, temp_max_avr_aout_2011)), na.rm = TRUE),
         moyenne_temp_min_avr_aout_2010_2011 = rowMeans(select(., c(temp_min_avr_aout_2010, temp_min_avr_aout_2011)), na.rm = TRUE),
         moyenne_prec_moy_avr_aout_2010_2011 = rowMeans(select(., c(prec_moy_avr_aout_2010, prec_moy_avr_aout_2011)), na.rm = TRUE),
         moyenne_radia_moy_avr_aout_2010_2011 = rowMeans(select(., c(radia_moy_avr_aout_2010, radia_moy_avr_aout_2011)), na.rm = TRUE),
         moyenne_temp_moy_juil_oct_2010_2011 = rowMeans(select(., c(temp_moy_juil_oct_2010, temp_moy_juil_oct_2011)), na.rm = TRUE),
         moyenne_prec_moy_juil_oct_2010_2011 = rowMeans(select(., c(prec_moy_juil_oct_2010, prec_moy_juil_oct_2011)), na.rm = TRUE),
         moyenne_radia_moy_juil_oct_2010_2011 = rowMeans(select(., c(radia_moy_juil_oct_2010, radia_moy_juil_oct_2011)), na.rm = TRUE),
         moyenne_temp_max_juil_oct_2010_2011 = rowMeans(select(., c(temp_max_juil_oct_2010, temp_max_juil_oct_2011)), na.rm = TRUE),
         moyenne_temp_min_juil_oct_2010_2011 = rowMeans(select(., c(temp_min_juil_oct_2010, temp_min_juil_oct_2011)), na.rm = TRUE)
  )


donnees_diff <- donnees_diff %>%
  mutate(diff_rend = moyenne_rendement_2021_2022 - moyenne_rendement_2010_2011,
         diff_temp_moy_avr_aout = moyenne_temp_moy_avr_aout_2021_2022 - moyenne_temp_moy_avr_aout_2010_2011,
         diff_temp_max_avr_aout = moyenne_temp_max_avr_aout_2021_2022 - moyenne_temp_max_avr_aout_2010_2011,
         diff_temp_min_avr_aout = moyenne_temp_min_avr_aout_2021_2022 - moyenne_temp_min_avr_aout_2010_2011,
         diff_prec_moy_avr_aout = moyenne_prec_moy_avr_aout_2021_2022 - moyenne_prec_moy_avr_aout_2010_2011,
         diff_radia_moy_avr_aout = moyenne_radia_moy_avr_aout_2021_2022 - moyenne_radia_moy_avr_aout_2010_2011,
         diff_temp_moy_juil_oct = moyenne_temp_moy_juil_oct_2021_2022 - moyenne_temp_moy_juil_oct_2010_2011,
         diff_prec_moy_juil_oct = moyenne_prec_moy_juil_oct_2021_2022 - moyenne_prec_moy_juil_oct_2010_2011,
         diff_radia_moy_juil_oct = moyenne_radia_moy_juil_oct_2021_2022 - moyenne_radia_moy_juil_oct_2010_2011,
         diff_temp_max_juil_oct = moyenne_temp_max_juil_oct_2021_2022 - moyenne_temp_max_juil_oct_2010_2011,
         diff_temp_min_juil_oct = moyenne_temp_min_juil_oct_2021_2022 - moyenne_temp_min_juil_oct_2010_2011
  )

# on garde les variables significatives seulement


modele_regression_diff_long <- lm(diff_rend ~ 0  + diff_prec_moy_avr_aout + diff_prec_moy_juil_oct 
                                  ,
                                  data = donnees_diff)


# Obtenir les résultats du modèle
summary(modele_regression_diff_long)
#valable pour les projections 2050 car pas radiations


