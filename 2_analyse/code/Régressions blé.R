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
## Blé ##
##########
base <-base %>%filter(Culture =="01 - BlÃ© tendre d'hiver et Ã©peautre")



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

# Ajouter les colonnes pour mars-juillet
base$temp_moy_mar_juil <- sapply(base$temperature_moyenne24h, calculer_moyenne_periode, debut = 61, fin = 214)
base$temp_max_mar_juil <- sapply(base$temperature_moyenne24h, calculer_max_periode, debut = 61, fin = 214)
base$temp_min_mar_juil <- sapply(base$temperature_moyenne24h, calculer_min_periode, debut = 61, fin = 214)
base$prec_moy_mar_juil <- sapply(base$precipitation_somme24h, calculer_moyenne_periode, debut = 61, fin = 214)
base$radia_moy_mar_juil <- sapply(base$radiation_somme24h, calculer_moyenne_periode, debut = 61, fin = 214)

# Ajouter les colonnes pour octobre-avril en prenant en compte les années N et N-1
base$temp_moy_oct_avr <- sapply(1:nrow(base), function(i) {
  debut_annee_N <- 1
  fin_annee_N_1 <- 243
  fin_annee_N <- 364
  
  if (i > 1) {
    debut_annee_N <- 243
    fin_annee_N_1 <- 364
    fin_annee_N <- 485
  }
  
  moyenne <- calculer_moyenne_periode(base$temperature_moyenne24h[i], debut = debut_annee_N, fin = fin_annee_N)
  
  if (i > 1) {
    moyenne <- (moyenne + calculer_moyenne_periode(base$temperature_moyenne24h[i - 1], debut = fin_annee_N_1 - 121, fin = fin_annee_N_1)) / 2
  }
  return(moyenne)
})

base$prec_moy_oct_avr <- sapply(1:nrow(base), function(i) {
  debut_annee_N <- 1
  fin_annee_N_1 <- 243
  fin_annee_N <- 364
  
  if (i > 1) {
    debut_annee_N <- 243
    fin_annee_N_1 <- 364
    fin_annee_N <- 485
  }
  
  moyenne <- calculer_moyenne_periode(base$precipitation_somme24h[i], debut = debut_annee_N, fin = fin_annee_N)
  
  if (i > 1) {
    moyenne <- (moyenne + calculer_moyenne_periode(base$precipitation_somme24h[i - 1], debut = fin_annee_N_1 - 121, fin = fin_annee_N_1)) / 2
  }
  return(moyenne)
})


base$radia_moy_oct_avr <- sapply(1:nrow(base), function(i) {
  debut_annee_N <- 1
  fin_annee_N_1 <- 243
  fin_annee_N <- 364
  
  if (i > 1) {
    debut_annee_N <- 243
    fin_annee_N_1 <- 364
    fin_annee_N <- 485
  }
  
  moyenne <- calculer_moyenne_periode(base$radiation_somme24h[i], debut = debut_annee_N, fin = fin_annee_N)
  
  if (i > 1) {
    moyenne <- (moyenne + calculer_moyenne_periode(base$radiation_somme24h[i - 1], debut = fin_annee_N_1 - 121, fin = fin_annee_N_1)) / 2
  }
  return(moyenne)
})





base$temp_max_oct_avr <- sapply(1:nrow(base), function(i) {
  debut_annee_N <- 1
  fin_annee_N_1 <- 243
  fin_annee_N <- 364
  
  if (i > 1) {
    debut_annee_N <- 243
    fin_annee_N_1 <- 364
    fin_annee_N <- 485
  }
  
  maximum <- calculer_max_periode(base$temperature_moyenne24h[i], debut = debut_annee_N, fin = fin_annee_N)
  
  if (i > 1) {
    maximum <- max(maximum, calculer_max_periode(base$temperature_moyenne24h[i - 1], debut = fin_annee_N_1 - 121, fin = fin_annee_N_1))
  }
  return(maximum)
})

base$temp_min_oct_avr <- sapply(1:nrow(base), function(i) {
  debut_annee_N <- 1
  fin_annee_N_1 <- 243
  fin_annee_N <- 364
  
  if (i > 1) {
    debut_annee_N <- 243
    fin_annee_N_1 <- 364
    fin_annee_N <- 485
  }
  
  minimum <- calculer_min_periode(base$temperature_moyenne24h[i], debut = debut_annee_N, fin = fin_annee_N)
  
  if (i > 1) {
    minimum <- min(minimum, calculer_min_periode(base$temperature_moyenne24h[i - 1], debut = fin_annee_N_1 - 121, fin = fin_annee_N_1))
  }
  return(minimum)
})


####################
#Random forest# 
####################

library(caret)
library(randomForest)

train_data <- base[, c("rend","Departement", "Annee", "temp_moy_mar_juil", "temp_max_mar_juil", "temp_min_mar_juil", 
                       "prec_moy_mar_juil", "radia_moy_mar_juil", "temp_moy_oct_avr", 
                       "prec_moy_oct_avr", "radia_moy_oct_avr", "temp_max_oct_avr", "temp_min_oct_avr")]


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
model <- train(rend ~ temp_moy_mar_juil + temp_max_mar_juil + temp_min_mar_juil + prec_moy_mar_juil + radia_moy_mar_juil + temp_moy_oct_avr + prec_moy_oct_avr
               + radia_moy_oct_avr + temp_max_oct_avr + temp_min_oct_avr + Departement,
               data = train_data,
               method = "rf",
               trControl = train_control)

print(model)


# Tracer l'importance des prédicteurs
importance <- varImp(model)
print(importance)


variables_a_analyser <- c("temp_moy_mar_juil", "temp_max_mar_juil", "temp_min_mar_juil", 
                          "prec_moy_mar_juil", "radia_moy_mar_juil", "temp_moy_oct_avr", 
                          "prec_moy_oct_avr", "radia_moy_oct_avr", "temp_max_oct_avr", "temp_min_oct_avr")
pdf("effets_partiels_ble.pdf")  # Ouvre un fichier PDF pour sauvegarder les graphiques

par(mfrow = c(3, 4)) 

# Boucle pour tracer l'effet partiel pour chaque variable
for (variable in variables_a_analyser) {
  effet_partiel <- pdp::partial(object = model, pred.var = variable, train = train_data)
  plot(effet_partiel, main = paste("Effet partiel de", variable, "sur le rendement"))
}

dev.off() 
## j'essaie de déterminer les "seuils critiques" pour chaque variable grâce à 
# ces effets partiels

#"radia_moy_oct_avr" : seuil 9e+06
#"radia_moy_mar_juil" : seuils à 1.75e+07 et 2.05e+07 
#"temp_min_mar_juil" : seuils à 276 et 280 
#"prec_moy_oct_avr" : seuils : 3.5 et 4
#"prec_moy_mar_juil" : seuils à 1.5 et à 3
#"temp_moy_oct_avr", seuis à 283 et 286
#"temp_moy_mar_juil" : seuil à 286
#"temp_max_mar_juil" : LIN
#"temp_max_oct_avr", : seuil 298
#"temp_min_oct_avr" : seuils à 277

###############
#### Within ####
###############

# Création des variables seuils (dummies)
train_data$radia_moy_oct_avr_seuil9 <- as.numeric(train_data$radia_moy_oct_avr <= 9e+06)
train_data$radia_moy_mar_juil_seuil1 <- as.numeric(train_data$radia_moy_mar_juil <= 1.75e+07)
train_data$radia_moy_mar_juil_seuil2 <- as.numeric(train_data$radia_moy_mar_juil > 1.75e+07 & train_data$radia_moy_mar_juil <= 2.05e+07)
train_data$temp_min_mar_juil_seuil276 <- as.numeric(train_data$temp_min_mar_juil <= 276)
train_data$temp_min_mar_juil_seuil280 <- as.numeric(train_data$temp_min_mar_juil > 276 & train_data$temp_min_mar_juil <= 280)
train_data$prec_moy_oct_avr_seuil3 <- as.numeric(train_data$prec_moy_mar_juil <= 3.5)
train_data$prec_moy_oct_avr_seuil3_4 <- as.numeric(train_data$prec_moy_mar_juil > 3.5 & train_data$prec_moy_mar_juil <= 4)
train_data$prec_moy_mar_juil_seuil1 <- as.numeric(train_data$prec_moy_mar_juil <= 1.5)
train_data$prec_moy_mar_juil_seuil1_3 <- as.numeric(train_data$prec_moy_mar_juil > 1.5 & train_data$prec_moy_mar_juil <= 3)
train_data$temp_moy_oct_avr_seuil283 <- as.numeric(train_data$temp_moy_oct_avr < 283)
train_data$temp_moy_oct_avr_seuil283_286 <- as.numeric(train_data$temp_moy_oct_avr >= 283 & train_data$temp_moy_oct_avr < 286)
train_data$temp_moy_mar_juil_seuil286 <- as.numeric(train_data$temp_moy_mar_juil <= 286)
train_data$temp_max_oct_avr_seuil298 <- as.numeric(train_data$temp_max_oct_avr <= 298)
train_data$temp_min_oct_avr_seuil277 <- as.numeric(train_data$temp_min_oct_avr <= 277)

library(plm)

# modele_regression_within <- plm(rend ~ radia_moy_oct_avr_seuil9 + radia_moy_oct_avr + 
#                                   radia_moy_mar_juil_seuil1 + radia_moy_mar_juil_seuil2 + radia_moy_mar_juil +
#                                   temp_min_mar_juil_seuil276 + temp_min_mar_juil_seuil280 + temp_min_mar_juil +
#                                   prec_moy_oct_avr_seuil3 + prec_moy_oct_avr_seuil3_4 + prec_moy_oct_avr +
#                                 prec_moy_mar_juil_seuil1  + prec_moy_mar_juil + prec_moy_mar_juil_seuil1_3 +
#                                 temp_moy_oct_avr_seuil283  + temp_moy_oct_avr + temp_moy_oct_avr_seuil283_286 +
#                                 temp_moy_mar_juil_seuil286 + temp_moy_mar_juil +
#                                   temp_max_oct_avr_seuil298 + temp_max_oct_avr +
#                                   temp_min_oct_avr_seuil277 + temp_min_oct_avr +
#                                   temp_max_mar_juil
#                                 ,
#                                 data = train_data,
#                                 index = c("Annee", "Departement"),
#                                 model = "within")
# summary(modele_regression_within)


# Je ne garde que les variables significatives

modele_regression_within <- plm(rend ~ radia_moy_oct_avr_seuil9 + radia_moy_oct_avr +
                                  radia_moy_mar_juil_seuil2 + radia_moy_mar_juil +
                                  prec_moy_oct_avr_seuil3 +
                                  prec_moy_mar_juil_seuil1  + prec_moy_mar_juil + prec_moy_mar_juil_seuil1_3 +
                                  temp_moy_oct_avr_seuil283 + temp_moy_mar_juil +
                                  temp_max_oct_avr +
                                  temp_max_mar_juil
                                ,
                                data = train_data,
                                index = c("Annee", "Departement"),
                                model = "within")
summary(modele_regression_within)



###########
#Long Dif ## 
#############
donnees_2010 <- subset(base, Annee == 2010)
donnees_2011 <- subset(base, Annee == 2011)
donnees_2021 <- subset(base, Annee == 2021)
donnees_2022 <- subset(base, Annee == 2022)

# Calculer les rendements moyens pour chaque département et chaque période, pour chaque année
rendements_moyens_2010 <- donnees_2010 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2010 = mean(rend),
    temp_moy_mar_juil_2010 = mean(temp_moy_mar_juil),
    temp_max_mar_juil_2010 = mean(temp_max_mar_juil),
    temp_min_mar_juil_2010 = mean(temp_min_mar_juil),
    temp_moy_oct_avr_2010 = mean(temp_moy_oct_avr),
    temp_max_oct_avr_2010 = mean(temp_max_oct_avr),
    temp_min_oct_avr_2010 = mean(temp_min_oct_avr),
    prec_moy_mar_juil_2010 = mean(prec_moy_mar_juil),
    radia_moy_mar_juil_2010 = mean(radia_moy_mar_juil),
    prec_moy_oct_avr_2010 = mean(prec_moy_oct_avr),
    radia_moy_oct_avr_2010 = mean(radia_moy_oct_avr)
  )

rendements_moyens_2011 <- donnees_2011 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2011 = mean(rend),
    temp_moy_mar_juil_2011 = mean(temp_moy_mar_juil),
    temp_max_mar_juil_2011 = mean(temp_max_mar_juil),
    temp_min_mar_juil_2011 = mean(temp_min_mar_juil),
    temp_moy_oct_avr_2011 = mean(temp_moy_oct_avr),
    temp_max_oct_avr_2011 = mean(temp_max_oct_avr),
    temp_min_oct_avr_2011 = mean(temp_min_oct_avr),
    prec_moy_mar_juil_2011 = mean(prec_moy_mar_juil),
    radia_moy_mar_juil_2011 = mean(radia_moy_mar_juil),
    prec_moy_oct_avr_2011 = mean(prec_moy_oct_avr),
    radia_moy_oct_avr_2011 = mean(radia_moy_oct_avr)
  )

rendements_moyens_2021 <- donnees_2021 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2021 = mean(rend),
    temp_moy_mar_juil_2021 = mean(temp_moy_mar_juil),
    temp_max_mar_juil_2021 = mean(temp_max_mar_juil),
    temp_min_mar_juil_2021 = mean(temp_min_mar_juil),
    temp_moy_oct_avr_2021 = mean(temp_moy_oct_avr),
    temp_max_oct_avr_2021 = mean(temp_max_oct_avr),
    temp_min_oct_avr_2021 = mean(temp_min_oct_avr),
    prec_moy_mar_juil_2021 = mean(prec_moy_mar_juil),
    radia_moy_mar_juil_2021 = mean(radia_moy_mar_juil),
    prec_moy_oct_avr_2021 = mean(prec_moy_oct_avr),
    radia_moy_oct_avr_2021 = mean(radia_moy_oct_avr)
  )

rendements_moyens_2022 <- donnees_2022 %>%
  group_by(Departement, Culture) %>%
  summarize(
    rendement_moyen_2022 = mean(rend),
    temp_moy_mar_juil_2022 = mean(temp_moy_mar_juil),
    temp_max_mar_juil_2022 = mean(temp_max_mar_juil),
    temp_min_mar_juil_2022 = mean(temp_min_mar_juil),
    temp_moy_oct_avr_2022 = mean(temp_moy_oct_avr),
    temp_max_oct_avr_2022 = mean(temp_max_oct_avr),
    temp_min_oct_avr_2022 = mean(temp_min_oct_avr),
    prec_moy_mar_juil_2022 = mean(prec_moy_mar_juil),
    radia_moy_mar_juil_2022 = mean(radia_moy_mar_juil),
    prec_moy_oct_avr_2022 = mean(prec_moy_oct_avr),
    radia_moy_oct_avr_2022 = mean(radia_moy_oct_avr)
  )

# Fusionner les jeux de données pour calculer la différence des rendements
donnees_diff <- merge(merge(merge(rendements_moyens_2010, rendements_moyens_2021, by = c("Departement", "Culture")), rendements_moyens_2022, by = c("Departement", "Culture")), rendements_moyens_2011, by = c("Departement", "Culture"))
donnees_diff[is.na(donnees_diff)] <- 0
donnees_diff <- donnees_diff %>%
  mutate(moyenne_rendement_2021_2022 = rowMeans(select(., c(rendement_moyen_2022, rendement_moyen_2021)), na.rm = TRUE),
         moyenne_rendement_2010_2011 = rowMeans(select(., c(rendement_moyen_2010, rendement_moyen_2011)), na.rm = TRUE),
moyenne_temp_moy_mar_juil_2021_2022 = rowMeans(select(., c(temp_moy_mar_juil_2021, temp_moy_mar_juil_2022)), na.rm = TRUE),
moyenne_temp_max_mar_juil_2021_2022 = rowMeans(select(., c(temp_max_mar_juil_2021, temp_max_mar_juil_2022)), na.rm = TRUE),
moyenne_temp_min_mar_juil_2021_2022 = rowMeans(select(., c(temp_min_mar_juil_2021, temp_min_mar_juil_2022)), na.rm = TRUE),
moyenne_prec_moy_mar_juil_2021_2022 = rowMeans(select(., c(prec_moy_mar_juil_2021, prec_moy_mar_juil_2022)), na.rm = TRUE),
moyenne_radia_moy_mar_juil_2021_2022 = rowMeans(select(., c(radia_moy_mar_juil_2021, radia_moy_mar_juil_2022)), na.rm = TRUE),
moyenne_temp_moy_oct_avr_2021_2022 = rowMeans(select(., c(temp_moy_oct_avr_2021, temp_moy_oct_avr_2022)), na.rm = TRUE),
moyenne_prec_moy_oct_avr_2021_2022 = rowMeans(select(., c(prec_moy_oct_avr_2021, prec_moy_oct_avr_2022)), na.rm = TRUE),
moyenne_radia_moy_oct_avr_2021_2022 = rowMeans(select(., c(radia_moy_oct_avr_2021, radia_moy_oct_avr_2022)), na.rm = TRUE),
moyenne_temp_max_oct_avr_2021_2022 = rowMeans(select(., c(temp_max_oct_avr_2021, temp_max_oct_avr_2022)), na.rm = TRUE),
moyenne_temp_min_oct_avr_2021_2022 = rowMeans(select(., c(temp_min_oct_avr_2021, temp_min_oct_avr_2022)), na.rm = TRUE),

moyenne_temp_moy_mar_juil_2010_2011 = rowMeans(select(., c(temp_moy_mar_juil_2010, temp_moy_mar_juil_2011)), na.rm = TRUE),
moyenne_temp_max_mar_juil_2010_2011 = rowMeans(select(., c(temp_max_mar_juil_2010, temp_max_mar_juil_2011)), na.rm = TRUE),
moyenne_temp_min_mar_juil_2010_2011 = rowMeans(select(., c(temp_min_mar_juil_2010, temp_min_mar_juil_2011)), na.rm = TRUE),
moyenne_prec_moy_mar_juil_2010_2011 = rowMeans(select(., c(prec_moy_mar_juil_2010, prec_moy_mar_juil_2011)), na.rm = TRUE),
moyenne_radia_moy_mar_juil_2010_2011 = rowMeans(select(., c(radia_moy_mar_juil_2010, radia_moy_mar_juil_2011)), na.rm = TRUE),
moyenne_temp_moy_oct_avr_2010_2011 = rowMeans(select(., c(temp_moy_oct_avr_2010, temp_moy_oct_avr_2011)), na.rm = TRUE),
moyenne_prec_moy_oct_avr_2010_2011 = rowMeans(select(., c(prec_moy_oct_avr_2010, prec_moy_oct_avr_2011)), na.rm = TRUE),
moyenne_radia_moy_oct_avr_2010_2011 = rowMeans(select(., c(radia_moy_oct_avr_2010, radia_moy_oct_avr_2011)), na.rm = TRUE),
moyenne_temp_max_oct_avr_2010_2011 = rowMeans(select(., c(temp_max_oct_avr_2010, temp_max_oct_avr_2011)), na.rm = TRUE),
moyenne_temp_min_oct_avr_2010_2011 = rowMeans(select(., c(temp_min_oct_avr_2010, temp_min_oct_avr_2011)), na.rm = TRUE))



donnees_diff <- donnees_diff %>%
  mutate(diff_rend = moyenne_rendement_2021_2022 - moyenne_rendement_2010_2011,
    diff_temp_moy_mar_juil = moyenne_temp_moy_mar_juil_2021_2022 - moyenne_temp_moy_mar_juil_2010_2011,
    diff_temp_max_mar_juil = moyenne_temp_max_mar_juil_2021_2022 - moyenne_temp_max_mar_juil_2010_2011,
    diff_temp_min_mar_juil = moyenne_temp_min_mar_juil_2021_2022 - moyenne_temp_min_mar_juil_2010_2011,
    diff_prec_moy_mar_juil = moyenne_prec_moy_mar_juil_2021_2022 - moyenne_prec_moy_mar_juil_2010_2011,
    diff_radia_moy_mar_juil = moyenne_radia_moy_mar_juil_2021_2022 - moyenne_radia_moy_mar_juil_2010_2011,
    diff_temp_moy_oct_avr = moyenne_temp_moy_oct_avr_2021_2022 - moyenne_temp_moy_oct_avr_2010_2011,
    diff_prec_moy_oct_avr = moyenne_prec_moy_oct_avr_2021_2022 - moyenne_prec_moy_oct_avr_2010_2011,
    diff_radia_moy_oct_avr = moyenne_radia_moy_oct_avr_2021_2022 - moyenne_radia_moy_oct_avr_2010_2011,
    diff_temp_max_oct_avr = moyenne_temp_max_oct_avr_2021_2022 - moyenne_temp_max_oct_avr_2010_2011,
    diff_temp_min_oct_avr = moyenne_temp_min_oct_avr_2021_2022 - moyenne_temp_min_oct_avr_2010_2011
  )




#Création des dummies seuils
donnees_diff$diff_radia_moy_oct_avr_seuil9 <- as.numeric(donnees_diff$diff_radia_moy_oct_avr <= 9e+06)
donnees_diff$diff_radia_moy_mar_juil_seuil1 <- as.numeric(donnees_diff$diff_radia_moy_mar_juil <= 1.75e+07)
donnees_diff$diff_radia_moy_mar_juil_seuil2 <- as.numeric(donnees_diff$diff_radia_moy_mar_juil > 1.75e+07 & donnees_diff$diff_radia_moy_mar_juil <= 2.05e+07)
donnees_diff$diff_temp_min_mar_juil_seuil276 <- as.numeric(donnees_diff$diff_temp_min_mar_juil <= 276)
donnees_diff$diff_temp_min_mar_juil_seuil280 <- as.numeric(donnees_diff$diff_temp_min_mar_juil > 276 & donnees_diff$diff_temp_min_mar_juil <= 280)
donnees_diff$diff_prec_moy_oct_avr_seuil3 <- as.numeric(donnees_diff$diff_prec_moy_mar_juil <= 3.5)
donnees_diff$diff_prec_moy_oct_avr_seuil3_4 <- as.numeric(donnees_diff$diff_prec_moy_mar_juil > 3.5 & donnees_diff$diff_prec_moy_mar_juil <= 4)
donnees_diff$diff_prec_moy_mar_juil_seuil1 <- as.numeric(donnees_diff$diff_prec_moy_mar_juil <= 1.5)
donnees_diff$diff_prec_moy_mar_juil_seuil1_3 <- as.numeric(donnees_diff$diff_prec_moy_mar_juil > 1.5 & donnees_diff$diff_prec_moy_mar_juil <= 3)
donnees_diff$diff_temp_moy_oct_avr_seuil283 <- as.numeric(donnees_diff$diff_temp_moy_oct_avr < 283)
donnees_diff$diff_temp_moy_oct_avr_seuil283_286 <- as.numeric(donnees_diff$diff_temp_moy_oct_avr >= 283 & donnees_diff$diff_temp_moy_oct_avr < 286)
donnees_diff$diff_temp_moy_mar_juil_seuil286 <- as.numeric(donnees_diff$diff_temp_moy_mar_juil <= 286)
donnees_diff$diff_temp_max_oct_avr_seuil298 <- as.numeric(donnees_diff$diff_temp_max_oct_avr <= 298)
donnees_diff$diff_temp_min_oct_avr_seuil277 <- as.numeric(donnees_diff$diff_temp_min_oct_avr <= 277)


# modèle de régression linéaire (long diff)
#je pense que je dois enlever le département à cause de la différence
# modele_regression_diff_long <- lm(diff_rend ~ diff_radia_moy_oct_avr_seuil9 + diff_radia_moy_oct_avr + 
#                                     diff_radia_moy_mar_juil_seuil1 + diff_radia_moy_mar_juil_seuil2 + diff_radia_moy_mar_juil +
#                                     diff_temp_min_mar_juil_seuil276 + diff_temp_min_mar_juil_seuil280 + diff_temp_min_mar_juil +
#                                     diff_prec_moy_oct_avr_seuil3 + diff_prec_moy_oct_avr_seuil3_4 + diff_prec_moy_oct_avr +
#                                     diff_prec_moy_mar_juil_seuil1  + diff_prec_moy_mar_juil + diff_prec_moy_mar_juil_seuil1_3 +
#                                     diff_temp_moy_oct_avr_seuil283  + diff_temp_moy_oct_avr + diff_temp_moy_oct_avr_seuil283_286 +
#                                     diff_temp_moy_mar_juil_seuil286 + diff_temp_moy_mar_juil +
#                                     diff_temp_max_oct_avr_seuil298 + diff_temp_max_oct_avr +
#                                     diff_temp_min_oct_avr_seuil277 + diff_temp_min_oct_avr + diff_temp_max_mar_juil,
#                                   data = donnees_diff)

##Les seuils font tous des coef NA, je les enlève
# 
# modele_regression_diff_long <- lm(diff_rend ~ 0 + diff_radia_moy_oct_avr  + diff_radia_moy_mar_juil +
#                                     diff_temp_min_mar_juil  + diff_prec_moy_oct_avr +diff_prec_moy_mar_juil + diff_temp_moy_oct_avr
#                                   + diff_temp_moy_mar_juil + diff_temp_max_mar_juil + diff_temp_max_oct_avr + diff_temp_min_oct_avr ,
#                                   data = donnees_diff)
# 
# # Obtenir les résultats du modèle
# summary(modele_regression_diff_long)

# on garde les variables significatives seulement

modele_regression_diff_long <- lm(diff_rend ~ 0 + diff_radia_moy_oct_avr  + diff_radia_moy_mar_juil +
                                    diff_temp_moy_oct_avr
                                   ,
                                  data = donnees_diff)

# Obtenir les résultats du modèle
summary(modele_regression_diff_long)

# 
# #Now sans les radiations pour les projections 2050
#  modele_regression_diff_long_projections <- lm(diff_rend ~ 0 + diff_temp_min_mar_juil  + diff_prec_moy_oct_avr +diff_prec_moy_mar_juil + diff_temp_moy_oct_avr
#                                   + diff_temp_moy_mar_juil + diff_temp_max_mar_juil + diff_temp_max_oct_avr + diff_temp_min_oct_avr ,
#                                   data = donnees_diff)
# 
# # Obtenir les résultats du modèle
# summary(modele_regression_diff_long_projections)

#on garde les variables significatives seulement

modele_regression_diff_long_projections <- lm(diff_rend ~ 0 + diff_temp_min_mar_juil  + diff_temp_moy_oct_avr
                                              + diff_temp_min_oct_avr ,
                                              data = donnees_diff)

# Obtenir les résultats du modèle
summary(modele_regression_diff_long_projections)




# 
# ## essai 3 ans
# donnees_2012 <- subset(base, Annee == 2012)
# 
# rendements_moyens_2012 <- donnees_2012 %>%
#   group_by(Departement, Culture) %>%
#   summarize(
#     rendement_moyen_2012 = mean(rend),
#     temp_moy_mar_juil_2012 = mean(temp_moy_mar_juil),
#     temp_max_mar_juil_2012 = mean(temp_max_mar_juil),
#     temp_min_mar_juil_2012 = mean(temp_min_mar_juil),
#     temp_moy_oct_avr_2012 = mean(temp_moy_oct_avr),
#     temp_max_oct_avr_2012 = mean(temp_max_oct_avr),
#     temp_min_oct_avr_2012 = mean(temp_min_oct_avr),
#     prec_moy_mar_juil_2012 = mean(prec_moy_mar_juil),
#     radia_moy_mar_juil_2012 = mean(radia_moy_mar_juil),
#     prec_moy_oct_avr_2012 = mean(prec_moy_oct_avr),
#     radia_moy_oct_avr_2012 = mean(radia_moy_oct_avr)
#   )
# 
# donnees_2020 <- subset(base, Annee == 2020)
# 
# rendements_moyens_2020 <- donnees_2020 %>%
#   group_by(Departement, Culture) %>%
#   summarize(
#     rendement_moyen_2020 = mean(rend),
#     temp_moy_mar_juil_2020 = mean(temp_moy_mar_juil),
#     temp_max_mar_juil_2020 = mean(temp_max_mar_juil),
#     temp_min_mar_juil_2020 = mean(temp_min_mar_juil),
#     temp_moy_oct_avr_2020 = mean(temp_moy_oct_avr),
#     temp_max_oct_avr_2020 = mean(temp_max_oct_avr),
#     temp_min_oct_avr_2020 = mean(temp_min_oct_avr),
#     prec_moy_mar_juil_2020 = mean(prec_moy_mar_juil),
#     radia_moy_mar_juil_2020 = mean(radia_moy_mar_juil),
#     prec_moy_oct_avr_2020 = mean(prec_moy_oct_avr),
#     radia_moy_oct_avr_2020 = mean(radia_moy_oct_avr)
#   )
# 
# 
# # Fusionner les jeux de données pour calculer la différence des rendements
# donnees_diff2 <- merge(
#   merge(
#     merge(rendements_moyens_2010, rendements_moyens_2011, by = c("Departement", "Culture")),
#     rendements_moyens_2012, by = c("Departement", "Culture")
#   ),
#   merge(
#     merge(rendements_moyens_2020, rendements_moyens_2021, by = c("Departement", "Culture")),
#     rendements_moyens_2022, by = c("Departement", "Culture")
#   ),
#   by = c("Departement", "Culture")
# )
# 
# # Remplacer les valeurs NA par 0
# 
# donnees_diff2[is.na(donnees_diff2)] <- 0
# donnees_diff2 <- donnees_diff2 %>%
#   mutate(
#     moyenne_rendement_2010_2011_2012 = rowMeans(select(., c(rendement_moyen_2010, rendement_moyen_2011, rendement_moyen_2012)), na.rm = TRUE),
#      moyenne_temp_moy_mar_juil_2010_2011_2012 = rowMeans(select(., c(temp_moy_mar_juil_2010, temp_moy_mar_juil_2011, temp_moy_mar_juil_2012)), na.rm = TRUE),
#     moyenne_temp_max_mar_juil_2010_2011_2012 = rowMeans(select(., c(temp_max_mar_juil_2010, temp_max_mar_juil_2011, temp_max_mar_juil_2012)), na.rm = TRUE),
#     moyenne_temp_min_mar_juil_2010_2011_2012 = rowMeans(select(., c(temp_min_mar_juil_2010, temp_min_mar_juil_2011, temp_min_mar_juil_2012)), na.rm = TRUE),
#     moyenne_prec_moy_mar_juil_2010_2011_2012 = rowMeans(select(., c(prec_moy_mar_juil_2010, prec_moy_mar_juil_2011, prec_moy_mar_juil_2012)), na.rm = TRUE),
#     moyenne_radia_moy_mar_juil_2010_2011_2012 = rowMeans(select(., c(radia_moy_mar_juil_2010, radia_moy_mar_juil_2011, radia_moy_mar_juil_2012)), na.rm = TRUE),
#     moyenne_temp_moy_oct_avr_2010_2011_2012 = rowMeans(select(., c(temp_moy_oct_avr_2010, temp_moy_oct_avr_2011, temp_moy_oct_avr_2012)), na.rm = TRUE),
#     moyenne_prec_moy_oct_avr_2010_2011_2012 = rowMeans(select(., c(prec_moy_oct_avr_2010, prec_moy_oct_avr_2011, prec_moy_oct_avr_2012)), na.rm = TRUE),
#     moyenne_radia_moy_oct_avr_2010_2011_2012 = rowMeans(select(., c(radia_moy_oct_avr_2010, radia_moy_oct_avr_2011, radia_moy_oct_avr_2012)), na.rm = TRUE),
#     moyenne_temp_max_oct_avr_2010_2011_2012 = rowMeans(select(., c(temp_max_oct_avr_2010, temp_max_oct_avr_2011, temp_max_oct_avr_2012)), na.rm = TRUE),
#     moyenne_temp_min_oct_avr_2010_2011_2012 = rowMeans(select(., c(temp_min_oct_avr_2010, temp_min_oct_avr_2011, temp_min_oct_avr_2012)), na.rm = TRUE),
#     moyenne_rendement_2020_2021_2022 = rowMeans(select(., c(rendement_moyen_2020, rendement_moyen_2021, rendement_moyen_2022)), na.rm = TRUE),
#     moyenne_temp_moy_mar_juil_2020_2021_2022 = rowMeans(select(., c(temp_moy_mar_juil_2020, temp_moy_mar_juil_2021, temp_moy_mar_juil_2022)), na.rm = TRUE),
#     moyenne_temp_max_mar_juil_2020_2021_2022 = rowMeans(select(., c(temp_max_mar_juil_2020, temp_max_mar_juil_2021, temp_max_mar_juil_2022)), na.rm = TRUE),
#     moyenne_temp_min_mar_juil_2020_2021_2022 = rowMeans(select(., c(temp_min_mar_juil_2020, temp_min_mar_juil_2021, temp_min_mar_juil_2022)), na.rm = TRUE),
#     moyenne_prec_moy_mar_juil_2020_2021_2022 = rowMeans(select(., c(prec_moy_mar_juil_2020, prec_moy_mar_juil_2021, prec_moy_mar_juil_2022)), na.rm = TRUE),
#     moyenne_radia_moy_mar_juil_2020_2021_2022 = rowMeans(select(., c(radia_moy_mar_juil_2020, radia_moy_mar_juil_2021, radia_moy_mar_juil_2022)), na.rm = TRUE),
#     moyenne_temp_moy_oct_avr_2020_2021_2022 = rowMeans(select(., c(temp_moy_oct_avr_2020, temp_moy_oct_avr_2021, temp_moy_oct_avr_2022)), na.rm = TRUE),
#     moyenne_prec_moy_oct_avr_2020_2021_2022 = rowMeans(select(., c(prec_moy_oct_avr_2020, prec_moy_oct_avr_2021, prec_moy_oct_avr_2022)), na.rm = TRUE),
#     moyenne_radia_moy_oct_avr_2020_2021_2022 = rowMeans(select(., c(radia_moy_oct_avr_2020, radia_moy_oct_avr_2021, radia_moy_oct_avr_2022)), na.rm = TRUE),
#     moyenne_temp_max_oct_avr_2020_2021_2022 = rowMeans(select(., c(temp_max_oct_avr_2020, temp_max_oct_avr_2021, temp_max_oct_avr_2022)), na.rm = TRUE),
#     moyenne_temp_min_oct_avr_2020_2021_2022 = rowMeans(select(., c(temp_min_oct_avr_2020, temp_min_oct_avr_2021, temp_min_oct_avr_2022)), na.rm = TRUE)
#     )
# 
# 
# 
# 
# 
# 
# donnees_diff2 <- donnees_diff2 %>%
#   mutate(diff_rend = moyenne_rendement_2020_2021_2022 - moyenne_rendement_2010_2011_2012,
#          diff_temp_moy_mar_juil = moyenne_temp_moy_mar_juil_2020_2021_2022 - moyenne_temp_moy_mar_juil_2010_2011_2012,
#          diff_temp_max_mar_juil = moyenne_temp_max_mar_juil_2020_2021_2022 - moyenne_temp_max_mar_juil_2010_2011_2012,
#          diff_temp_min_mar_juil = moyenne_temp_min_mar_juil_2020_2021_2022 - moyenne_temp_min_mar_juil_2010_2011_2012,
#          diff_prec_moy_mar_juil = moyenne_prec_moy_mar_juil_2020_2021_2022 - moyenne_prec_moy_mar_juil_2010_2011_2012,
#          diff_radia_moy_mar_juil = moyenne_radia_moy_mar_juil_2020_2021_2022 - moyenne_radia_moy_mar_juil_2010_2011_2012,
#          diff_temp_moy_oct_avr = moyenne_temp_moy_oct_avr_2020_2021_2022 - moyenne_temp_moy_oct_avr_2010_2011_2012,
#          diff_prec_moy_oct_avr = moyenne_prec_moy_oct_avr_2020_2021_2022 - moyenne_prec_moy_oct_avr_2010_2011_2012,
#          diff_radia_moy_oct_avr = moyenne_radia_moy_oct_avr_2020_2021_2022 - moyenne_radia_moy_oct_avr_2010_2011_2012,
#          diff_temp_max_oct_avr = moyenne_temp_max_oct_avr_2020_2021_2022 - moyenne_temp_max_oct_avr_2010_2011_2012,
#          diff_temp_min_oct_avr = moyenne_temp_min_oct_avr_2020_2021_2022 - moyenne_temp_min_oct_avr_2010_2011_2012
#   )
# 
# 
# # # modèle de régression linéaire (long diff)
# # 
# # modele_regression_diff_long2 <- lm(diff_rend ~ 0 + diff_radia_moy_oct_avr  + diff_radia_moy_mar_juil +
# #                                     diff_temp_min_mar_juil  + diff_prec_moy_oct_avr +diff_prec_moy_mar_juil + diff_temp_moy_oct_avr 
# #                                   + diff_temp_moy_mar_juil + diff_temp_max_mar_juil + diff_temp_max_oct_avr + diff_temp_min_oct_avr ,
# #                                   data = donnees_diff2)
# # 
# # # Obtenir les résultats du modèle
# # summary(modele_regression_diff_long2)
# 
# # On garde la seule variable significative 
# 
# modele_regression_diff_long3 <- lm(diff_rend ~ 0 + diff_radia_moy_oct_avr  + diff_radia_moy_mar_juil +
#                                     diff_temp_min_mar_juil  + diff_prec_moy_oct_avr +diff_prec_moy_mar_juil + diff_temp_moy_oct_avr
#                                   + diff_temp_moy_mar_juil + diff_temp_max_mar_juil + diff_temp_max_oct_avr + diff_temp_min_oct_avr ,
#                                   data = donnees_diff2)
# 
# # Obtenir les résultats du modèle
# summary(modele_regression_diff_long3)
# 
# #Now sans les radiations pour les projections 2050
# modele_regression_diff_long_projections2 <- lm(diff_rend ~ diff_temp_min_mar_juil  + diff_prec_moy_oct_avr +diff_prec_moy_mar_juil + diff_temp_moy_oct_avr 
#                                               + diff_temp_moy_mar_juil + diff_temp_max_mar_juil + diff_temp_max_oct_avr + diff_temp_min_oct_avr ,
#                                               data = donnees_diff2)
# 
# # Obtenir les résultats du modèle
# summary(modele_regression_diff_long_projections2)
