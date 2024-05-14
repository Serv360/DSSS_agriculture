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

#a changer par le lien vers le cloned repository
setwd("C:/Users/Serv3/Desktop/DSSS project/DSSS agriculture/DSSS_agriculture")


chemin <- "2_analyse/data/"

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
## proposition mais : avril -> octobre

##########
## Blé ##
##########
base <-base %>%filter(Culture =="14 - Maïs grain irrigué" |Culture == "15 - Maïs grain non irrigué" )

calculer_max_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  valeurs_numeriques <- valeurs_numeriques[!is.na(valeurs_numeriques)]  # Exclure les valeurs NA
  
  if (length(valeurs_numeriques) == 0) {
    return(NA)  # Retourner NA si aucune valeur numérique n'est disponible
  } else {
    max_val <- max(valeurs_numeriques)
    return(max_val)
  }
}

calculer_min_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  valeurs_numeriques <- valeurs_numeriques[!is.na(valeurs_numeriques)]  # Exclure les valeurs NA
  
  if (length(valeurs_numeriques) == 0) {
    return(NA)  # Retourner NA si aucune valeur numérique n'est disponible
  } else {
    min_val <- min(valeurs_numeriques)
    return(min_val)
  }
}

calculer_moyenne_periode <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_periode <- valeurs[debut:fin]  # Sélectionner les valeurs pour la période spécifiée
  valeurs_numeriques <- as.numeric(valeurs_periode)
  valeurs_numeriques <- valeurs_numeriques[!is.na(valeurs_numeriques)]  # Exclure les valeurs NA
  
  if (length(valeurs_numeriques) == 0) {
    return(NA)  # Retourner NA si aucune valeur numérique n'est disponible
  } else {
    moyenne <- mean(valeurs_numeriques)
    return(moyenne)
  }
}

calculer_max_jours_precipitation_nulle <- function(ligne, debut, fin) {
  valeurs <- unlist(strsplit(ligne, ", "))
  valeurs_precipitation <- as.numeric(valeurs[debut:fin])  # Sélectionner les valeurs de précipitation pour la période spécifiée
  valeurs_precipitation <- valeurs_precipitation[!is.na(valeurs_precipitation)]  # Exclure les valeurs NA
  
  if (length(valeurs_precipitation) == 0) {
    return(NA)  # Retourner NA si aucune valeur numérique n'est disponible
  } else {
    jours_precipitation_nulle <- sum(valeurs_precipitation == 0)
    return(jours_precipitation_nulle)
  }
}


  # Votre code pour agréger par mois
mois <- c("avril","mai","juin","juillet","aout","septembre","octobre")
  
  # Fonction pour ajouter les colonnes pour chaque mois
  ajouter_colonnes_mois <- function(base, mois) {
    # Boucle pour chaque mois
    for (m in mois) {
        # Détermination des débuts et fins de périodes selon le mois
       if (m == "janvier") {
          debut <- 1
          fin <- 31
        } else if (m == "février") {
          debut <- 32
          fin <- 59
          }else if (m == "avril-mai-juin") {
            debut <- 91
            fin <- 181
          } 
          else if (m == "juillet-aout-septembre-octobre") {
            debut <- 182
            fin <- 304
          } 
         else if (m == "mars") {
          debut <- 60
          fin <- 90
        } else if (m == "avril") {
          debut <- 91
          fin <- 120
        } else if (m == "mai") {
          debut <- 121
          fin <- 151
        } else if (m == "juin") {
          debut <- 152
          fin <- 181
        } else if (m == "juillet") {
          debut <- 182
          fin <- 212
        } else if (m == "août") {
          debut <- 213
          fin <- 243
        } else if (m == "septembre") {
          debut <- 244
          fin <- 273
        } else if (m == "octobre") {
          debut <- 274
          fin <- 304
        } else if (m == "novembre") {
          debut <- 305
          fin <- 335
        } else if (m == "décembre") {
          debut <- 336
          fin <- 365
        }
      
      # Application des fonctions pour le mois spécifique
      temp_moy <- sapply(base$temperature_moyenne24h, calculer_moyenne_periode, debut = debut, fin = fin)
      temp_max <- sapply(base$temperature_moyenne24h, calculer_max_periode, debut = debut, fin = fin)
      temp_min <- sapply(base$temperature_moyenne24h, calculer_min_periode, debut = debut, fin = fin)
      prec_moy <- sapply(base$precipitation_somme24h, calculer_moyenne_periode, debut = debut, fin = fin)
      radia_moy <- sapply(base$radiation_somme24h, calculer_moyenne_periode, debut = debut, fin = fin)
      max_jours_prec_nulles <- sapply(base$precipitation_somme24h, calculer_max_jours_precipitation_nulle, debut = debut, fin = fin)
      
      # Nom des nouvelles colonnes
      nom_col_temp_moy <- paste("temp_moy", m, sep = "_")
      nom_col_temp_max <- paste("temp_max", m, sep = "_")
      nom_col_temp_min <- paste("temp_min", m, sep = "_")
      nom_col_prec_moy <- paste("prec_moy", m, sep = "_")
      nom_col_radia_moy <- paste("radia_moy", m, sep = "_")
      nom_col_max_jours_prec_nulles <- paste("max_jours_prec_nulles", m, sep = "_")
      
      # Ajout des nouvelles colonnes au dataframe
      base[[nom_col_temp_moy]] <- temp_moy
      base[[nom_col_temp_max]] <- temp_max
      base[[nom_col_temp_min]] <- temp_min
      base[[nom_col_prec_moy]] <- prec_moy
      base[[nom_col_radia_moy]] <- radia_moy
      base[[nom_col_max_jours_prec_nulles]] <- max_jours_prec_nulles
    }
    return(base)
  }
base <- ajouter_colonnes_mois(base,mois)
  
colnames(base)

####################
#Random forest# 
####################

library(caret)
library(randomForest)



train_data <- subset(base, select = -c(LIB_DEP, temperature_moyenne24h, precipitation_somme24h, radiation_somme24h, Culture, surf, prod))
train_data_filtered <- train_data[!(is.na(train_data$rend) | train_data$rend == 0), ]
for (col in names(train_data_filtered)) {
  if (col != "rend") {
    train_data_filtered[[col]][is.na(train_data_filtered[[col]])] <- median(train_data_filtered[[col]], na.rm = TRUE)
  }
}
train_data<-train_data_filtered
set.seed(123) # Pour la reproductibilité

###Robustesses : on entraine successivement notre random forest sur 2/3 des données, 
###et tout sauf 2 années tirées au hasard
### et on teste sur le reste de l'échantillon à chaque fois

## Diviser les données en ensembles d'entraînement et de test de façon aléatoire (par exemple, 70% pour l'entraînement et 30% pour le test)

trainIndex <- createDataPartition(train_data$rend, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
train_data1 <- train_data[trainIndex, ]
data_test <- train_data[-trainIndex, ]

# Définir la méthode de cross-validation
# Créer un index de groupe pour chaque année
group_index <- groupKFold(train_data1$Annee, k = 3)


# Réindexer les groupes pour inclure la nouvelle année factice
group_index <- groupKFold(train_data1$Annee, k = 3)

# Définir la méthode de cross-validation avec timeslice et groupKFold : prise en compte du caractère temporel des données
train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE,
                              index = group_index)
# Entraînement du modèle de forêt aléatoire avec cross-validation
model <- train(rend ~ . - Annee,
               data = train_data1,
               method = "rf",
               trControl = train_control)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(model, newdata = data_test)

# Calculer les métriques de performance
rmse <- RMSE(predictions, data_test$rend)
mae <- MAE(predictions, data_test$rend)
rsquared <- R2(predictions, data_test$rend)

# Afficher les métriques
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", rsquared))
# Créer un dataframe avec les valeurs réelles et prédites
plot_data <- data.frame(Reel = data_test$rend, Predit = predictions)

# Tracer le vrai rendement par rapport au rendement prédit
ggplot(plot_data, aes(x = Reel, y = Predit)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Ajouter une ligne d'égalité
  labs(x = "Rendement réel", y = "Rendement prédit", title = "Rendement réel vs. Rendement prédit")


## Diviser les données en ensembles d'entraînement et de test en enlevant 2 années au hasard

annees_uniques <- unique(train_data$Annee)

annees_a_retirer <- sample(annees_uniques, 2)

# Filtrer les données d'entraînement pour retirer les observations correspondant à ces trois années
train_data1 <- train_data[!train_data$Annee %in% annees_a_retirer, ]
data_test <- train_data[train_data$Annee %in% annees_a_retirer, ]


# Définir la méthode de cross-validation

train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE)

# Créer un index de groupe pour chaque département
group_index <- groupKFold(train_data$Departement, k = 3)

# Entraîner le modèle avec validation croisée
model <- train(rend ~ . -Annee,
               data = train_data1,
               method = "rf",
               trControl = train_control,
               index = group_index)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(model, newdata = data_test)

# Calculer les métriques de performance
rmse <- RMSE(predictions, data_test$rend)
mae <- MAE(predictions, data_test$rend)
rsquared <- R2(predictions, data_test$rend)

# Afficher les métriques
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", rsquared))
# Créer un dataframe avec les valeurs réelles et prédites
plot_data <- data.frame(Reel = data_test$rend, Predit = predictions)

# Tracer le vrai rendement par rapport au rendement prédit
ggplot(plot_data, aes(x = Reel, y = Predit)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Ajouter une ligne d'égalité
  labs(x = "Rendement réel", y = "Rendement prédit", title = "Rendement réel vs. Rendement prédit")




### Maintenant on entraine sur train_data en entier
group_index <- groupKFold(train_data$Annee, k = 3)

# Définir la méthode de cross-validation avec timeslice et groupKFold : prise en compte du caractère temporel des données
train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE,
                              index = group_index)
# Entraînement du modèle de forêt aléatoire avec cross-validation
model <- train(rend ~ . -Annee,
               data = train_data,
               method = "rf",
               trControl = train_control)

# Tracer l'importance des prédicteurs
importance <- varImp(model)
print(importance)

# on regarde l'effet partiel sur le rendement des variables les plus importantes: importance > 5
variables_a_analyser <- c(# à mettre                          
  )
pdf("effets_partiels_mais.pdf")  # Ouvre un fichier PDF pour sauvegarder les graphiques

par(mfrow = c(3, 4)) 


# Boucle pour tracer l'effet partiel pour chaque variable
for (variable in variables_a_analyser) {
  effet_partiel <- pdp::partial(object = model, pred.var = variable, train = train_data)
  plot(effet_partiel)
}

dev.off() 



## Modèle 3 ans pour capter l'effet CT

train_data_3ans <- train_data %>%
  filter(Annee == 2010 | Annee == 2011 | Annee == 2012)
### Maintenant on entraine sur train_data en entier
group_index <- groupKFold(train_data_3ans$Annee, k = 3)

# Définir la méthode de cross-validation avec timeslice et groupKFold : prise en compte du caractère temporel des données
train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE,
                              index = group_index)
# Entraînement du modèle de forêt aléatoire avec cross-validation
model_3ans <- train(rend ~ . -Annee,
               data = train_data_3ans,
               method = "rf",
               trControl = train_control)

# Tracer l'importance des prédicteurs
importance <- varImp(model_3ans)
print(importance)

# on regarde l'effet partiel sur le rendement des variables les plus importantes: > 5
variables_a_analyser <-  c(# à mettre                       
)
pdf("effets_partiels_mais_3ans.pdf")  # Ouvre un fichier PDF pour sauvegarder les graphiques

par(mfrow = c(3, 4)) 


# Boucle pour tracer l'effet partiel pour chaque variable
for (variable in variables_a_analyser) {
  effet_partiel <- pdp::partial(object = model, pred.var = variable, train = train_data)
  plot(effet_partiel)
}

dev.off() 

###Robustesse : on entraine successivement notre random forest sur 2/3 des données, 

## Diviser les données en ensembles d'entraînement et de test de façon aléatoire (par exemple, 70% pour l'entraînement et 30% pour le test)

trainIndex <- createDataPartition(train_data_3ans$rend, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
train_data1 <- train_data_3ans[trainIndex, ]
data_test <- train_data_3ans[-trainIndex, ]

# Définir la méthode de cross-validation
# Créer un index de groupe pour chaque année
group_index <- groupKFold(train_data1$Annee, k = 3)


# Réindexer les groupes pour inclure la nouvelle année factice
group_index <- groupKFold(train_data1$Annee, k = 3)

# Définir la méthode de cross-validation avec timeslice et groupKFold : prise en compte du caractère temporel des données
train_control <- trainControl(method = "timeslice", 
                              initialWindow = 3, 
                              horizon = 1, 
                              fixedWindow = TRUE,
                              index = group_index)
# Entraînement du modèle de forêt aléatoire avec cross-validation
model3 <- train(rend ~ . - Annee,
               data = train_data1,
               method = "rf",
               trControl = train_control)

# Faire des prédictions sur l'ensemble de test
data_test <- data_test %>%
  filter(Departement %in% unique(train_data1$Departement))
predictions <- predict(model3, newdata = data_test)

# Calculer les métriques de performance
rmse <- RMSE(predictions, data_test$rend)
mae <- MAE(predictions, data_test$rend)
rsquared <- R2(predictions, data_test$rend)

# Afficher les métriques
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", rsquared))



## Prédictions des rendements en 2020, 2021 et 2022


##pour comparer les modèles long terme et court terme
##je ne conserve que les départements qui cultivaient déjà du blé en 2010-2012
#20

data20 <- train_data %>% 
  filter(Annee == 2020)

data20 <- data20 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions20 <- predict(model, newdata = data20)

data20 <- data20 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))
predictions20_3ans <- predict(model_3ans, newdata = data20)

comparison_table20 <- data.frame(département = data20$Departement, Vraies_Valeurs = data20$rend, Prédictions = predictions20, Prédictions_CT = predictions20_3ans, Diff_LT_moins_CT=predictions20-predictions20_3ans)

reg20 <- lm(Vraies_Valeurs ~ Prédictions, data = comparison_table20)
summary(reg20)
reg20_3ans <- lm(Prédictions ~ Prédictions_CT, data = comparison_table20)
summary(reg20_3ans)
sum(comparison_table20$Diff_LT_moins_CT)/nrow(comparison_table20)

#21
data21 <- train_data %>% 
  filter(Annee == 2021)


data21 <- data21 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions21 <- predict(model, newdata = data21)

data21 <- data21 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))
predictions21_3ans <- predict(model_3ans, newdata = data21)

comparison_table21 <- data.frame(département = data21$Departement, Vraies_Valeurs = data21$rend, Prédictions = predictions21, Prédictions_CT = predictions21_3ans, Diff_LT_moins_CT=predictions21-predictions21_3ans)

reg21 <- lm(Vraies_Valeurs ~ Prédictions, data = comparison_table21)
summary(reg21)
reg21_3ans <- lm(Prédictions ~ Prédictions_CT, data = comparison_table21)
summary(reg21_3ans)
sum(comparison_table21$Diff_LT_moins_CT)/nrow(comparison_table21)

#22
data22 <- train_data %>%
  filter(Annee == 2022)


data22 <- data22 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions22 <- predict(model, newdata = data22)

data22 <- data22 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))
predictions22_3ans <- predict(model_3ans, newdata = data22)

comparison_table22 <- data.frame(département = data22$Departement, Vraies_Valeurs = data22$rend, Prédictions = predictions22, Prédictions_CT = predictions22_3ans, Diff_LT_moins_CT=predictions22-predictions22_3ans)

reg22 <- lm(Vraies_Valeurs ~ Prédictions, data = comparison_table22)
summary(reg22)
reg22_3ans <- lm(Prédictions ~ Prédictions_CT, data = comparison_table22)
summary(reg22_3ans)

sum(comparison_table22$Diff_LT_moins_CT)/nrow(comparison_table21)




## On tient maintenant compte de la surface cultivée

train_data <- subset(base, select = -c(LIB_DEP, temperature_moyenne24h, precipitation_somme24h, radiation_somme24h, Culture, surf, prod))
train_data_filtered <- train_data[!(is.na(train_data$rend) | train_data$rend == 0), ]
for (col in names(train_data_filtered)) {
  if (col != "rend") {
    train_data_filtered[[col]][is.na(train_data_filtered[[col]])] <- median(train_data_filtered[[col]], na.rm = TRUE)
  }
}
train_data<-train_data_filtered




data20 <- train_data %>% 
  filter(Annee == 2020)

data20 <- data20 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions20 <- predict(model, newdata = data20)

data20 <- data20 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))
predictions20_3ans <- predict(model_3ans, newdata = data20)

comparison_table20 <- data.frame(département = data20$Departement, surface = data20$surf, Vraies_Valeurs = data20$rend, Prédictions = predictions20, Prédictions_CT = predictions20_3ans)
comparison_table20$Vraies_valeurs_Multipliées <- comparison_table20$Vraies_Valeurs * comparison_table20$surface
comparison_table20$Prédictions_Multipliées <- comparison_table20$Prédictions * comparison_table20$surface
comparison_table20$Prédictions_CT_Multipliées <- comparison_table20$Prédictions_CT * comparison_table20$surface

reg20 <- lm(Vraies_valeurs_Multipliées ~ Prédictions_Multipliées, data = comparison_table20)
summary(reg20)
reg20_3ans <- lm(Prédictions_Multipliées ~ Prédictions_CT_Multipliées, data = comparison_table20)
summary(reg20_3ans)

comparison_table20$dif <- (comparison_table20$Prédictions_Multipliées-comparison_table20$Prédictions_CT_Multipliées)
sum(comparison_table20$dif)


data21 <- train_data %>% 
  filter(Annee == 2021)


data21 <- data21 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions21 <- predict(model, newdata = data21)

data21 <- data21 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))
predictions21_3ans <- predict(model_3ans, newdata = data21)

comparison_table21 <- data.frame(département = data21$Departement, surface = data21$surf, Vraies_Valeurs = data21$rend, Prédictions = predictions21, Prédictions_CT = predictions21_3ans)
comparison_table21$Vraies_valeurs_Multipliées <- comparison_table21$Vraies_Valeurs * comparison_table21$surface
comparison_table21$Prédictions_Multipliées <- comparison_table21$Prédictions * comparison_table21$surface
comparison_table21$Prédictions_CT_Multipliées <- comparison_table21$Prédictions_CT * comparison_table21$surface

reg21 <- lm(Vraies_valeurs_Multipliées ~ Prédictions_Multipliées, data = comparison_table21)
summary(reg21)
reg21_3ans <- lm(Prédictions_Multipliées ~ Prédictions_CT_Multipliées, data = comparison_table21)
summary(reg21_3ans)

comparison_table21$dif <- (comparison_table21$Prédictions_Multipliées-comparison_table21$Prédictions_CT_Multipliées)
sum(comparison_table21$dif)


data22 <- train_data %>% 
  filter(Annee == 2022)

data22 <- data22 %>%
  filter(Departement %in% unique(train_data_3ans$Departement))

predictions22 <- predict(model, newdata = data22)

predictions22_3ans <- predict(model_3ans, newdata = data22)

comparison_table22 <- data.frame(département = data22$Departement, surface = data22$surf, Vraies_Valeurs = data22$rend, Prédictions = predictions22, Prédictions_CT = predictions22_3ans)
comparison_table22$Vraies_valeurs_Multipliées <- comparison_table22$Vraies_Valeurs * comparison_table22$surface
comparison_table22$Prédictions_Multipliées <- comparison_table22$Prédictions * comparison_table22$surface
comparison_table22$Prédictions_CT_Multipliées <- comparison_table22$Prédictions_CT * comparison_table22$surface

reg22 <- lm(Vraies_valeurs_Multipliées ~ Prédictions_Multipliées, data = comparison_table22)
summary(reg22)
reg22_3ans <- lm(Prédictions_Multipliées ~ Prédictions_CT_Multipliées, data = comparison_table22)
summary(reg22_3ans)


comparison_table22$dif <- (comparison_table22$Prédictions_Multipliées-comparison_table22$Prédictions_CT_Multipliées)
sum(comparison_table22$dif)