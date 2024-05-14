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
library(cowplot)
library(mapdata)

#a changer avec lien vers cloned repository
setwd("C:/Users/Serv3/Desktop/DSSS project/DSSS agriculture/DSSS_agriculture")

# Téléchargement de la première base data_adaptation
chemin <- "2_analyse/data_adaptation/"

base <- list()

for (annee in c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) {
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
base_ble <- base %>%filter(Culture =="01 - Blé tendre d'hiver et épeautre")

#######################################################################
# Téléchargement de la seconde base data

chemin2 <- "2_analyse/data/"

base2 <- list()

for (annee in c(2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) {
  donnees_annee <- list()
  for (region in c(11,24,27,28,32,44,52,53,75,76,84,93,94)) {
    chemin_fichier <- paste0(chemin2, "merged_tibble_", annee, "_", region, ".csv")
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
  base2[[as.character(annee)]] <- donnees_assemblees
}

base2 <- as.data.frame(do.call(rbind, base2))


## définir les cycles de croissance du blé et du mais
## proposition ble : octobre -> avril / mars -> juillet
## proposition mais : avril -> aout / juillet -> octobre

##########
## Blé ##
##########
base_ble2 <- base2 %>%filter(Culture =="01 - Blé tendre d'hiver et épeautre")

#######################################################################

# Graphique pour la Figure 5

base_ble_filtered <- base_ble %>% filter(surf >= 20000) #########

# Calculate the percentage change in rend from 2015 to 2016 for each Departement
change_data <- base_ble_filtered %>%
  filter(Annee %in% c(2015, 2016)) %>%
  group_by(Departement) %>%
  summarize(change = (last(rend) - first(rend)) / first(rend),
            surf = first(surf),
            surf_parcelles = first(Mean_surface_parcelles),
            nom_dep=substr(first(LIB_DEP), start=7, stop=nchar(first(LIB_DEP))))

# Classify departements into groups based on the percentage change
change_data <- change_data %>%
  mutate(group = case_when(
    change < -0.4 ~ "Diminution > 40%",
    change >= -0.4 & change < -0.25 ~ "Diminution de 25% à 40%",
    change >= -0.25 & change < -0.1 ~ "Diminution de 10% à 25%",
    change >= -0.10 & change < 1 ~ "Dim. de moins de 10% ou augmentation",
    TRUE ~ "Other"
  ))

# Aggregate data to calculate mean surface for each year, crop, and group
agg_data5 <- base_ble_filtered %>%
  filter(Annee >= 2015) %>% ###########
left_join(change_data, by = "Departement") %>%
  group_by(Annee, Culture, group) %>%
  summarize(mean_surface = weighted.mean(Mean_surface_parcelles),
            variance_within_group = var(Mean_surface_parcelles)) ##########

# Calculate the normalization factor as the ratio of the mean surface for each year to the mean surface for the year 2016, multiplied by 100
agg_data5 <- agg_data5 %>%
  group_by(Culture, group) %>%
  mutate(normalization_factor = mean_surface / mean_surface[Annee == 2016])

# Print out normalization factor to check
print(agg_data5$normalization_factor)

# Normalize the mean surface values at 100 for the year 2016
agg_data5norm <- agg_data5 %>%
  ungroup() %>%
  mutate(mean_surface_normalized = normalization_factor * 100)

# Plotting with ggplot
p1 <- ggplot(agg_data5norm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface_normalized, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  labs(x = "", y = "Surf. moy. des parcelles", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal()

base_ble_filtered2 <- base_ble2 %>% filter(surf >= 20000) #########

# Calculate the percentage change in rend from 2010 to 2016 for each Departement
change_data2 <- base_ble_filtered2 %>%
  filter(Annee %in% c(2015, 2016)) %>%
  group_by(Departement) %>%
  summarize(change = (last(rend) - first(rend)) / first(rend))

# Classify departements into groups based on the percentage change
change_data2 <- change_data2 %>%
  mutate(group = case_when(
    change < -0.4 ~ "Diminution > 40%",
    change >= -0.4 & change < -0.25 ~ "Diminution de 25% à 40%",
    change >= -0.25 & change < -0.1 ~ "Diminution de 10% à 25%",
    change >= -0.10 & change < 1 ~ "Dim. de moins de 10% ou augmentation",
    TRUE ~ "Other"
  ))

# Aggregate data to calculate mean surface for each year, crop, and group
agg_data5_2 <- base_ble_filtered2 %>%
  filter(Annee >= 2010) %>% ###########
left_join(change_data2, by = "Departement") %>%
  group_by(Annee, Culture, group) %>%
  summarize(mean_surface = weighted.mean(surf),
            variance_within_group = var(surf)) ##########

# Calculate the normalization factor as the ratio of the mean surface for each year to the mean surface for the year 2016, multiplied by 100
agg_data5_2 <- agg_data5_2 %>%
  group_by(Culture, group) %>%
  mutate(normalization_factor = mean_surface / mean_surface[Annee == 2016])

# Print out normalization factor to check
print(agg_data5_2$normalization_factor)

# Normalize the mean surface values at 100 for the year 2016
agg_data5_2norm <- agg_data5_2 %>%
  ungroup() %>%
  mutate(mean_surface_normalized = normalization_factor * 100)

# Plotting with ggplot
p2 <- ggplot(agg_data5_2norm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface_normalized, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  labs(x = "", y = "Surf. moy.", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

# Normalize the mean surface values at 100 for the year 2016
agg_data5notnorm <- agg_data5 %>%
  ungroup()

# Plotting with ggplot
p3 <- ggplot(agg_data5notnorm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  #geom_ribbon(aes(ymin = mean_surface - 2 * sqrt(variance_within_group), ymax = mean_surface + 2 * sqrt(variance_within_group), fill = group), alpha = 0.2) +
  labs(x = "", y = "Surf. moy. des parcelles", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

# Normalize the mean surface values at 100 for the year 2016
agg_data5_2notnorm <- agg_data5_2 %>%
  ungroup()

# Plotting with ggplot
p4 <- ggplot(agg_data5_2notnorm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  #geom_ribbon(aes(ymin = mean_surface - 2 * sqrt(variance_within_group), ymax = mean_surface + 2 * sqrt(variance_within_group), fill = group), alpha = 0.2) +
  labs(x = "", y = "Surf. moy.", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

plot_grid(p1, p2, nrow = 2, align = "v")

#######################################################################

# Variante du code au dessus pour pouvoir notamment visualiser la variance
# Il faut décommenter quelques morceaux de code

base_ble_filtered <- base_ble %>% filter(surf >= 20000) #########

# Calculate the percentage change in rend from 2015 to 2016 for each Departement
change_data <- base_ble_filtered %>%
  filter(Annee %in% c(2015, 2016)) %>%
  group_by(Departement) %>%
  summarize(change = (last(rend) - first(rend)) / first(rend))

# Classify departements into groups based on the percentage change
change_data <- change_data %>%
  mutate(group = case_when(
    change < -0.4 ~ "Diminution > 40%",
    change >= -0.4 & change < -0.25 ~ "Diminution de 25% à 40%",
    change >= -0.25 & change < -0.1 ~ "Diminution de 10% à 25%",
    change >= -0.10 & change < 1 ~ "Dim. de moins de 10% ou augmentation",
    TRUE ~ "Other"
  ))

# Aggregate data to calculate mean surface for each year, crop, and group
agg_data5 <- base_ble_filtered %>%
  filter(Annee >= 2015) %>% ###########
  left_join(change_data, by = "Departement") %>%
  group_by(Annee, Culture, group) %>%
  summarize(mean_surface = weighted.mean(Mean_surface_parcelles),
            variance_within_group = var(Mean_surface_parcelles)) ##########

# Calculate the normalization factor as the ratio of the mean surface for each year to the mean surface for the year 2016, multiplied by 100
agg_data5 <- agg_data5 %>%
  group_by(Culture, group) %>%
  mutate(normalization_factor = mean_surface / mean_surface[Annee == 2016])

# Print out normalization factor to check
print(agg_data5$normalization_factor)

# Normalize the mean surface values at 100 for the year 2016
agg_data5norm <- agg_data5 %>%
  ungroup() %>%
  mutate(mean_surface_normalized = normalization_factor * 100)

# Plotting with ggplot
p1 <- ggplot(agg_data5norm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface_normalized, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  labs(x = "", y = "Surf. moy. des parcelles", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                    values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal()

base_ble_filtered2 <- base_ble2 %>% filter(surf >= 20000) #########

# Calculate the percentage change in rend from 2010 to 2016 for each Departement
change_data2 <- base_ble_filtered2 %>%
  filter(Annee %in% c(2015, 2016)) %>%
  group_by(Departement) %>%
  summarize(change = (last(rend) - first(rend)) / first(rend))

# Classify departements into groups based on the percentage change
change_data2 <- change_data2 %>%
  mutate(group = case_when(
    change < -0.4 ~ "Diminution > 40%",
    change >= -0.4 & change < -0.25 ~ "Diminution de 25% à 40%",
    change >= -0.25 & change < -0.1 ~ "Diminution de 10% à 25%",
    change >= -0.10 & change < 1 ~ "Dim. de moins de 10% ou augmentation",
    TRUE ~ "Other"
  ))

# Aggregate data to calculate mean surface for each year, crop, and group
agg_data5_2 <- base_ble_filtered2 %>%
  filter(Annee >= 2010) %>% ###########
left_join(change_data2, by = "Departement") %>%
  group_by(Annee, Culture, group) %>%
  summarize(mean_surface = weighted.mean(surf),
            variance_within_group = var(surf)) ##########

# Calculate the normalization factor as the ratio of the mean surface for each year to the mean surface for the year 2016, multiplied by 100
agg_data5_2 <- agg_data5_2 %>%
  group_by(Culture, group) %>%
  mutate(normalization_factor = mean_surface / mean_surface[Annee == 2016])

# Print out normalization factor to check
print(agg_data5_2$normalization_factor)

# Normalize the mean surface values at 100 for the year 2016
agg_data5_2norm <- agg_data5_2 %>%
  ungroup() %>%
  mutate(mean_surface_normalized = normalization_factor * 100)

# Plotting with ggplot
p2 <- ggplot(agg_data5_2norm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface_normalized, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  labs(x = "", y = "Surf. moy.", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

# Normalize the mean surface values at 100 for the year 2016
agg_data5notnorm <- agg_data5 %>%
  ungroup()

# Plotting with ggplot
p1 <- ggplot(agg_data5notnorm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  #geom_ribbon(aes(ymin = mean_surface - 2 * sqrt(variance_within_group), ymax = mean_surface + 2 * sqrt(variance_within_group), fill = group), alpha = 0.2) +
  labs(x = "", y = "Surf. moy. des parcelles", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

# Normalize the mean surface values at 100 for the year 2016
agg_data5_2notnorm <- agg_data5_2 %>%
  ungroup()

# Plotting with ggplot
p2 <- ggplot(agg_data5_2notnorm %>% filter(group != "Other"), aes(x = Annee, y = mean_surface, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  #geom_ribbon(aes(ymin = mean_surface - 2 * sqrt(variance_within_group), ymax = mean_surface + 2 * sqrt(variance_within_group), fill = group), alpha = 0.2) +
  labs(x = "", y = "Surf. moy.", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

# Aggregate data to calculate mean surface for each year, crop, and group
agg_data5_rend <- base_ble_filtered2 %>%
  filter(Annee >= 2010) %>% ###########
left_join(change_data2, by = "Departement") %>%
  group_by(Annee, Culture, group) %>%
  summarize(mean_surface = mean(rend, na.rm=TRUE),
            variance_within_group = var(rend)) ##########

p3 <- ggplot(agg_data5_rend %>% filter(group != "Other"), aes(x = Annee, y = mean_surface, color = group)) +
  geom_line(linetype = "solid", size = 1.5, alpha=1) +
  #geom_ribbon(aes(ymin = mean_surface - 2 * sqrt(variance_within_group), ymax = mean_surface + 2 * sqrt(variance_within_group), fill = group), alpha = 0.2) +
  labs(x = "", y = "Rend. moy.", color = "Group") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), limits = c(2010, 2022)) +
  scale_color_manual(name = "Diminution du rendement entre 2015 et 2016",
                     values = c("Diminution > 40%" = "#DF8278", 
                                "Diminution de 25% à 40%" = "#E48A00",
                                #"Decrease -10% to -20%" = "orange", 
                                "Diminution de 10% à 25%" = "#6BC064",
                                "Dim. de moins de 10% ou augmentation" = "#98B4F9"),
                     limits = c("Diminution > 40%", "Diminution de 25% à 40%", "Diminution de 10% à 25%", "Dim. de moins de 10% ou augmentation")) +
  theme_minimal() +
  theme(legend.position = "none")

plot_grid(p1, p2, p3, nrow = 3, align = "v")

###################################################################
# Affichage de la carte

# Graphique pour la Figure 7 en Annexe

# Charger les données géographiques des départements français
france <- map_data("france")

france <- replace(france, france == "Cote-Dor", "Côte-d'Or")
france <- replace(france, france == "Cotes-Darmor", "Côtes-d'Armor")
france <- replace(france, france == "Drome", "Drôme")
france <- replace(france, france == "Finistere", "Finistère")
france <- replace(france, france == "Isere", "Isère")
france <- replace(france, france == "Nievre", "Nièvre")
france <- replace(france, france == "Puy-de-Dome", "Puy-de-Dôme")
france <- replace(france, france == "Haute-Saone", "Haute-Saône")
france <- replace(france, france == "Saone-et-Loire", "Saône-et-Loire")
france <- replace(france, france == "Deux-Sevres", "Deux-Sèvres")
france <- replace(france, france == "Vendee", "Vendée")
france <- replace(france, france == "Val-Doise", "Val-d'Oise")


# Joindre les données de changement aux données géographiques des départements
map_data <- merge(france, change_data, by.x="region", by.y="nom_dep", all.x=TRUE)

map_data <- map_data[order(map_data$group.x, map_data$order), ]

# Tracer la carte de France avec les départements colorés en fonction des groupes
ggplot(map_data, aes(x=long, y=lat, group=group.x, fill=group.y)) +
  geom_polygon(color="black") +
  scale_fill_manual(values=c(
    "Diminution > 40%" = "#DF8278", 
    "Diminution de 25% à 40%" = "#E48A00",
    "Diminution de 10% à 25%" = "#6BC064",
    "Dim. de moins de 10% ou augmentation" = "#98B4F9")) +
  theme_void() +
  labs(fill = "Group") +
  ggtitle("Carte de France avec les départements colorés en fonction du groupe") +
  theme(legend.position = "bottom")

