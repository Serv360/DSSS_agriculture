library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(tibble)
library(ggdist)


data_COP <- read_excel("D:/Cours/ENSAE/Projet DSSS/Data/SAA_2010-2022_provisoires_donnees_departementales-v2.xlsx", 
                       sheet = "COP", skip=4)

# taux de croissance de la surface, de la production et du rendement 2010-2021
data_COP$Taux_Croissance_Surface <- (data_COP$SURF_2021 / data_COP$SURF_2010) - 1
data_COP$Taux_Croissance_Production <- (data_COP$PROD_2021 / data_COP$PROD_2010) - 1
data_COP$Taux_Croissance_Rendement <- (data_COP$REND_2021 / data_COP$REND_2010) - 1

tableau_double_entree <- data_COP[, c("LIB_DEP", "LIB_CODE", "Taux_Croissance_Surface", "Taux_Croissance_Production", "Taux_Croissance_Rendement")]

print(tableau_double_entree)

# Top 10 aux plus faibles taux de croissance de la prod
tableau_trie_prod <- subset(tableau_double_entree, Taux_Croissance_Production != -1)
tableau_trie_prod <- tableau_trie_prod[order(tableau_trie_prod$Taux_Croissance_Production),]

top_10_bas_prod <- head(tableau_trie_prod, 10)

print(top_10_bas_prod)

# Top 10 aux plus faibles taux de croissance de la surf
tableau_trie_surf <- subset(tableau_double_entree, Taux_Croissance_Surface != -1)
tableau_trie_surf <- tableau_trie_surf[order(tableau_trie_surf$Taux_Croissance_Surface),]

top_10_bas_surf <- head(tableau_trie_surf, 10)

print(top_10_bas_surf)


# Top 10 aux plus faibles taux de croissance du rend
tableau_trie_rend <- subset(tableau_double_entree, Taux_Croissance_Rendement != -1)
tableau_trie_rend <- tableau_trie_rend[order(tableau_trie_rend$Taux_Croissance_Rendement),]

top_10_bas_rend <- head(tableau_trie_rend, 10)

print(top_10_bas_rend)

data_trie_rend <- subset(data_COP, Taux_Croissance_Rendement != -1)
data_trie_rend <- tableau_trie_rend[order(tableau_trie_prod$Taux_Croissance_Rendement),]

top_10_selection <- head(data_trie_rend, 10)


## Graphiques
annees <- 2010:2022

##01 - Blé tendre d'hiver et épeautre
##02 - Blé tendre de printemps
##04 - Blé dur d'hiver
##05 - Blé dur de printemps
donnees_ble1 <- data_COP[data_COP$LIB_CODE %in% c("01 - Blé tendre d'hiver et épeautre"),]
donnees_ble2 <- data_COP[data_COP$LIB_CODE %in% c("02 - Blé tendre de printemps"),]
donnees_ble3 <- data_COP[data_COP$LIB_CODE %in% c("04 - Blé dur d'hiver"),]
donnees_ble4 <- data_COP[data_COP$LIB_CODE %in% c("05 - Blé dur de printemps"),]




# Production

production_ble1 <- donnees_ble1[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]
production_ble2 <- donnees_ble2[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]
production_ble3 <- donnees_ble3[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]
production_ble4 <- donnees_ble4[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]
production_ble1 <- na.omit(production_ble1)
production_ble2 <- na.omit(production_ble2)
production_ble3 <- na.omit(production_ble3)
production_ble4 <- na.omit(production_ble4)
sum_productionble1 <- colSums(production_ble1)
sum_productionble2 <- colSums(production_ble2)
sum_productionble3 <- colSums(production_ble3)
sum_productionble4 <- colSums(production_ble4)

ble_prod <- data.frame(Années = annees, Production_ble_tendre_hiver= sum_productionble1, Production_ble_tendre_printemps = sum_productionble2, Production_ble_dur_hiver = sum_productionble3, Production_ble_dur_printemps = sum_productionble4)
ble_prod <- as.data.frame(scale(ble_prod))
class(ble_prod)
## valeurs normalisées
ggplot(ble_prod, aes(x = Années)) +
  geom_line(aes(y = Production_ble_tendre_hiver, color = "BTH"), size = 1) +
  geom_line(aes(y = Production_ble_tendre_printemps, color = "BTP"), size = 1) +
  geom_line(aes(y = Production_ble_dur_hiver, color = "BDH"), size = 1) +
  geom_line(aes(y = Production_ble_dur_printemps, color = "BDP"), size = 1) +
  scale_color_manual(values = c("BTH" = "blue", "BTP" = "red", "BDH" = "green", "BDP" = "yellow"),
                     labels = c("BTH", "BTP", "BDH", "BDP")) +
  labs(y = "Production",
       color = "Variables") +
  theme(legend.position = "top") 

# Rendement
rendement_ble1 <- donnees_ble1[, c("REND_2010", "REND_2011", "REND_2012", "REND_2013", "REND_2014", "REND_2015", "REND_2016", "REND_2017", "REND_2018", "REND_2019", "REND_2020", "REND_2021", "REND_2022")]
rendement_ble2 <- donnees_ble2[, c("REND_2010", "REND_2011", "REND_2012", "REND_2013", "REND_2014", "REND_2015", "REND_2016", "REND_2017", "REND_2018", "REND_2019", "REND_2020", "REND_2021", "REND_2022")]
rendement_ble3 <- donnees_ble3[, c("REND_2010", "REND_2011", "REND_2012", "REND_2013", "REND_2014", "REND_2015", "REND_2016", "REND_2017", "REND_2018", "REND_2019", "REND_2020", "REND_2021", "REND_2022")]
rendement_ble4 <- donnees_ble4[, c("REND_2010", "REND_2011", "REND_2012", "REND_2013", "REND_2014", "REND_2015", "REND_2016", "REND_2017", "REND_2018", "REND_2019", "REND_2020", "REND_2021", "REND_2022")]
rendement_ble1 <- na.omit(rendement_ble1)
rendement_ble2 <- na.omit(rendement_ble2)
rendement_ble3 <- na.omit(rendement_ble3)
rendement_ble4 <- na.omit(rendement_ble4)
sum_rendementble1 <- colSums(rendement_ble1)
sum_rendementble2 <- colSums(rendement_ble2)
sum_rendementble3 <- colSums(rendement_ble3)
sum_rendementble4 <- colSums(rendement_ble4)

ble_rend <- data.frame(Années = annees, Rendement_ble_tendre_hiver= sum_rendementble1, Rendement_ble_tendre_printemps = sum_rendementble2, Rendement_ble_dur_hiver = sum_rendementble3, Rendement_ble_dur_printemps = sum_rendementble4)
ble_rend <- as.data.frame(scale(ble_rend))
class(ble_prod)
## valeurs normalisées
ggplot(ble_rend, aes(x = Années)) +
  geom_line(aes(y = Rendement_ble_tendre_hiver, color = "BTH"), size = 1) +
  geom_line(aes(y = Rendement_ble_tendre_printemps, color = "BTP"), size = 1) +
  geom_line(aes(y = Rendement_ble_dur_hiver, color = "BDH"), size = 1) +
  geom_line(aes(y = Rendement_ble_dur_printemps, color = "BDP"), size = 1) +
  scale_color_manual(values = c("BTH" = "blue", "BTP" = "red", "BDH" = "green", "BDP" = "yellow"),
                     labels = c("BTH", "BTP", "BDH", "BDP")) +
  labs(y = "Rendement",
       color = "Variables") +
  theme(legend.position = "top")


# Surface
donnees_ble1 <- data_COP[data_COP$LIB_CODE %in% c("01 - Blé tendre d'hiver et épeautre"),]
surface_ble1 <- donnees_ble1[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]
donnees_ble2 <- data_COP[data_COP$LIB_CODE %in% c("02 - Blé tendre de printemps"),]
surface_ble2 <- donnees_ble2[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]
donnees_ble3 <- data_COP[data_COP$LIB_CODE %in% c("04 - Blé dur d'hiver"),]
surface_ble3 <- donnees_ble3[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]
donnees_ble4 <- data_COP[data_COP$LIB_CODE %in% c("05 - Blé dur de printemps"),]
surface_ble4 <- donnees_ble4[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]
surface_ble1 <- na.omit(surface_ble1)
surface_ble2 <- na.omit(surface_ble2)
surface_ble3 <- na.omit(surface_ble3)
surface_ble4 <- na.omit(surface_ble4)
sum_surface_ble1 <- colSums(surface_ble1)
sum_surface_ble2 <- colSums(surface_ble2)
sum_surface_ble3 <- colSums(surface_ble3)
sum_surface_ble4 <- colSums(surface_ble4)

ble_surf <- data.frame(Années = annees, Surface_ble_tendre_hiver= sum_surface_ble1, Surface_ble_tendre_printemps = sum_surface_ble2, Surface_ble_dur_hiver = sum_surface_ble3, Surface_ble_dur_printemps = sum_surface_ble4)
ble_surf <- as.data.frame(scale(ble_surf))
class(ble_surf)
## valeurs normalisées
ggplot(ble_surf, aes(x = Années)) +
  geom_line(aes(y = Surface_ble_tendre_hiver, color = "BTH"), size = 1) +
  geom_line(aes(y = Surface_ble_tendre_printemps, color = "BTP"), size = 1) +
  geom_line(aes(y = Surface_ble_dur_hiver, color = "BDH"), size = 1) +
  geom_line(aes(y = Surface_ble_dur_printemps, color = "BDP"), size = 1) +
  scale_color_manual(values = c("BTH" = "blue", "BTP" = "red", "BDH" = "green", "BDP" = "yellow"),
                     labels = c("BTH", "BTP", "BDH", "BDP")) +
  labs(y = "Surface",
       color = "Variables") +
  theme(legend.position = "top")



####################################
### Choix des céréales à étudier ###
####################################




donnees_par_culture <- data_COP[,c("LIB_CODE","PROD_2021","SURF_2021")] %>%
  na.omit() %>% 
  group_by(LIB_CODE) %>%
  summarise(PROD_2021 = sum(PROD_2021),
            SURF_2021 = sum(SURF_2021)) %>% 
  arrange(PROD_2021)


## Sélection de 01 - Blé tendre d'hiver et épeautre
## 15 - Maïs grain non irrigué
## 14 - Maïs grain irrigué

donnees_ble1 <- data_COP[data_COP$LIB_CODE %in% c("01 - Blé tendre d'hiver et épeautre"),]
donnees_mais14 <- data_COP[data_COP$LIB_CODE %in% c("14 - Maïs grain irrigué"),]
donnees_mais15 <- data_COP[data_COP$LIB_CODE %in% c("15 - Maïs grain non irrigué"),]

production_ble1 <- colSums(na.omit(donnees_ble1[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]))
production_mais14 <- colSums(na.omit(donnees_mais14[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]))
production_mais15 <- colSums(na.omit(donnees_mais15[, c("PROD_2010", "PROD_2011", "PROD_2012", "PROD_2013", "PROD_2014", "PROD_2015", "PROD_2016", "PROD_2017", "PROD_2018", "PROD_2019", "PROD_2020", "PROD_2021", "PROD_2022")]))

production <- data.frame(Années = annees, Production_ble_tendre_hiver= production_ble1, Production_mais_grain_irrigue = production_mais14, Production_mais_grain_non_irrigue = production_mais15)

ggplot(production, aes(x = Années)) +
  geom_line(aes(y = Production_ble_tendre_hiver, color = "BTH"), size = 1) +
  geom_line(aes(y = Production_mais_grain_irrigue, color = "MGI"), size = 1) +
  geom_line(aes(y = Production_mais_grain_non_irrigue, color = "MGNI"), size = 1) +
  scale_color_manual(values = c("BTH" = "blue", "MGI" = "red", "MGNI" = "green"),
                     labels = c("Blé TH", "Mais grain irrigué", "Mais grain non irrigué")) +
  labs(y = "Production",
       color = "Variables") +
  theme(legend.position = "top") 

surface_ble1 <- colSums(na.omit(donnees_ble1[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]))
surface_mais14 <- colSums(na.omit(donnees_mais14[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]))
surface_mais15 <- colSums(na.omit(donnees_mais15[, c("SURF_2010", "SURF_2011", "SURF_2012", "SURF_2013", "SURF_2014", "SURF_2015", "SURF_2016", "SURF_2017", "SURF_2018", "SURF_2019", "SURF_2020", "SURF_2021", "SURF_2022")]))

surface <- data.frame(Années = annees, Surface_ble_tendre_hiver= surface_ble1, Surface_mais_grain_irrigue = surface_mais14, Surface_mais_grain_non_irrigue = surface_mais15)

ggplot(surface, aes(x = Années)) +
  geom_line(aes(y = Surface_ble_tendre_hiver, color = "BTH"), size = 1) +
  geom_line(aes(y = Surface_mais_grain_irrigue, color = "MGI"), size = 1) +
  geom_line(aes(y = Surface_mais_grain_non_irrigue, color = "MGNI"), size = 1) +
  scale_color_manual(values = c("BTH" = "blue", "MGI" = "red", "MGNI" = "green"),
                     labels = c("Blé TH", "Mais grain irrigué", "Mais grain non irrigué")) +
  labs(y = "Surface",
       color = "Variables") +
  theme(legend.position = "top") 
