# DSSS_agriculture

Notre étude exploite les variations météorologiques récentes en France métropolitaine pour identifier l'impact du changement climatique sur les cultures de blé tendre d'hiver et d'épeautre. Nous utilisons des forêts aléatoires pour modéliser les effets non linéaires des variables météorologiques sur les rendements des cultures. Notre analyse révèle que des mécanismes d'adaptation à long terme des cultures atténuent l'impact nocif du changement climatique sur les rendements. Nous mettons en évidence l'un de ces potentiels mécanismes en étudiant l'évolution des surfaces cultivées et de la taille des parcelles. L'identification et la quantification de ces mécanismes d'adaptation est au coeur des enjeux alimentaires et économiques de la transition écologique. 

Le projet est organisé ainsi en quatre dossiers.

1. Construction base
  - Code pour réaliser la jointure des trois bases de données disponibles sur internet grâce aux liens indiqués dans le fichier txt.
  - Deux .Rmd pour faire la jointure 2010 - 2022 simple ou 2015 - 2022 avec moyenne/médiane/variance surfaces parcelles.
2. Analyse
  - Deux dossiers avec les données propres obtenues grâce aux codes précédents : "data" (2010 - 2022) et "data_adaptation" (2015 - 2022).
  - Des scripts (setpwd à changer) pour :
    - les statistiques descriptives ("statistiques descriptives.Rmd") : permet d'obtenir la Figure 1.
    - la modélisation ("rf Blé.R" et "rf Mais.R") : permet d'obtenir les Figures 3 et 4 et les tables.
    - étudier l'adaptation, principalement en terme de surface et de surfaces de parcelles (les trois autres fichiers) : permet d'obtenir les Figures 5 et 7 et d'autres Figures dans la vidéo.
3. Plots
  - Les images présentes dans le rendu et dans la vidéo.
4. Rendu
  - Le pdf rendu, le diapo de la vidéo et le code LateX du rendu.
