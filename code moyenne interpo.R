#For Etat major
# Chargement des bibliothèques nécessaires
library(sf)
library(dplyr)
centersCfr <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur.gpkg")


output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ETM/SNR Rescale/Profile_etm_50m.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ETM/Old way rescale/etm_50m_resc.csv')

output_fr_2 <-output_fr_2[,-2]


# Création d'une fonction pour l'interpolation et le calcul de moyenne
interpolate_and_mean <- function(group_df, desired_distance) {
  # Interpolation linéaire
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calcul de la moyenne pour chaque distance interpolée
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}


#Test de boucle sur toute les villes
# On récupère la population maximale
popmax <- max(output_fr_3$Pop_EtatMajor)

# Interpolation pour toutes les villes
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# On crée le dataframe de résultats avec juste les distances désirées
interpolated_LU <- data.frame(desired_distance = desired_distance)

# Boucle sur chaque colonne de la table output_fr_2 sauf la colonne des distances recalculées
for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  
  # Récupérer la population EM
  pop=centersCfr[centersCfr$city==city,]$Pop_EtatMajor
  
  # Distances après changement d'échelle
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.33
  
  # Effectuer l'interpolation pour la ville actuelle
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Ajouter la colonne avec les valeurs interpolées au dataframe
  interpolated_LU[[city]] <- interpolated_value
  
}

# Afficher le dataframe des valeurs interpolées pour chaque ville
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/profils_interpolé0.33.csv")

interpolated_LU <- read.csv("C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/ETM/profils_interpolé_ETM.csv")

library(reshape2)

centersCfr$city

centersCfr <- arrange(centersCfr, city)




# Moyenne de villes
# Séparation des données en groupes
Groupe1etm <- centersCfr%>% filter(centersCfr$Pop_EtatMajor > 200000)
Groupe2etm <- centersCfr%>% filter(Pop_EtatMajor < 200000 & Pop_EtatMajor > 100000)
Groupe3etm <- centersCfr%>% filter(Pop_EtatMajor < 100000 & Pop_EtatMajor > 50000)
Groupe4etm <- centersCfr%>% filter(Pop_EtatMajor < 50000 & Pop_EtatMajor > 25000)
Groupe5etm <- centersCfr%>% filter(Pop_EtatMajor < 25000)


G1etm= Groupe1etm$city
G2etm= Groupe2etm$city
G3etm= Groupe3etm$city
G4etm= Groupe4etm$city
G5etm= Groupe5etm$city



# Sélectionner les colonnes communes entre interpolated_LU et G1Cass
cols_to_select <- intersect(names(interpolated_LU), G1etm)
# Sélectionner les colonnes dans 
G1 <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

----
  cols_to_select <- intersect(names(interpolated_LU), G2etm)

G2 <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
----
  cols_to_select <- intersect(names(interpolated_LU), G3etm)

G3 <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
-----
  cols_to_select <- intersect(names(interpolated_LU), G4etm)
G4 <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
------
  cols_to_select <- intersect(names(interpolated_LU), G5etm)
G5 <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

write.csv(G5, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/ETM/g5_etm.csv")


#Affichage / plot


# afficher les courbes ETM interpollé


# Utilisez matplot pour tracer les courbes
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0, 50), ylim = c(min(interpolated_LU[, -1], na.rm = TRUE), 1),
        main = "Interpolation des valeurs pour différentes villes")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol = 2)

plot(interpolated_LU$desired_distance,interpolated_LU$Paris)

# afficher les courbes ETM non-interpollées
colors <- rainbow(ncol(output_fr_2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs ",
        xlim = c(0,50),
        main = "Profils différentes villes")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher la groupes de villes
#g1
colors <- rainbow(ncol(G1) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1$X, G1[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,20), ylim=c(0,1),
        main = "Interpolation des valeurs le groupe 1")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#afficher la moyennes de villes


#g2
colors <- rainbow(ncol(G2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G2$X, G2[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs le groupe 2")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G2)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g3
colors <- rainbow(ncol(G3) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G3$X, G3[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs le groupe 3")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G3)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g4
colors <- rainbow(ncol(G4) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G4$X, G4[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "Interpolation des valeurs le groupe 4")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G4)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g5
colors <- rainbow(ncol(G5) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5$X, G5[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "Interpolation des valeurs le groupe 5 ")

# Ajoutez une légende pour identifier chaque courbe
#legend("topright", legend = colnames(G5)[-1], col = colors, lty = 1, cex = 0.75, bty = 'n', ncol = 3)
legend("topright", legend = colnames(G5)[-1], col = colors, lty = 1, cex = 0.75, bty = 'n', ncol = 3, x.intersp = 0.001, adj = 0.005)


#afficher la moyennes de villes 
G1_etm_mean <- G1 %>%
  mutate(Moyenne = rowMeans(select(G1, -X), na.rm = TRUE))

G1_etm_mean <- G1_etm_mean[,-c(2:2)]
#G2
G2_etm_mean <- G2 %>%
  mutate(Moyenne = rowMeans(select(G2, -X), na.rm = TRUE))

G2_etm_mean <- G2_etm_mean[,-c(2:5)]
#G3
G3_etm_mean <- G3 %>%
  mutate(Moyenne = rowMeans(select(G3, -X), na.rm = TRUE))

G3_etm_mean <- G3_etm_mean[,-c(2:11)]

#G4
G4_etm_mean <- G4 %>%
  mutate(Moyenne = rowMeans(select(G4, -X), na.rm = TRUE))

G4_etm_mean <- G4_etm_mean[,-c(2:21)]

#G5
G5_etm_mean <- G5 %>%
  mutate(Moyenne = rowMeans(select(G5, -X), na.rm = TRUE))

G5_etm_mean <- G5_etm_mean[,-c(2:41)]

write.csv(G5_etm_mean, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/ETM/G5_etm_mean.csv")
#ploter les moyennes etm
colors <- rainbow(ncol(G5_etm_mean) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5_etm_mean$X, G5_etm_mean[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "moyenne des valeurs le groupe 5")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5_etm_mean)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)


# Tracer la première série avec la fonction plot
plot(G1_etm_mean$X, G1_etm_mean$Moyenne, type = 'l', col = 'red', xlab = 'X', ylab = 'Moyenne',log="y")

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G2_etm_mean$Moyenne, col = 'blue')
lines(G3_etm_mean$Moyenne, col = 'green')
lines(G4_etm_mean$Moyenne, col = 'purple')
lines(G5_etm_mean$Moyenne, col = 'orange')

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = c("red", "blue", "green", "purple", "orange"), lty = 1, cex = 0.8)



# Définir une palette de couleurs automatique
colors <- rainbow(5)

# Tracer la première série avec la fonction plot
plot(G1_etm_mean$X, G1_etm_mean$Moyenne, type = 'l', col = colors[1], xlab = 'X', ylab = 'Moyenne', main='ETM0.33', xlim=c(0,10))

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G1_etm_mean$X, G2_etm_mean$Moyenne, col = colors[2])
lines(G1_etm_mean$X, G3_etm_mean$Moyenne, col = colors[3])
lines(G1_etm_mean$X, G4_etm_mean$Moyenne, col = colors[4])
lines(G1_etm_mean$X, G5_etm_mean$Moyenne, col = colors[5])

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = colors, lty = 1, cex = 0.8)
# ----------------------------------
#For Cassini
library(dplyr)

output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/Cassini/SNR Rescale/Profile_L_74_cass50m.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/Cassini/Old way rescale/cass_new_50m_resc.csv')

output_fr_2 <-output_fr_2[,-2]

plot(output_fr_2$X/2, output_fr_2$Paris, xlim=c(0,5))
centersCfr <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur_Cass.gpkg")
centersCfr2 <-centersCfr%>%
  filter(!city %in% c('ANNEMASSE', 'BOULOGNE_SUR_MER', 'CREIL', 'FREJUS', 'HENIN_CARVIN', 'Nice', 'Ajaccio', 'ANNECY', 'CHAMBERY'))

# Création d'une fonction pour l'interpolation et le calcul de moyenne
interpolate_and_mean <- function(group_df, desired_distance) {
  # Interpolation linéaire
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calcul de la moyenne pour chaque distance interpolée
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}



#Test de boucle sur toute les villes
# On récupère la population maximale
popmax <- max(output_fr_3$PopCassini)


# Interpolation pour toutes les villes
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# On crée le dataframe de résultats avec juste les distances désirées
interpolated_LU <- data.frame(desired_distance = desired_distance)

# Boucle sur chaque colonne de la table output_fr_2 sauf la colonne des distances recalculées


for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  
  # Récupérer la population EM
  pop=centersCfr[centersCfr$city==city,]$PopCassini
  
  # Distances après changement d'échelle
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.33
  
  # Effectuer l'interpolation pour la ville actuelle
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Ajouter la colonne avec les valeurs interpolées au dataframe
  interpolated_LU[[city]] <- interpolated_value
  
}


# Afficher le dataframe des valeurs interpolées pour chaque ville
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/Cass/profils_interpolé_Cass0.33.csv")


interpolated_LU<- read.csv("C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/Cass/profils_interpolé_Cass.csv")

# Moyenne de villes
# Séparation des données en groupes
Groupe1cass <- centersCfr%>% filter(PopCassini > 120000)
Groupe2cass <- centersCfr%>% filter(PopCassini < 120000 & PopCassini > 60000)
Groupe3cass <- centersCfr%>% filter(PopCassini < 60000 & PopCassini > 30000)
Groupe4cass <- centersCfr%>% filter(PopCassini < 30000 & PopCassini > 15000)
Groupe5cass <- centersCfr%>% filter(PopCassini < 15000)

G1cass= Groupe1cass$city
G2cass= Groupe2cass$city
G3cass= Groupe3cass$city
G4cass= Groupe4cass$city
G5cass= Groupe5cass$city


# Sélectionner les colonnes communes entre output_fr_2 et G1Cass
cols_to_select <- intersect(names(interpolated_LU), G1cass)
# Sélectionner les colonnes dans interpolated_LU
G1c <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

----
cols_to_select <- intersect(names(interpolated_LU), G2cass)

G2c <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
----
cols_to_select <- intersect(names(interpolated_LU), G3cass)

G3c <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
-----
cols_to_select <- intersect(names(interpolated_LU), G4cass)
G4c <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
------
cols_to_select <- intersect(names(interpolated_LU), G5cass)
G5c <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

write.csv(G5c, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/Cass/g5c.csv")
#Affichage / plot


# Courbes interpolés pour Cassini
colors <- rainbow(ncol(interpolated_LU) - 1)
# Utilisez matplot pour tracer les courbes
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50), ylim = c(min(interpolated_LU[, -1], na.rm = TRUE), 1),
        main = "Interpolation des valeurs pour différentes villes_Cassini")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher les Courbes sans interpol Cassini
colors <- rainbow(ncol(output_fr_2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlim = c(0,60), ylim = c(min(interpolated_LU[, -1], na.rm = TRUE), 1),
        xlab = "Distance (desired_distance)", ylab = "Valeurs ",
        main = "Profils différentes villes_Cassini")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher la groupes de villes
#g1
colors <- rainbow(ncol(G1c) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1c$X, G1c[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 1 ")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1c)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#afficher la moyennes de villes


#g2
colors <- rainbow(ncol(G2c) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G2c$X, G2c[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs le groupe 2 ")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G2c)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g3
colors <- rainbow(ncol(G3c) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G3c$X, G3c[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs le groupe 3")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G3c)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g4
colors <- rainbow(ncol(G4c) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G4c$X, G4c[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "Interpolation des valeurs le groupe 4")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G4c)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g5
colors <- rainbow(ncol(G5c) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5c$X, G5c[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "Interpolation des valeurs le groupe 5")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5c)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#afficher la moyennes de villes 
G1_cass_mean <- G1c %>%
  mutate(Moyenne = rowMeans(select(G1c, -X), na.rm = TRUE))

G1_cass_mean <- G1_cass_mean[,-c(2:2)]
#G2
G2_cass_mean <- G2c %>%
  mutate(Moyenne = rowMeans(select(G2c, -X), na.rm = TRUE))

G2_cass_mean <- G2_cass_mean[,-c(2:7)]
#G3
G3_cass_mean <- G3c %>%
  mutate(Moyenne = rowMeans(select(G3c, -X), na.rm = TRUE))

G3_cass_mean <- G3_cass_mean[,-c(2:9)]

#G4
G4_cass_mean <- G4c %>%
  mutate(Moyenne = rowMeans(select(G4c, -X), na.rm = TRUE))

G4_cass_mean <- G4_cass_mean[,-c(2:22)]

#G5
G5_cass_mean <- G5c %>%
  mutate(Moyenne = rowMeans(select(G5c, -X), na.rm = TRUE))

G5_cass_mean <- G5_cass_mean[,-c(2:32)]

write.csv(G5_cass_mean, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/Cass/G5_cass_mean.csv")

#ploter les moyennes cass
colors <- rainbow(ncol(G5_cass_mean) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5_cass_mean$X, G5_cass_mean[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,40),
        main = "moyenne des valeurs le groupe 5 Cass")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5_cass_mean)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#

# Définir une palette de couleurs automatique
colors <- rainbow(5)

# Tracer la première série avec la fonction plot
plot(G1_cass_mean$X, G1_cass_mean$Moyenne, type = 'l', col = colors[1], xlab = 'X', ylab = 'Moyenne', main='Cassini', xlim=c(0,10),ylim=c(0.001,1))

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G1_cass_mean$X, G2_cass_mean$Moyenne, col = colors[2])
lines(G1_cass_mean$X, G3_cass_mean$Moyenne, col = colors[3])
lines(G1_cass_mean$X, G4_cass_mean$Moyenne, col = colors[4])
lines(G1_cass_mean$X, G5_cass_mean$Moyenne, col = colors[5])

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = colors, lty = 1, cex = 0.8)



# ----------------------------------
#For sc
library(dplyr)

output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/sc/V3/SNR Rescale/Profile_82_sc.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/sc/V3/Oldway_rescale/last_SC_resc.csv')

output_fr_2 <-output_fr_2[,-2]

centersCfr2 <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur_Cass.gpkg")

# Création d'une fonction pour l'interpolation et le calcul de moyenne
interpolate_and_mean <- function(group_df, desired_distance) {
  # Interpolation linéaire
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calcul de la moyenne pour chaque distance interpolée
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}



#Test de boucle sur toute les villes
# On récupère la population maximale
popmax <- max(output_fr_3$PopScanHisto)
popmax <- 2850189


# Interpolation pour toutes les villes
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# On crée le dataframe de résultats avec juste les distances désirées
interpolated_LU <- data.frame(desired_distance = desired_distance)
# Boucle sur chaque colonne de la table output_fr_2 sauf la colonne des distances recalculées
for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  
  # Récupérer la population EM
  pop=centersCfr[centersCfr$city==city,]$PopScanHisto
  
  # Distances après changement d'échelle
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.5
  
  # Effectuer l'interpolation pour la ville actuelle
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Ajouter la colonne avec les valeurs interpolées au dataframe
  interpolated_LU[[city]] <- interpolated_value
  
}

# Afficher le dataframe des valeurs interpolées pour chaque ville
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/profils_interpolé_Sc_0.5.csv")

interpolated_LU<- read.csv("C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/profils_interpolé_Sc_0.5.csv")



# Moyenne de villes
# Séparation des données en groupes
Groupe1sc <- centersCfr%>% filter(PopScanHisto > 662000)
Groupe2sc <- centersCfr%>% filter(PopScanHisto < 662000 & PopScanHisto > 331000)
Groupe3sc <- centersCfr%>% filter(PopScanHisto < 331000 & PopScanHisto > 165500)
Groupe4sc <- centersCfr%>% filter(PopScanHisto < 165500 & PopScanHisto > 82750)
Groupe5sc <- centersCfr%>% filter(PopScanHisto < 82500 & city != "MARTIGUES" & city != "FREJUS")

G1sc= Groupe1sc$city
G2sc= Groupe2sc$city
G3sc= Groupe3sc$city
G4sc= Groupe4sc$city
G5sc= Groupe5sc$city



# Sélectionner les colonnes communes entre output_fr_2 et G1sc
cols_to_select <- intersect(names(interpolated_LU), G1sc)
# Sélectionner les colonnes dans output_fr_2
G1s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

----
  cols_to_select <- intersect(names(interpolated_LU), G2sc)

G2s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
----
  cols_to_select <- intersect(names(interpolated_LU), G3sc)

G3s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
-----
  cols_to_select <- intersect(names(interpolated_LU), G4sc)
G4s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
------
  cols_to_select <- intersect(names(interpolated_LU), G5sc)
G5s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

write.csv(G1s, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/g1s.csv")
#Affichage / plot


# Courbes interpolés pour sc
colors <- rainbow(ncol(interpolated_LU) - 1)
# Utilisez matplot pour tracer les courbes
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs pour différentes villes_ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher les Courbes sans interpol 
colors <- rainbow(ncol(output_fr_2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlim = c(0,250),
        xlab = "Distance (desired_distance)", ylab = "Valeurs ",
        main = "Profils différentes villes_ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher la groupes de villes
#g1
colors <- rainbow(ncol(G1s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1s$X, G1s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 1 ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#afficher la moyennes de villes


#g2
colors <- rainbow(ncol(G2s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G2s$X, G2s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 2 Pop_sc")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G2s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g3
colors <- rainbow(ncol(G3s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G3s$X, G3s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 3 pop_sc ")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G3s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g4
colors <- rainbow(ncol(G4s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G4s$X, G4s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 4 Pop_sc")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G4s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g5
colors <- rainbow(ncol(G5s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5s$X, G5s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 5 ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#afficher la moyennes de villes 
G1_sc_mean <- G1s %>%
  mutate(Moyenne = rowMeans(select(G1s, -X), na.rm = TRUE))

G1_sc_mean <- G1_sc_mean[,-c(2:2)]
#G2
G2_sc_mean <- G2s %>%
  mutate(Moyenne = rowMeans(select(G2s, -X), na.rm = TRUE))

G2_sc_mean <- G2_sc_mean[,-c(2:3)]
#G3
G3_sc_mean <- G3s %>%
  mutate(Moyenne = rowMeans(select(G3s, -X), na.rm = TRUE))

G3_sc_mean <- G3_sc_mean[,-c(2:8)]

#G4
G4_sc_mean <- G4s %>%
  mutate(Moyenne = rowMeans(select(G4s, -X), na.rm = TRUE))

G4_sc_mean <- G4_sc_mean[,-c(2:20)]

#G5
G5_sc_mean <- G5s %>%
  mutate(Moyenne = rowMeans(select(G5s, -X), na.rm = TRUE))

G5_sc_mean <- G5_sc_mean[,-c(2:52)]

write.csv(G5_sc_mean, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/G5_sc_mean.csv")

#ploter les moyennes sc
colors <- rainbow(ncol(G1_sc_mean) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1_sc_mean$X, G1_sc_mean[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "moyenne des valeurs le groupe 1 sc")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1_sc_mean)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#
colors <- rainbow(5)

# Tracer la première série avec la fonction plot
plot(G1_sc_mean$X, G1_sc_mean$Moyenne, type = 'l', col = colors[1], xlab = 'X', ylab = 'Moyenne', main='sc0.5', xlim=c(0,70))

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G1_sc_mean$X, G2_sc_mean$Moyenne, col = colors[2])
lines(G1_sc_mean$X, G3_sc_mean$Moyenne, col = colors[3])
lines(G1_sc_mean$X, G4_sc_mean$Moyenne, col = colors[4])
lines(G1_sc_mean$X, G5_sc_mean$Moyenne, col = colors[5])

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = colors, lty = 1, cex = 0.8)

# ----------------------------------
#For GHSL
library(dplyr)

output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ghsl/SNR rescale/Profile_ghsl.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ghsl/Old way rescale/ghsl50m_resc.csv')

output_fr_2 <-output_fr_2[,-2]

centersCfr2 <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur.gpkg")

# Création d'une fonction pour l'interpolation et le calcul de moyenne
interpolate_and_mean <- function(group_df, desired_distance) {
  # Interpolation linéaire
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calcul de la moyenne pour chaque distance interpolée
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}



#Test de boucle sur toute les villes
# On récupère la population maximale
popmax <- max(output_fr_3$PopGHSL2018)
popmax <- 2850189


# Interpolation pour toutes les villes
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# On crée le dataframe de résultats avec juste les distances désirées
interpolated_LU <- data.frame(desired_distance = desired_distance)
# Boucle sur chaque colonne de la table output_fr_2 sauf la colonne des distances recalculées
for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  
  # Récupérer la population EM
  pop=centersCfr[centersCfr$city==city,]$PopGHSL2018
  
  # Distances après changement d'échelle
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.5
  
  # Effectuer l'interpolation pour la ville actuelle
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Ajouter la colonne avec les valeurs interpolées au dataframe
  interpolated_LU[[city]] <- interpolated_value
  
}

# Afficher le dataframe des valeurs interpolées pour chaque ville
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/GHSL/profils_interpolé_GHSL0.5.csv")

interpolated_LU<- read.csv("C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/GHSL/profils_interpolé_GHSL0.5.csv")



# Moyenne de villes
# Séparation des données en groupes
Groupe1g <- centersCfr%>% filter(PopGHSL2018 > 2030000)
Groupe2g <- centersCfr%>% filter(PopGHSL2018 < 2030000 & PopGHSL2018 > 1015000)
Groupe3g <- centersCfr%>% filter(PopGHSL2018 < 1015000 & PopGHSL2018 > 500750)
Groupe4g <- centersCfr%>% filter(PopGHSL2018 < 500750 & PopGHSL2018 > 250375)
Groupe5g <- centersCfr%>% filter(PopGHSL2018 < 250375)


G1g= Groupe1g$city
G2g= Groupe2g$city
G3g= Groupe3g$city
G4g= Groupe4g$city
G5g= Groupe5g$city


# Sélectionner les colonnes communes entre output_fr_2 et G1g
cols_to_select <- intersect(names(interpolated_LU), G1g)
# Sélectionner les colonnes dans output_fr_2
G1gh <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

----
  cols_to_select <- intersect(names(interpolated_LU), G2g)

G2gh <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
----
  cols_to_select <- intersect(names(interpolated_LU), G3g)

G3gh <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
-----
  cols_to_select <- intersect(names(interpolated_LU), G4g)
G4gh <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
------
  cols_to_select <- intersect(names(interpolated_LU), G5g)
G5gh <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

write.csv(G5gh, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/GHSL/g5gh.csv")
#Affichage / plot


# Courbes interpolés pour sc
colors <- rainbow(ncol(interpolated_LU) - 1)
# Utilisez matplot pour tracer les courbes
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs pour différentes villes_GHSL")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher les Courbes sans interpol 
colors <- rainbow(ncol(output_fr_2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlim = c(0,250),
        xlab = "Distance (desired_distance)", ylab = "Valeurs ",
        main = "Profils différentes villes_GHSL")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher la groupes de villes
#g1
colors <- rainbow(ncol(G1gh) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1gh$X, G1gh[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 1 ghsl")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1gh)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#afficher la moyennes de villes


#g2
colors <- rainbow(ncol(G2gh) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G2gh$X, G2gh[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 2 ghsl")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G2gh)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g3
colors <- rainbow(ncol(G3gh) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G3gh$X, G3gh[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 3 ghsl")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G3gh)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g4
colors <- rainbow(ncol(G4gh) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G4gh$X, G4gh[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 4 ghsl ")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G4gh)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g5
colors <- rainbow(ncol(G5gh) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5gh$X, G5gh[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "Interpolation des valeurs le groupe 5ghsl")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5gh)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#afficher la moyennes de villes 
G1_gh_mean <- G1gh %>%
  mutate(Moyenne = rowMeans(select(G1gh, -X), na.rm = TRUE))

G1_gh_mean <- G1_gh_mean[,-c(2:2)]
#G2
G2_gh_mean <- G2gh %>%
  mutate(Moyenne = rowMeans(select(G2gh, -X), na.rm = TRUE))

G2_gh_mean <- G2_gh_mean[,-c(2:6)]
#G3
G3_gh_mean <- G3gh %>%
  mutate(Moyenne = rowMeans(select(G3gh, -X), na.rm = TRUE))

G3_gh_mean <- G3_gh_mean[,-c(2:10)]

#G4
G4_gh_mean <- G4gh %>%
  mutate(Moyenne = rowMeans(select(G4gh, -X), na.rm = TRUE))

G4_gh_mean <- G4_gh_mean[,-c(2:26)]

#G5
G5_gh_mean <- G5gh %>%
  mutate(Moyenne = rowMeans(select(G5gh, -X), na.rm = TRUE))

G5_gh_mean <- G5_gh_mean[,-c(2:42)]

write.csv(G1_gh_mean, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/GHSL/G1_gh_mean.csv")

#ploter les moyennes gh
colors <- rainbow(ncol(G5_gh_mean) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5_gh_mean$X, G5_gh_mean[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,100),
        main = "moyenne des valeurs le groupe 5 gh")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5_gh_mean)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#
colors <- rainbow(5)

# Tracer la première série avec la fonction plot
plot(G1_gh_mean$X, G1_gh_mean$Moyenne, type = 'l', col = colors[1], xlab = 'X', ylab = 'Moyenne', main='gh0.5', xlim=c(0,100))

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G1_gh_mean$X, G2_gh_mean$Moyenne, col = colors[2])
lines(G1_gh_mean$X, G3_gh_mean$Moyenne, col = colors[3])
lines(G1_gh_mean$X, G4_gh_mean$Moyenne, col = colors[4])
lines(G1_sc_mean$X, G5_gh_mean$Moyenne, col = colors[5])

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = colors, lty = 1, cex = 0.8)


# ----------------------------------
#For sc
library(dplyr)

output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/sc/V3/SNR Rescale/Profile_82_sc.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/sc/V3/Oldway_rescale/last_SC_resc.csv')

output_fr_2 <-output_fr_2[,-2]

centersCfr2 <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur_Cass.gpkg")

# Création d'une fonction pour l'interpolation et le calcul de moyenne
interpolate_and_mean <- function(group_df, desired_distance) {
  # Interpolation linéaire
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calcul de la moyenne pour chaque distance interpolée
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}



#Test de boucle sur toute les villes
# On récupère la population maximale
popmax <- max(output_fr_3$PopScanHisto)
popmax <- 2850189


# Interpolation pour toutes les villes
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# On crée le dataframe de résultats avec juste les distances désirées
interpolated_LU <- data.frame(desired_distance = desired_distance)
# Boucle sur chaque colonne de la table output_fr_2 sauf la colonne des distances recalculées
for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  
  # Récupérer la population EM
  pop=centersCfr[centersCfr$city==city,]$PopScanHisto
  
  # Distances après changement d'échelle
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.5
  
  # Effectuer l'interpolation pour la ville actuelle
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Ajouter la colonne avec les valeurs interpolées au dataframe
  interpolated_LU[[city]] <- interpolated_value
  
}

# Afficher le dataframe des valeurs interpolées pour chaque ville
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/profils_interpolé_Sc_0.5.csv")




# Moyenne de villes
# Séparation des données en groupes
Groupe1sc <- centersCfr%>% filter(PopScanHisto > 200000)
Groupe2sc <- centersCfr%>% filter(PopScanHisto < 200000 & PopScanHisto > 100000)
Groupe3sc <- centersCfr%>% filter(PopScanHisto < 100000 & PopScanHisto > 50000)
Groupe4sc <- centersCfr%>% filter(PopScanHisto < 50000 & PopScanHisto > 20000)
Groupe5sc <- centersCfr%>% filter(PopScanHisto < 20000 & city != "MARTIGUES" & city != "FREJUS")

G1sc= Groupe1sc$city
G2sc= Groupe2sc$city
G3sc= Groupe3sc$city
G4sc= Groupe4sc$city
G5sc= Groupe5sc$city



# Sélectionner les colonnes communes entre output_fr_2 et G1sc
cols_to_select <- intersect(names(interpolated_LU), G1sc)
# Sélectionner les colonnes dans output_fr_2
G1s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

----
  cols_to_select <- intersect(names(interpolated_LU), G2sc)

G2s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
----
  cols_to_select <- intersect(names(interpolated_LU), G3sc)

G3s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
-----
  cols_to_select <- intersect(names(interpolated_LU), G4sc)
G4s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())
------
  cols_to_select <- intersect(names(interpolated_LU), G5sc)
G5s <- interpolated_LU %>%
  select(all_of(cols_to_select)) %>%
  mutate(X = interpolated_LU$desired_distance) %>%
  select(X, everything())

write.csv(G5s, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/g5s.csv")
#Affichage / plot


# Courbes interpolés pour sc
colors <- rainbow(ncol(interpolated_LU) - 1)
# Utilisez matplot pour tracer les courbes
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,50),
        main = "Interpolation des valeurs pour différentes villes_ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher les Courbes sans interpol 
colors <- rainbow(ncol(output_fr_2) - 1)
# Utilisez matplot pour tracer les courbes
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlim = c(0,250),
        xlab = "Distance (desired_distance)", ylab = "Valeurs ",
        main = "Profils différentes villes_ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol=2)

#afficher la groupes de villes
#g1
colors <- rainbow(ncol(G1s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G1s$X, G1s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 1 > 200000 ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G1s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#afficher la moyennes de villes


#g2
colors <- rainbow(ncol(G2s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G2s$X, G2s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 2 < 200000 & Pop_sc > 100000")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G2s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g3
colors <- rainbow(ncol(G3s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G3s$X, G3s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 3 < 100000 & Pop_sc > 50000")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G3s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g4
colors <- rainbow(ncol(G4s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G4s$X, G4s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 4 < 50000 & Pop_sc > 20000")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G4s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#g5
colors <- rainbow(ncol(G5s) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5s$X, G5s[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "Interpolation des valeurs le groupe 5 < 20000 ScanHisto")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5s)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)

#afficher la moyennes de villes 
G1_sc_mean <- G1s %>%
  mutate(Moyenne = rowMeans(select(G1s, -X), na.rm = TRUE))

G1_sc_mean <- G1_sc_mean[,-c(2:10)]
#G2
G2_sc_mean <- G2s %>%
  mutate(Moyenne = rowMeans(select(G2s, -X), na.rm = TRUE))

G2_sc_mean <- G2_sc_mean[,-c(2:15)]
#G3
G3_sc_mean <- G3s %>%
  mutate(Moyenne = rowMeans(select(G3s, -X), na.rm = TRUE))

G3_sc_mean <- G3_sc_mean[,-c(2:22)]

#G4
G4_sc_mean <- G4s %>%
  mutate(Moyenne = rowMeans(select(G4s, -X), na.rm = TRUE))

G4_sc_mean <- G4_sc_mean[,-c(2:34)]

#G5
G5_sc_mean <- G5s %>%
  mutate(Moyenne = rowMeans(select(G5s, -X), na.rm = TRUE))

G5_sc_mean <- G5_sc_mean[,-c(2:4)]

write.csv(G1_sc_mean, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/SC/G1_sc_mean.csv")

#ploter les moyennes sc
colors <- rainbow(ncol(G5_sc_mean) - 1)
# Utilisez matplot pour tracer les courbes
matplot(G5_sc_mean$X, G5_sc_mean[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Valeurs interpolées",
        xlim = c(0,250),
        main = "moyenne des valeurs le groupe 5 sc")

# Ajoutez une légende pour identifier chaque courbe
legend("topright", legend = colnames(G5_sc_mean)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol=1)
#
colors <- rainbow(5)

# Tracer la première série avec la fonction plot
plot(G1_sc_mean$X, G1_sc_mean$Moyenne, type = 'l', col = colors[1], xlab = 'X', ylab = 'Moyenne', main='sc0.5', xlim=c(0,70), log="y")

# Ajouter les autres séries avec la fonction lines, en spécifiant une couleur différente pour chacune
lines(G1_sc_mean$X, G2_sc_mean$Moyenne, col = colors[2])
lines(G1_sc_mean$X, G3_sc_mean$Moyenne, col = colors[3])
lines(G1_sc_mean$X, G4_sc_mean$Moyenne, col = colors[4])
lines(G1_sc_mean$X, G5_sc_mean$Moyenne, col = colors[5])

# Optionnel: ajouter une légende pour identifier chaque courbe
legend("topright", legend = c("G1", "G2", "G3", "G4", "G5"),
       col = colors, lty = 1, cex = 0.8)

