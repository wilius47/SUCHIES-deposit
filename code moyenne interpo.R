Bien sûr, voici la traduction des commentaires en anglais :

```r
#For Headquarters
# Loading the necessary libraries
library(sf)
library(dplyr)
centersCfr <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/CBD/New CBD/Historical Centers_83cities - Testeur.gpkg")

output_fr_2 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ETM/SNR Rescale/Profile_etm_50m.csv')
output_fr_3 <- read.csv('C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/13_01/ETM/Old way rescale/etm_50m_resc.csv')

output_fr_2 <- output_fr_2[,-2]

# Creating a function for interpolation and average calculation
interpolate_and_mean <- function(group_df, desired_distance) {
  # Linear interpolation
  interpolated_values <- sapply(group_df, function(col) {
    print(length(group_df$X))
    print(length(col))
    approx(group_df$X, col,
           xout = desired_distance,
           method = "linear",
           ties = "ordered")$y
  })
  
  # Calculating the mean for each interpolated distance
  mean_values <- apply(interpolated_values, 1, mean, na.rm = TRUE)
  
  return(mean_values)
}

# Testing loop on all cities
# Getting the maximum population
popmax <- max(output_fr_3$Pop_EtatMajor)

# Interpolation for all cities
desired_distance <- seq(from = min(output_fr_2$X),
                        to = max(output_fr_2$X),
                        by = 2)
desired_distance <- (1:200) / 2

# Creating the results dataframe with only the desired distances
interpolated_LU <- data.frame(desired_distance = desired_distance)

# Looping through each column of the table output_fr_2 except the recalculated distance column
for (city in names(output_fr_2)[names(output_fr_2) != "X"]) {
  
  # Retrieve the HQ population
  pop = centersCfr[centersCfr$city == city,]$Pop_EtatMajor
  
  # Rescaled distances
  rescaled_dist <- output_fr_2$X * 50 / 1000 * (popmax / pop)^0.33
  
  # Performing the interpolation for the current city
  interpolated_value <- approx(rescaled_dist, output_fr_2[[city]], xout = desired_distance, method = "linear")$y
  
  # Adding the column with interpolated values to the dataframe
  interpolated_LU[[city]] <- interpolated_value
}

# Display the dataframe of interpolated values for each city
print(interpolated_LU)

write.csv(interpolated_LU, "C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/profils_interpolé0.33.csv")

interpolated_LU <- read.csv("C:/Users/rabehwal/Desktop/Last rescale/12_01_2024/27_05_interpolation/ETM/profils_interpolé_ETM.csv")

library(reshape2)

centersCfr$city

centersCfr <- arrange(centersCfr, city)

# Averages of cities
# Splitting data into groups
Groupe1etm <- centersCfr %>% filter(centersCfr$Pop_EtatMajor > 200000)
Groupe2etm <- centersCfr %>% filter(Pop_EtatMajor < 200000 & Pop_EtatMajor > 100000)
Groupe3etm <- centersCfr %>% filter(Pop_EtatMajor < 100000 & Pop_EtatMajor > 50000)
Groupe4etm <- centersCfr %>% filter(Pop_EtatMajor < 50000 & Pop_EtatMajor > 25000)
Groupe5etm <- centersCfr %>% filter(Pop_EtatMajor < 25000)

G1etm = Groupe1etm$city
G2etm = Groupe2etm$city
G3etm = Groupe3etm$city
G4etm = Groupe4etm$city
G5etm = Groupe5etm$city

# Selecting common columns between interpolated_LU and G1Cass
cols_to_select <- intersect(names(interpolated_LU), G1etm)
# Selecting columns in 
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

# Display / plot

# display interpolated ETM curves

# Using matplot to plot the curves
matplot(interpolated_LU$desired_distance, interpolated_LU[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Interpolated values",
        xlim = c(0, 50), ylim = c(min(interpolated_LU[, -1], na.rm = TRUE), 1),
        main = "Interpolation of values for different cities")

# Adding a legend to identify each curve
legend("topright", legend = colnames(interpolated_LU)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol = 2)

plot(interpolated_LU$desired_distance, interpolated_LU$Paris)

# display non-interpolated ETM curves
colors <- rainbow(ncol(output_fr_2) - 1)
# Using matplot to plot the curves
matplot(output_fr_2$X, output_fr_2[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Values",
        xlim = c(0, 50),
        main = "Profiles of different cities")

# Adding a legend to identify each curve
legend("topright", legend = colnames(output_fr_2)[-1], col = colors, lty = 1, cex = 0.5, bty = 'n', ncol = 2)

# display city groups
#g1
colors <- rainbow(ncol(G1) - 1)
# Using matplot to plot the curves
matplot(G1$X, G1[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Interpolated values",
        xlim = c(0, 20), ylim = c(0, 1),
        main = "Interpolation of values for group 1")

# Adding a legend to identify each curve
legend("topright", legend = colnames(G1)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol = 1)
# display averages of cities

#g2
colors <- rainbow(ncol(G2) - 1)
# Using matplot to plot the curves
matplot(G2$X, G2[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Interpolated values",
        xlim = c(0, 50),
        main = "Interpolation of values for group 2")

# Adding a legend to identify each curve
legend("topright", legend = colnames(G2)[-1], col = colors, lty = 1, cex = 1, bty = 'n', ncol = 1)

#g3
colors <- rainbow(ncol(G3) - 1)
# Using matplot to plot the curves
matplot(G3$X, G3[, -1], type = 'l', lty = 1, col = colors,
        xlab = "Distance (desired_distance)", ylab = "Interpolated values",
        xlim = c(0, 50),
        main = "Interpolation of values for group 3")

# Adding a legend
