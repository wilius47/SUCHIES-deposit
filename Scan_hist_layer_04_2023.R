#rescaling factor analysis

setwd("C:/SUCHIES/Cassini taille ville testing/Raster _ shape/new_test")
library(raster)
library(fasterize)
library(sf)
library(st)
library(rgdal)
library(rgeos)
library(tidyverse)
if("dplyr" %in% (.packages())){
  detach("package:dplyr", unload=TRUE) 
  #detach("package:plyr", unload=TRUE) 
} 
install.packages("rowr")
library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library("readxl")
library("rowr")
install.packages("stars")
library(stars)
library(gdata)

Sc_hist <- raster("C:/SUCHIES/Cassini taille ville testing/Raster _ shape/Scan histo raster/Scanhisto_100m_agr.tif")

#calibrer les raster


centres <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/Centre_urb_atls_France.shp")
centresC <- st_read(
  "C:/SUCHIES/Cassini taille ville testing/Raster _ shape/Centre_urb_atls_France_cassini_pop.shp")





centersC <- st_transform(centresC, st_crs(built20))
Sc_histo <-projectRaster(Sc_hist, crs = crs(built20))








# writeRaster(built20, filename = "C:/Users/rabehwal/Desktop/test/ghs.test.tif")







Scan_hist46_v <- data.frame(seq(1,100,1))
colnames(Scan_hist46_v)[1] <- "Radius"



write.csv(Scan_hist46_v,"C:/SUCHIES/Cassini taille ville testing/Results/Scan_hist46_v_mean.csv", row.names = TRUE)




vec2 <- centersC$fua_name


for(j in 1:46){
  
  Urb_resc_Sc (vec2[j])
  
}


Urb_resc ("Lille")   #suffit juste d'exécuter cetter ligne
Urb_resc_Sc <- function(ville) {
  
  removeTmpFiles(h=1) 
  
  ville_centerC <- centersC[centersC$fua_name == ville,]

  
  #create the Buffer(using the city area formula)


  D_sc <- 100000 *(((ville_centerC$Pop1950)/(2725374))^0.3333)

  
  

  buffer_sc <- st_buffer (ville_centerC, dist =D_sc)
 
  
  
  #Subset GHSL raster files on the computed city area
  
  Sc_histo_ville <- crop(Sc_histo, buffer_sc)
 
  
  Sca_histo_ville <- mask(Sc_histo_ville, buffer_sc)

  
  
  
  #extraction of centroid coordinates from each pixel (built file)
  
  xy <- coordinates(raster(Sca_histo_ville))
  
  #xy <- coordinates(raster(Cass_Lille)) 
  
  PV1<- as.data.frame(Sca_histo_ville)

  # PV2<- as.data.frame(pop_Lille)

  B_sc <- cbind.data.frame(xy, PV1)

  
  
  # Extract le XY from Rouen center - point, and adding them to dataframe
  XYcent =  as.data.frame(st_coordinates(ville_centerC))
  
  #XYcent = as.data.frame(st_coordinates(Lille_center))
  names(XYcent)[names(XYcent) == "X"] <- "Xcenter"
  names(XYcent)[names(XYcent) == "Y"] <- "Ycenter"
  

  # 
  B_sc1=cbind.data.frame(XYcent, B_sc)
  B_sc2 <- na.omit(B_sc1)
  # # 

  #renaming the Built up value column 
  
  # names(B_ETM2)[names(B_ETM2) == "ETM_Mosa_100m2"] <- "EtatMaj_built"
  names(B_sc2)[names(B_sc2) == "layer"] <- "scan_hist"
  # names(B_GHS2)[names(B_GHS2) == "GHS_BUILT_S_E2020_GLOBE_R2022A_54009_100_V1_0"] <- "GHS_built"
  # names(B_Cas2)[names(B_Cas2) == "layer"] <- "Cass_built"
  
  
  
  #adding distance field, Radius rings, and resacaled distance
  

  
  B_sc2['dist'] <- NA
  B_sc2['Radius'] <- NA
  #

  # 
  #Compute distance (cartesian) between each pixel and city_center
  
  
 
  B_sc2[,6] <- (((B_sc2[,3] - B_sc2[,1])^2)+ ((B_sc2[,4] - B_sc2[,2])^2))^0.5

  
  
  
  #creating 1 km radius (then round values)
  
  B_sc2[,7] <- (B_sc2[,6]) / 1000

  # 
  B_sc2$Rounded = round(B_sc2$Radius)
  B_sc2$Radius <- B_sc2$Rounded
  B_sc2=subset(B_sc2, select = -c(Rounded) )
  B_sc2$Radius = B_sc2$Radius + 1
  
  # 
  # 

  #summurazie data (mean built-up by Radius/ring)

  
  Synth_sc <- B_sc2 %>%
    select(scan_hist) %>%
    group_by(B_sc2[,7]) %>%
    summarise(scan_hist = mean(scan_hist))
  # 


  # 
  
  Synth_sc2 <- as.data.frame(Synth_sc)

  
  #Calibrer les valeurs
  
 
  
  max<- maxValue(Sca_histo_ville)
  Synth_sc2$scan_hist <- Synth_sc2$scan_hist/max
  

  # 
  
  
  #renaming field

  # # 
  names(Synth_sc2)[names(Synth_sc2) == "B_sc2[,7]"] <- "Radius"
  names(Synth_sc2)[names(Synth_sc2) == "scan_hist"] <- ville
  

  
  
  
  
  # 
  
  # 
  Scan_hist46_v<<- cbindX(Scan_hist46_v,
                          Synth_sc2[2])
  
  

  # 
  # 
  print(Scan_hist46_v)


}

centersBisC <- as.data.frame(cbindX(centersC[4], centersC[9], centersC[10], centersC[11], centersC[12]))



Scan_hist46_v2 <- Scan_hist46_v %>% 
  gather(.,ville, densite,-Radius) %>% 
  merge(centersBisC %>% select(1,4), by.x="ville", by.y="fua_name")

Scan_hist46_v2['Resc_fact1950_0_1'] <- (2725374/Scan_hist46_v2[4])^0.1
Scan_hist46_v2['Resc_fact1950_0_5'] <- (2725374/Scan_hist46_v2[4])^0.5
Scan_hist46_v2['Resc_fact1950_0_9'] <- (2725374/Scan_hist46_v2[4])^0.9

write.csv(Scan_hist46_v2,"C:/SUCHIES/Cassini taille ville testing/Results/Scan_hist46_v2_mean.csv", row.names = TRUE)
Scan_hist46_v2 <- read.csv("C:/SUCHIES/Cassini taille ville testing/Results/Scan_hist46_v2_mean.csv")


----------------
  # Graphique sur l'ensemble des villes avant rescaling
  
  #representation sans mise à l'échelle
  ggplotly(
    ggplot(Scan_hist46_v2, aes(x = Radius, y = densite, 
                                 color = log(Pop1950), group = ville)) + 
      geom_line(show.legend = F, size = 0.18) +
      scale_colour_gradient(low = "yellow", high = "purple") +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1)) + 
      # breaks = c(0,0.2,0.4,0.6,0.8) 
      labs(title = "Scanhisto/pop", 
           x = "Distance from City Hall (km)", 
           y = "artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11))
  )


#Premiére mis l'échelle  (0.5)
library(plotly)

ggplotly(
  ggplot(Scan_hist46_v2, aes(x = Radius*Resc_fact1950_0_5, y = densite, 
                               color = log(Pop1950), group = ville)) + 
    geom_line(show.legend = F, size = 0.18) +
    scale_colour_gradient(low = "yellow", high = "purple") +
    scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
    scale_y_continuous(limit = c(0,1)) + 
    # breaks = c(0,0.2,0.4,0.6,0.8) 
    labs(title = "Scanhisto/pop 0.5 ", 
         x = "Distance from City Hall (km)", 
         y = "artificial land use", color = "Legend:") +
    theme_bw() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
                       legend.title = element_text(size = 13), 
                       legend.text = element_text(size = 11))
)




#changement de facteur d'échelle (0.1)
ggplotly(
  ggplot(Etat_majors46_v2, aes(x = Radius*Resc_fact1950_0_5, y = densite, 
                               color = log(PopEtatmaj), group = ville)) + 
    geom_line(show.legend = F, size = 0.18) +
    scale_colour_gradient(low = "yellow", high = "purple") +
    scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
    scale_y_continuous(limit = c(0,377340)) + 
    # breaks = c(0,0.2,0.4,0.6,0.8) 
    labs(title = "Etat major/pop", 
         x = "Distance from City Hall (km)", 
         y = "artificial land use", color = "Legend:") +
    theme_bw() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
                       legend.title = element_text(size = 13), 
                       legend.text = element_text(size = 11))
  
  
  
  
)
