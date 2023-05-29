#Library
library(terra)
library(sf)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library (rpart.plot)


#Opening data
#old map

map <- rast("C/filepath/map.tif")

#shapefile "point" on urban pixels
point_DM <- st_read("C/filepath/roads.shp")
point_V <- st_read("C/filepath/roads.shp")

#landcover map classes (corine landcover in Europe for ex)
ldcc <- rast("C/filepath/ldcc.tif")
#roads and administrative boundary
roads <- st_read("C/filepath/roads.shp")
admi_bound <- st_read("C/filepath/bound.shp")


#Preparing data 
#selecting natural areas
ldcc_nat1 <- ldcc$4
ldcc_nat2 <- ldcc$5
ldcc_nat  <- ldcc$5 + ldcc$4
#buffering roads and administrative boundary
roads_B <- st_buffer(roads, 10)
road_B_r <- rast(roads_B)
roads_Br <- rasterize(roads_B, road_B_r)

admi_bound_B <- st_buffer(admi_bound, 100)
admi_bound_B_r <- rast(admi_bound_B)
admi_bound_Br <- rasterize(admi_bound_B, admi_bound_B_r)
#Data mining

dtm<- rpart(Class~.,point_DM, method="class", control= rpart.control(minsplit = 1))
plot(dtm, uniform=TRUE, main="Decision-Tree Scan histo_urban classifier")
text(dtm, use.n=TRUE, all=TRUE, cex=.8)

#verifying the quality of threshold (prediction)
p=predict(dtm, point_V, type="class") #if too low, update the point_V file by adding more urban points


#extracting and applying Threshold filter
map_T <- map$1 > dtm$min and map$1 > dtm$max

# extracting and deleting missclassified non-urban data(natural areas)

Map_Tn <- map_T - admi_bound_Br
#  extracting and deleting missclassified non urban-data (roads/boundaries)

Map_Tnr <- Map_Tn - roads_Br

# Deleting isolated pixel (sieve less than 3 pixels)
Map_Tnr <- terra::sieve(Map_Tnr, 3)

# void filling in urban blocks (clump)
Map_Tnr <- clump(Map_Tnr, 8)


# vectorisation (for manual enhacement)

Map_vector <- rasterToPolygons(Map_Tnr)

#exporting results into a vector (Shapefile) 
st_write(Map_vector, "Map_vector.shp")

