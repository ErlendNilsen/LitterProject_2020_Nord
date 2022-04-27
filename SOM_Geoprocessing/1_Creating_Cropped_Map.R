

### Loading some libraries; 

library(raster)
library(rgeos)
library(sp)
library(landscapemetrics)
library(mapview)
library(maptools)

### Reading the SatVeg-raster map;
Sat <- raster::raster("SOM_Geoprosessing/Satveg/sn25_geocorr.tif")
plot(Sat)

### 

## Defining Study area as a buffer around campus;
crs_temp32 <- CRS("+proj=utm +zone=32 +ellps=WGS84")

coords <- cbind(622235, 7100896)
CampusNord <- sp::SpatialPoints(coords, proj4string=crs_temp32)
Study_area_1 <- rgeos::gBuffer(CampusNord, width=15000)
plot(Study_area_1, col="red", add=T)

## Cropping the raster; 

Study_area_2 <- crop(Sat, Study_area_1)                # Crop by the extent of the Study area object
Study_area_2_circ <- mask(Study_area_1, Study_area_1)    # Mask - by the buffer created above

plot(Study_area_2)

## Reclassify raster object; 

## This will give us; 1=forest, 2=water, 3=agri; 4=urban; 0=unclassified

Cur <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,22,23,24,25)
New <- c(0,1,1,1,1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 4, 0)

Reclass <- matrix(c(Cur, New), ncol=2)

Study_area_2b <- raster::reclassify(Study_area_2, Reclass, rigth=FALSE)
plot(Study_area_2b, breaks=c(0,0.99,1.99,2.99,3.99,4.01), col=c("white", "olivedrab3", 
                                                                    "dodgerblue4", "gold1", 
                                                                    "red"))


#######################################################################################
#### Adding data from NVE Elvenett; 

W1 <-  maptools::readShapeSpatial("SOM_Geoprosessing/NVE_Elvenett/Elv_Hovedelv.shp", proj4string=CRS("+proj=utm +zone=32 +ellps=WGS84"))
W2 <-  maptools::readShapeSpatial("SOM_Geoprosessing/NVE_Elvenett/Innsjo_Innsjo.shp", proj4string=CRS("+proj=utm +zone=32 +ellps=WGS84"))

River_raster <- raster::rasterize(W1, Study_area_2, field=50, background=0)
Lake_raster <- raster::rasterize(W2, Study_area_2, field=100, background=0)

## Adding them together; 
Lake_river_raster <- River_raster+Lake_raster

old <- c(0, 50, 100, 150)
new <- c(0, 50, 100, 100)
Reclass_water <- matrix(c(old, new), ncol=2)
Lake_river_raster <- reclassify(Lake_river_raster, Reclass_water, rigth=FALSE)

Lake_river_raster <- crop(Lake_river_raster, Study_area_2)

Study_area_3 <- Lake_river_raster+Study_area_2b

old <- c(0, 1, 2, 3, 4, 50, 51, 52, 53, 54, 100, 101, 102, 103)
new <- c(0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6)
Reclass <- matrix(c(old, new), ncol=2)
Study_area_3b <- reclassify(Study_area_3, Reclass, rigth=FALSE)

########################################################################################

V1 <-  readShapeSpatial("SOM_Geoprosessing/Vbase/vbase_50_linje.shp", proj4string=CRS("+proj=utm +zone=33 +ellps=WGS84"))
V1 <- sp::spTransform(V1, crs_temp32)

V2 <- crop(V1, Study_area_1)

## Making a raster of the road vector data 
## This takses a bit of time (10-15 min)
#Road_raster <- raster::rasterize(V2, Study_area_2, field=50, background=0)

################3

Study_area_4 <- Study_area_3b + Road_raster

old <- c(0, 1, 2, 3, 4, 5, 6, 50, 51, 52, 53, 54, 55, 56)
new <- c(0, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 4, 7, 7)
Reclass <- matrix(c(old, new), ncol=2)
Study_area_4b <- reclassify(Study_area_4, Reclass, rigth=FALSE)


########################################################################################
## Finding edge habitat;  

edge_habitat <- raster::boundaries(Study_area_4b, directions = 4, classes=TRUE, asNA=TRUE)
plot(edge_habitat)

########################################################################################
## Edge between forest and agri; 

Cur <- c(0,1,2,3,4,5, 6, 7)
New_class1 <- c(NA, 1, NA, 1, NA, NA, NA, NA)
Reclass_edge <- matrix(c(Cur, New_class1), ncol=2)

temp <- reclassify(Study_area_4b, Reclass_edge, rigth=FALSE)
Forest_agri_Edge <- temp*edge_habitat

Cur <- c(NA, 1)
new <- c(0, 20)
Reclass_edge <- matrix(c(Cur, new), ncol=2)
temp2 <- reclassify(Forest_agri_Edge, Reclass_edge, rigth=FALSE)

Study_area_5 <- temp2+Study_area_4b

Cur <- c(21, 23)
new <- c(8, 8)
Reclass_edge <- matrix(c(Cur, new), ncol=2)

Study_area_5b <- reclassify(Study_area_5, Reclass_edge, rigth=FALSE)


#########################################################################
######## Seashore

Cur <- c(0,1,2,3,4,5, 6, 7, 8)
New_class1 <- c(NA, NA, 1, NA, NA, NA, NA, NA, NA)
Reclass_edge <- matrix(c(Cur, New_class1), ncol=2)

temp <- reclassify(Study_area_5b, Reclass_edge, rigth=FALSE)
Seashore_edge <- temp*edge_habitat

Cur <- c(NA, 1)
new <- c(0, 20)
Reclass_edge <- matrix(c(Cur, new), ncol=2)
temp2 <- reclassify(Seashore_edge, Reclass_edge, rigth=FALSE)

Study_area_6 <- temp2+Study_area_5b

Cur <- c(22)
new <- c(9)
Reclass_edge <- matrix(c(Cur, new), ncol=2)

Study_area_6b <- reclassify(Study_area_6, Reclass_edge, rigth=FALSE)


#########################################################################
######## Lakeshore

Cur <- c(0,1,2,3,4,5, 6, 7, 8, 9)
New_class1 <- c(NA, NA, NA, NA, NA, NA, 1, NA, NA,NA)
Reclass_edge <- matrix(c(Cur, New_class1), ncol=2)

temp <- reclassify(Study_area_6b, Reclass_edge, rigth=FALSE)
Lake_edge <- temp*edge_habitat

Cur <- c(NA, 1)
new <- c(0, 20)
Reclass_edge <- matrix(c(Cur, new), ncol=2)
temp2 <- reclassify(Lake_edge, Reclass_edge, rigth=FALSE)

Study_area_7 <- temp2+Study_area_6b

Cur <- c(26)
new <- c(10)
Reclass_edge <- matrix(c(Cur, new), ncol=2)

Study_area_7b <- reclassify(Study_area_7, Reclass_edge, rigth=FALSE)

# 0 = unclass
# 1 = forest
# 2 = sea
# 3 = agri
# 4 = urban
# 5 = river
# 6 = lake
# 7 = road
# 8 = agri:forest edge
# 9 = sea shore
# 10 = lake shore

###############################################################################
###############################################################################
### Setting the final size of study area; 

Study_area_final_size <- rgeos::gBuffer(CampusNord, width=7000)

## Cropping the raster; 

Study_area_final <- crop(Study_area_7b, Study_area_final_size)                # Crop by the extent of the Study area object
#writeRaster(Study_area_final, "Study_area_Litter_Project", format="GTiff")

## Cropping raster - no buffer zone

Study_area_4b_final <- crop(Study_area_4b, Study_area_final_size)
#writeRaster(Study_area_4b_final, "Study_area_Litter_Project_NoBufferZone", format="GTiff")








