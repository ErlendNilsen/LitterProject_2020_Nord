
#### This code is used to make the map of the study area (Figure 1) in the manuscript. 
#### The input raster (Study_area_Litter_Project_NoBufferZone.tif) is produced based on the
#### code presented in "SOM_Geoprocessing/1_Creating_Cropped_Map.R". 

### Map of study area; 

library(tidyverse)
library(ggplot2)
library(sp)
library(raster)
library(cowplot)


###################################################
## Defining Study area as a buffer around campus;
crs_temp32 <- CRS("+proj=utm +zone=32 +ellps=WGS84")
crs_latlong <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs")

coords <- cbind(622235, 7100896)
CampusNord <- sp::SpatialPoints(coords, proj4string=crs_temp32)
CampusNord1 <- spTransform(CampusNord, crs_latlong)


### Making map of Norway: 

m1a <- ggplot() + 
  xlim(2, 32)+
  ylim(57, 72) +
  borders(regions = "Norway(?!:Svalbard)", colour = "gray70", fill = "orange", alpha=0.9) +
  geom_point(data=data.frame(CampusNord1@coords), aes(x=CampusNord1@coords[1], y=CampusNord1@coords[2]), size= 3, col=1, pch=16) +
  xlab("") +
  ylab("") + 
  theme_map() + theme(text=element_text(size=12)) 
  
#m1a 
  
###  Prearing raster map for study area; 
  
Study_map <- raster("Study_area_Litter_Project_NoBufferZone.tif")
test_spdf <- as(Study_map, "SpatialPixelsDataFrame")


#########
## The raster layer include the following categoreis:
# 0 = unclass
# 1 = forest
# 2 = sea
# 3 = agri
# 4 = urban
# 5 = river
# 6 = lake
# 7 = road

##Making data frame for plotting + renaming categoreis
Study_Area_df <- as_tibble(test_spdf) %>% rename(value=Study_area_Litter_Project_NoBufferZone, long=x, lat=y) %>%
  mutate(Habitat=ifelse(value!=0, value, NA)) %>% 
  mutate(LandCover=recode(Habitat, "1"="Forest", 
                                "2"="Sea",
                                "3"="Agriculture", 
                                "4"="Urban",
                                "5"="Lake/river", 
                                "6"="Lake/river",
                                "7"="Road"))
## Study area map
m2 <- ggplot() +
  xlab("UTM - easting") + 
  ylab("UTM - northing") +
  geom_raster(data=Study_Area_df, aes(x=long, y=lat, fill=LandCover), interpolate=TRUE) +
  scale_fill_manual(values=c("Agriculture"="yellow", "Forest"="darkolivegreen4", "Road"="red","Lake/river"="blue", "Sea"="deepskyblue2", "Urban"="grey90")) +
  theme_minimal()+ theme(text=element_text(size=12)) +
  labs(fill="Land Cover")

##############################################################################
##### Making the final figure - with Norway-map as inset

gg_inset_map1 <- ggdraw() +
  draw_plot(m2, width=0.8, height=0.8, x=0.15) +
  draw_plot(m1a, x = 0.03, y = 0.62, width = 0.35, height = 0.35) 

gg_inset_map1
#ggsave("figures/Figure1.jpg", plot=last_plot(), width = 9.5, dpi=500) 









