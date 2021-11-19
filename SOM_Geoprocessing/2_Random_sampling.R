
library(tidyverse)

############################################
#### Setting up the sampling; 

## Taking random points; 
## n=Size in each strata
set.seed(457)
LitterPoints_temp <- raster::sampleStratified(Study_area_final , size=30, xy=T)

LitterPoints <- LitterPoints_temp %>% as_tibble(LitterPoints) %>%
                rename(Strata=layer) %>%
                filter(Strata==1 | Strata==3 | Strata==4 | Strata==5 | Strata==7 | Strata==8 | Strata==9 | Strata==10)

RandomDirection1 <- as_tibble(base::sample(c("North", "South", "East", "West"), dim(LitterPoints)[1], replace = TRUE))
RandomDirection2 <- as_tibble(base::sample(c("North", "South", "East", "West"), dim(LitterPoints)[1], replace = TRUE))

LitterPoints <- bind_cols(LitterPoints, RandomDirection1, RandomDirection2) %>%
                rename(Direction1=value, Direction2=value1) %>%
                select(-cell)

#### Writing the point list; 

write.csv(LitterPoints, "RandomPoints.csv", row.names=FALSE)


#######################################################################################
############## Making spatial points from sampled points; 




