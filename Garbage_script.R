
#### DATA ANALYSIS SPATIAL DISTRIBUTION OF GARBAGE IN STEINKJER ####

### Loading packages ###
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(googlesheets)
library(AER)
library(MASS)
library(ggwordcloud)
library(lme4)
library(sp)
library(maptools)
library(raster)
library(spatstat)
library(MuMIn)
library(readxl)
library(gridExtra)
library(wordcloud2)
library(dplyr)
library(gmodels)
library(ggpubr)


### Loading necessary data ###

d1 <- read_excel("Nord_LitterProject_2020.xlsx",sheet = "Plotdata") 
d2 <- read_excel("Nord_LitterProject_2020.xlsx",sheet = "Items")


### Descriptive statistics ###
table(d1$HabitatGT)
table(d1$TrashDetected)

table(d1$HabitatGT, d1$TrashDetected)
min(d1$TrashAbundance)
max(d1$TrashAbundance)
sum(d1$TrashAbundance)


##########################################################################################

#### HYPOTHESIS 1A. GARBAGE DETECTION PROBABILITY ####


M1 <- glm(TrashDetected~HabitatGT-1, data=d1, family="binomial")

M2 <- glm(TrashDetected~1, data=d1, family="binomial")


### Comparing the AIC-values; ###
model.sel(M1, M2) # M1 wins

118.73/102 # Dispersion stat =  1.16402

summary(M1)


### Plotting the results, using functions from the packages ggeffects and ggplot ###
pred1 <- ggpredict(M1, terms="HabitatGT")

p1 <- ggplot(data=pred1, aes(x, predicted)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour="dark grey", size= 1) + 
  geom_point(colour="dark blue", size=4) +
  #theme_minimal() + 
  ylim(0,1) + 
  xlab("") +
  ylab("Garbage detection prob.") 

##  The "minimal" version
p1 + theme_minimal() + 
     theme(axis.text.x = element_text(angle=90, hjust=1, size=12), 
           axis.text.y=element_text(size=12),
           axis.title.y = element_text(size=14)) 
     
ggsave("figures/Figure2A.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)

##  The "minimal" version
p1 +   theme(axis.text.x = element_text(angle=90, hjust=1, size=12), 
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=14)) 

ggsave("figures/Figure2A_alt.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)


##########################################################################################

#### HYPOTHESIS 1B. GARBAGE ABUNDANCE ####


### Test Poisson ###

M3 <- glm(TrashAbundance~HabitatGT, data=d1, family="poisson")

M4 <- glm(TrashAbundance~1, data=d1, family="poisson")


### Comparing the AIC-values; ###
model.sel(M3, M4) # M3 wins


### Check dispersion ###
dispersiontest(M4) # Dispersion: 49.06


### Negative binomial glm instead of poisson ###

M3a <- glm.nb(TrashAbundance~HabitatGT-1, data=d1)

M4a <- glm.nb(TrashAbundance~1, data=d1)

### Comparing the AIC-values; ###
model.sel(M3a, M4a) # M3a wins

summary(M3a)


### Plotting the results, using functions from the packages ggeffects and ggplot ###
pred2 <- ggpredict(M3a, terms="HabitatGT")

p2 <- ggplot(data=pred2, aes(x, predicted)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour="dark grey", size=1) +  
  geom_point(colour="dark blue", size=4) +
  xlab("") +
  ylab("Litter abundance") 

## Minimal theme
p2 + theme_minimal() +
      theme(axis.text.x = element_text(angle=90, hjust=1, size=14), 
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=14)) 

ggsave("figures/Figure2B.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)

## Normal theme
p2 +    theme(axis.text.x = element_text(angle=90, hjust=1, size=14), 
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=14)) 

ggsave("figures/Figure2B_alt.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)

##########################################################################################

#### HYPOTHESIS 1C. DISTANCE TO ROADS ####


boxplot(d1$distRoad~d1$HabitatGT) # Colinearity

d1b <- d1[d1$HabitatGT!="urban" & d1$HabitatGT!="road",]


M5 <- glm(TrashDetected~distRoad, data=d1b, family="binomial")

M5b <- glm(TrashDetected~distRoad*HabitatGT, data=d1b, family="binomial")

M5c <- glm(TrashDetected~distRoad+HabitatGT, data=d1b, family="binomial")

M5d <- glm(TrashDetected~HabitatGT, data=d1b, family="binomial")

M5e <- glm(TrashDetected~1, data=d1b, family="binomial")


model.sel(M5, M5b, M5c, M5d, M5e) # M5c wins 

84.367/74 # Dispersion statisic M5c = 1.140095

summary(M5c)


### Plotting the results, using functions from the packages ggeffects and ggplot ###
pred3 <- ggpredict(M5c, terms=c("distRoad", "HabitatGT"))

p3 <- ggplot(data=pred3, aes(x, predicted)) +
  geom_line(colour="dark blue", size=1.2) +
  facet_wrap(~group) +
  xlab("Distance to road (m)") +
  ylab("Garbage detection probability") +
  ylim(0,1) + 
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "grey", alpha = 0.3)

## Minimal
p3 + theme_minimal() +
     theme(axis.text.x = element_text(angle=0, hjust=1, size=12),
           axis.title = element_text(size=14),
           strip.text = element_text(size=14))
ggsave("figures/Figure3.jpg", plot=last_plot(), width = 7, height = 5, dpi=500)

## alternative
p3 +   theme(axis.text.x = element_text(angle=0, hjust=1, size=12),
        axis.title = element_text(size=14),
        strip.text = element_text(size=14))
ggsave("figures/Figure3_alt.jpg", plot=last_plot(), width = 7, height = 5, dpi=500)


##########################################################################################

#### HYPOTHESIS 1D. LITTER SIZE ####

### Finding the longest side of the item and using that as a measure for fragment size ###

d_size <- d2  %>%
  dplyr::select(PlotID, ItemNumber, Description, Lenght_cm, Width_cm, Height_cm) %>% 
  gather(direction, size, Lenght_cm:Height_cm) %>%
  mutate(size=if_else(is.na(size), 0, size)) %>%
  group_by(ItemNumber, PlotID) %>%
  summarise(size=max(size)) %>% 
  left_join(., d1)


### Fitting the models with REML=F for model selection ###

M6 <- lmer(log(size)~HabitatGT+(1|PlotID), data=d_size, REML=F)

M7 <- lmer(log(size)~1+(1|PlotID), data=d_size, REML=F)


model.sel(M6, M7) # M6 wins, delta AICc is 9.78

plot(M6, xlab= "Fitted Values", ylab= "Pearson residuals")

### Refitting model 6 with REML=T for model interpretation ###
M6b <- lmer(log(size)~HabitatGT+(1|PlotID), data=d_size, REML=T)

summary(M6b)

plot(M6b)

plot(M6b, xlab= "Fitted values", ylab= "Pearson residuals")



### Plotting the results, using functions from the packages ggeffects and ggplot ###
pred4 <- ggpredict(M6b, terms="HabitatGT")

### Setting the order the same as the previous plots in H1A and H1B ###
pred4b <- ggpredict(M6b, terms="HabitatGT") %>%
  mutate(x=factor(x, levels=c("forest", "agriculture", "urban", "river",
                              "road", "edge", "lakeshore", "beach"))) 

p4 <- ggplot(data=pred4b, aes(x, predicted)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour="dark grey", size=1) +
  geom_point(colour="dark blue", size=4) +
  xlab("") +
  ylab("Litter size (cm)") +
  ylim(0,65)

## Minimal
p4 + theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1, size=14), 
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=14)) 
ggsave("figures/Figure2C.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)

## Normal
## Minimal
p4 +   theme(axis.text.x = element_text(angle=90, hjust=1, size=14), 
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=14)) 
ggsave("figures/Figure2C_alt.jpg", plot=last_plot(), width = 5, height = 5, dpi=500)

######################################################################################
#### Combining figure 2A - C into one figure file

## Minimal
p2_all <- ggarrange(p1 +   theme_minimal() + theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                          axis.text.y=element_text(size=12),
                          axis.title.y = element_text(size=12)), 
             p2 +  theme_minimal() + theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                          axis.text.y=element_text(size=12),
                          axis.title.y = element_text(size=12)), 
             p4 +  theme_minimal() + theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                          axis.text.y=element_text(size=12),
                          axis.title.y = element_text(size=12)), 
             nrow = 3, labels = c("A)", "B)", "C)"), hjust=-3)
ggsave("figures/Figure2.jpg", p2_all, width = 8, height = 9, dpi=500)

## Normal
p2_all <- ggarrange(p1 +   theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                                         axis.text.y=element_text(size=12),
                                         axis.title.y = element_text(size=12)), 
                            p2 +  theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                                        axis.text.y=element_text(size=12),
                                        axis.title.y = element_text(size=12)), 
                            p4 +  theme(axis.text.x = element_text(angle=60, hjust=1, size=14), 
                                        axis.text.y=element_text(size=12),
                                        axis.title.y = element_text(size=12)), 
                            nrow = 3, labels = c("A)", "B)", "C)"), hjust=-3)

ggsave("figures/Figure2_alt.jpg", p2_all, width = 8, height = 9, dpi=500)


##########################################################################################
##########################################################################################
#### HYPOTHESIS 2 - chi.sq and WORDCLOUDS ####

d3 <- d2 %>% left_join(., d1) %>%
  group_by(HabitatGT) %>%
  count(MatClass) %>%
  rename("Antall"="n")

### CONTINGENCY TABLES ###
# Rare material classes were assigned to the material type "other" 
agg <- subset(d3, MatClass=="clay" | MatClass=="paint" | MatClass=="wood" | MatClass=="other composite" | MatClass=="glass")

c <- aggregate(agg$Antall, by=list(agg$HabitatGT), FUN=sum)
c1 <- c %>% rename(HabitatGT = Group.1, Antall = x)
c1$MatClass <- c("other", "other", "other", "other", "other", "other", "other")

c2 <- subset(d3, subset=!(MatClass=="clay" | MatClass=="paint" | MatClass=="wood" | MatClass=="other composite" | MatClass=="glass"))

ctest2 <- rbind(c2, c1)

# Generating a cross tab and chisquare test
mytable <- xtabs(Antall~HabitatGT+MatClass, data=ctest2)
CrossTable(mytable, expected=T, chisq=T, simulate.p.value = FALSE)

##############################################################################################
##############################################################################################

### Wordcloud 1 Type of item ###

## Preparing data
test <- d2 %>% left_join(., d1) %>%
  group_by(HabitatGT) %>%
  count(Description) %>%
  rename("Antall"="n") %>%
  as.data.frame()

# Convert test to dataframe to allow subsetting in ggplot2
#test2 <- as.data.frame(test) 

# Create an "angle" column to rotate some of the words, rotate 40 percent
test_with_rotated <- test %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

# Dont rotate the largest words
test_with_rotated$angle <- ifelse(test_with_rotated$Antall > 5,
                                  0,
                                  test_with_rotated$angle)


used_theme <- theme_minimal() 
used_scale_radius <- scale_radius(range = c(0, 8), limits = c(0, 5)) 
#

test_with_rotated$log_transformed <- log(test_with_rotated$Antall) + 1

set.seed(123)
g1  <- ggplot(subset(test_with_rotated, HabitatGT %in% "agriculture"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10,6)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 25), limits = c(0, 5)) + 
  used_theme +
  ggtitle("Agriculture") +
  theme(plot.title = element_text(hjust = 0.5, size =32)) 

g1

set.seed(123)
g2 <- ggplot(subset(test_with_rotated, HabitatGT %in% "beach"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 34, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 35), limits = c(0, 10)) +
  used_theme +
  ggtitle("Beach") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g2

set.seed(123)
g3 <- ggplot(subset(test_with_rotated, HabitatGT %in% "edge"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 8, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 30), limits = c(0, 5)) +
  used_theme +
  ggtitle("Edge") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g3

set.seed(123)
g4 <- ggplot(subset(test_with_rotated, HabitatGT %in% "forest"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 8, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 25), limits = c(0, 5)) +
  used_theme +
  ggtitle("Forest") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g4

set.seed(123)
g5 <- ggplot(subset(test_with_rotated, HabitatGT %in% "lakeshore"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 39, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 10), limits = c(0, 2)) +
  used_theme +
  ggtitle("Lakeshore") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g5

set.seed(123)
g6 <- ggplot(subset(test_with_rotated, HabitatGT %in% "river"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 28, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 14), limits = c(0, 2)) +
  used_theme +
  ggtitle("River") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g6

set.seed(123)
g7 <- ggplot(subset(test_with_rotated, HabitatGT %in% "road"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 39, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 16), limits = c(0, 2)) +
  used_theme +
  ggtitle("Road") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g7

set.seed(123)
g8 <- ggplot(subset(test_with_rotated, HabitatGT %in% "urban"),aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 26, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 16), limits = c(0, 2)) +
  used_theme +
  ggtitle("Urban") +
  theme(plot.title = element_text(hjust = 0.5, size =32))

g8

totaldata <- aggregate(test_with_rotated$Antall, by=list(Description=test2_with_rotated$Description), FUN=sum)

## Rotate 60 percent of the words, 90 degrees

totaldata <- totaldata %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

totaldata$angle <- ifelse(totaldata$x > 5,
                          0,
                          totaldata$angle)

totaldata$log_transformed <- log(totaldata$x) + 1

set.seed(123)
g9 <- ggplot(totaldata,aes(label = Description, size=log_transformed,angle = angle,color = factor(sample.int(10, 89, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 25), limits = c(0, 15)) +
  used_theme +
  ggtitle("Total") +
  theme(plot.title = element_text(hjust = 0.5, size =35))

g9

p8 <- grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, nrow = 3)
ggsave("figures/Wordcloud_Appendix.jpg", p8, width = 12, height = 12, dpi=500)


##################################################################################
##################################################################################
### Wordcloud 2 Material type ###

test2 <- d2 %>% left_join(., d1) %>%
  group_by(HabitatGT) %>%
  count(MatClass) %>%
  rename("Antall"="n") %>%
  as.data.frame()

# Convert test to dataframe to allow subsetting in ggplot2
#test123 <- as.data.frame(test2) 

# Create an "angle" column to rotate some of the words, rotate 40 percent
test2_rotated <- test2 %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))  

# Dont rotate the largest words
test2_rotated$angle <- ifelse(test2_rotated$Antall > 5,
                              0,
                              test2_rotated$angle)


used_theme <- theme_minimal() 
used_scale_radius <- scale_radius(range = c(0, 8), limits = c(0, 5))

used_theme <- theme_minimal() 
used_scale_radius <- scale_radius(range = c(0, 8), limits = c(0, 5)) 
#

# log test
test2_rotated$log_transformed <- log(test2_rotated$Antall) + 1

set.seed(123)
A1 <- ggplot(subset(test2_rotated, HabitatGT %in% "agriculture" ), aes(label = MatClass,size=log_transformed, angle = angle,color = factor(sample.int(10,3)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 50), limits = c(0, 12)) + 
  used_theme +
  ggtitle("Agriculture") +
  theme(plot.title = element_text(hjust = 0.5, size =25))
A1

set.seed(123)
A2  <- ggplot(subset(test2_rotated, HabitatGT %in% "beach"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10,7)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 40), limits = c(0, 12)) + 
  used_theme +
  ggtitle("Beach") +
  theme(plot.title = element_text(hjust = 0.5, size =25)) 

A2

set.seed(123)
A3 <- ggplot(subset(test2_rotated, HabitatGT %in% "edge"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 3, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 40), limits = c(0, 12)) +
  used_theme +
  ggtitle("Edge") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A3

set.seed(123)
A4 <- ggplot(subset(test2_rotated, HabitatGT %in% "forest"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 3, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 50), limits = c(0, 12)) +
  used_theme +
  ggtitle("Forest") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A4

set.seed(123)
A5 <- ggplot(subset(test2_rotated, HabitatGT %in% "lakeshore"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 5, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 40), limits = c(0, 12)) +
  used_theme +
  ggtitle("Lakeshore") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A5

set.seed(123)
A6 <- ggplot(subset(test2_rotated, HabitatGT %in% "river"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 5, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 40), limits = c(0, 8)) +
  used_theme +
  ggtitle("River") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A6

set.seed(123)
A7 <- ggplot(subset(test2_rotated, HabitatGT %in% "road"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 6, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 30), limits = c(0, 8)) +
  used_theme +
  ggtitle("Road") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A7

set.seed(123)
A8 <- ggplot(subset(test2_rotated, HabitatGT %in% "urban"),aes(label = MatClass, size=log_transformed,angle = angle,color = factor(sample.int(10, 4, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 40), limits = c(0, 8)) +
  used_theme +
  ggtitle("Urban") +
  theme(plot.title = element_text(hjust = 0.5, size =25))

A8

totaldata2 <- aggregate(test2_rotated$log_transformed, by=list(Description=test5_rotated$MatClass), FUN=sum)

totaldata2 <- totaldata2 %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

totaldata2$angle <- ifelse(totaldata2$x > 5,
                           0,
                           totaldata2$angle)
set.seed(123)
A9 <- ggplot(totaldata2,aes(label = Description, size=x,angle = angle,color = factor(sample.int(10, 8, replace = TRUE)))) +
  geom_text_wordcloud() + 
  scale_radius(range = c(0, 30), limits = c(0, 50)) +
  used_theme +
  ggtitle("Total") +
  theme(plot.title = element_text(hjust = 0.5, size =25)) 
A9

p9 <- grid.arrange(A1, A2, A3, A4, A5, A6, A7, A8, A9, nrow = 3)
ggsave("figures/Figure4.jpg", p9, width = 12, height = 12, dpi=500)

