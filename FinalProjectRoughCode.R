##R code
##Micaela Final Project
##Last Edit: November 8, 2021

library(tidyverse)
library(dplyr)
library(ggplot2) 
library(readr)
library(RCurl)
library(tidyverse)
library(maps)
library(ggthemes)
library(scales)
library(gganimate)
library(devtools)
library(urbnmapr)
library(readxl)
devtools::install_github("UrbanInstitute/urbnmapr")

#IMPORT DATA FROM DESKTOP
LongForm2 <- read_excel("~/Desktop/LongForm.xlsx")


#IMPORT DATA FROM GITHUB
LongForm <-read.csv("https://github.com/GEO511-2021/2021_project-MicaelaFLipman/blob/ea0119a7c180fabed5d48131db971a109c57de23/LongForm.csv")

##Merging Data
counties_sf <- get_urbn_map("counties", sf = TRUE) %>% 
  filter(state_abbv=="NY") 
AllCounties<-left_join(x=counties_sf, y=LongForm, by="county_name", all.x=TRUE) 

#Map1-StaticPercentageofCountyInFarms
static1<-ggplot() +
  geom_sf(
    data = AllCounties,
    mapping = aes(
                fill = PercTotAcresInFarm, 
                geometry = geometry),
                color = NA) +
  scale_fill_distiller(
    palette = "YlGn", 
    direction = 1,
    limits=c(0,1),
    n.breaks = 6,
    name = "Proportion of acres in farmland",
    guide = "colorbar") +
  labs(
    title = "Proportion of County Acreage in Farmland: 1910-2017",
    x = NULL,
    y = NULL) +
  theme_map() +
  theme(
    legend.position = "bottom") 

anim1 <- static1 +
  transition_manual(Year) + 
  ease_aes('linear')

loop <- animate(
  anim1,
  fps = 10,          
  nframes = 22,  
  end_pause = 3      
) + 
  labs(subtitle = {year})

anim1

##Faceted Plot 1
facet1<-ggplot() +
  geom_sf(
    data = AllCounties,
    mapping = aes(
      fill = PercTotAcresInFarm, 
      geometry = geometry),
    color = NA) +
  scale_fill_distiller(
    palette = "YlGn", 
    direction = 1,
    limits=c(0,1),
    n.breaks = 6,
    name = "Proportion of acres in farmland",
    guide = "colorbar") +
  labs(
    title = "Proportion of County Acreage in Farmland: 1910-2017",
    x = NULL,
    y = NULL) +
  theme_map() +
  theme(
    legend.position = "bottom") + 
  facet_wrap(facets=.~Year)
facet1
##Map 2 land in farms 
static2<-ggplot() +
  geom_sf(
    data = AllCounties,
    mapping = aes(
                fill = LandInAcres, 
                geometry = geometry),
                color = NA) +
  scale_fill_distiller(
    palette = "YlGn", 
    direction = 1,
    limits=c(0,900000),
    n.breaks = 4,
    label=comma,
    name = "Acres in farmland",
    guide = "colorbar") +
  labs(
    title = "Acres in Farmland: 1910-2017", 
    x = NULL, 
    y = NULL) +
  theme_map() +
  theme(legend.position = "bottom") 

anim2 <- static2 +
  transition_manual(Year) + 
  ease_aes('linear')

loop <- animate(
  anim2,
  fps = 10,          
  nframes = 22,  
  end_pause = 3) + 
  labs(subtitle = {year})

anim2

##Faceted Plot 2
facet2<-ggplot() +
  geom_sf(
    data = AllCounties,
    mapping = aes(
      fill = LandInAcres, 
      geometry = geometry),
    color = NA) +
  scale_fill_distiller(
    palette = "YlGn", 
    direction = 1,
    limits=c(0,900000),
    n.breaks = 4,
    label=comma,
    name = "Acres in farmland",
    guide = "colorbar") +
  labs(
    title = "Acres in Farmland: 1910-2017", 
    x = NULL, 
    y = NULL) +
  theme_map() +
  theme(legend.position = "bottom") +
  facet_wrap(facets = ~ Year)
facet2


##Animated Scatterplot with acres in farms and number of farms
static3<-ggplot(
    AllCounties, 
    aes(
        LandInAcres, 
        NumberofFarms)) +
  geom_point(
    alpha = 0.7, 
    show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  theme_minimal() + 
  labs(
    title = "Erosion of Farms in New York State", 
    x = 'Farmland (Acres)', 
    y = 'Number Of Farms')

anim3 <- static3 +
  transition_manual(Year) + 
  ease_aes('linear')

loop <- animate(
  anim3,
  fps = 10,          
  nframes = 22,  
  end_pause = 3)

anim3


##Static Trend Line Plots

ggplot(
  data=AllCounties, 
  aes(
    x=Year, 
    y = AvSizeFarmAcres)) + 
  geom_smooth() + 
  theme_minimal() +
  labs(
    x='Year',
    y='Acres') +
  ggtitle("Average Size of Farms In New York State")

ggplot(
  data=AllCounties, 
  aes(
    x=Year, 
    y = LandInAcres)) + 
  geom_smooth() + 
  theme_minimal() +
  labs(
    x='Year',
    y='Acres') +
  ggtitle("Farm Acres In New York State")


ggplot(
  data=AllCounties, 
  aes(
    x=Year, 
    y = LandInAcresPerPopulation)) + 
  geom_smooth() + 
  theme_minimal() +
  labs(
    x='Year', 
    y = 'Acres/Person') +
  ggtitle("Acres of Farms per Person in New York State")



