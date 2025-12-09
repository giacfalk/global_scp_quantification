
####

# Global systemic cooling poverty
# index calculation

####

setwd(stub)

####

library(terra)
library(exactextractr)
library(sf)
library(raster)
library(tidyverse)
library(pbapply)
library(geodata)
library(writexl)
library(readxl)
library(googledrive)
library(fasterize)
library(matrixStats)
library(maptools)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

##########

load("data_and_sources_dimensions/cities_all_data.Rdata")

##########

# plot heat hazard

wrld_simpl_sf <-  st_as_sf(world(path=getwd())) %>% dplyr::filter(NAME_0!="Antarctica")
colnames(wrld_simpl_sf)[1] <- "ISO3"
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

sf::sf_use_s2(F)

###

# plot wetbulb T

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=wb_t_max), size=1.8)+
  scale_colour_distiller(name="Wetbulb max T (° C)", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/wb_t_cities.png", height = 2.25, width = 5, scale=3, bg="white")

# plot heat exposure

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=wb_t_max*P15/1e6), size=1.8)+
  scale_colour_viridis_c(name="Wetbulb max T (° C) x million people", trans="log10")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/heat_exposure_total.png", height = 2.25, width = 5, scale=3, bg="white")

# plot green_blue_infra_share

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=green_blue_infra_share), size=1.8)+
  scale_colour_viridis_c(name="Density of green and blue areas", trans="log10")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/green_blue_infra_share_cities.png", height = 2.25, width = 5, scale=3, bg="white")

# plot fridge_ownership_pctg

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=fridge_ownership_pctg), size=1.8)+
  scale_colour_viridis_c(name="Fridge ownership rate")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/fridge_ownership_pctg_cities.png", height = 2.25, width = 5, scale=3, bg="white")

# plot people_70_plus_pctg

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=people_70_plus_pctg), size=1.8)+
  scale_colour_viridis_c(name="Prevalence of individuals aged 70+", trans="log10")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/people_70_plus_pctg_cities.png", height = 2.25, width = 5, scale=3, bg="white")

# plot ac

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=ac), size=1.8)+
  scale_colour_viridis_c(name="Prevalence of air conditioning ownership")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/ac_cities.png", height = 2.25, width = 5, scale=3, bg="white")


# plot policy

# plot ac

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities), aes(colour=mean_ac_reg), size=1.8)+
  scale_colour_viridis_c(name="AC sector regulatory quality index")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/ac_regulatory_index.png", height = 2.25, width = 5, scale=3, bg="white")

