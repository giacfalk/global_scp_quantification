
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
library(rworldmap)
library(writexl)
library(readxl)
library(googledrive)
library(fasterize)
library(matrixStats)
library(maptools)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

##########

load("data_and_sources_dimensions/regions_all_data.Rdata")

##########

# plot heat hazard

data(wrld_simpl)
wrld_simpl_sf <- st_as_sf(wrld_simpl)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

sf::sf_use_s2(F)

###

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.1)+
  geom_sf(data=cities, aes(fill=wb_t_max), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Wetbulb max T (° C)", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")


ggsave("figures/wb_t_regions.png", height = 2.25, width = 5, scale=3, bg="white")

# plot heat exposure

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities, aes(fill=wb_t_max*P15/1e6), colour="white", lwd=0.1)+
  scale_fill_viridis_c(name="Wetbulb max T (° C) x million people", trans="log10")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/heat_exposure_total_regions.png", height = 2.25, width = 5, scale=3, bg="white")

# plot green_blue_infra_share

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities, aes(fill=green_blue_infra_share), colour="white", lwd=0.1)+
  scale_fill_viridis_c(name="Density of green and blue areas")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/green_blue_infra_share_regions.png", height = 2.25, width = 5, scale=3, bg="white")

# plot fridge_ownership_pctg

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities, aes(fill=fridge_ownership_pctg), colour="white", lwd=0.1)+
  scale_fill_viridis_c(name="Fridge ownership rate")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/fridge_ownership_pctg_regions.png", height = 2.25, width = 5, scale=3, bg="white")

# plot people_70_plus_pctg

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities, aes(fill=people_70_plus_pctg), colour="white", lwd=0.1)+
  scale_fill_viridis_c(name="Prevalence of individuals aged 70+", trans="log10")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/people_70_plus_pctg_regions.png", height = 2.25, width = 5, scale=3, bg="white")

# plot ac

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities, aes(fill=ac), colour="white", lwd=0.1)+
  scale_fill_viridis_c(name="Prevalence of air conditioning ownership")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/ac_regions.png", height = 2.25, width = 5, scale=3, bg="white")
