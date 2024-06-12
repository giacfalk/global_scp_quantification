
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
  geom_sf(data=cities, aes(fill=v1_wbt), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Climate, deprivation index", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/v1_wbt_regions.png", height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities, aes(fill=v4a_water_quality*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Water quality, % of population deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/v4a_water_quality_regions.png", height = 2.25, width = 5, scale=3, bg="white")

####

dhs_rasterss <- rast(paste0(stub,"results/rasters_indicators.tif"))

for (i in 1:nlyr(dhs_rasterss)){
png(paste0("figures/",names(dhs_rasterss)[i], ".png"), height = 600, width = 600)
plot(dhs_rasterss[[i]], main=names(dhs_rasterss)[i], col=map.pal("inferno", 50))
dev.off()
}
