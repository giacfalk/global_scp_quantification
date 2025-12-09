
cities <- read_sf("data_and_sources_dimensions/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
cities <- cities %>% group_by(CTR_MN_ISO) %>% slice_max(P15, n = n_largest_cities_by_country)

wrld_simpl_sf <-  st_as_sf(world(path=getwd())) %>% dplyr::filter(NAME_0!="Antarctica")
colnames(wrld_simpl_sf)[1] <- "ISO3"
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

sf::sf_use_s2(F)

###

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities), colour="orange", size=1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")


ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf %>% filter(GID_0=="AFG"), fill="lightgrey", colour="black", lwd=0.25)+
  geom_sf(data=cities[1,], fill="orange", colour="black", lwd=0.25)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

###

dhs_regions<- read_sf("data_and_sources_dimensions/4_Health/Child mortality/shps/sdr_subnational_data.shp")

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.1)+
  geom_sf(data=dhs_regions, fill="orange", colour="white", lwd=0.1)+
  scale_fill_distiller(name="Wetbulb max T (° C)", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")
