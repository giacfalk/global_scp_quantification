
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
library(countrycode)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

##########

load("data_and_sources_dimensions/regions_all_data.Rdata")

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

barplot_ndim <- cities %>% dplyr::select(ISO, P15, starts_with("n_dim_deprived_")) %>% st_set_geometry(NULL)

barplot_ndim <- reshape2::melt(barplot_ndim, c(1,2))
barplot_ndim <- barplot_ndim %>% group_by(ISO, variable) %>% dplyr::summarise(value=weighted.mean(value, P15, na.rm=T))

barplot_a <- ggplot(barplot_ndim)+
  theme_classic()+
  geom_col(aes(x=countrycode(ISO, 'iso2c', 'iso3c'), y=value*100, fill=as.factor(parse_number(as.character(variable)))))+
  xlab("Country")+
  ylab("% of population")+
  scale_fill_brewer(name="# SCP dimensions deprived", palette = "Reds")

###

cities$GRGN_L1 <- countrycode::countrycode(cities$ISO, 'iso2c', 'region')


barplot_ndim2 <- cities %>% dplyr::select(ISO, GRGN_L1, P15, starts_with("n_dim_deprived_")) %>% st_set_geometry(NULL)

barplot_ndim2 <- reshape2::melt(barplot_ndim2, c(1,2,3))
barplot_ndim2 <- barplot_ndim2 %>% group_by(ISO, GRGN_L1, variable) %>% dplyr::summarise(value=sum(P15*value, na.rm=T))

#####

write.csv(barplot_ndim, "barplot_ndim_share_data.csv")
write.csv(barplot_ndim2, "barplot_ndim_count_data.csv")

####

barplot_b <- list()

for(regreg in unique(barplot_ndim2$GRGN_L1)){
  
  barplot_b[[as.character(regreg)]] <- ggplot(barplot_ndim2 %>% filter(GRGN_L1==regreg))+
    theme_classic()+
    geom_col(aes(x=countrycode(ISO, 'iso2c', 'iso3c'), y=value/1e6, fill=factor(parse_number(as.character(variable)), levels=c("5", "4", "3", "2", "1", "0"))))+
    xlab("Country")+
    ylab("Number of people (million)")+
    scale_fill_brewer(name="# SCP dimensions deprived", palette = "Reds")+
    theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5)) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE))
  
  lgt <- length(unique(barplot_ndim2 %>% filter(GRGN_L1==regreg) %>% pull(ISO)))
  
  ggsave(paste0("figures/barplots_", regreg, "_scpi.png"), barplot_b[[as.character(regreg)]] + theme(legend.position = "none"), width = lgt*(1.5), height = 3, scale=1.35)
  ggsave(paste0("figures/barplots_", regreg, "_scpi.pdf"), barplot_b[[as.character(regreg)]] + theme(legend.position = "none"), width = lgt*(1.5), height = 3, scale=1.35)
  
    ggsave(paste0("figures/barplots_legend_scpi.pdf"), ggpubr::as_ggplot(ggpubr::get_legend(barplot_a + theme(legend.position = "bottom", legend.direction = "horizontal")+guides(fill=guide_legend(nrow=1,byrow=TRUE)))), width = lgt*(1.5), height = 1, scale=2)
  
}

###

barplot_ndim2 <- cities %>% dplyr::select(ISO, P15, starts_with("n_dim_deprived_")) %>% st_set_geometry(NULL)

barplot_ndim2 <- reshape2::melt(barplot_ndim2, c(1,2))
barplot_ndim2 <- barplot_ndim2 %>% group_by(ISO, variable) %>% dplyr::summarise(value=sum(P15*value, na.rm=T))

barplot_b <- ggplot(barplot_ndim2 %>% filter(countrycode(ISO, 'iso2c', 'iso3c')!="IND"))+
  theme_classic()+
  geom_col(aes(x=countrycode(ISO, 'iso2c', 'iso3c'), y=value/1e6, fill=factor(parse_number(as.character(variable)), levels=c("5", "4", "3", "2", "1", "0"))))+
  xlab("Country")+
  ylab("Number of people (million)")+
  scale_fill_brewer(name="# SCP dimensions deprived", palette = "Reds")

barplot_b2 <- ggplot(barplot_ndim2 %>% filter(countrycode(ISO, 'iso2c', 'iso3c')=="IND"))+
  theme_classic()+
  geom_col(aes(x=countrycode(ISO, 'iso2c', 'iso3c'), y=value/1e6, fill=factor(parse_number(as.character(variable)), levels=c("5", "4", "3", "2", "1", "0"))), )+
  xlab("")+
  ylab("")+
  scale_fill_brewer(name="# SCP dimensions deprived", palette = "Reds")

library(patchwork)

barplot_b_m <- barplot_b + barplot_b2 + plot_layout(guides = "collect", widths = c(1, 0.1))

barplot_a + barplot_b_m + plot_layout(guides="collect", ncol=1) & theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5)) & guides(fill=guide_legend(nrow=1,byrow=TRUE))

ggsave("figures/barplots_scpi.png", width = 6, height = 6, scale=1.35)
ggsave("figures/barplots_scpi.pdf", width = 6, height = 6, scale=1.35)

###

for(ctry in unique(cities$ISO)){

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v1_wbt), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Climate, deprivation index", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v1_wbt_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v2_urban_planning), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Urban planning, deprivation index", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v2_urban_planning_regions", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v3_green_bleu_infra), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Green and blue urban infrastructure, deprivation index", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v3_green_bleu_infra_regions", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v4a_water_quality*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Water quality, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v4a_water_quality_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v4b_sanitation*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Water sanitation, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v4b_sanitation_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")


ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v5_housing*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Housing, % of households deprived", palette = "Reds", direction = 1)+
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v5_housing_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v7_cooling_assets*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Cooling assets, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v7_cooling_assets_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v7b_transport_assets*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Transport assets, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v7b_transport_assets_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")


ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v8_energy*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Energy, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v8_energy_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v9_justice*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Thermal inequality, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v9_justice_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v10_health1*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Diarrhea, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v10_health1_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")


ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v11_health2*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="NCDs issues, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v11_health2_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v12_health3*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Pregnancy issues, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v12_health3_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v12_health4*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Healthcare accessibility, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v12_health4_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")


ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v13_education*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Education, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v13_education_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")


ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v14_work*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Heat exposure at work, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v14_work_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v15_adapt_knowledge*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Heat adaptation knowledge, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v15_adapt_knowledge_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

ggplot()+
  theme_void()+
  geom_sf(data=cities %>% filter(ISO==ctry), aes(fill=v16_cooling_policy*100), colour="white", lwd=0.1)+
  scale_fill_distiller(name="Cooling policy and regulation, % of households deprived", palette = "Reds", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave(paste0("figures/v16_cooling_policy_regions_", ctry, ".png"), height = 2.25, width = 5, scale=3, bg="white")

####

dhs_rasterss <- stack(paste0(stub,"results/rasters_indicators.tif"))

library(rgis)

dhs_rasterss <- rgis::mask_raster_to_polygon(dhs_rasterss, cities %>% filter(ISO==ctry))

for (i in 1:nlayers(dhs_rasterss)){
png(paste0("figures/",names(dhs_rasterss)[i], "_", ctry, ".png"), height = 600, width = 600)
plot(dhs_rasterss[[i]], main=names(dhs_rasterss)[i], col=map.pal("elevation", 50))
dev.off()
}

}

