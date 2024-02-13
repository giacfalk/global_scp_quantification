
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

load("data_and_sources_dimensions/cities_all_data.Rdata")

# how many people are covered by the analysis?

sum(cities$P15) / 1e9

####

# normalise the variables and adjust the sign

normalize_variable <- function(variable) {
  min_val <- min(variable, na.rm = TRUE)
  max_val <- max(variable, na.rm = TRUE)
  
  normalized <- (variable - min_val) / (max_val - min_val)
  return(normalized)
}


##

cities <- dplyr::select(cities, -c(1:10, 12:15, 17, 19:44, 46:52, 54:160))

# positive indicators
pos_indic <- c("green_blue_infra_share", "ac", "radio_ownership_pctg", "tv_ownership_pctg", "cellphone_ownership_pctg", "computer_ownership_pctg", "multiple_information_means_ownership_pctg", "fridge_ownership_pctg", "primary_education_rate_women", "primary_education_rate_men")

# negative indicators
neg_indic <- setdiff(colnames(cities), pos_indic)

###

source("script/table_indicators.R")

###

remove_uppercase_items <- function(vector) {
  vector[!grepl("[A-Z]", vector) & !grepl("geom", vector)]
}

neg_indic <- remove_uppercase_items(neg_indic)


# define weights

weights = data.frame(indic=c(pos_indic, neg_indic), weight=NA)
weights$weight <- 1/nrow(weights)

###

cities_norm <- cities
cities_norm$geom <- NULL
cities_norm <- dplyr::select(cities_norm, 1:4, weights$indic)

for (i in 1:nrow(weights)){
cities_norm[,weights$indic[i]] <- ifelse(weights$indic[i] %in% neg_indic, normalize_variable(cities_norm[,weights$indic[i]]), -(normalize_variable(cities_norm[,weights$indic[i]])))
}

# weight them and calculate index

cities_norm$SCPI <- rowSums(cities_norm[,c(pos_indic, neg_indic)] * weights[,c(2)], na.rm = T)
  
cities_norm$SCPI <- normalize_variable(cities_norm$SCPI)

###

cities_norm = cities_norm %>% group_by(GRGN_L1) %>% dplyr::mutate(ordvar=median(SCPI, na.rm=T)) %>% ungroup()
cities_norm$GRGN_L1 <- ifelse(grepl("Latin", cities_norm$GRGN_L1), "Latin America & Carib.", cities_norm$GRGN_L1)

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.5))
  names(out) <- c("y")
  return(out) 
}

###

data(wrld_simpl)
wrld_simpl_sf <- st_as_sf(wrld_simpl)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

sf::sf_use_s2(F)

cities_norm <- st_set_geometry(cities_norm, cities$geom)

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities_norm), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities_norm), aes(colour=SCPI), size=1.8)+
  scale_colour_viridis_c(name="SCPI", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

ggsave("figures/SCPI_map.png", height = 2.25, width = 5, scale=3, bg="white")


###

# add radar plot

cities_subset <- c("Rome", "Milan", "Turin")
source("scripts/make_radar_plots.R")

###

ggplot(data= cities_norm, aes(y=SCPI, x=reorder(GRGN_L1, -ordvar), fill=GRGN_L1))+
  theme_classic()+
  geom_violin()+
  xlab("")+
  ylab("SCPI")+
  scale_fill_discrete(name="")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", legend.direction = "horizontal")+
  stat_summary(fun=median.quartile,geom='point', colour="black", size=2)

ggsave("figures/SCPI_histogram.png", height = 2.25, width = 3, scale=3, bg="white")


###

cities_norm$P15_orig <- cities$P15

ggplot(data= cities_norm %>% group_by(GRGN_L1) %>%  arrange(SCPI) %>% dplyr::mutate(P15_cumsum=cumsum(P15_orig)/1e9) %>% ungroup(), aes(x=SCPI, y=P15_cumsum, group=GRGN_L1, colour=GRGN_L1))+
  theme_classic()+
  geom_step()+
  ylab("Cum. pop.(billion)")+
  xlab("SCPI")+
  scale_colour_discrete(name="")+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_cumplot.png", height = 3, width = 3, scale=1.7, bg="white")

###

ggplot(data= cities_norm %>% group_by(GRGN_L1) %>%  arrange(SCPI) %>% dplyr::mutate(P15_cumsum=(cumsum(P15_orig)/sum(P15_orig))*100) %>% ungroup(), aes(x=SCPI, y=P15_cumsum, group=GRGN_L1, colour=GRGN_L1))+
  theme_classic()+
  geom_step()+
  ylab("Cum. pop.(%)")+
  xlab("SCPI")+
  scale_fill_discrete(name="")+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_cumplot_pctg_regions.png", height = 3, width = 3, scale=1.7, bg="white")

###

cities_norm$GDP15_SM <- cities$GDP15_SM

ggplot(data= cities_norm %>% st_set_geometry(NULL) %>%  group_by(CTR_MN_ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig), P15=sum(P15_orig)/1e6) %>% ungroup())+
  theme_classic()+
  geom_point(aes(x=SCPI, y=gdp_pc, colour=GRGN_L1, size=P15))+
  geom_smooth(aes(x=SCPI, y=gdp_pc), method = "lm")+
  ylab("GDP per capita (2015)")+
  xlab("SCPI")+
  scale_colour_discrete(name="")+
  scale_y_continuous(trans="log10", labels = scales::number, limits = c(750, 75000))+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")+
  scale_size_continuous(breaks=c(10, 50, 100))

ggsave("figures/SCPI_ctrs.png", height = 3, width = 3.5, scale=1.7, bg="white")

###

ggplot(data= cities_norm, aes(x=cut(SCPI, breaks=c(-Inf, 0.25, 0.5, 0.75, Inf), labels=c("0-25", "25-50", "50-75", "75-100")), y=P15_orig/1e9, group=GRGN_L1, fill=GRGN_L1))+
  theme_classic()+
  geom_bar(position = "stack", stat="sum")+
  ylab("Cum. pop.(billion)")+
  xlab("SCPI")+
  scale_colour_discrete(name="")+
  theme(axis.text.x = element_text(angle = 0, hjust=1, size = ), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_colplot.png", height = 3, width = 3, scale=1.7, bg="white")
