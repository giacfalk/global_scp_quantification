
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

##########

load("data_and_sources_dimensions/regions_all_data.Rdata")

####

# positive indicators
pos_indic <- c()

# negative indicators
neg_indic <- setdiff(colnames(cities), pos_indic)

###

# source("scripts/table_indicators.R")

###

remove_uppercase_items <- function(vector) {
  vector[!grepl("[A-Z]", vector) & !grepl("geom", vector)& !grepl("dim", vector)]
}

neg_indic <- remove_uppercase_items(neg_indic)

# define weights

if(exclude_climate_dim_SCPI==T){
  weights = data.frame(indic=c(pos_indic, neg_indic[-1]), weight=NA)
  weights$weight <- 1/nrow(weights)} else{
    
    weights = data.frame(indic=c(pos_indic, neg_indic), weight=NA)
    weights$weight <- 1/nrow(weights)
  }

###

cities_norm <- cities
cities_norm$geometry <- NULL

# weight them and calculate index

# SCPI by variables

cities_norm$SCPI_byvar <- rowSums(cities_norm[,c(match(weights$indic, colnames(cities_norm)))] * weights[,c(2)], na.rm = T)

weighted.mean(cities_norm$SCPI_byvar, cities_norm$P15)

###

# SCPI by dimensions

cities_norm$SCPI <- rowMeans(data.frame(cities_norm$dim1_climate, cities_norm$dim2_infra_assets, cities_norm$dim3_social_thermal_ineq, cities_norm$dim4_health, cities_norm$dim5_edu_work_stds))

weighted.mean(cities_norm$SCPI, cities_norm$P15)

###

plot(cities_norm$SCPI_byvar, cities_norm$SCPI)
cor(cities_norm$SCPI_byvar, cities_norm$SCPI)

###

cities_norm$GRGN_L1 <- countrycode::countrycode(cities_norm$ISO, 'iso2c', 'region')

###

cities_norm = cities_norm %>% group_by(GRGN_L1) %>% dplyr::mutate(ordvar=mean(SCPI, na.rm=T)) %>% ungroup()
cities_norm$GRGN_L1 <- ifelse(grepl("Latin", cities_norm$GRGN_L1), "Latin America & Carib.", cities_norm$GRGN_L1)

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.5))
  names(out) <- c("y")
  return(out) 
}

###

library(rnaturalearth)

wrld_simpl_sf <- rnaturalearth::countries110
colnames(wrld_simpl_sf)[47] <- "ISO3"
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

sf::sf_use_s2(F)

###

# SCPI_top_dim <-  cities_norm %>% group_by(ISO) %>% dplyr::summarise(dim1_climate=weighted.mean(dim1_climate, P15, na.rm=T), dim2_infra_assets=weighted.mean(dim2_infra_assets, P15, na.rm=T), dim3_social_thermal_ineq=weighted.mean(dim3_social_thermal_ineq, P15, na.rm=T), dim4_health=weighted.mean(dim4_health, P15, na.rm=T), dim5_edu_work_stds=weighted.mean(dim5_edu_work_stds, P15, na.rm=T))

###

cities_norm <- st_set_geometry(cities_norm, cities$geometry)

a <- ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="white", lwd=0.25)+
  geom_sf(data=cities_norm, aes(fill=SCPI*100), colour="white", lwd=0.001)+
  geom_sf(data=wrld_simpl_sf, fill="transparent", colour="black", lwd=0.25)+
  scale_fill_distiller(name="SCPI", palette = "YlOrRd", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")

###

cities_norm_export = cities_norm %>% dplyr::select(SCPI, geometry) %>% dplyr::mutate(SCPI=SCPI*100)
write_sf(cities_norm_export, "cities_norm_export.shp")

###

ggsave("figures/SCPI_map_regions.png", a, height = 2.25, width = 5, scale=3, bg="white")
ggsave("figures/SCPI_map_regions.pdf", a, height = 2.25, width = 5, scale=3, bg="white")

b <- ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="white", lwd=0.25)+
  geom_sf(data=cities_norm, aes(fill=(n_dim_deprived_3+n_dim_deprived_4+n_dim_deprived_5)*100), colour="white", lwd=0.001)+
  geom_sf(data=wrld_simpl_sf, fill="transparent", colour="black", lwd=0.25)+
  scale_fill_distiller(name="%", palette = "YlOrRd", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")+
  ggtitle("Share of total households deprived along at least three dimensions of SCP")

ggsave("figures/SCPI_map_hotspots.png", b, height = 2.25, width = 5, scale=3, bg="white")
ggsave("figures/SCPI_map_hotspots.pdf", b, height = 2.25, width = 5, scale=3, bg="white")


###

b <- pblapply(unique(cities_norm$ISO), function(ctry){ ggplot(data=cities_norm %>% filter(ISO==ctry))+
    theme_void()+
    geom_sf(aes(fill=SCPI*100), colour="white", lwd=0.001)+
    scale_fill_distiller(name="SCPI", palette = "YlOrRd", direction = 1, limits=c(20, 62))+
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(0.6, 'cm'), legend.key.height = unit(0.1, 'cm'),   plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
    xlab("")+
    ylab("")+
    ggtitle(ctry)})


library(patchwork)

plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}


b <- plot_a_list(b, 7, 4)

ggsave("figures/SCPI_map_regions_ctrys.png", b, height = 4, width = 3.75, scale=3.25, bg="white")
ggsave("figures/SCPI_map_regions_ctrys.pdf", b, height = 4, width = 3.75, scale=3.25, bg="white")

###

cities_norm <- cities_norm %>% group_by(ISO) %>% dplyr::mutate(SCPI_df=(SCPI-mean(SCPI, na.rm=T))*100)

###

View(cities_norm %>% st_set_geometry(NULL) %>% group_by(ISO) %>% dplyr::summarise(sd_scpi = sd(SCPI, na.rm=T)/mean(SCPI, na.rm=T)))

library(xtable)

xtable::xtable(as.data.frame(cities_norm %>% st_set_geometry(NULL) %>% group_by(ISO) %>% dplyr::summarise(CV_scpi = round(sd(SCPI, na.rm=T)/mean(SCPI, na.rm=T), 3)) %>% arrange(desc(CV_scpi))))

View(cities_norm %>% st_set_geometry(NULL) %>% group_by(ISO) %>% dplyr::summarise(diff_dim1 = max(dim1_climate, na.rm=T) - min(dim1_climate, na.rm=T), diff_dim2 = max(dim2_infra_assets, na.rm=T) - min(dim2_infra_assets, na.rm=T), diff_dim3 = max(dim3_social_thermal_ineq, na.rm=T) - min(dim3_social_thermal_ineq, na.rm=T), diff_dim4 = max(dim4_health, na.rm=T) - min(dim4_health, na.rm=T), diff_dim5 = max(dim5_edu_work_stds, na.rm=T) - min(dim5_edu_work_stds, na.rm=T)))


###


library(ggh4x)

cities_norm$SCPI_df <- ifelse(cities_norm$SCPI_df<(-5), -5, cities_norm$SCPI_df)
cities_norm$SCPI_df <- ifelse(cities_norm$SCPI_df>5, 5, cities_norm$SCPI_df)


b <- pblapply(unique(cities_norm$ISO), function(ctry){ ggplot()+
    theme_void()+
    geom_sf(data=regions %>% filter(ISO_A2==ctry), colour="black", lwd=0.1, datum=NA)+
    geom_sf(data=cities_norm %>% filter(ISO==ctry) %>% st_transform(3395), aes(fill=(SCPI_df)), colour="black", lwd=0.1, datum=NA)+
    scale_fill_gradient2(name="difference from mean national SCPI", limits=c(-5, 5), low="#30123BFF", midpoint = 0, mid = "white", high="#7A0403FF")+
    theme(legend.position="none")+
    xlab("")+
    ylab("")+
    ggtitle(paste0(countrycode::countrycode(ctry, 'iso2c', 'country.name'), ", ", round(cities_norm %>% filter(ISO==ctry) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15, na.rm=T)) %>% pull(SCPI), 2)))
})

names(b) <- unique( cities_norm$ISO)

###

library(patchwork)

plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols, guides = "collect") & theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(0.6, 'cm'), legend.key.height = unit(0.1, 'cm'),   plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm")) & force_panelsizes(rows = unit(1.5, "in"),
                                                                                                                                                                                                                                                                                                           cols = unit(1.35, "in"))
}


b_plot <- plot_a_list(b, 7, 4) 

ggsave("figures/SCPI_map_regions_ctrys_diff.png", b_plot, height = 4, width = 3, scale=3.25, bg="white")
ggsave("figures/SCPI_map_regions_ctrys_diff.pdf", b_plot, height = 4, width = 3.75, scale=3.25, bg="white")

##

ctry <- "IN"

legend_plot <- ggplot()+
  theme_void()+
  geom_sf(data=regions %>% filter(ISO_A2==ctry), colour="black", lwd=0.1, datum=NA)+
  geom_sf(data=cities_norm %>% filter(ISO==ctry) %>% st_transform(3395), aes(fill=(SCPI_df)), colour="black", lwd=0.1, datum=NA)+
  scale_fill_gradient2(name="difference from mean national SCPI", limits=c(-5, 5), low="#30123BFF", midpoint = 0, mid = "white", high="#7A0403FF")+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(0.6, 'cm'), legend.key.height = unit(0.1, 'cm'),   plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")+
  ggtitle(paste0(countrycode::countrycode(ctry, 'iso2c', 'country.name'), ", ", round(cities_norm %>% filter(ISO==ctry) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15, na.rm=T)) %>% pull(SCPI), 2)))

ggsave("figures/SCPI_map_regions_ctrys_diff_legend.png", legend_plot
, height = 6, width = 6, scale=1, bg="white")


##

for(reg in unique(cities_norm$GRGN_L1)){
  
  plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
    
    patchwork::wrap_plots(master_list_with_plots, 
                          nrow = no_of_rows, ncol = no_of_cols, guides = "collect") & theme(legend.position = 'bottom')
  }
  
  bb <- b[which(cities_norm$GRGN_L1[match(names(b), cities_norm$ISO)]==reg)]
  
  bb <- plot_a_list(bb, no_of_cols = length(bb), no_of_rows = 1) 
  
  ggsave(paste0("figures/SCPI_map_regions_ctrys_diff", reg, ".png"), bb + theme(legend.position="none"), height = 4, width = 3.75, scale=3.25, bg="white")
  ggsave(paste0("figures/SCPI_map_regions_ctrys_diff", reg, ".pdf"), bb + theme(legend.position="none"), height = 4, width = 3.75, scale=3.25, bg="transparent")
  
  library(ggpubr)
  
  ggsave(paste0("figures/SCPI_map_regions_ctrys_diff_legend.pdf"), ggpubr::as_ggplot(ggpubr::get_legend(bb)), height = 1, width = 3.75, scale=3.25, bg="transparent")
  ggsave(paste0("figures/SCPI_map_regions_ctrys_diff_legend.png"), ggpubr::as_ggplot(ggpubr:::get_legend(bb)), height = 1, width = 3.75, scale=3.25, bg="transparent")
  
}

###

for(reg in unique(cities_norm$ISO)){
  
  bb <- b[which(cities_norm$ISO[match(names(b), cities_norm$ISO)]==reg)]
  
  bb <- plot_a_list(bb, no_of_cols = 1, no_of_rows = 1) 
  
  
  ggsave(paste0("figures/SCPI_map_ctrys_diff", reg, ".pdf"), bb + theme(legend.position="none") + ggtitle(""), height = 4, width = 4, bg="transparent")
  
  ggsave(paste0("figures/SCPI_map_ctrys_diff", reg, ".png"), bb + theme(legend.position="none", lot.title = element_text(size = 32)), height = 4, width = 4, bg="transparent")
  
  
}

ggsave(paste0("figures/SCPI_map_ctrys_diff", "_legend", ".png"), get_legend(bb), height = 0.5, width = 6, bg="transparent")


###


library(magick)

dep_order <- cities_norm %>% st_set_geometry(NULL) %>% dplyr::group_by(ISO) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15, na.rm=T)) %>% mutate(id=as.numeric(row.names(.))) %>% arrange(desc(SCPI)) %>% pull(id)

list_panels <- lapply(unique(cities_norm$ISO)[dep_order], function(X){image_read(paste0("figures/SCPI_map_ctrys_diff", X, ".png"))  %>% image_border(geometry = "25x25", color = "white")})

# Creating a grid with 2 columns (manually arranging)
row1 <- image_append(c(list_panels[[1]], list_panels[[2]], list_panels[[3]], list_panels[[4]], list_panels[[5]], list_panels[[6]]), stack = FALSE)  # First row (horizontal)
row2 <- image_append(c(list_panels[[7]], list_panels[[8]], list_panels[[9]], list_panels[[10]], list_panels[[11]], list_panels[[12]]), stack = FALSE)  # Second row (horizontal)
row3 <- image_append(c(list_panels[[13]], list_panels[[14]], list_panels[[15]], list_panels[[16]], list_panels[[17]], list_panels[[18]]), stack = FALSE)  # Second row (horizontal)
row4 <- image_append(c(list_panels[[19]], list_panels[[20]], list_panels[[21]], list_panels[[22]], list_panels[[23]], list_panels[[24]]), stack = FALSE)  # Second row (horizontal)
row5 <- image_append(c(list_panels[[25]], list_panels[[26]], list_panels[[27]], list_panels[[28]]), stack = FALSE)  # Second row (horizontal)

# Stack rows to form a 2-column structure
grid_image <- image_append(c(row1, row2, row3, row4, row5), stack = TRUE)

img_with_bg <- image_background(grid_image, "white", flatten = TRUE)
image_write(img_with_bg, path = "figures/figure5.png", format = "png")


###

cities_norm_subnat <- cities_norm
cities_norm_subnat$geometry <- NULL
cities_norm_subnat <- dplyr::select(cities_norm_subnat, ISO, DHSREGEN, SCPI, SCPI_df, starts_with("dim"))
View(cities_norm_subnat)

options(dplyr.print_max = 30)

cities_norm_subnat %>% group_by(ISO) %>% dplyr::summarise(dim1_climate=max(dim1_climate)-min(dim1_climate)) %>% arrange(desc(dim1_climate))
cities_norm_subnat %>% group_by(ISO) %>% dplyr::summarise(dim2_infra_assets=max(dim2_infra_assets)-min(dim2_infra_assets)) %>% arrange(desc(dim2_infra_assets))
cities_norm_subnat %>% group_by(ISO) %>% dplyr::summarise(dim3_social_thermal_ineq=max(dim3_social_thermal_ineq)-min(dim3_social_thermal_ineq)) %>% arrange(desc(dim3_social_thermal_ineq))
cities_norm_subnat %>% group_by(ISO) %>% dplyr::summarise(dim4_health=max(dim4_health)-min(dim4_health)) %>% arrange(desc(dim4_health))
cities_norm_subnat %>% group_by(ISO) %>% dplyr::summarise(dim5_edu_work_stds=max(dim5_edu_work_stds)-min(dim5_edu_work_stds)) %>% arrange(desc(dim5_edu_work_stds))

###

plot_c <- ggplot(data= cities_norm, aes(y=SCPI*100, x=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)]), fill=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])))+
  theme_classic()+
  geom_boxplot(outlier.alpha = 0)+
  xlab("")+
  ylab("SCPI")+
  scale_fill_brewer(name="", palette="Set2")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", legend.direction = "horizontal")+
  stat_summary(fun=median.quartile,geom='point', colour="black", size=2)

ggsave("figures/SCPI_histogram_regions.png", plot_c+ theme(legend.position="none"), height = 2.25, width = 3, scale=3, bg="white")
ggsave("figures/SCPI_histogram_regions.pdf", plot_c+ theme(legend.position="none"), height = 2.25, width = 3, scale=3, bg="transparent")


###

cities_norm$P15_orig <- cities$P15

plot_d <- ggplot(data= cities_norm %>% group_by(GRGN_L1) %>%  arrange(SCPI) %>% dplyr::mutate(P15_cumsum=cumsum(P15_orig)/1e9) %>% ungroup(), aes(y=SCPI*100, x=P15_cumsum, group=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)]), colour=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])))+
  theme_classic()+
  geom_step()+
  xlab("Cum. pop.(billion)")+
  scale_y_continuous(position = "right")+
  ylab("SCPI")+
  scale_colour_brewer(name="", palette="Set2")+
  theme(axis.text.y = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_cumplot_regions.png", plot_d+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="white")
ggsave("figures/SCPI_cumplot_regions.pdf", plot_d+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="transparent")

###

plot_e <- ggplot(data= cities_norm %>% group_by(GRGN_L1) %>%  arrange(SCPI) %>% dplyr::mutate(P15_cumsum=(cumsum(P15_orig)/sum(P15_orig))*100) %>% ungroup(), aes(y=SCPI*100, x=P15_cumsum, group=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)]), colour=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])))+
  theme_classic()+
  geom_step()+
  xlab("Cum. pop.(%)")+
  # ylab("SCPI")+
  scale_colour_brewer(name="", palette="Set2")+
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(),    # Remove y-axis text
        axis.ticks.y = element_blank(),   # Remove y-axis ticks
        axis.title.y = element_blank(), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_cumplot_pctg_regions.png", plot_e+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="white")
ggsave("figures/SCPI_cumplot_pctg_regions.pdf", plot_e+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="transparent")


###

gdp <- raster("data_and_sources_dimensions/GDP2030_ssp2.tif")

cities_norm$GDP15_SM <- exact_extract(gdp, cities_norm, "sum")

library(ggpmisc)

plot_f <- ggplot(data= cities_norm %>% st_set_geometry(NULL) %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6) %>% ungroup(), aes(x=gdp_pc, y=SCPI*100))+
  theme_classic()+
  geom_point(aes(colour=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)]), size=P15))+
  ggpmisc::stat_poly_line( alpha=0.1) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2"))) +
  xlab("GDP per capita (2015 USD)")+
  ylab("SCPI")+
  scale_colour_brewer(name="", palette="Set2")+
  scale_x_continuous( labels = scales::number)+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")+
  scale_size_continuous(breaks=c(1, 5, 10))

ggsave("figures/SCPI_ctrs_regions.png", plot_f+ theme(legend.position="none"), height = 3, width = 3.5, scale=1.7, bg="white")
ggsave("figures/SCPI_ctrs_regions.pdf", plot_f+ theme(legend.position="none"), height = 3, width = 3.5, scale=1.7, bg="transparent")

cities_norm$P15_orig <- cities$P15

library(ggrepel)

scpi_against_climate <- ggplot(data= cities_norm %>% st_set_geometry(NULL) %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  v1_wbt = weighted.mean(v1_wbt, P15_orig, na.rm=T)) %>% ungroup(), aes(x=v1_wbt*100, y=SCPI*100, label=ISO))+
  geom_vline(xintercept = 58, linetype="dashed")+
  geom_hline(yintercept = 36, linetype="dashed")+
  theme_classic()+
  geom_point(aes(colour=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])))+
  geom_label_repel()+
  xlab("% of climate deprived pop.")+
  ylab("SCPI (without climate dimension)")+
  scale_colour_brewer(name="", palette="Set2")+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position = "bottom", legend.direction = "horizontal")+
  scale_size_continuous(breaks=c(1, 5, 10))

ggsave("figures/scpi_against_climate.png", scpi_against_climate+ theme(legend.position="none"), height = 3, width = 3.5, scale=1.7, bg="white")
ggsave("figures/scpi_against_climate.pdf", scpi_against_climate+ theme(legend.position="none"), height = 3, width = 3.5, scale=1.7, bg="transparent")


###

plot_g <- ggplot(data= cities_norm %>% st_set_geometry(NULL) %>%  dplyr::mutate(SCPI=cut(SCPI, breaks=c(-Inf, 0.15, 0.3, 0.45, 0.6, 0.75, Inf), labels=c("0-15", "15-30", "30-45", "45-60", "60-75", "75-100"))) %>% dplyr::group_by(GRGN_L1, SCPI) %>% dplyr::summarise(P15_orig=sum(P15_orig, na.rm = T)/1e9) %>% ungroup(), aes(x=SCPI , y=P15_orig, group=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)]), fill=factor(GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])))+
  theme_classic()+
  geom_bar(position = "stack", stat="sum", show.legend=c(size=FALSE))+
  ylab("Cum. pop.(billion)")+
  xlab("SCPI")+
  scale_fill_brewer(name="", palette="Set2")+
  theme(axis.text.x = element_text(angle = 0, hjust=1, size = ), legend.position = "bottom", legend.direction = "horizontal")

ggsave("figures/SCPI_colplot_regions.png", plot_g+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="white")
ggsave("figures/SCPI_colplot_regions.pdf", plot_g+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="transparent")

library(ggpubr)

#

SCPI_top_dim <-  cities_norm %>% dplyr::select(GRGN_L1, starts_with("dim"), P15)
SCPI_top_dim$geometry = NULL

library(reshape2)
SCPI_top_dim <- melt(SCPI_top_dim, c(1,7))

SCPI_top_dim$variable = factor(SCPI_top_dim$variable, levels = rev(levels(SCPI_top_dim$variable)), labels=rev(c("Climate", "Infrastr. and assets", "Social & thermal ineq.", "Health", "Edu. & work. stds.")))

SCPI_top_dim <- SCPI_top_dim %>% group_by(GRGN_L1, variable) %>% dplyr::summarise(value=sum(value*P15, na.rm=T))
SCPI_top_dim$GRGN_L1 <- factor(SCPI_top_dim$GRGN_L1, levels=unique(cities_norm$GRGN_L1)[c(4,1,5,3,2,6)])

plot_h <- ggplot(SCPI_top_dim)+
  theme_classic()+
  geom_col(aes(y=variable, x=value, fill=GRGN_L1), position = position_stack(reverse = T))+
  scale_x_continuous(labels = function(x) x / 1e9)+
  scale_y_discrete(drop=FALSE)+
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(6, "Set2"))+
  ylab("")+
  xlab("Billion people deprived")+
  theme(axis.text.x = element_text(angle = 0, hjust=0.3), legend.position = "bottom", legend.direction = "horizontal")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

ggsave("figures/SCPI_count_bydim.png", plot_h+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="white")
ggsave("figures/SCPI_count_bydim.pdf", plot_h+ theme(legend.position="none"), height = 3, width = 3, scale=1.7, bg="transparent")

#

#####################
#####################

(plot_c + ggtitle("b") + theme(legend.position = "none", axis.text.x=element_blank(), plot.title = element_text(size = 14)) + plot_e + ggtitle("c") + theme(legend.position = "none", plot.title = element_text(size = 14)) + plot_h + ggtitle("d") + theme(legend.position = "none", plot.title = element_text(size = 14))) + plot_layout(ncol=3, widths = c(1,1,1))

ggsave("figures/composite.pdf", height = 1.3, width = 4, scale=3.5, bg="white")

###

ggsave("figures/legend.pdf", as_ggplot(ggpubr::get_legend(plot_g)), height = 0.35, width = 3.7, scale=2.5, bg="white")

#####################
#####################

source("scripts/make_radar_plots.R")

