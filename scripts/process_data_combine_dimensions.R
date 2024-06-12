
####

# Global systemic cooling poverty
# data processing

####

setwd(paste0(stub, "data_and_sources_dimensions"))

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
library(countrycode)
library(haven)
library(stringr)
library(stars)
library(sp)
library(automap) 

googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

####

# https://docs.google.com/spreadsheets/d/1RCgTnyEbomKqVAiIskorDkkHwzWe9e64RHV-Y0wrg8U/edit#gid=984566709

###

cities <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
cities <- cities %>% group_by(CTR_MN_ISO) %>% slice_max(P15, n = n_largest_cities_by_country)

write_sf(cities, "cities_selected.shp")

dhs_regions<- read_sf("4_Health/Child mortality/shps/sdr_subnational_data.shp") %>% dplyr::select(geometry, ISO, DHSREGEN) %>% write_sf("dhs_regions.shp") 

if((countrylist!="all")[1]){
  
  dhs_regions <- dhs_regions %>% filter(countrycode(ISO, 'iso2c', 'country.name') %in% countrylist)
}

countrylist <- countrycode(unique(dhs_regions$ISO), 'iso2c', 'country.name')

regions <- st_as_sf(rworldmap::countriesLow) %>% filter(SOVEREIGNT!="Antarctica")

gridded_pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

###

if (scale == "cities"){
  cities <- cities %>% filter(CTR_MN_NM %in% countrylist)
} else if (scale == "regions"){
  cities <- dhs_regions %>% filter(countrycode(ISO, 'iso2c', 'country.name') %in% countrylist)
}

###

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Clean Data/DHS_cleaned.dta")
r <- as.data.frame(r) %>% filter(country %in% countrylist)

r$weights <- r$weights_v <-  r$hv005
r$CTRY <- countrycode(r$country, 'country.name', 'iso2c')
r$DHSID <- paste0(r$CTRY, r$year, str_sub(paste0("00000000", r$hv001), start= -8))

###

s <- list.files(pattern=".shp", path="F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Raw Data", recursive = T, full.names = T)  
s <- s[!grepl("xml", s)]
s <- s[grepl(paste0(countrylist, collapse = "|"), s)]
s <- lapply(s, read_sf)
s <- bind_rows(s)
s$CTRY <- s$DHSCC

t <- list.files(pattern=".csv", path="F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Raw Data", recursive = T, full.names = T)  
t <- t[grepl(paste0(countrylist, collapse = "|"), t)]

t <- lapply(t, read.csv)
t <- bind_rows(t)
t$CTRY <-  t$DHSCC
t$DHSID <- NULL
t$DHSYEAR <- NULL

st <- merge(s, t, c("CTRY", "DHSCC", "DHSCLUST"))
rst <- merge(r, st, by=c("DHSID", "CTRY"))

###

rst <- st_as_sf(rst)

rst <- rst %>% st_transform(3395) %>% st_buffer(10000) %>% st_transform(4326)

####
## 1) Climate

# 1.1) Wet bulb temperature
fls1 <- drive_find(
  q = "name contains 'wetbulb_temp_ts_1980_2020_monthly_'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls1 <- fls1  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls1 <- fls1  %>% filter(!grepl("regions", name))}

fls1_d <- pblapply(fls1$id, drive_download, overwrite=T)
fls1_d <- bind_rows(fls1_d)
fls1_d_1 <- fls1_d[c(grep("max", fls1$name)),]
fls1_d_1 <- lapply(fls1_d_1$local_path, terra::rast)
fls1_d_2 <- fls1_d[c(grep("avg", fls1$name)),]
fls1_d_2 <- lapply(fls1_d_2$local_path, terra::rast)


wb_t_max <- if(length(fls1_d_1)>1) {do.call(mosaic, fls1_d_1)} else{fls1_d_1[[1]]}
wb_t_avg <- if(length(fls1_d_2)>1) {do.call(mosaic, fls1_d_2)} else{fls1_d_2[[1]]}

wb_t_max <- terra::mask(wb_t_max, vect(st_as_sfc(st_bbox(extent(cities)))))
wb_t_avg <- terra::mask(wb_t_avg, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, wb_t_max[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$wb_t_max <- exact_extract(tapp(wb_t_max, 1, max), rst, "mean")
rst$wb_t_avg <- exact_extract(tapp(wb_t_avg, 1, mean), rst, "mean")

###

fls1_b <- drive_find(
  q = "name contains 'wetbulb_temp_ts_2010_2020_ndays_above_30'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls1_b <- fls1_b  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls1_b <- fls1_b  %>% filter(!grepl("regions", name))}

fls1_b <- pblapply(fls1_b$id, drive_download, overwrite=T)
fls1_b <- bind_rows(fls1_b)
fls1_b <- lapply(fls1_b$local_path, terra::rast)

wb_t_gt_30_ndays <- if(length(fls1_b)>1) {do.call(mosaic, fls1_b)} else{fls1_b[[1]]}

wb_t_gt_30_ndays <- terra::mask(wb_t_gt_30_ndays, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, wb_t_gt_30_ndays[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$wb_t_gt_30_ndays <- exact_extract(wb_t_gt_30_ndays, rst, "mean")

####
## 2) Thermal comfort Infrastructures & Assets

# 2.1) Urban planning

fls2 <- drive_find(
  q = "name contains 'index_builtup'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls2 <- fls2  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls2 <- fls2  %>% filter(!grepl("regions", name))}

fls2 <- pblapply(fls2$id, drive_download, overwrite=T)
fls2 <- bind_rows(fls2)
fls2 <- lapply(fls2$local_path, terra::rast)

index_builtup <- if(length(fls2)>1) {do.call(mosaic, fls2)} else{fls2[[1]]}

index_builtup <- terra::mask(index_builtup, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, index_builtup[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$index_builtup <- exact_extract(index_builtup, rst, "mean")

####
# 2.2) Green & Blue  infrastructure

fls3 <- drive_find(
  q = "name contains 'share_green_blue_spaces'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls3 <- fls3  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls3 <- fls3  %>% filter(!grepl("regions", name))}

fls3 <- pblapply(fls3$id, drive_download, overwrite=T)
fls3 <- bind_rows(fls3)
fls3 <- lapply(fls3$local_path, terra::rast)

green_blue_infra_share <- if(length(fls3)>1) {do.call(mosaic, fls3)} else{fls3[[1]]}

green_blue_infra_share <- terra::mask(green_blue_infra_share, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, green_blue_infra_share[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$green_blue_infra_share <- exact_extract(green_blue_infra_share, rst, "mean")

####
# 2.3) Water quality and Sanitation

water <- raster("2_Thermal comfort Infrastructures & Assets/Water quality and Sanitation/IHME_LMIC_WASH_2000_2017_W_IMP_PERCENT_MEAN_2017_Y2020M06D02.TIF")
water <- projectRaster(water, raster(wb_t_max))
values(water) <- ifelse(is.na(values(water)), 100, values(water))
water <- rgis::mask_raster_to_polygon(water, regions)

###

sanitation <- raster("2_Thermal comfort Infrastructures & Assets/Water quality and Sanitation/IHME_LMIC_WASH_2000_2017_S_IMP_PERCENT_MEAN_2017_Y2020M06D02.TIF")
sanitation <- projectRaster(sanitation, raster(wb_t_max))
values(sanitation) <- ifelse(is.na(values(sanitation)), 100, values(sanitation))
sanitation <- rgis::mask_raster_to_polygon(sanitation, regions)

#######

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")
ac_pop <- rast("2_Thermal comfort Infrastructures & Assets/Assets/pop_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")

ac <- ac[[6]]
ac_pop <- ac_pop[[6]]

rst$ac <- exact_extract(ac, rst, "weighted_mean", weights=ac_pop)

ely_prices <- read_xls(paste0(getwd(), "/2_Thermal comfort Infrastructures & Assets/Energy/global-electricity-per-kwh-pricing-2021.xls"))
ely_prices$avg <- ely_prices$`Average price of 1KW/h (USD)`
ely_prices$low <- ely_prices$`Cheapest KW/h (USD)`
ely_prices$high <- ely_prices$`Most expensive KW/h (USD)`
ely_prices$iso2c <- ely_prices$`Country code`

ely_prices <- dplyr::select(ely_prices, iso2c, low, avg, high)

ely_prices <- merge(ely_prices, regions, by.y="ISO_A2", by.x="iso2c")

ely_prices_avg <- terra::rasterize(st_as_sf(ely_prices), ac, field="avg", fun="mean")
ely_prices_high <- terra::rasterize(st_as_sf(ely_prices), ac, field="high", fun="mean")
ely_prices_low <- terra::rasterize(st_as_sf(ely_prices), ac, field="low", fun="mean")
ely_prices_ineq <- ely_prices_high - ely_prices_low

###

rst$ely_prices_avg <- exact_extract(ely_prices_avg, rst, "mean")
rst$ely_prices_high <- exact_extract(ely_prices_high, rst, "mean")
rst$ely_prices_low <- exact_extract(ely_prices_low, rst, "mean")
rst$ely_prices_ineq <- exact_extract(ely_prices_ineq, rst, "mean")

#

ely_outages <- read_xls("2_Thermal comfort Infrastructures & Assets/Energy/API_IC.ELC.OUTG_DS2_en_excel_v2_6000553.xls")
ely_outages$mrv <- NA
ely_outages$iso3c <- ely_outages$`Country Code`

for(i in 1:nrow(ely_outages)){
  ely_outages$mrv[i] <- ely_outages[i, last(which( as.numeric(!is.na(ely_outages[i,5:67])) == max(max(as.numeric(!is.na(ely_outages[i,5:67])))) )) + 4]
}

ely_outages <- merge(ely_outages, regions, by.y="ISO_A3", by.x="iso3c")
ely_outages$mrv <- ifelse(is.na(ely_outages$mrv), 0, ely_outages$mrv)
ely_outages <- terra::rasterize(st_as_sf(ely_outages), ac, field="mrv", fun="mean")

rst$ely_outages <- exact_extract(ely_outages, rst, "mean")

###############################
###############################

fls4 <- drive_find(
  q = "name contains 'people_70_plus'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls4 <- fls4  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls4 <- fls4  %>% filter(!grepl("regions", name))}

fls4 <- pblapply(fls4$id, drive_download, overwrite=T)
fls4 <- bind_rows(fls4)
fls4 <- lapply(fls4$local_path, terra::rast)

people_70_plus <- if(length(fls4)>1) {do.call(mosaic, fls4)} else{fls4[[1]]}

people_70_plus <- terra::mask(people_70_plus, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, people_70_plus[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$people_70_plus_pctg <- exact_extract(people_70_plus, rst, "mean")

###

fls5 <- drive_find(
  q = "name contains 'people_children_less_10yo_share'",
  q = "modifiedTime > '2023-12-02T12:00:00'", n_max = 1000
)

if(scale=="regions"){fls5 <- fls5  %>% filter(grepl("regions", name))}
if(scale=="cities"){fls5 <- fls5  %>% filter(!grepl("regions", name))}

fls5 <- pblapply(fls5$id, drive_download, overwrite=T)
fls5 <- bind_rows(fls5)
fls5 <- lapply(fls5$local_path, terra::rast)

children_less_10yo_share <- if(length(fls5)>1) {do.call(mosaic, fls5)} else{fls5[[1]]}

children_less_10yo_share <- terra::mask(children_less_10yo_share, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, children_less_10yo_share[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$people_10_minus_pctg <- exact_extract(children_less_10yo_share, rst, "mean")

# 5.2) Working standards + 5.3) Heat adaptation knowledge

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/policy/unep_cooling_policies/C Data Library/Clean_Dataset.dta")
r <- arrange(r, country)
r$mean_cooling_reg <- rowMeans(r[,c(2:48)], na.rm=T)
r$country <- countrycode::countrycode(r$country, 'country.name', 'iso2c')

rst <- merge(rst, r %>% dplyr::select(country, mean_cooling_reg), by.x=c("CTRY"), by=c("country"), all.x=T)

######################
# calculate indicators

source(paste0(stub, "scripts/define_calculate_scp_indicators_binary.r"))

######################
######################

# aggregate indicators 

scp_indics <- colnames(rst)[substr(colnames(rst), 1, 1) == "v"]
  
rr <- list()

for (ind in scp_indics){
  
  rr[[match(ind, scp_indics)]] <- st_as_sf(rst) %>% group_by(CTRY, ADM1NAME, URBAN_RURA) %>% st_set_geometry(NULL) %>%  dplyr::summarise(ind=weighted.mean(.data[[ind]], weights_v, na.rm=T)) %>% pull(ind)
  
}

rr <- bind_cols(rr)
colnames(rr) <- scp_indics

rst$geometry <- NULL
rst <- bind_cols( rst %>% group_by(CTRY, ADM1NAME, URBAN_RURA) %>% dplyr::summarise(weights_v=weighted.mean(weights_v, weights_v, na.rm=T)), rr)

#

rst <- merge(rst, s, by=c("CTRY", "ADM1NAME", "URBAN_RURA"))
rst <- dplyr::select(rst, CTRY, ADM1NAME, URBAN_RURA, all_of(scp_indics), geometry)
rst <- sf::st_as_sf(rst)

dhs_rasters <- list()
rrfilled_l <- list()

for(ind in scp_indics){
  
  print(ind)
  
  for(country in unique(rst$CTRY)){
    
    rstt <- filter(rst, CTRY == country)
    
    library(GVI)
    
    rrfilled <- sf_to_rast(st_transform(rstt, 3395), ind, dhs_regions %>% filter(ISO==country), raster_res=10000, progress = F, beta=1)
    
    rrfilled_l[[country]] <- rrfilled
    
  }
  
  if(length(unique(rst$CTRY))>1){
  names(rrfilled_l) <- NULL
  rrfilled_b <- do.call(mosaic, rrfilled_l)
  } else{
    rrfilled_b <- rrfilled_l[[1]]
    
  }
  
  dhs_rasters[match(ind, scp_indics)] <- rrfilled_b
  
  
}

names(dhs_rasters) <- scp_indics

for (i in 1:length(dhs_rasters)){
  
  dhs_rasters[[i]] <- terra::project(dhs_rasters[[i]], y="epsg:4326")
  
}

# plot(dhs_rasters[[5]])

writeRaster(rast(dhs_rasters), paste0(stub,"results/rasters_indicators.tif"), overwrite=T)

######################

# ancillary data

cities <- dhs_regions

cities$P15 <- exact_extract(gridded_pop, cities, "sum") 

gp <- projectRaster(gridded_pop, dhs_rasters[[1]])
values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

for(i in scp_indics){
  
  cities[,i] <- exact_extract(dhs_rasters[[i]], cities, "weighted_mean", weights=gp)
  
}

# plot(cities["v9_justice"])

write_rds(cities, paste0(stub,"results/provincial_indicators.rds"))

######################
######################

saveRDS(cities, ifelse(scale=="cities", "cities_with_data.rds", "regions_with_data.rds"))
save.image(ifelse(scale=="cities", "cities_all_data.Rdata", "regions_all_data.Rdata"))

