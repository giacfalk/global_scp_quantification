
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
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

####

# https://docs.google.com/spreadsheets/d/1RCgTnyEbomKqVAiIskorDkkHwzWe9e64RHV-Y0wrg8U/edit#gid=0

###

cities <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
cities <- cities %>% group_by(CTR_MN_ISO) %>% slice_max(P15, n = n_largest_cities_by_country)

write_sf(cities, "cities_selected.shp")

dhs_regions<- read_sf("4_Health/Child mortality/shps/sdr_subnational_data.shp") %>% dplyr::select(geometry, DHSREGEN) %>% write_sf("dhs_regions.shp")
regions <- st_as_sf(rworldmap::countriesLow) %>% filter(SOVEREIGNT!="Antarctica")

gridded_pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

###

if (scale == "cities"){
  cities <- cities
} else if (scale == "regions"){
  cities <- dhs_regions
}

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
fls1_d_1 <- fls1_d[c(grep("max-", fls1$name)),]
fls1_d_1 <- lapply(fls1_d_1$local_path, terra::rast)
fls1_d_2 <- fls1_d[c(grep("avg-", fls1$name)),]
fls1_d_2 <- lapply(fls1_d_2$local_path, terra::rast)


wb_t_max <- if(length(fls1_d_1)>1) {do.call(mosaic, fls1_d_1)} else{fls1_d_1[[1]]}
wb_t_avg <- if(length(fls1_d_2)>1) {do.call(mosaic, fls1_d_2)} else{fls1_d_2[[1]]}


gp <- projectRaster(gridded_pop, wb_t_avg[[1]])

cities$wb_t_max <- exact_extract(tapp(wb_t_max, 1, max), cities, "weighted_mean", weights=gp)
cities$wb_t_avg <- exact_extract(tapp(wb_t_avg, 1, mean), cities, "weighted_mean", weights=gp)

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

gp <- projectRaster(gridded_pop, wb_t_gt_30_ndays[[1]])

cities$wb_t_gt_30_ndays <- exact_extract(wb_t_gt_30_ndays, cities, "weighted_mean", weights=gp)

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

gp <- projectRaster(gridded_pop, index_builtup[[1]])

cities$index_builtup <- exact_extract(index_builtup, cities, "weighted_mean", weights=gp)

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

gp <- projectRaster(gridded_pop, green_blue_infra_share[[1]])

cities$green_blue_infra_share <- exact_extract(green_blue_infra_share, cities, "weighted_mean", weights=gp)

####
# 2.3) Water quality and Sanitation
  
water <- raster("2_Thermal comfort Infrastructures & Assets/Water quality and Sanitation/IHME_LMIC_WASH_2000_2017_W_IMP_PERCENT_MEAN_2017_Y2020M06D02.TIF")
water <- projectRaster(water, raster(wb_t_max))
values(water) <- ifelse(is.na(values(water)), 100, values(water))
water <- rgis::mask_raster_to_polygon(water, regions)

sanitation <- raster("2_Thermal comfort Infrastructures & Assets/Water quality and Sanitation/IHME_LMIC_WASH_2000_2017_S_IMP_PERCENT_MEAN_2017_Y2020M06D02.TIF")
sanitation <- projectRaster(sanitation, raster(wb_t_max))
values(sanitation) <- ifelse(is.na(values(sanitation)), 100, values(sanitation))
sanitation <- rgis::mask_raster_to_polygon(sanitation, regions)

# 2.4) Housing materials

housing_quality_dhs <- read_sf("2_Thermal comfort Infrastructures & Assets/housing_materials/shps/sdr_subnational_data.shp")
housing_quality_dhs <- housing_quality_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(HCRMSLH1RM[HCRMSLH1RM!=9999], na.rm=T), .)))
housing_quality_dhs <- dplyr::select(housing_quality_dhs, 28:49)

# rudimentary floors
ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")[[6]]
HCFLRMHRUD <- terra::rasterize(housing_quality_dhs, ac, field="HCFLRMHRUD", fun="mean", background = 100)
HCFLRMHRUD[is.na(HCFLRMHRUD)] <- 100
HCFLRMHRUD <- rast(rgis::mask_raster_to_polygon(raster(HCFLRMHRUD), regions))

HCRMSLH1RM <- terra::rasterize(housing_quality_dhs, ac, field="HCRMSLH1RM", fun="mean", background = 100)
HCRMSLH1RM[is.na(HCRMSLH1RM)] <- 100
HCRMSLH1RM <- rast(rgis::mask_raster_to_polygon(raster(HCRMSLH1RM), regions))

cities$rudimentary_floor_perc <- exact_extract(HCFLRMHRUD, cities, "mean")
cities$only_one_sleeping_room_perc <- exact_extract(HCRMSLH1RM, cities, "mean")

# 2.5) Clothing

## ??

# 2.6) Assets

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")
ac_pop <- rast("2_Thermal comfort Infrastructures & Assets/Assets/pop_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")

ac <- ac[[6]]
ac_pop <- ac_pop[[6]]

assets_dhs <- read_sf("2_Thermal comfort Infrastructures & Assets/Assets/sdr_subnational_data_2023-12-06/shps/sdr_subnational_data.shp")
assets_dhs <- assets_dhs %>% mutate_if(is.numeric, funs(ifelse(.==9999, 0, .)))

# radio
HCHEFFHRDO <- terra::rasterize(assets_dhs, ac, field="HCHEFFHRDO", fun="mean", background = 100)
HCHEFFHRDO[is.na(HCHEFFHRDO)] <- 100
HCHEFFHRDO <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHRDO), regions))

# tv
HCHEFFHTLV <- terra::rasterize(assets_dhs, ac, field="HCHEFFHTLV", fun="mean", background = 100)
HCHEFFHTLV[is.na(HCHEFFHTLV)] <- 100
HCHEFFHTLV <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHTLV), regions))

# mobile phone
HCHEFFHMPH <- terra::rasterize(assets_dhs, ac, field="HCHEFFHMPH", fun="mean", background = 100)
HCHEFFHMPH[is.na(HCHEFFHMPH)] <- 100
HCHEFFHMPH <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHMPH), regions))

# computer
HCHEFFHCMP <- terra::rasterize(assets_dhs, ac, field="HCHEFFHCMP", fun="mean", background = 100)
HCHEFFHCMP[is.na(HCHEFFHCMP)] <- 100
HCHEFFHCMP <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHCMP), regions))

# refrigerator
HCHEFFHFRG <- terra::rasterize(assets_dhs, ac, field="HCHEFFHFRG", fun="mean", background = 100)
HCHEFFHFRG[is.na(HCHEFFHFRG)] <- 100
HCHEFFHFRG <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHFRG), regions))

# total number of households
HCHEFFHNUM <- terra::rasterize(assets_dhs, ac, field="HCHEFFHNUM", fun="mean", background = 100)
HCHEFFHNUM[is.na(HCHEFFHNUM)] <- 100
HCHEFFHNUM <- rast(rgis::mask_raster_to_polygon(raster(HCHEFFHNUM), regions))

##

cities$ac <- exact_extract(ac, cities, "weighted_mean", weights=ac_pop)

cities$radio_ownership_pctg <- exact_extract(HCHEFFHRDO, cities, "weighted_mean", weights=HCHEFFHNUM)
cities$tv_ownership_pctg <- exact_extract(HCHEFFHTLV, cities, "weighted_mean", weights=HCHEFFHNUM)
cities$cellphone_ownership_pctg <- exact_extract(HCHEFFHMPH, cities, "weighted_mean", weights=HCHEFFHNUM)
cities$computer_ownership_pctg <- exact_extract(HCHEFFHCMP, cities, "weighted_mean", weights=HCHEFFHNUM)

cities$multiple_information_means_ownership_pctg <- rowMeans(cities[,c("radio_ownership_pctg", "tv_ownership_pctg", "cellphone_ownership_pctg", "computer_ownership_pctg")]  %>% st_set_geometry(NULL), na.rm = T)

cities$fridge_ownership_pctg <- exact_extract(HCHEFFHFRG, cities, "weighted_mean", weights=HCHEFFHNUM)

# 2.7) Energy

ely_prices <- read_xls("2_Thermal comfort Infrastructures & Assets/Energy/global-electricity-per-kwh-pricing-2021.xls")
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

cities$ely_prices_avg <- exact_extract(ely_prices_avg, cities, "mean")
cities$ely_prices_high <- exact_extract(ely_prices_high, cities, "mean")
cities$ely_prices_low <- exact_extract(ely_prices_low, cities, "mean")
cities$ely_prices_ineq <- exact_extract(ely_prices_ineq, cities, "mean")

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

cities$ely_outages <- exact_extract(ely_outages, cities, "mean")

####
## 3) Social and thermal inequality

# 3.1) Thermal justice

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

gp <- projectRaster(gridded_pop, people_70_plus[[1]])

cities$people_70_plus_pctg <- exact_extract(people_70_plus, cities, "weighted_mean", weights=gp)

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

gp <- projectRaster(gridded_pop, children_less_10yo_share[[1]])

cities$people_10_minus_pctg <- exact_extract(children_less_10yo_share, cities, "weighted_mean", weights=gp)


####
## 4) Health

# 4.1) Child mortality; Diarrheal diseases; Food poisoning; ELDERLY mortality & morbidity

child_mortality_dhs <- read_sf("4_Health/Child mortality/shps/sdr_subnational_data.shp")
child_mortality_dhs <- child_mortality_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(CMECMRCCMR[CMECMRCCMR!=9999], na.rm=T), .)))
child_mortality_dhs <- dplyr::select(child_mortality_dhs, 28:38)

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")[[6]]
CMECMRCCMR <- terra::rasterize(child_mortality_dhs, ac, field="CMECMRCCMR", fun="mean", background = 0)
CMECMRCCMR[is.na(CMECMRCCMR)] <- 0
CMECMRCCMR <- rast(rgis::mask_raster_to_polygon(raster(CMECMRCCMR), regions))

cities$child_mortality_rate_1_5_yo <- exact_extract(CMECMRCCMR, cities, "mean")

# 4.2) Non-Communicable Diseases

diarrhea_dhs <- read_sf("4_Health/Diarrheal diseases/shps/sdr_subnational_data.shp")
diarrhea_dhs <- diarrhea_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(CHDIARCDIA[CHDIARCDIA!=9999], na.rm=T), .)))

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")[[6]]
CHDIARCDIA <- terra::rasterize(diarrhea_dhs, ac, field="CHDIARCDIA", fun="mean", background = 0)
CHDIARCDIA[is.na(CHDIARCDIA)] <- 0
CHDIARCDIA <- rast(rgis::mask_raster_to_polygon(raster(CHDIARCDIA), regions))

cities$diarrhea_rate_children <- exact_extract(CHDIARCDIA, cities, "mean")

# 4.3) Adverse birth outcome in pregnant women

# adverse_birth_outcome_dhs <- read_sf("4_Health/maternal_mortality/shps/")
# adverse_birth_outcome_dhs <- adverse_birth_outcome_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(CMECMRCCMR[CMECMRCCMR!=9999], na.rm=T), .)))

####
## 5) Education and Work standards

# 5.1) School attendance

school_attendance_dhs <- read_sf("5_ Education and Work standards/school_attendance/sdr_subnational_data_2023-12-06/shps/sdr_subnational_data.shp")
school_attendance_woman_dhs <- school_attendance_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(EDEDUCWCPR[EDEDUCWCPR!=9999], na.rm=T), .)))
school_attendance_man_dhs <- school_attendance_dhs %>% group_by(DHSCC, SVYYEAR) %>% mutate_if(is.numeric, funs(ifelse(.==9999, mean(EDEDUCMCPR[EDEDUCMCPR!=9999], na.rm=T), .)))

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")[[6]]
EDEDUCWCPR <- terra::rasterize(school_attendance_woman_dhs, ac, field="EDEDUCWCPR", fun="mean", background = 0)
EDEDUCWCPR[is.na(EDEDUCWCPR)] <- 100
EDEDUCWCPR <- rast(rgis::mask_raster_to_polygon(raster(EDEDUCWCPR), regions))

cities$primary_education_rate_women <- exact_extract(EDEDUCWCPR, cities, "mean")

###

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")[[6]]
EDEDUCMCPR <- terra::rasterize(school_attendance_woman_dhs, ac, field="EDEDUCMCPR", fun="mean", background = 0)
EDEDUCMCPR[is.na(EDEDUCMCPR)] <- 100
EDEDUCMCPR <- rast(rgis::mask_raster_to_polygon(raster(EDEDUCMCPR), regions))

cities$primary_education_rate_men <- exact_extract(EDEDUCMCPR, cities, "mean")

# 5.2) Working standards

# 5.3) Heat adaptation knowledge

######################

# ancillary data

if(scale=="regions"){cities$P15 <- exact_extract(gridded_pop, cities, "sum") }

######################
######################

saveRDS(cities, ifelse(scale=="cities", "cities_with_data.rds", "regions_with_data.rds"))
save.image(ifelse(scale=="cities", "cities_all_data.Rdata", "regions_all_data.Rdata"))

