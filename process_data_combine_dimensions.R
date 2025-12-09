
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
library(geodata)
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
library(pbapply)
library(matrixStats)

googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

####

# https://docs.google.com/spreadsheets/d/1RCgTnyEbomKqVAiIskorDkkHwzWe9e64RHV-Y0wrg8U/edit#gid=984566709

###

cities <- read_sf("GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
cities <- cities %>% group_by(CTR_MN_ISO) %>% slice_max(P15, n = n_largest_cities_by_country)

write_sf(cities, "cities_selected.shp")

r <- r_bk <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Clean Data/DHS_cleaned.dta") %>% filter(country!="Turkey")


wrld_simpl <-  st_as_sf(world(path=getwd())) %>% dplyr::filter(NAME_0!="Antarctica")
colnames(wrld_simpl)[1] <- "ISO3"
wrld_simpl <- filter(wrld_simpl, wrld_simpl$ISO3 %in% countrycode(r$country, 'country.name', 'iso3c'))
write_sf(wrld_simpl %>% dplyr::select(ISO3) , "dhs_regions.shp")

dhs_regions<- read_sf("4_Health/Child mortality/shps/sdr_subnational_data.shp") %>% dplyr::select(geometry, ISO, DHSREGEN)


if((countrylist!="all")[1]){

  dhs_regions <- dhs_regions %>% filter(ISO %in% countrycode(countrylist, 'country.name', 'iso2c'))

} else{

  dhs_regions <- dhs_regions %>% filter(ISO %in% countrycode(unique(r$country), 'country.name', 'iso2c'))

}

countrylist <- countrycode(unique(dhs_regions$ISO), 'iso2c', 'country.name')

regions <-  st_as_sf(world(path=getwd())) %>% dplyr::filter(NAME_0!="Antarctica")
colnames(regions)[1] <- "ISO_A3"

regions$ISO_A2 <- countrycode::countrycode(regions$ISO_A3, 'iso3c', 'iso2c')

gridded_pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

###

if (scale == "cities"){
  cities <- cities %>% filter(CTR_MN_NM %in% countrylist)
} else if (scale == "regions"){
  cities <- dhs_regions %>% filter(ISO %in% countrycode(countrylist, 'country.name', 'iso2c'))
}

###

r <- as.data.frame(r) %>% filter(countrycode(country, 'country.name', 'iso2c') %in% countrycode(countrylist, 'country.name', 'iso2c'))

r$weights <- r$weights_v <-  r$sampling_weight
r$CTRY <- countrycode(r$country, 'country.name', 'iso2c')

r$year[r$CTRY=="AL"] <- 2017
r$year[r$CTRY=="AM"] <- 2015
r$year[r$CTRY=="BD"] <- 2018
r$year[r$CTRY=="GH"] <- 2022
r$year[r$CTRY=="GT"] <- 2015
r$year[r$CTRY=="HN"] <- 2011
r$year[r$CTRY=="HT"] <- 2016
r$year[r$CTRY=="IN"] <- 2020
r$year[r$CTRY=="JO"] <- 2017
r$year[r$CTRY=="MW"] <- 2015
r$year[r$CTRY=="NP"] <- 2022
r$year[r$CTRY=="PK"] <- 2017
r$year[r$CTRY=="RW"] <- 2019
r$year[r$CTRY=="SN"] <- 2010
r$year[r$CTRY=="SN"] <- 2010
r$year[r$CTRY=="ZM"] <- 2019

r$year[r$CTRY=="CO"] <- 2015
r$year[r$CTRY=="ID"] <- 2017
r$year[r$CTRY=="TR"] <- 2018
r$year[r$CTRY=="YE"] <- 2013

r$DHSID <- paste0(r$CTRY, r$year, str_sub(paste0("00000000", r$cluster_number), start= -8))

###

s <- list.files(pattern=".shp", path="F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Raw Data", recursive = T, full.names = T)
s <- s[!grepl("xml", s)]
s <- lapply(s, read_sf)
s <- bind_rows(s)

###
# country names parser

s$CTRY <- countrycode(s$DHSCC, 'dhs', 'iso2c')
s$year <- r$year[match(s$CTRY, r$CTRY)]
s$DHSID <- paste0(s$CTRY, s$year, substr(s$DHSID, 7, 100))
s$year <- NULL

# t <- list.files(pattern=".csv", path="F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Raw Data", recursive = T, full.names = T)
# t <- t[grepl(paste0(countrylist, collapse = "|"), t)]
#
# t <- lapply(t, read.csv)
# t <- bind_rows(t)
# t$CTRY <-  t$CCFIPS
# t$DHSID <- NULL
# t$DHSYEAR <- NULL
# st <- merge(s, t, c("CTRY", "DHSCC", "DHSCLUST"), all=T)

###

rst <- merge(r, s, by=c("DHSID", "CTRY"))

rst_unmatched <- r[!(r$CTRY %in% rst$CTRY),]

###

library(rdhs)

#

if(nrow(rst_unmatched)>0){

  sp <- lapply(1:length(unique(paste0(substr(rst_unmatched$DHSID, 1, 6), "DHS"))), function(X){ download_boundaries(surveyId = unique(paste0(substr(rst_unmatched$DHSID, 1, 6), "DHS"))[X])})
  sp <- lapply(1:length(unique(paste0(substr(rst_unmatched$DHSID, 1, 6), "DHS"))), function(X){ st_as_sf(sp[[X]]$sdr_subnational_boundaries)})
  sp <- bind_rows(sp)
  sf::sf_use_s2(F)
  # sp <- st_centroid(sp)
  sp$CTRY <- countrycode(sp$DHSCC, 'dhs', 'iso2c')
  sp$year <- r$year[match(sp$CTRY, r$CTRY)]

  ###

  data_c_map_2 <- rst_unmatched
  data_c_map_2$CTRY <- data_c_map_2$CTRY
  data_c_map_2$DHSREGEN <- data_c_map_2$prefecture

  data_c_map_2 <- data_c_map_2[!duplicated(data_c_map_2[ , c("CTRY", "DHSREGEN")]), ]

  library(fuzzyjoin)

  data_c_sp_2 <- lapply(unique(data_c_map_2$CTRY), function(X){stringdist_join(data_c_map_2 %>% filter(CTRY==X), sp  %>% filter(CTRY==X),
                                                                               by = "DHSREGEN",
                                                                               mode = "left",
                                                                               ignore_case = TRUE,
                                                                               method = "jw",
                                                                               max_dist = 99,
                                                                               distance_col = "dist") %>%
      group_by(country, DHSREGEN.x) %>%
      slice_min(order_by = dist, n = 1) %>% ungroup()})

  data_c_sp_2 <- bind_rows(data_c_sp_2)

  data_c_sp_2 <- dplyr::select(data_c_sp_2, CTRY.x, CTRY.y, DHSREGEN.x, DHSREGEN.y,  geometry)

  rst2 <- merge(rst_unmatched, data_c_sp_2, by.x=c("CTRY", "prefecture"), by.y=c("CTRY.x", "DHSREGEN.x"))

  rst <- bind_rows(rst, rst2)

}

rst <- st_as_sf(rst)

rst1 <- rst[st_geometry_type(rst$geometry)=="POINT",]
rst1 <- rst1 %>% st_transform(3395) %>% st_buffer(5000) %>% st_transform(4326)
rst2 <- slice(rst, which(st_geometry_type(rst$geometry)!="POINT"))
rst <- bind_rows(rst1, rst2)

rm(rst1, rst2); gc()

rst <- rst %>% filter( is.na(st_dimension(.)) == FALSE )

####
## 1) Climate

# 1.1) Wet bulb temperature

fls1_d <- list.files(pattern = "wetbulb_temp_ts_1980_2020_monthly_", path="D:/Il mio Drive/scp_gee", full.names=T)
fls1_d_1 <- fls1_d[grep("max", fls1_d)]
fls1_d_1 <- lapply(fls1_d_1, terra::rast)
fls1_d_2 <- fls1_d[grep("avg", fls1_d)]
fls1_d_2 <- lapply(fls1_d_2, terra::rast)


wb_t_max <- if(length(fls1_d_1)>1) {do.call(mosaic, fls1_d_1)} else{fls1_d_1[[1]]}
wb_t_avg <- if(length(fls1_d_2)>1) {do.call(mosaic, fls1_d_2)} else{fls1_d_2[[1]]}

wb_t_max <- terra::mask(wb_t_max, vect(st_as_sfc(st_bbox(extent(cities)))))
wb_t_avg <- terra::mask(wb_t_avg, vect(st_as_sfc(st_bbox(extent(cities)))))

gp <- projectRaster(gridded_pop, wb_t_max[[1]])
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$wb_t_max <- rowMaxs(as.matrix(exact_extract(tapp(wb_t_max, 1:12, mean), rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp,  default_weight=1)))
rst$wb_t_avg <- rowMaxs(as.matrix(exact_extract(tapp(wb_t_avg, 1:12, mean), rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)))

###

fls1_b <- list.files(pattern = "wetbulb_temp_ts_2010_2020_ndays_above_30", path="D:/Il mio Drive/scp_gee", full.names=T)
fls1_b <- lapply(fls1_b, terra::rast)

wb_t_gt_30_ndays <- if(length(fls1_b)>1) {do.call(mosaic, fls1_b)} else{fls1_b[[1]]}

wb_t_gt_30_ndays <- terra::mask(wb_t_gt_30_ndays, vect(st_as_sfc(st_bbox(extent(cities)))))

gp <- projectRaster(gridded_pop, wb_t_gt_30_ndays[[1]])
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$wb_t_gt_30_ndays <- exact_extract(wb_t_gt_30_ndays, rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)

####
## 2) Thermal comfort Infrastructures & Assets

# 2.1) Urban planning

fls2 <- list.files(pattern = "index_builtup", path="D:/Il mio Drive/scp_gee", full.names=T)
fls2 <- lapply(fls2, terra::rast)

index_builtup <- if(length(fls2)>1) {do.call(mosaic, fls2)} else{fls2[[1]]}

index_builtup <- terra::mask(index_builtup, vect(st_as_sfc(st_bbox(extent(cities)))))

gp <- projectRaster(gridded_pop, index_builtup[[1]])
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$index_builtup <- exact_extract(index_builtup, rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)
rst$index_builtup <- round(rst$index_builtup, 0)

###

lcz <- terra::rast("2_Thermal comfort Infrastructures & Assets/Urban planning/lcz_filter_v3.tif")

rst$lcz <- exact_extract(lcz, rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)
rst$lcz <- round(rst$lcz, 0)

####
# 2.2) Green & Blue  infrastructure

fls3 <- list.files(pattern = "share_green_blue_spaces", path="D:/Il mio Drive/scp_gee", full.names=T)
fls3 <- lapply(fls3, terra::rast)

green_blue_infra_share <- if(length(fls3)>1) {do.call(mosaic, fls3)} else{fls3[[1]]}

green_blue_infra_share <- terra::mask(green_blue_infra_share, vect(st_as_sfc(st_bbox(extent(cities)))))

gp <- projectRaster(gridded_pop, green_blue_infra_share[[1]])
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$green_blue_infra_share <- exact_extract(green_blue_infra_share, rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)

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

###

healthcaretime <- raster("4_Health/2020_walking_only_travel_time_to_healthcare.geotiff")

gp <- projectRaster(gridded_pop, healthcaretime[[1]])
gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$healthcaretime <- exact_extract(healthcaretime, rst, "weighted_mean", max_cells_in_memory =3e+08, weights=gp, default_weight=1)


#######

ac <- rast("2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")
ac_pop <- rast("2_Thermal comfort Infrastructures & Assets/Assets/pop_ssp1_ssp2_ssp3_ssp5_2010_2050.nc")

ac <- ac[[6]]
ac_pop <- ac_pop[[6]]

rst$ac <- exact_extract(ac, rst, "weighted_mean", weights=ac_pop, max_cells_in_memory =3e+08)

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

rst$ely_prices_avg <- exact_extract((ely_prices_avg), rst, "mean")
rst$ely_prices_high <- exact_extract((ely_prices_high), rst, "mean")
rst$ely_prices_low <- exact_extract((ely_prices_low), rst, "mean")
rst$ely_prices_ineq <- exact_extract((ely_prices_low), rst, "mean")

#

ely_outages <- read_xls("2_Thermal comfort Infrastructures & Assets/Energy/API_IC.ELC.OUTG_DS2_en_excel_v2_6000553.xls")
ely_outages$mrv <- NA
ely_outages$iso3c <- ely_outages$`Country Code`

for(i in 1:nrow(ely_outages)){
  ely_outages$mrv[i] <- unlist(ely_outages[i, last(which( as.numeric(!is.na(ely_outages[i,5:67])) == max(max(as.numeric(!is.na(ely_outages[i,5:67])))) )) + 4])
}

ely_outages <- merge(ely_outages, regions, by.y="ISO_A3", by.x="iso3c")
ely_outages$mrv <- ifelse(is.na(ely_outages$mrv), 0, ely_outages$mrv)
ely_outages_r <- terra::rasterize(st_as_sf(ely_outages), ac, field="mrv", fun="mean")

rst$ely_outages <- exact_extract(ely_outages_r, rst, "mean", max_cells_in_memory =3e+08)

###############################
###############################

fls4 <- list.files(pattern = "people_70_plus", path="D:/Il mio Drive/scp_gee", full.names=T)
fls4 <- lapply(fls4, terra::rast)

people_70_plus <- if(length(fls4)>1) {do.call(mosaic, fls4)} else{fls4[[1]]}

people_70_plus <- terra::mask(people_70_plus, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, people_70_plus[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$people_70_plus_pctg <- exact_extract(people_70_plus, rst, "mean", max_cells_in_memory =3e+08)

###

fls5 <- list.files(pattern = "people_children_less_10yo_share", path="D:/Il mio Drive/scp_gee", full.names=T)
fls5 <- lapply(fls5, terra::rast)

children_less_10yo_share <- if(length(fls5)>1) {do.call(mosaic, fls5)} else{fls5[[1]]}

children_less_10yo_share <- terra::mask(children_less_10yo_share, vect(st_as_sfc(st_bbox(extent(cities)))))

# gp <- projectRaster(gridded_pop, children_less_10yo_share[[1]])
# values(gp) <- ifelse(is.na(values(gp)), 0, values(gp))
# gp <-  terra::mask(rast(gp), vect(st_as_sfc(st_bbox(extent(cities)))))

rst$people_10_minus_pctg <- exact_extract(children_less_10yo_share, rst, "mean", max_cells_in_memory =3e+08)

# 5.2) Working standards + 5.3) Heat adaptation knowledge

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/policy/unep_cooling_policies/C Data Library/Clean_Dataset.dta")
r <- arrange(r, country)
r$mean_cooling_reg <- rowMeans(r[,c(2:48)], na.rm=T)
r$country <- countrycode::countrycode(r$country, 'country.name', 'iso2c')

rst <- merge(rst, r %>% dplyr::select(country, mean_cooling_reg), by.x=c("CTRY"), by=c("country"), all.x=T)

######################

# NA filling

rst_geo <- rst$geometry
rst$geometry <- NULL

mode_fun <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

rst <- rst %>%
  group_by(CTRY, ADM1NAME, URBAN_RURA) %>%
  mutate(across(everything(), ~replace(., is.na(.), mode_fun(.)))) %>%
  ungroup()

rst <- rst %>%
  group_by(CTRY, ADM1NAME, URBAN_RURA) %>%
  mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
  ungroup()

####

rst$Electricity[rst$CTRY=="HN"] <- 1
rst$Electricity[rst$CTRY=="AL"] <- 1
rst$Electricity[rst$CTRY=="JO"] <- 1

rst$Schooling[rst$CTRY=="YE"] <- 0.65 # https://data.worldbank.org/indicator/SE.PRM.CMPT.ZS?locations=YE

###

# write data before indicators calculation

save.image("rst_bk_before_indicators.Rdata")
