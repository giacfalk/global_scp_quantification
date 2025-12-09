
sf::sf_use_s2(F)

# aggregate indicators

scp_indics <- c(colnames(rst)[substr(colnames(rst), 1, 1) == "v"], colnames(rst)[substr(colnames(rst), 1, 3) == "dim"], grep("n_dim_deprived", colnames(rst), value = T))

rst$URBAN_RURA <- ifelse(is.na(rst$URBAN_RURA), "U", rst$URBAN_RURA)

rst$ADM1NAME <- ifelse(is.na(rst$ADM1NAME), rst$DHSREGEN.y, rst$ADM1NAME)

rr <- list()

for (ind in scp_indics){

  rr[[match(ind, scp_indics)]] <- st_as_sf(rst) %>% group_by(CTRY, ADM1NAME, URBAN_RURA, DHSID) %>% st_set_geometry(NULL) %>%  dplyr::summarise(ind=weighted.mean(.data[[ind]], weights_v, na.rm=T)) %>% pull(ind)

}

rr <- bind_cols(rr)
colnames(rr) <- scp_indics

#

# library(Hmisc)
# hist.data.frame(rr)

#

rst$geometry <- NULL
rst <- bind_cols( rst %>% group_by(CTRY, ADM1NAME, URBAN_RURA, DHSID) %>% dplyr::summarise(weights_v=sum(weights_v, na.rm=T)), rr)

#

rst1 <- merge(rst, s, by=c("CTRY", "ADM1NAME", "URBAN_RURA", "DHSID"))

if(nrow(rst_unmatched)>0){

  rst2 <- merge(rst, sp, by.x=c("CTRY", "ADM1NAME"), by.y=c("CTRY", "DHSREGEN"))

  rst <- bind_rows(rst1, rst2)

} else{

  rst <- rst1
}

rst <- dplyr::select(rst, CTRY, ADM1NAME, URBAN_RURA, DHSID, all_of(scp_indics), weights_v, geometry)
rst <- sf::st_as_sf(rst)

rst <- rst %>% group_by(CTRY) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., replace = mean(., na.rm = TRUE))),
  )

#

rst <- st_centroid(rst)

#

dhs_rasters <- list()
rrfilled_l <- list()

###

for(ind in c(scp_indics)){

  print(ind)

  for(country in unique(rst$CTRY)){

    print(country)

    rstt <- filter(rst, CTRY == country)

    library(GVI)

    rrfilled <- sf_to_rast(st_transform(rstt, 3395), ind, dhs_regions %>% filter(ISO==country), raster_res=10000, progress =T, beta=1)

    rrfilled_l[[country]] <- rrfilled

  }

  if(length(unique(rst$CTRY))>1){
    names(rrfilled_l) <- NULL

    filename <- lapply(1:length(rrfilled_l), function(X){paste0(tempfile(), "_.tif")})
    filename <- unlist(filename)
    ff <- lapply(1:length(rrfilled_l), function(X){writeRaster(rrfilled_l[[X]], filename[X], overwrite=T)})
    rrfilled_b <- vrt(filename)

  } else{
    rrfilled_b <- rrfilled_l[[1]]

  }

  dhs_rasters[match(ind, c(scp_indics))] <- rrfilled_b


}

names(dhs_rasters) <- c(scp_indics)

# plot(dhs_rasters[[1]])

writeRaster(rast(dhs_rasters), paste0(stub,"results/rasters_indicators.tif"), overwrite=T)

######################

# ancillary data
cities <- dhs_regions

cities$P15 <- exact_extract(gridded_pop, cities, "sum", max_cells_in_memory =3e+08)

## calibrate pop

library(wbstats)

pop_wb <-read_xls("API_SP.POP.TOTL_DS2_en_excel_v2_2001437.xls", skip = 3)
pop_wb <- dplyr::select(pop_wb, 2, ncol(pop_wb))
colnames(pop_wb) <- c("iso2c", "SP.POP.TOTL")

pop_wb$iso2c <- countrycode(pop_wb$iso2c, 'iso3c', 'iso2c')

cities <- merge(cities, pop_wb, by.x="ISO", by.y="iso2c")

cities <- group_by(cities, ISO) %>% dplyr::mutate(P15= P15 * (1 + abs(1-sum(P15) / first(SP.POP.TOTL))))

cities$SP.POP.TOTL <- NULL

###

for(i in scp_indics){

  cities <- bind_cols(cities, exact_extract(dhs_rasters[[i]], cities, "mean", max_cells_in_memory =3e+08))
  colnames(cities)[ncol(cities)] <- i
}

# plot(cities["v1_wbt"])
# plot(cities["n_dim_deprived_3"])

#

write_rds(cities, paste0(stub,"results/provincial_indicators.rds"))
saveRDS(cities, ifelse(scale=="cities", "cities_with_data.rds", "regions_with_data.rds"))
save.image(ifelse(scale=="cities", "cities_all_data.Rdata", "regions_all_data.Rdata"))

##############
##############

library(rgis)

# Save to PNG (or use pdf(), jpeg(), etc.)
png("collage_SCP.png", width = 2500, height = 3500, res = 200)

# Set up layout: 7 rows, 4 columns
par(mfrow = c(7, 4), mar = c(2, 2, 2, 1))  # smaller margins to fit more

for(ctry in(unique(cities$ISO))){
  
  cities_boundary <- cities %>% filter(ISO==ctry)
  
  dims <- stack(paste0(stub,"results/rasters_indicators.tif"))[[19:23]]
  dims <- mask_raster_to_polygon(dims, cities_boundary)
  dims <- stackApply(dims, 1, fun="mean")
  
  plot(dims, axes = FALSE, box = FALSE, frame.plot = FALSE, main=ctry)}

dev.off()