# recover some missing objects

ac <- rast(paste0(getwd(), "/data_and_sources_dimensions/2_Thermal comfort Infrastructures & Assets/Assets/ac_penetration_ssp1_ssp2_ssp3_ssp5_2010_2050.nc"))
ac <- ac[[6]]

ely_prices <- read_xls(paste0(getwd(), "/data_and_sources_dimensions/2_Thermal comfort Infrastructures & Assets/Energy/global-electricity-per-kwh-pricing-2021.xls"))
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

#

ely_outages <- read_xls(paste0(getwd(), "/data_and_sources_dimensions/2_Thermal comfort Infrastructures & Assets/Energy/API_IC.ELC.OUTG_DS2_en_excel_v2_6000553.xls"))
ely_outages$mrv <- NA
ely_outages$iso3c <- ely_outages$`Country Code`

for(i in 1:nrow(ely_outages)){
  ely_outages$mrv[i] <- unlist(ely_outages[i, last(which( as.numeric(!is.na(ely_outages[i,5:67])) == max(max(as.numeric(!is.na(ely_outages[i,5:67])))) )) + 4])
}

ely_outages <- merge(ely_outages, regions, by.y="ISO_A3", by.x="iso3c")
ely_outages$mrv <- ifelse(is.na(ely_outages$mrv), 0, ely_outages$mrv)
ely_outages_r <- terra::rasterize(st_as_sf(ely_outages), ac, field="mrv", fun="mean")

#

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/policy/unep_cooling_policies/C Data Library/Clean_Dataset.dta")
r <- arrange(r, country)
r$mean_cooling_reg <- rowMeans(r[,c(2:48)], na.rm=T)
r$country <- countrycode::countrycode(r$country, 'country.name', 'iso2c')
