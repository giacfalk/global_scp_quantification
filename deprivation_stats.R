
write_rds(cities_norm, "cities_norm_forstats.rds")

###

cities_norm %>% ungroup() %>% dplyr::summarise(scpi=weighted.mean(SCPI, P15_orig, na.rm=T))

cities_norm %>% ungroup() %>% dplyr::summarise(scpi=weightedSd(SCPI*100, P15_orig, na.rm=T))

###

sum(cities_norm$P15[cities_norm$SCPI<0.55 & cities_norm$SCPI>0.25]) / sum(cities_norm$P15)

sum(cities_norm$P15[cities_norm$SCPI<0.55 & cities_norm$SCPI>0.25]) / 1e9

###

sum(cities_norm$P15[cities_norm$SCPI>0.6]) / sum(cities_norm$P15)

sum(cities_norm$P15[cities_norm$SCPI>0.60]) / 1e9

###

sum(cities_norm$P15[cities_norm$SCPI<0.25]) / sum(cities_norm$P15)

sum(cities_norm$P15[cities_norm$SCPI<0.25]) / 1e9


###

sum(cities_norm$dim1_climate*cities_norm$P15) / 1e9
sum(cities_norm$dim2_infra_assets*cities_norm$P15) / 1e9
sum(cities_norm$dim3_social_thermal_ineq*cities_norm$P15) / 1e9
sum(cities_norm$dim4_health*cities_norm$P15) / 1e9
sum(cities_norm$dim5_edu_work_stds*cities_norm$P15) / 1e9

quantile(cities_norm$SCPI[cities_norm$GRGN_L1=="Latin America & Carib."])

weighted.mean(cities_norm$dim1_climate[cities_norm$GRGN_L1=="South Asia"], cities_norm$P15[cities_norm$GRGN_L1=="South Asia"]) 

#########

weighted.mean(cities_norm$dim1_climate, cities_norm$P15) 
weightedSd(cities_norm$dim1_climate*100, cities_norm$P15) 

weighted.mean(cities_norm$dim2_infra_assets, cities_norm$P15) 
weightedSd(cities_norm$dim2_infra_assets*100, cities_norm$P15) 

weighted.mean(cities_norm$dim3_social_thermal_ineq, cities_norm$P15) 
weightedSd(cities_norm$dim3_social_thermal_ineq*100, cities_norm$P15) 

weighted.mean(cities_norm$dim4_health, cities_norm$P15) 
weightedSd(cities_norm$dim4_health*100, cities_norm$P15) 


weighted.mean(cities_norm$dim5_edu_work_stds, cities_norm$P15) 
weightedSd(cities_norm$dim5_edu_work_stds*100, cities_norm$P15) 

cities_norm %>% ungroup() %>% dplyr::summarise(scpi=weighted.mean(SCPI, P15_orig, na.rm=T))

###

cities_norm %>%  group_by(ISO, GRGN_L1) %>% dplyr::summarise(across(starts_with("v"), mean, na.rm=T))

View(cities_norm %>%  group_by(ISO, GRGN_L1) %>% dplyr::summarise(across(starts_with("v"), mean, na.rm=T)))

###

View(cities_norm %>%  group_by(ISO, GRGN_L1) %>% dplyr::summarise(across(starts_with("dim"), weighted.mean, P15, na.rm=T), SCPI=weighted.mean(SCPI, P15, na.rm=T)))


###

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, v1_wbt=weighted.mean(v1_wbt, P15_orig)) %>% ungroup() %>% arrange(desc(SCPI))


cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, v1_wbt=weighted.mean(v1_wbt, P15_orig)) %>% ungroup() %>% arrange(SCPI)

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, v1_wbt=weighted.mean(v1_wbt, P15_orig)) %>% ungroup() %>% arrange(desc(v1_wbt))

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, v1_wbt=weighted.mean(v1_wbt, P15_orig)) %>% ungroup() %>% arrange(v1_wbt)

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim2_infra_assets=weighted.mean(dim2_infra_assets, P15_orig)) %>% ungroup() %>% arrange(desc(dim2_infra_assets))

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim2_infra_assets=weighted.mean(dim2_infra_assets, P15_orig)) %>% ungroup() %>% arrange(dim2_infra_assets)

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim3_social_thermal_ineq=weighted.mean(dim3_social_thermal_ineq, P15_orig)) %>% ungroup() %>% arrange(desc(dim3_social_thermal_ineq))

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim3_social_thermal_ineq=weighted.mean(dim3_social_thermal_ineq)) %>% ungroup() %>% arrange(dim3_social_thermal_ineq)

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim4_health=weighted.mean(dim4_health, P15_orig)) %>% ungroup() %>% arrange(desc(dim4_health))

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim4_health=weighted.mean(dim4_health)) %>% ungroup() %>% arrange(dim4_health)

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim5_edu_work_stds=weighted.mean(dim5_edu_work_stds, P15_orig)) %>% ungroup() %>% arrange(desc(dim5_edu_work_stds))

cities_norm %>%  group_by(ISO, GRGN_L1) %>%  arrange(SCPI) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15_orig, na.rm=T),  gdp_pc = weighted.mean(GDP15_SM/P15_orig, P15_orig), P15=sum(P15_orig)/1e6, dim5_edu_work_stds=weighted.mean(dim5_edu_work_stds)) %>% ungroup() %>% arrange(dim5_edu_work_stds)

