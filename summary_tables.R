
library(modelsummary)
options("modelsummary_format_numeric_latex" = "plain")

cities_norm$wb_t_max_orig <- cities$v1_wbt
cities_norm$P15_orig <- cities_norm$P15_orig/1e6

if (!("CTR_MN_ISO" %in% colnames(cities_norm))) {
  cities_norm[, "CTR_MN_ISO"] <- cities_norm$ISO
}

datasummary(Factor(CTR_MN_ISO) ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_countries.html")

datasummary(Factor(CTR_MN_ISO) ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_countries.tex")

###

datasummary(v1_wbt * mean + v2_urban_planning * mean + v3_green_bleu_infra * mean + v4a_water_quality * mean + v4b_sanitation * mean + v5_housing * mean + v7_cooling_assets * mean + v7b_transport_assets * mean + v8_energy * mean + v9_justice * mean + v10_health1 * mean + v11_health2 * mean + v12_health3 * mean  + v12_health4 * mean + v13_education * mean + v14_work * mean + v15_adapt_knowledge * mean + v16_cooling_policy * mean + dim1_climate * mean + dim2_infra_assets * mean + dim3_social_thermal_ineq * mean + dim4_health * mean + dim5_edu_work_stds * mean + SCPI * mean + P15_orig * sum ~  Factor(CTR_MN_ISO) , data = cities_norm, output = "figures/table_countries_alldims.html")

datasummary(v1_wbt * mean + v2_urban_planning * mean + v3_green_bleu_infra * mean + v4a_water_quality * mean + v4b_sanitation * mean + v5_housing * mean + v7_cooling_assets * mean + v7b_transport_assets * mean + v8_energy * mean + v9_justice * mean + v10_health1 * mean + v11_health2 * mean + v12_health3 * mean + v12_health4 * mean + v13_education * mean + v14_work * mean + v15_adapt_knowledge * mean + v16_cooling_policy * mean + dim1_climate * mean + dim2_infra_assets * mean + dim3_social_thermal_ineq * mean + dim4_health * mean + dim5_edu_work_stds * mean + SCPI * mean + P15_orig * sum ~  Factor(CTR_MN_ISO) , data = cities_norm %>% filter(CTR_MN_ISO %in% unique(cities_norm$CTR_MN_ISO)[1:14]), output = "figures/table_countries_alldims_pt1.tex")

datasummary(v1_wbt * mean + v2_urban_planning * mean + v3_green_bleu_infra * mean + v4a_water_quality * mean + v4b_sanitation * mean + v5_housing * mean + v7_cooling_assets * mean + v7b_transport_assets * mean + v8_energy * mean + v9_justice * mean + v10_health1 * mean + v11_health2 * mean + v12_health3 * mean + v12_health4 * mean + v13_education * mean + v14_work * mean + v15_adapt_knowledge * mean + v16_cooling_policy * mean + dim1_climate * mean + dim2_infra_assets * mean + dim3_social_thermal_ineq * mean + dim4_health * mean + dim5_edu_work_stds * mean + SCPI * mean + P15_orig * sum ~  Factor(CTR_MN_ISO) , data = cities_norm %>% filter(CTR_MN_ISO %in% unique(cities_norm$CTR_MN_ISO)[15:28]), output = "figures/table_countries_alldims_pt2.tex")


###

cities_norm$DHSREGEN_c <- gsub("[^A-Za-z0-9]", "",  cities_norm$DHSREGEN)

cities_norm$DHSREGEN_c <- paste0(countrycode(cities_norm$ISO, 'iso2c', 'country.name'), " - ", cities_norm$DHSREGEN_c)

if(scale=="cities"){

library(stargazer)

cc <- as.data.frame(cities_norm %>% dplyr::select(CTR_MN_ISO, UC_NM_MN, SCPI, P15_orig) %>% st_set_geometry(NULL))
cc <- arrange(cc, desc(SCPI))

stargazer(cc, summary = F, out =  "figures/table_cites.html", type = "html")

stargazer(cc, summary = F, out =  "figures/table_cites.tex", type = "latex")


} else {

  datasummary(DHSREGEN_c ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_regions.html")
  
  datasummary(DHSREGEN_c ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_regions.tex")
  
  
}

####
####

cities_norm$geometry = NULL

cities_norm_summstat <- cities_norm %>%  group_by(ISO) %>% dplyr::summarise(SCPI_mean = weighted.mean(SCPI, P15, na.rm=T), SCPI_min = min(SCPI, na.rm=T), SCPI_max = max(SCPI, na.rm=T), SCPI_sd = sd(SCPI, na.rm=T), SCPI_IQR = IQR(SCPI, na.rm=T), dim1_climate=weighted.mean(dim1_climate, P15, na.rm=T), dim2_infra_assets=weighted.mean(dim2_infra_assets, P15, na.rm=T), dim3_social_thermal_ineq=weighted.mean(dim3_social_thermal_ineq, P15, na.rm=T), dim4_health=weighted.mean(dim4_health, P15, na.rm=T), dim5_edu_work_stds=weighted.mean(dim5_edu_work_stds, P15, na.rm=T), share_dim_deprived_0=weighted.mean(n_dim_deprived_0, P15, na.rm=T), share_dim_deprived_1=weighted.mean(n_dim_deprived_1, P15, na.rm=T), share_dim_deprived_2=weighted.mean(n_dim_deprived_2, P15, na.rm=T), share_dim_deprived_3=weighted.mean(n_dim_deprived_3, P15, na.rm=T), share_dim_deprived_4=weighted.mean(n_dim_deprived_4, P15, na.rm=T), share_dim_deprived_5=weighted.mean(n_dim_deprived_5, P15, na.rm=T))

cities_norm_summstat$ISO <- countrycode(cities_norm_summstat$ISO, 'iso2c', 'country.name')

write.csv(cities_norm_summstat, "SCP_global_south_summary.csv")

hist(cities_norm_summstat$SCPI_mean)

