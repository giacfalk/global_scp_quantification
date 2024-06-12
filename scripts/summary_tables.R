
library(modelsummary)

cities_norm$wb_t_max_orig <- cities$wb_t_max
cities_norm$P15_orig <- cities_norm$P15_orig/1e6

if (!("CTR_MN_ISO" %in% colnames(cities_norm))) {
  cities_norm[, "CTR_MN_ISO"] <- cities_norm$ISO
}

datasummary(Factor(CTR_MN_ISO) ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_countries.html")


###

if(scale=="cities"){

library(stargazer)

cc <- as.data.frame(cities_norm %>% dplyr::select(CTR_MN_ISO, UC_NM_MN, SCPI, P15_orig) %>% st_set_geometry(NULL))
cc <- arrange(cc, desc(SCPI))

stargazer(cc, summary = F, out =  "figures/table_cites.html", type = "html")

} else {

  datasummary(Factor(CTR_MN_ISO) *  DHSREGEN ~  SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_regions.html")
  
}
