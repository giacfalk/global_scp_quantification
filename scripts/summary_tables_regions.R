
library(modelsummary)

cities_norm$wb_t_max_orig <- cities$wb_t_max

datasummary(Factor(CTR_MN_ISO) ~  wb_t_max_orig*mean + SCPI * mean + P15_orig * sum, data = cities_norm, output = "figures/table_countries.html")


###

library(stargazer)

cc <- as.data.frame(cities_norm %>% dplyr::select(UC_NM_MN, wb_t_max_orig, SCPI, P15_orig) %>% st_set_geometry(NULL))
cc <- arrange(cc, desc(SCPI))

stargazer(cc, summary = F, out =  "figures/table_ciites.html", type = "html")
