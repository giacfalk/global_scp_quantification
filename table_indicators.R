
data_dimensions_indicators <- data.frame(indicator_name=c(pos_indic, neg_indic))
data_dimensions_indicators <- dplyr::filter(data_dimensions_indicators, indicator_name != "geom" & indicator_name != "geometry")

data_dimensions_indicators <- dplyr::filter(data_dimensions_indicators, indicator_name != "GDP15_SM" &  indicator_name != "GRGN_L1" &  indicator_name != "GRGN_L1" &  indicator_name != "P15"&  indicator_name != "UC_NM_MN" &  indicator_name != "CTR_MN_ISO")

data_dimensions_indicators <- arrange(data_dimensions_indicators, indicator_name)

data_dimensions_indicators$indicator_name[data_dimensions_indicators$indicator_name=="mean_ac_reg"] <- "ac_sector_regulatory_quality"

data_dimensions_indicators

categories <- c("1-Heat exposure", "2_Thermal comfort Infrastructures & Assets", "3_Social and thermal inequality", "4_Health", "5_ Education and Work standards")

dimensions <- c("WBT", "Urban Planning", "Green & Blue  infrastructure", "Water quality and Sanitation", "Housing materials", "Clothing", "Assets", "Energy", "Thermal Justice",  "Child mortality"," Diarrheal diseases", "Food poisoning", "ELDERLY mortality & morbidity", "Non-Communicable Diseases", "Adverse birth outcome in pregnant women", "School attendance", "Working standards", "Heat adaptation knowledge")

###########

data_dimensions_indicators$category <- categories[c(2, 5, 4, 5, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 2, 3, 3, 5, 5, 5, 2, 5, 1, 1, 1)]
data_dimensions_indicators$dimension <- dimensions[c(7, 18, 10, 18, 11, 8, 8, 8, 8, 8, 7, 3, 2, 18, 18, 5, 9, 9, 16, 16, 18, 5, 18, 1, 1, 1)]
data_dimensions_indicators$source <- c("Falchetta et al. 2024", "DHS", "DHS", "DHS", "DHS", "World Bank", "Cable.co.uk", "Cable.co.uk", "Cable.co.uk", "Cable.co.uk", "DHS", "Dynamic World (Google)", "EU-JRC GHSL", "UNEP", "DHS", "DHS", "WorldPop", "WorldPop", "DHS", "DHS", "DHS", "DHS", "DHS", "ERA5", "ERA5", "ERA5")
data_dimensions_indicators$units <- c("% of households", "% of households", "% of households", "% of households", "% of households", "# per month", "USD/kWh", "USD/kWh", "USD/kWh", "USD/kWh", "% of households", "% of urban area", "Index", "Index", "% of households", "% of households", "% of people", "% of people", "% of households", "% of households", "% of households", "% of households",  "% of households", "°C", "days", "°C")
data_dimensions_indicators$notes <- c(NA)

####

data_dimensions_indicators <- arrange(data_dimensions_indicators, category, dimension, indicator_name)
data_dimensions_indicators <- data_dimensions_indicators[,c(2, 3, 1, 4, 5, 6)]

###

library(stargazer)

stargazer(data_dimensions_indicators, summary=F, out="table_dimensions_indicators.tex", rownames = F)

###

library("xtable")
print(xtable(data_dimensions_indicators), type="html", file="table_dimensions_indicators.html", include.rownames=FALSE)
