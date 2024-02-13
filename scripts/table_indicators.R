
data_dimensions_indicators <- data.frame(indicator_name=c(pos_indic, neg_indic))
data_dimensions_indicators <- dplyr::filter(data_dimensions_indicators, indicator_name != "geom" & indicator_name != "geometry")

data_dimensions_indicators <- dplyr::filter(data_dimensions_indicators, indicator_name != "GDP15_SM" &  indicator_name != "GRGN_L1" &  indicator_name != "GRGN_L1" &  indicator_name != "P15"&  indicator_name != "UC_NM_MN" &  indicator_name != "CTR_MN_ISO")

data_dimensions_indicators <- arrange(data_dimensions_indicators, indicator_name)

data_dimensions_indicators

categories <- c("1-Heat exposure", "2_Thermal comfort Infrastructures & Assets", "3_Social and thermal inequality", "4_Health", "5_ Education and Work standards")

dimensions <- c("WBT", "Urban Planning", "Green & Blue  infrastructure", "Water quality and Sanitation", "Housing materials", "Clothing", "Assets", "Energy", "Thermal Justice",  "Child mortality"," Diarrheal diseases", "Food poisoning", "ELDERLY mortality & morbidity", "Non-Communicable Diseases", "Adverse birth outcome in pregnant women", "School attendance", "Working standards", "Heat adaptation knowledge")

###########

data_dimensions_indicators$dimension <- c(dimensions[2], dimensions[2], dimensions[4], dimensions[2], dimensions[4], dimensions[2], dimensions[2], dimensions[2], dimensions[2], dimensions[2], dimensions[2], dimensions[3])
data_dimensions_indicators$category <- c(categories[1])
data_dimensions_indicators$source <- c("")
data_dimensions_indicators$units <- c("")
data_dimensions_indicators$notes <- c("")

####

data_dimensions_indicators <- arrange(data_dimensions_indicators, category, dimension)
data_dimensions_indicators <- data_dimensions_indicators[,c(3, 2, 1, 4, 5, 6)]

###

library(stargazer)

stargazer(data_dimensions_indicators, summary=F, out="table_dimensions_indicators.html", type = "html", rownames = F)
stargazer(data_dimensions_indicators, summary=F, out="table_dimensions_indicators.tex", rownames = F)
