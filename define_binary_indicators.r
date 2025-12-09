### indicators calculation

# 1. WBT
#https://www.nature.com/articles/d41586-024-02422-5#:~:text=But%20Jay%20and%20his%20team's,%C2%B0C%20for%20older%20people
#https://www.psu.edu/news/research/story/humans-cant-endure-temperatures-and-humidities-high-previously-thought/
rst$v1_wbt <- ifelse(rst$wb_t_max > 27 | rst$wb_t_gt_30_ndays>7, 1, 0)

# 2. Urban Planning

# rst$v2_urban_planning <- ifelse(rst$index_builtup>0 & rst$index_builtup<4, 1, 0)

#https://developers.google.com/earth-engine/datasets/catalog/RUB_RUBCLIM_LCZ_global_lcz_map_latest#bands
rst$v2_urban_planning <- ifelse((rst$lcz %in% c(1,2,3) | (rst$index_builtup>0 & rst$index_builtup<4)), 1, 0)

# 3. Green & Blue  infrastructure

rst <- rst %>% group_by(CTRY) %>% mutate(v3_green_bleu_infra = ifelse(green_blue_infra_share / mean(green_blue_infra_share, na.rm=T)<1, 1, 0))

# 4.a Water quality

rst$v4a_water_quality <- ifelse(rst$Time_Drink_Wat>10 | rst$Drink_Wat_Qual == "Low Quality", 1, 0)

# 4.b  Sanitation

rst$v4b_sanitation <- ifelse(rst$Toilet_Shared>3 | rst$Toilet_Qual == "Low Quality", 1, 0)

# 5. Housing materials

rst$v5_housing <- ifelse((rst$Floor_Architecture %in% c("Modern", "Makeshift") & rst$Wall_Architecture %in% c("Modern", "Makeshift") & rst$Roof_Architecture  %in% c("Modern", "Makeshift")) | rst$HH_members/rst$Sleeping_Rooms>3, 1, 0)

# 6. Clothing

# NOT AVAILABLE

# 7a. Cooling Assets
rst$v7_cooling_assets  <- NA

for(ctry in unique(rst$CTRY)){
  if(sum(is.na(rst$Air_Conditioner[rst$CTRY==ctry] | rst$Fan[rst$CTRY==ctry]))>=0.75*nrow(rst[rst$CTRY==ctry,])){
    rst$v7_cooling_assets[rst$CTRY==ctry] <- ifelse(rst$ac[rst$CTRY==ctry]<0.5 & rst$Refrigerator[rst$CTRY==ctry]==0, 1, 0)
  } else{
    rst$v7_cooling_assets[rst$CTRY==ctry] <- ifelse((rst$Air_Conditioner[rst$CTRY==ctry]==0 & rst$Fan[rst$CTRY==ctry] == 0) & rst$Refrigerator[rst$CTRY==ctry]==0, 1, 0)

  }}


# 7b. Transportation Assets

rst$v7b_transport_assets <- ifelse(rst$Car == 0 & rst$Motorcycle == 0, 1, 0)

# 8.Energy

rst <- rst %>% group_by(CTRY) %>% mutate(v8_energy = ifelse(Electricity==0 | (ely_prices_avg>mean(ely_prices$avg, na.rm=T) & ely_outages>mean(unlist(values(ely_outages_r)), na.rm=T)), 1, 0)) #blackouts? exceed global avg. or >1-2/month

# 9. Thermal Justice

# Ilaria: Ethnic_Minority and Religion_Minority calculated at the province level?
rst$v9_justice <- ifelse(((rst$Sex_head=="female" | rst$Sex_head=="male transgender" | rst$Sex_head=="transgender") &  (rst$Wealth %in% c("Poorest", "Poorer", "Middle"))) | ((rst$Religion_Minority_reg==1) & (rst$Wealth %in% c("Poorest", "Poorer", "Middle"))) | ((rst$Wealth %in% c("Poorest", "Poorer", "Middle") & (rst$Ethnic_Minority_reg==1))) | ((rst$Age_head>65) & rst$Wealth %in% c("Poorest", "Poorer", "Middle")), 1, 0) # ethnicity & religion?

# 10. Child mortality; Diarrheal diseases; Food poCTRYning; ELDERLY mortality & morbidity
rst <- rst %>% group_by(CTRY) %>% mutate(v10_health1 = ifelse(Diarrhea_Recently_Ratio>mean(Diarrhea_Recently_Ratio, na.rm=T), 1, 0)) #change to rate

# 11. Non-Communicable Diseases
rst <- rst %>% group_by(CTRY) %>% mutate(v11_health2 = ifelse(Dead_Child_Ratio>mean(Dead_Child_Ratio, na.rm=T), 1, 0)) #change to rate

# 12. Adverse birth outcome in pregnant women
rst <- rst %>% group_by(CTRY) %>% mutate(v12_health3 = ifelse(Terminated_Pregnancies_Ratio>mean(Terminated_Pregnancies_Ratio, na.rm=T) | Early_Pregnancies_Ratio>mean(Early_Pregnancies_Ratio, na.rm=T), 1, 0)) #change to rate

# 12b. Accessibility to healthcare

rst <- rst %>% mutate(v12_health4 = ifelse(healthcaretime>60, 1, 0)) #change to rate

# 13. School attendance

rst$v13_education <- ifelse(rst$Schooling!=1, 1, 0)

# 14. Working standards

rst$v14_work <- ifelse(rst$Work_Standards!="Not Exposed", 1, 0)

# 15. Heat adaptation knowledge

rst$v15_adapt_knowledge <- ifelse(rst$Radio!=1 & rst$Television!=1 & rst$Computer!=1 & rst$Phone!=1, 1, 0)

# 16. Cooling policy / heat adaptation plan
rst <- rst %>% group_by(CTRY) %>% mutate(v16_cooling_policy = ifelse(mean_cooling_reg<0.5, 1, 0))

###############
###############

rst$v13_education[rst$CTRY=="YE"] <- 0.65 # https://data.worldbank.org/indicator/SE.PRM.CMPT.ZS?locations=YE

# rst <- rst %>% mutate_if(is.numeric , replace_na, replace = 0)

###############
##############

rst$dim1_climate <- rst$v1_wbt

rst$dim2_infra_assets <- ifelse(((rst$v2_urban_planning==1 | rst$v3_green_bleu_infra==1 | rst$v5_housing ==1) & rst$v7b_transport_assets==1) | ((rst$v4a_water_quality==1 | rst$v4b_sanitation==1) & rst$v7b_transport_assets==1) | ((rst$v7_cooling_assets==1 | rst$v8_energy==1)  & rst$v7b_transport_assets==1), 1, 0)

rst$dim3_social_thermal_ineq <- ifelse(rst$v9_justice==1, 1, 0)

rst$dim4_health <- ifelse(rst$v10_health1==1 | rst$v11_health2==1 | rst$v12_health3==1 | rst$v12_health4==1, 1, 0)

rst$dim5_edu_work_stds <- ifelse(((rst$v13_education==1 | rst$v14_work==1 | rst$v15_adapt_knowledge==1)), 1, 0)

###

rst$geometry <- rst_geo
rst <- st_as_sf(rst)

rst_table <- rst
rst_table$geometry<-NULL

for(ctry in unique(rst$country)){
  sink("aa")
  tablo <- rst_table %>% filter(country==ctry) %>% dplyr::select(starts_with("v") | starts_with("dim")) %>% as.data.frame(.)
  stargazer::stargazer(tablo, summary = T, type = "html", out=paste0("summar_tab_vardim_", ctry, ".html"), title=paste0("Summary statistics for ", ctry), label=paste0())
  sink()
}

for(ctry in unique(rst$country)){
  sink("aa")
  tablo <- rst_table %>% filter(country==ctry) %>% dplyr::select(starts_with("v") | starts_with("dim")) %>% as.data.frame(.)
  stargazer::stargazer(tablo, summary = T, type = "latex", out=paste0("summar_tab_vardim_", ctry, ".tex"), title=paste0("Summary statistics for ", ctry), label=paste0("tab:si_", tolower(ctry)))
  sink()
}

###########

rst$n_dim_deprived <- rowSums(as.matrix(data.frame(rst$dim1_climate, rst$dim2_infra_assets, rst$dim3_social_thermal_ineq, rst$dim4_health, rst$dim5_edu_work_stds)), na.rm=T)

rst$n_dim_deprived_0 <- ifelse(rst$n_dim_deprived==0, 1, 0)
rst$n_dim_deprived_1 <- ifelse(rst$n_dim_deprived==1, 1, 0)
rst$n_dim_deprived_2 <- ifelse(rst$n_dim_deprived==2, 1, 0)
rst$n_dim_deprived_3 <- ifelse(rst$n_dim_deprived==3, 1, 0)
rst$n_dim_deprived_4 <- ifelse(rst$n_dim_deprived==4, 1, 0)
rst$n_dim_deprived_5 <- ifelse(rst$n_dim_deprived==5, 1, 0)


