### indicators calculation 

# 1. WBT

rst$v1_wbt <- rst$wb_t_max / mean(rst$wb_t_max, na.rm=T)

# 2. Urban Planning

rst$v2_urban_planning <- ifelse(rst$index_builtup>1 & rst$index_builtup<4, 1, 0)

# 3. Green & Blue  infrastructure

rst$v3_green_bleu_infra <- rst$green_blue_infra_share / mean(rst$green_blue_infra_share, na.rm=T) 

# 4.a Water quality

rst$v4a_water_quality <- ifelse(rst$Time_Drink_Wat>10 | rst$Time_Drink_Wat == "Low Quality", 1, 0)

# 4.b  Sanitation

rst$v4b_sanitation <- ifelse(rst$Toilet_Shared>3 | rst$Toilet_Qual == "Low Quality", 1, 0)

# 5. Housing materials

rst$v5_housing <- ifelse(rst$Floor_Qual== "Low Quality" | rst$Wall_Qual == "Low Quality" | rst$Roof_Qual == "Low Quality" | rst$HH_members/rst$Sleeping_Rooms>3, 1, 0)

# 6. Clothing

# NOT AVAILABLE

# 7a. Cooling Assets

rst$v7_cooling_assets <- ifelse((rst$Air_Conditioner==0 & rst$Fan == 0) | rst$Refrigerator == 0, 1, 0)

# 7b. Transportation Assets

rst$v7b_transport_assets <- ifelse(rst$Car == 0 | rst$Motorcycle == 0, 1, 0)

# 8.Energy

rst$v8_energy <- ifelse(rst$Electricity==0 | rst$ely_prices_avg>mean(rst$ely_prices_avg), 1, 0)

# 9. Thermal Justice

rst$v9_justice <- ifelse(rst$Sex_head=="female" | rst$Sex_head=="male transgender" | rst$Sex_head=="transgender" | rst$Age_head>65, 1, 0)

# 10. Child mortality; Diarrheal diseases; Food poisoning; ELDERLY mortality & morbidity

rst$n10_health1 <- ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 11. Non-Communicable Diseases
  
rst$v11_health2 <- ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 12. Adverse birth outcome in pregnant women

rst$n12_health3 < ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 13. School attendance

rst$n13_education < ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 14. Working standards

rst$v14_work <- ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 15. Heat adaptation knowledge

rst$v15_adapt_knowledge <- ifelse(rst$wb_t_max > mean(rst$wb_t_max, na.rm=T), 1, 0)

# 16. Cooling policy / heat adaptation plan

rst$v15_adapt_knowledge <- rst$mean_cooling_reg

