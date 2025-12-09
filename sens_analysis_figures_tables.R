
### figures and tables for sensitivity analysis

# 0)  leave one out exercise 

outer_cities_norm <- pblapply(1:18, function(X){
  
  outer_rst_temp <- rst_table
  
  outer_rst_temp <- dplyr::select(outer_rst_temp, starts_with("v"))
  outer_rst_temp$CTRY <- NULL
  outer_rst_temp[,X] <- NA
  
  outer_rst_temp$dim1_climate <- outer_rst_temp$v1_wbt
  
  outer_rst_temp$dim2_infra_assets <- ifelse(((outer_rst_temp$v2_urban_planning==1 | outer_rst_temp$v3_green_bleu_infra==1 | outer_rst_temp$v5_housing ==1) & outer_rst_temp$v7b_transport_assets==1) | ((outer_rst_temp$v4a_water_quality==1 | outer_rst_temp$v4b_sanitation==1) & outer_rst_temp$v7b_transport_assets==1) | ((outer_rst_temp$v7_cooling_assets==1 | outer_rst_temp$v8_energy==1)  & outer_rst_temp$v7b_transport_assets==1), 1, 0)
  
  outer_rst_temp$dim3_social_thermal_ineq <- ifelse(outer_rst_temp$v9_justice==1, 1, 0)
  
  outer_rst_temp$dim4_health <- ifelse(outer_rst_temp$v10_health1==1 | outer_rst_temp$v11_health2==1 | outer_rst_temp$v12_health3==1 | outer_rst_temp$v12_health4==1, 1, 0)
  
  outer_rst_temp$dim5_edu_work_stds <- ifelse(((outer_rst_temp$v13_education==1 | outer_rst_temp$v14_work==1 | outer_rst_temp$v15_adapt_knowledge==1)), 1, 0)
  
  outer_rst_temp <- dplyr::select(outer_rst_temp, starts_with("dim"))
  
  ###
  
  outer_rst_temp$SCPI <- rowMeans(outer_rst_temp[c("dim1_climate", "dim2_infra_assets", "dim3_social_thermal_ineq", "dim4_health", "dim5_edu_work_stds")], na.rm = T)
  
  return(outer_rst_temp$SCPI)
  
})

outer_rst_temp <- rst_table

outer_rst_temp$SCPI <- rowMeans(outer_rst_temp[c("dim1_climate", "dim2_infra_assets", "dim3_social_thermal_ineq", "dim4_health", "dim5_edu_work_stds")], na.rm = T)

#

names(outer_cities_norm) <- colnames(dplyr::select(rst_table %>% ungroup(.), starts_with("v")))

outer_cities_norm <- bind_cols(outer_cities_norm)

outer_cities_norm$...0 <- outer_rst_temp$SCPI

outer_cities_norm$CTRY <- rst_table$CTRY

outer_cities_norm <- dplyr::group_by(outer_cities_norm, CTRY) %>% summarise_all(mean, na.rm=T)

colnames(outer_cities_norm)[2:19] <- c("Wet-bulb temperature", "Urban planning", "Green-blue infrastructure", "Water quality", "Sanitation", "Housing", "Cooling assets", "Transport assets", "Energy", "Thermal justice", "Diarrhea prevalence", "Child mortality", "Pregnancy issues", "Healthcare accessibility", "Education", "Working standards", "Adaptation knowledge", "Cooling policy")

outer_cities_norm <- reshape2::melt(outer_cities_norm, 1)

#


outer_cities_norm2 <- outer_cities_norm %>%
  group_by(CTRY) %>%
  mutate(
    q1    = quantile(value, 0.25),
    q3    = quantile(value, 0.75),
    iqr   = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    is_out = (value < lower) | (value > upper)
  ) %>%
  ungroup()

# plot: boxplots without default outliers, then add outlier points colored by `variable`
ggplot() +
  theme_classic() +
  # use value*100 for y as in your base
  geom_boxplot(
    data = outer_cities_norm2,
    aes(x = CTRY, y = value * 100),
    outlier.shape = NA,    # suppress default outliers so we can draw them ourselves
    width = 0.6
  ) + 
  geom_point(data=outer_cities_norm[outer_cities_norm$variable=="...0",], aes(x=CTRY, y=value*100), colour="black", size=3, shape=18)+
  # the outliers, coloured by the categorical variable
  geom_point(
    data = filter(outer_cities_norm2, is_out),
    aes(x = CTRY, y = value * 100, colour = variable),
    position = position_jitter(width = 0.12, height = 0),
    size = 2
  ) +
  ylab("SCPI") +
  ggtitle("Leave-one-variable-out sensitivity analysis") +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_colour_brewer(palette = "Set1", name="Indicator")

ggsave("figures/scp_LOO.png", width=6, height=3.5, scale=1.4)

ggplot(outer_cities_norm2 %>% filter(is_out == TRUE),
       aes(x = 1, fill = variable)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_fill_brewer(palette = "Set1", name="Indicator")+
  geom_text(
    stat = "count",
    aes(label = paste0(round(..count../tapply(..count.., ..x.., sum)[as.character(..x..)] * 100, 1), "%")), size=3,
    position = position_fill(vjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggtitle("Share of outliers by variable")

ggsave("figures/scp_LOO_hist.png", width=3.5, height=3.5, scale=1.65)

###

# 1) indicators definition 

library(lhs)
library(pbapply)

set.seed(123)  # reproducibility
n_samples <- 1e3

sens_v1a <- list(25, 27, 29)
sens_v1b <- list(7, 5, 14)
sens_v2 <- list(list(1:3), list(1,2), list(2,3), list(1,3))
sens_v3 <- list(mean(rst_table$green_blue_infra_share, na.rm=T), 0.75, 1.5)
sens_v4a <- list(list(10, "Low Quality"), list(20, "Mid Quality"), list(5, "Low Quality"))
sens_v4b <- list(list(3, "Low Quality"), list(5, "Mid Quality"), list(10, "Low Quality"))
sens_v5 <- list(list("Modern", "Makeshift", 3), list("Modern", 3), list("Makeshift", 3), list("Modern", "Makeshift", 5), list("Modern", 5), list("Makeshift", 5))
sens_v7 <- list(list(0.75, 0.5, 0, 0, 0), list(0.5, 0.25, 0, 0, 0))
sens_v7b <- list(list(0, 0), list(0, 1), list(1,0))
sens_v8 <- list(list(0, mean(rst_table$ely_prices_avg, na.rm=T), mean(rst_table$ely_outages, na.rm=T)), list(1, 0.15, 5), list(1, 0.2, 7.5))
sens_v9_1 <- list(list("female", "male transgender", "transgender"), list("female"), list("female"))
sens_v9_2 <- list(list("Poorest", "Poorer", "Middle"), list("Poorest", "Poorer"), list("Poorest"))
sens_v9_3 <- list(1, 0, 0)
sens_v9_4 <- list(65, 50, 40)
sens_v10 <- list(mean(rst_table$Diarrhea_Recently_Ratio, na.rm=T), 0.02, 0.05)
sens_v11 <- list(mean(rst_table$Dead_Child_Ratio, na.rm=T), 0.01, 0.05)
sens_v12 <- list(list(mean(rst_table$Terminated_Pregnancies_Ratio, na.rm=T), mean(rst_table$Early_Pregnancies_Ratio, na.rm=T)), list(0.075, 0.035), list(0.15, 0.1))
sens_v12b <- list(60, 30, 120)
sens_v13 <- list(0.99, 0.6)
sens_v14 <- list("Not Exposed", "Exposed")
sens_v15 <- list(list(1,1,1,1), list(0,1,0,1), list(1,0,1,0))
sens_v16 <- list(mean(rst_table$mean_cooling_reg), 0.25, 0.75)

###########################
###########################
###########################

sens_grid <- list(sens_v1a=sens_v1a, sens_v1b=sens_v1b, sens_v2=sens_v2, sens_v3=sens_v3, sens_v4a=sens_v4a, sens_v4b=sens_v4b, sens_v5=sens_v5, sens_v7=sens_v7, sens_v7b=sens_v7b, sens_v8=sens_v8, sens_v9_1=sens_v9_1, sens_v9_2=sens_v9_2, sens_v9_3=sens_v9_3, sens_v9_4=sens_v9_4, sens_v10=sens_v10, sens_v11=sens_v11, sens_v12=sens_v12, sens_v12b=sens_v12b, sens_v13=sens_v13, sens_v14=sens_v14, sens_v15=sens_v15, sens_v16=sens_v16)

###

stargazer::stargazer(lapply(sens_grid, as.data.frame), summary=F, out="figures/sens_randomdraws.tex")

###

n_vars <- length(sens_grid)

# Generate Latin Hypercube samples in [0,1]
lhs_matrix <- randomLHS(n_samples, n_vars)

# Convert to discrete selections (each variable has 3 discrete values)
random_combo <- as.data.frame(mapply(function(values, probs) {
  # map each LHS value to one of the discrete choices
  idx <- ceiling(probs * length(values))
  idx[idx == 0] <- 1  # ensure index starts at 1
  values[idx]
}, sens_grid, as.data.frame(lhs_matrix)))

#

outer_rst_extract <- pblapply(1:n_samples, function(i){
  
  print(i)

  v1a <- unlist(random_combo[i,1])
  v1b <- unlist(random_combo[i,2])
  v2 <- unlist(random_combo[i,3])
  v3 <- unlist(random_combo[i,4])
  v4a <- unlist(random_combo[i,5])
  v4b <- unlist(random_combo[i,6])
  v5 <- unlist(random_combo[i,7])
  v7 <- unlist(random_combo[i,8])
  v7b <- unlist(random_combo[i,9])
  v8 <- unlist(random_combo[i,10])
  v9_1 <- unlist(random_combo[i,11])
  v9_2 <- unlist(random_combo[i,12])
  v9_3 <- unlist(random_combo[i,13])
  v9_4 <- unlist(random_combo[i,14])
  v10 <- unlist(random_combo[i,15])
  v11 <- unlist(random_combo[i,16])
  v12 <- unlist(random_combo[i,17])
  v12b <- unlist(random_combo[i,18])
  v13 <- unlist(random_combo[i,19])
  v14 <- unlist(random_combo[i,20])
  v15 <- unlist(random_combo[i,21])
  v16 <- unlist(random_combo[i,22])
  
  ######
  ######
  
  rst_temp <- rst_table
  
  rst_temp$v1_wbt <- ifelse(rst_temp$wb_t_max > v1a | rst_temp$wb_t_gt_30_ndays>v1b, 1, 0)
  
  rst_temp$v2_urban_planning <- ifelse((rst_temp$lcz %in% v2 | (rst_temp$index_builtup>v2 & rst_temp$index_builtup<v2)), 1, 0)
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% mutate(v3_green_bleu_infra = ifelse(green_blue_infra_share / v3<1, 1, 0))
  
  rst_temp$v4a_water_quality <- ifelse(rst_temp$Time_Drink_Wat>v4a[1] | rst_temp$Drink_Wat_Qual == v4a[2], 1, 0)
  
  rst_temp$v4b_sanitation <- ifelse(rst_temp$Toilet_Shared>v4b[1] | rst_temp$Toilet_Qual == v4b[2], 1, 0)
  
  rst_temp$v5_housing <- ifelse((rst_temp$Floor_Architecture %in% v5[1] & rst_temp$Wall_Architecture %in% v5[2] & rst_temp$Roof_Architecture  %in% v5[3]) | rst_temp$HH_members/rst_temp$Sleeping_Rooms>v5[4], 1, 0)
  
  rst_temp$v7_cooling_assets  <- NA
  
  for(ctry in unique(rst_temp$CTRY)){
    if(sum(is.na(rst_temp$Air_Conditioner[rst_temp$CTRY==ctry] | rst_temp$Fan[rst_temp$CTRY==ctry]))>=v7[1]*nrow(rst_temp[rst_temp$CTRY==ctry,])){
      rst_temp$v7_cooling_assets[rst_temp$CTRY==ctry] <- ifelse(rst_temp$ac[rst_temp$CTRY==ctry]<v7[2] & rst_temp$Refrigerator[rst_temp$CTRY==ctry]==0, 1, 0)
    } else{
      rst_temp$v7_cooling_assets[rst_temp$CTRY==ctry] <- ifelse((rst_temp$Air_Conditioner[rst_temp$CTRY==ctry]==v7[3] & rst_temp$Fan[rst_temp$CTRY==ctry] == v7[4]) & rst_temp$Refrigerator[rst_temp$CTRY==ctry]==v7[5], 1, 0)
      
    }}
  
  rst_temp$v7b_transport_assets <- ifelse(rst_temp$Car == v7b[1] & rst_temp$Motorcycle == v7b[2], 1, 0)
  
  # rm(ely_prices)
  # outages <- ely_outages$mrv
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% dplyr::mutate(v8_energy = ifelse(Electricity==v8[1] | (ely_prices_avg>v8[2] & ely_outages>v8[3]), 1, 0)) #blackouts? exceed global avg. or >1-2/month
  
  #############
  
  rst_temp$v9_justice <- ifelse(((rst_temp$Sex_head %in% v9_1) &  (rst_temp$Wealth %in% v9_2)) | ((rst_temp$Religion_Minority_reg==v9_3) & (rst_temp$Wealth %in% v9_2)) | ((rst_temp$Wealth %in% v9_2 & (rst_temp$Ethnic_Minority_reg==v9_3))) | ((rst_temp$Age_head>v9_4) & rst_temp$Wealth %in% v9_2), 1, 0) # ethnicity & religion?
  
  ###########
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% mutate(v10_health1 = ifelse(Diarrhea_Recently_Ratio>v10, 1, 0)) #change to rate
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% mutate(v11_health2 = ifelse(Dead_Child_Ratio>v11, 1, 0)) #change to rate
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% mutate(v12_health3 = ifelse(Terminated_Pregnancies_Ratio>v12[1] | Early_Pregnancies_Ratio>v12[2], 1, 0))
  
  rst_temp <- rst_temp %>% mutate(v12_health4 = ifelse(healthcaretime>v12b, 1, 0)) #change to rate
  
  rst_temp$v13_education <- ifelse(rst_temp$Schooling>v13, 1, 0)
  
  rst_temp$v13_education[rst_temp$CTRY=="YE"] <- 0.65 # https://data.worldbank.org/indicator/SE.PRM.CMPT.ZS?locations=YE
  
  rst_temp$v14_work <- ifelse(rst_temp$Work_Standards!=v14, 1, 0)
  
  rst_temp$v15_adapt_knowledge <- ifelse(rst_temp$Radio!=v15[1] & rst_temp$Television!=v15[2] & rst_temp$Computer!=v15[3] & rst_temp$Phone!=v15[4], 1, 0)
  
  rst_temp <- rst_temp %>% group_by(CTRY) %>% mutate(v16_cooling_policy = ifelse(mean_cooling_reg<v16, 1, 0))
  
  ###############
  ##############
  
  ### 
  
  # need to carefully think this through (variables to dimension aggregation)
  
  ###
  
  rst_temp$dim1_climate <- rst_temp$v1_wbt
  
  rst_temp$dim2_infra_assets <- ifelse(((rst_temp$v2_urban_planning==1 | rst_temp$v3_green_bleu_infra==1 | rst_temp$v5_housing ==1) & rst_temp$v7b_transport_assets==1) | ((rst_temp$v4a_water_quality==1 | rst_temp$v4b_sanitation==1) & rst_temp$v7b_transport_assets==1) | ((rst_temp$v7_cooling_assets==1 | rst_temp$v8_energy==1)  & rst_temp$v7b_transport_assets==1), 1, 0)
  
  rst_temp$dim3_social_thermal_ineq <- ifelse(rst_temp$v9_justice==1, 1, 0)
  
  rst_temp$dim4_health <- ifelse(rst_temp$v10_health1==1 | rst_temp$v11_health2==1 | rst_temp$v12_health3==1 | rst_temp$v12_health4==1, 1, 0)
  
  rst_temp$dim5_edu_work_stds <- ifelse(((rst_temp$v13_education==1 | rst_temp$v14_work==1 | rst_temp$v15_adapt_knowledge==1)), 1, 0)
  
  rst_temp$n_dim_deprived <- rowSums(as.matrix(data.frame(rst_temp$dim1_climate, rst_temp$dim2_infra_assets, rst_temp$dim3_social_thermal_ineq, rst_temp$dim4_health, rst_temp$dim5_edu_work_stds)), na.rm=T)
  
  rst_temp$n_dim_deprived_0 <- ifelse(rst_temp$n_dim_deprived==0, 1, 0)
  rst_temp$n_dim_deprived_1 <- ifelse(rst_temp$n_dim_deprived==1, 1, 0)
  rst_temp$n_dim_deprived_2 <- ifelse(rst_temp$n_dim_deprived==2, 1, 0)
  rst_temp$n_dim_deprived_3 <- ifelse(rst_temp$n_dim_deprived==3, 1, 0)
  rst_temp$n_dim_deprived_4 <- ifelse(rst_temp$n_dim_deprived==4, 1, 0)
  rst_temp$n_dim_deprived_5 <- ifelse(rst_temp$n_dim_deprived==5, 1, 0)
  
  rst_temp <- dplyr::select(rst_temp, 1:8, 10:11, 109:120)
  
  outer_rst_temp <- rst_temp
    
  outer_rst_temp$n_dim_deprived_0 <- ifelse(outer_rst_temp$n_dim_deprived==0, 1, 0)
  outer_rst_temp$n_dim_deprived_1 <- ifelse(outer_rst_temp$n_dim_deprived==1, 1, 0)
  outer_rst_temp$n_dim_deprived_2 <- ifelse(outer_rst_temp$n_dim_deprived==2, 1, 0)
  outer_rst_temp$n_dim_deprived_3 <- ifelse(outer_rst_temp$n_dim_deprived==3, 1, 0)
  outer_rst_temp$n_dim_deprived_4 <- ifelse(outer_rst_temp$n_dim_deprived==4, 1, 0)
  outer_rst_temp$n_dim_deprived_5 <- ifelse(outer_rst_temp$n_dim_deprived==5, 1, 0)
  
  outer_rst_temp <- outer_rst_temp %>% group_by(country) %>% dplyr::summarise(n_dim_deprived_0=weighted.mean(n_dim_deprived_0, sampling_weight, na.rm=T), n_dim_deprived_1=weighted.mean(n_dim_deprived_1, sampling_weight, na.rm=T), n_dim_deprived_2=weighted.mean(n_dim_deprived_2, sampling_weight, na.rm=T), n_dim_deprived_3=weighted.mean(n_dim_deprived_3, sampling_weight, na.rm=T), n_dim_deprived_4=weighted.mean(n_dim_deprived_4, sampling_weight, na.rm=T), n_dim_deprived_5=weighted.mean(n_dim_deprived_5, sampling_weight, na.rm=T))
  
  #
  
  outer_rst_temp2 <- rst_temp
  
  outer_rst_temp2$SCP <- rowMeans(outer_rst_temp2[,c("dim1_climate", "dim2_infra_assets", "dim3_social_thermal_ineq", "dim4_health", "dim5_edu_work_stds"  )], na.rm=T)
  
  outer_rst_temp2 <- outer_rst_temp2 %>% group_by(country) %>% dplyr::summarise(SCP=weighted.mean(SCP, sampling_weight, na.rm=T))

return(list(outer_rst_temp %>% reshape2::melt(1), outer_rst_temp2 %>% reshape2::melt(1)))

})

############################
############################

first_list  <- lapply(outer_rst_extract, `[[`, 1L)
second_list <- lapply(outer_rst_extract, `[[`, 2L)

############################
############################
############################
############################

names(first_list) <- paste0("draw_", 1:n_samples)

###

rst_extract <- bind_rows(first_list, .id = "draw")
rst_extract$variable <- gsub("n_dim_deprived_", "", rst_extract$variable)

ggplot(rst_extract)+
  theme_classic()+
         geom_boxplot(aes(y=value*100, fill=variable))+
  facet_wrap(vars(country), ncol=7)+
  theme(legend.position = "bottom", legend.direction="horizontal", axis.text.x = element_blank())+
  scale_fill_discrete(name="Number of dimensions deprived")+
  ylab("% of population")

ggsave("figures/scp_deprivation_bydimension.png", width=6, height=3, scale=1.2)

##################
##################

names(second_list) <- paste0("draw_", 1:n_samples)

###

rst_extract <- bind_rows(second_list, .id = "draw")

rst_extract$country <- countrycode::countrycode(rst_extract$country, 'country.name', 'iso2c')

#

rst_table_summ <- rst_table

rst_table_summ$SCPI <- rowMeans(data.frame(rst_table_summ$dim1_climate, rst_table_summ$dim2_infra_assets, rst_table_summ$dim3_social_thermal_ineq, rst_table_summ$dim4_health, rst_table_summ$dim5_edu_work_stds), na.rm=T)

rst_table_summ <- rst_table_summ %>%  group_by(CTRY) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, sampling_weight, na.rm=T))

#

rst_extract$country <- factor(rst_extract$country, levels=arrange(rst_table_summ, desc(SCPI)) %>% pull(CTRY))
rst_table_summ$CTRY <- factor(rst_table_summ$CTRY, levels=arrange(rst_table_summ, desc(SCPI)) %>% pull(CTRY))

#

ggplot()+
  theme_classic()+
  geom_boxplot(data=rst_extract, aes(x=country, y=value*100))+
  geom_point(data=rst_table_summ, aes(x=CTRY, y=SCPI*100), colour="red")+
  theme(legend.position = "none")+
  ylab("SCPI")

ggsave("figures/scp_deprivation_index.png", width=6, height=3, scale=1.2)

##################
##################
##################
##################

# 2) indicators to dimensions aggregation 

### dimensions calculation

library(pbapply)

iter_f <- function(iter){
  
  data <- rst_table
  
  if(iter==1){
    
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing ==1) & data$v7b_transport_assets==1) | ((data$v4a_water_quality==1 | data$v4b_sanitation==1) & data$v7b_transport_assets==1) | ((data$v7_cooling_assets==1 | data$v8_energy==1)  & data$v7b_transport_assets==1), 1, 0)
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    data$dim4_health <- ifelse(data$v10_health1==1 | data$v11_health2==1 | data$v12_health3==1 | data$v12_health4==1, 1, 0)
    
    data$dim5_edu_work_stds <- ifelse(((data$v13_education==1 | data$v14_work==1 | data$v15_adapt_knowledge==1)), 1, 0) } else if (iter==2){
      
      data$dim1_climate <- data$v1_wbt
      
      data$dim2_infra_assets <- ifelse(((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing ==1) | data$v7b_transport_assets==1) | ((data$v4a_water_quality==1 | data$v4b_sanitation==1) | data$v7b_transport_assets==1) | ((data$v7_cooling_assets==1 | data$v8_energy==1)  | data$v7b_transport_assets==1), 1, 0)
      
      data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
      
      data$dim4_health <- ifelse(data$v10_health1==1 | data$v11_health2==1 | data$v12_health3==1 | data$v12_health4==1, 1, 0)
      
      data$dim5_edu_work_stds <- ifelse(((data$v13_education==1 | data$v14_work==1 | data$v15_adapt_knowledge==1)), 1, 0) 
      
    } else if (iter==3){

      
      data$dim1_climate <- data$v1_wbt
      
      data$dim2_infra_assets <- ifelse(((data$v2_urban_planning==1 & data$v3_green_bleu_infra==1 & data$v5_housing ==1) & data$v7b_transport_assets==1) & ((data$v4a_water_quality==1 & data$v4b_sanitation==1) & data$v7b_transport_assets==1) & ((data$v7_cooling_assets==1 & data$v8_energy==1)  & data$v7b_transport_assets==1), 1, 0)
      
      data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
      
      data$dim4_health <- ifelse(data$v10_health1==1 & data$v11_health2==1 & data$v12_health3==1 & data$v12_health4==1, 1, 0)
      
      data$dim5_edu_work_stds <- ifelse(((data$v13_education==1 & data$v14_work==1 & data$v15_adapt_knowledge==1)), 1, 0) 
      
    }
      
  data$n_dim_deprived <- rowSums(as.matrix(data.frame(data$dim1_climate, data$dim2_infra_assets, data$dim3_social_thermal_ineq, data$dim4_health, data$dim5_edu_work_stds)), na.rm=T)
  
  data$n_dim_deprived_0 <- ifelse(data$n_dim_deprived==0, 1, 0)
  data$n_dim_deprived_1 <- ifelse(data$n_dim_deprived==1, 1, 0)
  data$n_dim_deprived_2 <- ifelse(data$n_dim_deprived==2, 1, 0)
  data$n_dim_deprived_3 <- ifelse(data$n_dim_deprived==3, 1, 0)
  data$n_dim_deprived_4 <- ifelse(data$n_dim_deprived==4, 1, 0)
  data$n_dim_deprived_5 <- ifelse(data$n_dim_deprived==5, 1, 0)
  
  data <- dplyr::select(data, 1:8, 10:11, 109:120)
  
  
  return(data)
  
}


outer_rst_dimensions <- pblapply(1:3, iter_f)

#######################################

outer_rst_dimensions_extract <- pblapply(1:length(outer_rst_dimensions), function(X){
  
  outer_rst_temp <- outer_rst_dimensions
  
  outer_rst_temp[[X]]$n_dim_deprived_0 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==0, 1, 0)
  outer_rst_temp[[X]]$n_dim_deprived_1 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==1, 1, 0)
  outer_rst_temp[[X]]$n_dim_deprived_2 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==2, 1, 0)
  outer_rst_temp[[X]]$n_dim_deprived_3 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==3, 1, 0)
  outer_rst_temp[[X]]$n_dim_deprived_4 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==4, 1, 0)
  outer_rst_temp[[X]]$n_dim_deprived_5 <- ifelse(outer_rst_temp[[X]]$n_dim_deprived==5, 1, 0)
  
  outer_rst_temp[[X]] <- outer_rst_temp[[X]] %>% group_by(country) %>% dplyr::summarise(n_dim_deprived_0=weighted.mean(n_dim_deprived_0, sampling_weight, na.rm=T), n_dim_deprived_1=weighted.mean(n_dim_deprived_1, sampling_weight, na.rm=T), n_dim_deprived_2=weighted.mean(n_dim_deprived_2, sampling_weight, na.rm=T), n_dim_deprived_3=weighted.mean(n_dim_deprived_3, sampling_weight, na.rm=T), n_dim_deprived_4=weighted.mean(n_dim_deprived_4, sampling_weight, na.rm=T), n_dim_deprived_5=weighted.mean(n_dim_deprived_5, sampling_weight, na.rm=T))
  
  return(outer_rst_temp[[X]] %>% reshape2::melt(1))
  
})

names(outer_rst_dimensions_extract) <- paste0("draw_", 1:length(outer_rst_dimensions))

###

rst_extract <- bind_rows(outer_rst_dimensions_extract, .id = "draw")
rst_extract$variable <- gsub("n_dim_deprived_", "", rst_extract$variable)

ggplot(rst_extract)+
  theme_classic()+
  geom_boxplot(aes(y=value*100, fill=variable))+
  facet_wrap(vars(country), ncol=7)+
  theme(legend.position = "bottom", legend.direction="horizontal", axis.text.x = element_blank())+
  ylab("% of population")+
  scale_fill_discrete(name="Number of dimensions deprived")

ggsave("figures/scp_deprivation_dimensionsensitivity_bydimension.png", width=6, height=3, scale=1.2)

#

outer_rst_dimensions_extract <- pblapply(1:length(outer_rst_dimensions), function(X){
  
  outer_rst_temp <- outer_rst_dimensions
  
  outer_rst_temp[[X]]$SCP <- rowMeans(outer_rst_temp[[X]][,c("dim1_climate", "dim2_infra_assets", "dim3_social_thermal_ineq", "dim4_health", "dim5_edu_work_stds"  )], na.rm=T)
  
  outer_rst_temp[[X]] <- outer_rst_temp[[X]] %>% group_by(country) %>% dplyr::summarise(SCP=weighted.mean(SCP, sampling_weight, na.rm=T))
  
  return(outer_rst_temp[[X]] %>% reshape2::melt(1))
  
})

names(outer_rst_dimensions_extract) <- paste0("draw_", 1:length(outer_rst_dimensions))

###

rst_extract <- bind_rows(outer_rst_dimensions_extract, .id = "draw")

rst_extract$country <- countrycode::countrycode(rst_extract$country, 'country.name', 'iso2c')

#

rst_extract$draw[rst_extract$draw=="draw_1"] <- "Base"
rst_extract$draw[rst_extract$draw=="draw_2"] <- "Upper bound"
rst_extract$draw[rst_extract$draw=="draw_3"] <- "Conservative"

ggplot()+
  theme_classic()+
  geom_point(data=rst_extract, aes(y=country, x=value*100, colour=draw))+
  xlab("SCPI")+
  theme(legend.position = "bottom", legend.direction="horizontal")

ggsave("figures/scp_deprivation_dimensionsensitivity_index.png", width=6, height=3, scale=1.2)

##############################
# 3) dimensions weighting for index calculation

library(MCMCpack)
set.seed(123)  # for reproducibility
random_vectors <- rdirichlet(1000, rep(1, 5))

mm_weights <- as.data.frame(random_vectors)

mm_weights_1 <- mm_weights[1,]
mm_weights_1[1,] <- rep(1/5, 5)

dims <- colnames(cities)[grepl("dim", colnames(cities))][1:5]

mm_weights <- bind_rows(mm_weights_1, mm_weights)

weights_matrix <- pblapply(1:nrow(mm_weights), function(X){

      weights = data.frame(indic=dims, weight=NA)
      weights$weight <- unlist(mm_weights[X,])
  return(weights)
  
})


outer_cities_norm <- pblapply(1:nrow(mm_weights), function(X){
  
  outer_rst_temp <- cities %>% st_set_geometry(NULL)
  
  outer_rst_temp$SCPI <- rowSums(outer_rst_temp %>% dplyr::select(starts_with("dim")) * weights_matrix[[X]][,c(2)], na.rm = T)
  
  return(outer_rst_temp$SCPI)
  
})

outer_cities_norm <- bind_cols(outer_cities_norm)
outer_cities_norm$CTRY <- cities$ISO

outer_cities_norm <- reshape2::melt(outer_cities_norm, c(ncol(outer_cities_norm)))

outer_cities_norm <- outer_cities_norm %>% dplyr::group_by(CTRY, variable) %>% dplyr::summarise(value=mean(value, na.rm=T))

outer_cities_norm$CTRY <- factor(outer_cities_norm$CTRY, levels=arrange(rst_table_summ, desc(SCPI)) %>% pull(CTRY))

ggplot()+
  theme_classic()+
  geom_boxplot(data=outer_cities_norm, aes(x=CTRY, y=value*100))+
  geom_point(data=outer_cities_norm[outer_cities_norm$variable=="...1",], aes(x=CTRY, y=value*100), colour="red")+
  theme(legend.position = "none")+
  ylab("SCPI")+
  ggtitle("Monte Carlo experiment, variables weighting: 10,000 draws from Dirichlet Distribution")

ggsave("figures/scp_weighting_sensitivity.png", width=6, height=3, scale=1.4)

