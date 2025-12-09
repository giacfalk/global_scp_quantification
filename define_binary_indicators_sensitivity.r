### indicators calculation

# discrete Monte Carlo simulation w/ Latin Hypercube Sampling


####

## Systemic cooling poverty in the Global South
## Author: Giacomo Falchetta
## Date Created: 2024-11
## Copyright (c) Giacomo Falchetta, 2024
## Email: giacomo.falchetta@cmcc.it

####

library(terra)
library(exactextractr)
library(sf)
library(raster)
library(tidyverse)
library(countrycode)
library(pbapply)
library(geodata)
library(writexl)
library(haven)
library(readxl)
library(googledrive)
library(fasterize)
library(qs)
library(qs2)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

####

user<-"gf"

if (user=='gf') {
  stub        <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/global_scp_quantification/'
}

setwd(stub)

####

# select the scale of the analysis (DHS regions or cities)

scale <- "regions"

n_largest_cities_by_country <- 25

# for which country(ies) to run the analysis

# countrylist <- c("Colombia")
countrylist <- "all"

# exclude climate dimension from SCPI index calculation?
exclude_climate_dim_SCPI = F


###########
# extract data

# source("scripts/process_data_combine_dimensions.R")

setwd(stub)
# load("rst_bk_before_indicators.Rdata")
load("data_and_sources_dimensions/regions_all_data.Rdata"); rst <- rst_table; rst$geometry <- rst_geo; rst <- st_as_sf(rst); source("scripts/recover_missing_objects_rst.R")

library(lhs)
library(pbapply)

gdata::keep(rst_table, sure=T)
gc()

set.seed(123)  # reproducibility
n_samples <- 1e3


###

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

##

# stargazer::stargazer(as.data.frame(unlist(sens_grid)), summary=F, out="figures/sens_grid.tex")

##

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


##

outer_rst <- pblapply(1:n_samples, function(i) {
  
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
  
  return(rst_temp)
  
})
  
write_rds(outer_rst, paste0("results/sensitivity_analysis/indicators_definition.rds"))  