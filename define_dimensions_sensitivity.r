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
    
    data$dim2_infra_assets <- ifelse(((data$v2_urban_planning==1 & data$v3_green_bleu_infra==1 & data$v5_housing ==1) | data$v7b_transport_assets==1) | ((data$v4a_water_quality==1 & data$v4b_sanitation==1) | data$v7b_transport_assets==1) | ((data$v7_cooling_assets==1 & data$v8_energy==1)  | data$v7b_transport_assets==1), 1, 0)
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    data$dim4_health <- ifelse(data$v10_health1==1 & data$v11_health2==1 & data$v12_health3==1 & data$v12_health4==1, 1, 0)
    
    data$dim5_edu_work_stds <- ifelse(((data$v13_education==1 & data$v14_work==1 & data$v15_adapt_knowledge==1)), 1, 0) 
    
  } else if (iter==3){
    # Transport required + at least 2 of the 3 thematic groups (planning/utilities/thermal-energy)
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(
      data$v7b_transport_assets==1 &
        (
          ((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) +
             (data$v4a_water_quality==1 | data$v4b_sanitation==1) +
             (data$v7_cooling_assets==1 | data$v8_energy==1)) >= 2
        ),
      1, 0
    )
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    # Any 2 of the 4 health indicators
    data$dim4_health <- ifelse(
      ((data$v10_health1==1) + (data$v11_health2==1) + (data$v12_health3==1) + (data$v12_health4==1)) >= 2,
      1, 0
    )
    
    # Any 2 of Edu/Work/Adapt
    data$dim5_edu_work_stds <- ifelse(
      ((data$v13_education==1) + (data$v14_work==1) + (data$v15_adapt_knowledge==1)) >= 2,
      1, 0
    )
    
  } else if (iter==4){
    # Transport alone passes; otherwise require all three groups
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(
      data$v7b_transport_assets==1 |
        ((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) &
           (data$v4a_water_quality==1 | data$v4b_sanitation==1) &
           (data$v7_cooling_assets==1 | data$v8_energy==1)),
      1, 0
    )
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    # At least 3 of the 4 health indicators
    data$dim4_health <- ifelse(
      ((data$v10_health1==1) + (data$v11_health2==1) + (data$v12_health3==1) + (data$v12_health4==1)) >= 3,
      1, 0
    )
    
    # Education mandatory + (Work OR Adapt)
    data$dim5_edu_work_stds <- ifelse(
      data$v13_education==1 & (data$v14_work==1 | data$v15_adapt_knowledge==1),
      1, 0
    )
    
  } else if (iter==5){
    # Transport required + exactly 2 of the 3 groups
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(
      data$v7b_transport_assets==1 &
        (
          ((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) +
             (data$v4a_water_quality==1 | data$v4b_sanitation==1) +
             (data$v7_cooling_assets==1 | data$v8_energy==1)) == 2
        ),
      1, 0
    )
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    # (Health1 OR Health2) AND (Health3 OR Health4)
    data$dim4_health <- ifelse(
      (data$v10_health1==1 | data$v11_health2==1) & (data$v12_health3==1 | data$v12_health4==1),
      1, 0
    )
    
    # Exactly one of Edu/Work/Adapt
    data$dim5_edu_work_stds <- ifelse(
      ((data$v13_education==1) + (data$v14_work==1) + (data$v15_adapt_knowledge==1)) == 1,
      1, 0
    )
    
  } else if (iter==6){
    # Very strict: ≥2 of planning trio AND both water & sanitation AND both cooling & energy AND transport
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(
      (((data$v2_urban_planning==1) + (data$v3_green_bleu_infra==1) + (data$v5_housing==1)) >= 2) &
        (data$v4a_water_quality==1 & data$v4b_sanitation==1) &
        (data$v7_cooling_assets==1 & data$v8_energy==1) &
        (data$v7b_transport_assets==1),
      1, 0
    )
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    # (Health1 AND Health2) OR (Health3 AND Health4)
    data$dim4_health <- ifelse(
      (data$v10_health1==1 & data$v11_health2==1) | (data$v12_health3==1 & data$v12_health4==1),
      1, 0
    )
    
    # Work mandatory + (Education OR Adapt)
    data$dim5_edu_work_stds <- ifelse(
      data$v14_work==1 & (data$v13_education==1 | data$v15_adapt_knowledge==1),
      1, 0
    )
    
  } else if (iter==7){
    # Weighted scores (softer, but still varied)
    data$dim1_climate <- data$v1_wbt
    
    infra_score <- 0.4*(data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) +
      0.3*(data$v4a_water_quality==1 | data$v4b_sanitation==1) +
      0.3*(data$v7_cooling_assets==1 | data$v8_energy==1) +
      0.2*(data$v7b_transport_assets==1)
    data$dim2_infra_assets <- ifelse(infra_score >= 0.7, 1, 0)
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    health_score <- 2*(data$v10_health1==1) + 2*(data$v11_health2==1) +
      1*(data$v12_health3==1) + 1*(data$v12_health4==1)
    data$dim4_health <- ifelse(health_score >= 2, 1, 0)
    
    edu_score <- 0.5*(data$v13_education==1) + 0.3*(data$v14_work==1) + 0.2*(data$v15_adapt_knowledge==1)
    data$dim5_edu_work_stds <- ifelse(edu_score >= 0.5, 1, 0)
    
  } else if (iter==8){
    # Ignore transport; pass if any 2 of the 3 groups are present
    data$dim1_climate <- data$v1_wbt
    
    data$dim2_infra_assets <- ifelse(
      ((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) &
         (data$v4a_water_quality==1 | data$v4b_sanitation==1)) |
        ((data$v2_urban_planning==1 | data$v3_green_bleu_infra==1 | data$v5_housing==1) &
           (data$v7_cooling_assets==1 | data$v8_energy==1)) |
        ((data$v4a_water_quality==1 | data$v4b_sanitation==1) &
           (data$v7_cooling_assets==1 | data$v8_energy==1)),
      1, 0
    )
    
    data$dim3_social_thermal_ineq <- ifelse(data$v9_justice==1, 1, 0)
    
    # Exactly 2 of the 4 health indicators
    data$dim4_health <- ifelse(
      ((data$v10_health1==1) + (data$v11_health2==1) + (data$v12_health3==1) + (data$v12_health4==1)) == 2,
      1, 0
    )
    
    # At least one overall, but must include Education or Work (Adapt alone doesn’t pass)
    data$dim5_edu_work_stds <- ifelse(
      (data$v13_education==1 | data$v14_work==1 | data$v15_adapt_knowledge==1) &
        (data$v13_education==1 | data$v14_work==1),
      1, 0
    )
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


  outer_rst_dimensions <- pblapply(1:8, iter_f)
