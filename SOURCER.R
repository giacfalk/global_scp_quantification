
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

# source("scripts/global_scp_quantification/process_data_combine_dimensions.R")

setwd(stub)
# load("rst_bk_before_indicators.Rdata")
load("data_and_sources_dimensions/regions_all_data.Rdata"); rst <- rst_table; rst$geometry <- rst_geo; rst <- st_as_sf(rst); source("scripts/global_scp_quantification/recover_missing_objects_rst.R")

##############
# calculate indicators

source("scripts/global_scp_quantification/define_binary_indicators.r")
source("scripts/global_scp_quantification/aggregate_indicators.R")

##############
# analyze results

source("scripts/global_scp_quantification/make_descriptive_plots_regions.R")
source("scripts/global_scp_quantification/calculate_index_regions.R")
source("scripts/global_scp_quantification/summary_tables.R")
source("scripts/global_scp_quantification/deprivation_stats.R")

###############
# sensitivity analysis

source("scripts/global_scp_quantification/sens_analysis_figures_tables.R")
