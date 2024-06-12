
####

# Global systemic cooling poverty
# Sourcer

####

library(terra)
library(exactextractr)
library(sf)
library(raster)
library(tidyverse)
library(pbapply)
library(rworldmap)
library(writexl)
library(readxl)
library(googledrive)
library(fasterize)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

####

user<-"gf"

if (user=='gf') {
  stub        <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/global_scp_quantification/'
}

setwd(stub)

####

# select the scale of the analysis (DHS regions or cities)

# scale <- "cities"
scale <- "regions"

# only relevant for cities: how many of the largest (by population size) cities per country to analyse?

n_largest_cities_by_country <- 10

# for which country(ies) to run the analysis

countrylist <- c("Ghana", "Senegal")
# countrylist <- "all"

###########

# source("scripts/plot_scales.R")
source("scripts/process_data_combine_dimensions.R")

setwd(stub)

if(scale=="cities"){

source("scripts/make_descriptive_plots.R")
source("scripts/calculate_index.R")
source("scripts/summary_tables.R")

} else {

source("scripts/make_descriptive_plots_regions.R")
source("scripts/calculate_index_regions.R")
source("scripts/summary_tables.R")
  
}




