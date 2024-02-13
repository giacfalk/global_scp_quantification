
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

scale <- "cities"
# scale <- "regions"

# only relevant for cities: how many of the largest (by population size) cities per country to analyse?

n_largest_cities_by_country <- 10

###########

file.edit("scripts/plot_scales.R")
file.edit("scripts/process_data_combine_dimensions.R")

setwd(stub)

if(scale=="cities"){

file.edit("scripts/make_descriptive_plots.R")
file.edit("scripts/calculate_index.R")
file.edit("scripts/summary_tables.R")

} else {

file.edit("scripts/make_descriptive_plots_regions.R")
file.edit("scripts/calculate_index_regions.R")
file.edit("scripts/summary_tables_regions.R")
  
}




