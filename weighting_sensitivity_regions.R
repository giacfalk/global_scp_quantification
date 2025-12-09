
####

# Global systemic cooling poverty
# index calculation

####

setwd(stub)

####

library(terra)
library(exactextractr)
library(sf)
library(raster)
library(tidyverse)
library(pbapply)
library(geodata)
library(writexl)
library(readxl)
library(googledrive)
library(fasterize)
library(matrixStats)
library(maptools)
googledrive::drive_auth(email = "giacomo.falchetta@gmail.com")

##########

load("data_and_sources_dimensions/regions_all_data.Rdata")

# how many people are covered by the analysis?

sum(cities$P15) / 1e9

####

# positive indicators
pos_indic <- c()

# negative indicators
neg_indic <- setdiff(colnames(cities), pos_indic)

###

# source("scripts/table_indicators.R")

###

remove_uppercase_items <- function(vector) {
  vector[!grepl("[A-Z]", vector) & !grepl("geom", vector)& !grepl("dim", vector)]
}

neg_indic <- remove_uppercase_items(neg_indic)

####################
# define weights 

weights = data.frame(indic=c(pos_indic, neg_indic), weight=NA)
weights <- list(weights, weights, weights, weights, weights)
names(weights) <- c("weights_a", "weights_b", "weights_c", "weights_d", "weights_e")

weights[[1]]$weight <- 1/nrow(weights[[1]])
weights[[2]]$weight <- 1/nrow(weights[[1]])
weights[[3]]$weight <- 1/nrow(weights[[1]])
weights[[4]]$weight <- 1/nrow(weights[[1]])
weights[[5]]$weight <- 1/nrow(weights[[1]])

###

cities_norm <- cities
cities_norm$geometry <- NULL

# weight them and calculate index

cities_norm$SCPI_w1 <- rowSums(cities_norm[,c(match(weights[[1]]$indic, colnames(cities_norm)))] * weights[[1]][,c(2)], na.rm = T)
cities_norm$SCPI_w2 <- rowSums(cities_norm[,c(match(weights[[2]]$indic, colnames(cities_norm)))] * weights[[2]][,c(2)], na.rm = T)
cities_norm$SCPI_w3 <- rowSums(cities_norm[,c(match(weights[[3]]$indic, colnames(cities_norm)))] * weights[[3]][,c(2)], na.rm = T)
cities_norm$SCPI_w4 <- rowSums(cities_norm[,c(match(weights[[4]]$indic, colnames(cities_norm)))] * weights[[4]][,c(2)], na.rm = T)
cities_norm$SCPI_w5 <- rowSums(cities_norm[,c(match(weights[[5]]$indic, colnames(cities_norm)))] * weights[[5]][,c(2)], na.rm = T)


cities_norm_w <- dplyr::select(cities_norm, SCPI_w1, SCPI_w2, SCPI_w3, SCPI_w4, SCPI_w5) %>% reshape2::melt()

ggplot(cities_norm_w)+
  geom_bar(aes(x=value, fill=variable))
