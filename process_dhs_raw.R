
## https://docs.google.com/spreadsheets/d/1RCgTnyEbomKqVAiIskorDkkHwzWe9e64RHV-Y0wrg8U/edit#gid=1969867535

#################

library(haven)
library(sf)
library(tidyverse)
library(stringr)

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/global_scp_quantification/data_and_sources_dimensions/dhs_raw_data_processing_example/GHHR8ADT/GHHR8AFL.DTA")

###

df <- as.data.frame(Hmisc::label(r))

df$`Hmisc::label(r)`[grep("weight", df$`Hmisc::label(r)`)]

###

r$weights <- pull(r[,rownames(df)[grep("weight", df$`Hmisc::label(r)`)][1]])

r$DHSID <- paste0("GH2022", str_sub(paste0("00000000", r$hv001), start= -8))

###

rr <- r %>% group_by(DHSID) %>% dplyr::summarise(hv243a = weighted.mean(hv243a, weights, na.rm=T), weights=weighted.mean(weights, weights, na.rm=T))

###

s <- read_sf("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/global_scp_quantification/data_and_sources_dimensions/dhs_raw_data_processing_example/GHGE8AFL/GHGE8AFL.shp")
t <- read_sf("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/global_scp_quantification/data_and_sources_dimensions/dhs_raw_data_processing_example/GHGC8AFL/GHGC8AFL.csv")

st <- merge(s, t, "DHSID")

rst <- merge(rr, st, by="DHSID")

###

rst = rst %>% group_by(ADM1NAME, URBAN_RURA) %>% dplyr::summarise(hv243a=weighted.mean(hv243a, weights, na.rm=T))

rst <- merge(rst, s, by=c("ADM1NAME", "URBAN_RURA"))

rst <- dplyr::select(rst, ADM1NAME, URBAN_RURA, hv243a, geometry)

rst <- sf::st_as_sf(rst)

plot(rst["hv243a"])

####

library(raster)
library(stars)

# rasterize based on geometry and a column named "value". Change the name of this column if necessary
r.enn2mean<-st_rasterize(rst %>% dplyr::select(hv243a, geometry))

st_as_raster <- function(rstars){
  rext <- st_bbox(rstars)
  raster(t(rstars[[1]]), xmn = rext[1], xmx = rext[3],
         ymn = rext[2], ymx=rext[4],
         crs = st_crs(rstars)$proj4string)
}

r.enn2mean <- st_as_raster(r.enn2mean)

library(sp)
library(automap) # for autoKrige - or adapt to use gstat

fillRaster <- function(r){
  
  xyV = as.data.frame(r,xy=TRUE)
  sp::coordinates(xyV)=~x+y
  miss = is.na(xyV$layer)
  
  m = automap::autoKrige(
    layer~1,
    input_data = xyV[!miss,],
    new_data=xyV[miss,])
  
  rfill = raster(r)
  rfill[] = r[]
  rfill[miss] = m$krige_output$var1.pred
  
  return(rfill)
}

rrfilled = fillRaster(r.enn2mean$layer)

plot(r.enn2mean)
plot(rrfilled)

library(mapview)

mapview(rrfilled)

