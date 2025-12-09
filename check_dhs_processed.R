library(haven)
library(tidyverse)

###

r <- read_dta("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/Oxford/CoolingPoverty/dhs/Clean Data/DHS_cleaned.dta")

water quality
toilet facility
housing material: floor material + wall material + roof material

sort(unique(r$hv205))

r
