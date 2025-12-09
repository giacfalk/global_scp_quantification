
library(rgis)

cities_dims <- pblapply(unique(cities$ISO), function(ctry){
  
  cities_boundary <- cities %>% filter(ISO==ctry)

  dims <- stack(paste0(stub,"results/rasters_indicators.tif"))[[19:23]]
  dims <- mask_raster_to_polygon(dims, cities_boundary)
  dims <- stackApply(dims, 1, fun="mean")

  gridded_pop_p <- mask_raster_to_polygon(gridded_pop, cities_boundary)
    
  dims <- projectRaster(dims, gridded_pop_p)

weighted_gini <- function(value, cov_frac, weights) {
  x <- value
  w <- weights

  x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x)) else w <- as.numeric(w)
  if (length(w) != length(x)) stop("`x` and `w` must have the same length.")
  
  # drop NAs in pair
  ok <- !(is.na(x) | is.na(w))
  x <- x[ok]; w <- w[ok]
  
  # clamp negative weights to 0
  w[w < 0] <- 0
  
  n <- length(x)
  if (n == 0 || sum(w) == 0) return(NA_real_)
  
  # sort by x (ascending)
  o <- order(x)
  x <- x[o]; w <- w[o]
  
  W  <- sum(w)
  wx <- w * x
  T  <- sum(wx)
  if (T == 0) return(0)  # all-zero values ⇒ perfectly equal
  
  # cumulative population and income shares
  p  <- cumsum(w)  / W
  L  <- cumsum(wx) / T
  
  # previous step (with 0 at start) for trapezoids
  p0 <- c(0, head(p, -1))
  L0 <- c(0, head(L, -1))
  
  # Gini = 1 - area under Lorenz curve (trapezoid rule)
  G <- 1 - sum((p - p0) * (L + L0))
  as.numeric(G)

}

cities_dims <- exact_extract(dims, cities_boundary, weighted_gini, weights=gridded_pop_p)

return(cities_dims)

})

###

cities_dims <- unlist(cities_dims)

cities_dims <- data.frame(gini=cities_dims)

cities_dims$CTRY <- cities$ISO
cities_dims$ADM1NAME <- cities$DHSREGEN

##

cities_dims$geometry <- cities$geometry
cities_dims <- st_as_sf(cities_dims)

b <- pblapply(unique(cities_dims$CTRY), function(ctry){ ggplot(data=cities_dims %>% filter(CTRY==ctry))+
    theme_void()+
    geom_sf(aes(fill=gini), colour="white", lwd=0.001)+
    scale_fill_distiller(name="", palette = "YlOrRd", direction = 1, limits=c(0,0.09))+
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'), legend.key.height = unit(0.2, 'cm'),   plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
    xlab("")+
    ylab("")+
    ggtitle(ctry)})


library(patchwork)

plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols) + plot_annotation(title="Within-region Gini of SCPI") + plot_layout(guides = "collect") &
    theme(legend.position = "bottom") 
}


b <- plot_a_list(b, 7, 4)

ggsave("figures/SCPI_GINI_map_regions_ctrys.png", b, height = 4, width = 3.75, scale=3.25, bg="white")
ggsave("figures/SCPI_GINI_map_regions_ctrys.pdf", b, height = 4, width = 3.75, scale=3.25, bg="white")
