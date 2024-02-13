
library(tidyverse)
library(scales)
library(tibble)
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(fmsb)

data <- cities_norm %>% filter(UC_NM_MN %in% cities_subset)
names_cities <- data$UC_NM_MN
data <- dplyr::select(data, -(c(1:4, 17:19)))
data$geometry <- NULL

data <- as.data.frame(data)

colnames(data) <- data_dimensions_indicators$category[match(colnames(data), data_dimensions_indicators$indicator_name)]
nms <- unique(names(data))
data <- sapply(nms, function(x)  rowMeans(data[names(data) %in% x]))
data <- as.data.frame(data)
data <- data[,order(colnames(data))]

library(scales)
data <- round(apply(data, 2, scales::rescale), 2)
data <- as.data.frame(data)

Min <- as.data.frame(t(colMins(as.matrix(data), na.rm = T)))
Max <- as.data.frame(t(colMaxs(as.matrix(data), na.rm = T)))
colnames(Min) <- colnames(Max) <- colnames(data)

data <- bind_rows(Max, Min, data)

data <- data.frame(data)

rownames(data) <- c("Max", "Min", names_cities)

library(zoo)
data <- na.aggregate(data)

# Define colors and titles
library(randomcoloR)
colors <- distinctColorPalette(length(cities_subset))
titles <- names_cities

# Reduce plot margin using par()
# Split the screen in 3 parts
png("figures/radars.png", width = 1200, height = 600, res=150)
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,round(length(cities_subset)/2, 0)))

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Create the radar chart
for(i in 1:length(cities_subset)){
  create_beautiful_radarchart(
    data = data[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
    color = colors[i], title = titles[i]
  )
}

par(op)
dev.off()
