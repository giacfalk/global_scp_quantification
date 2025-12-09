
# Poi, potremmo usare spider diagram per rappresentare le 5 dimensioni, mostrando magari median % of deprived HHs e SDEV? 

library(tidyverse)
library(scales)
library(tibble)
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(fmsb)

data <- cities 
data$geometry <- NULL
data$ordvar <- NULL

data <- as.data.frame(data)

dep_order <- cities_norm %>% st_set_geometry(NULL) %>% dplyr::group_by(ISO) %>% dplyr::summarise(SCPI=weighted.mean(SCPI, P15, na.rm=T)) %>% mutate(id=as.numeric(row.names(.))) %>% arrange(desc(SCPI)) %>% pull(id)

###

# condense variables into dimensions

for(ctry in unique(data$ISO)){

data_d <- data %>% filter(ISO==ctry)

dhs_rgns <- data_d$DHSREGEN

data_d <- dplyr::select(data_d, starts_with("dim"))

###

library(scales)
Min <- as.data.frame(t(colMins(as.matrix(data_d), na.rm = T)))
Min[1,] <- 0 
Max <- as.data.frame(t(colMaxs(as.matrix(data_d), na.rm = T)))
Max[1,] <- 1 
colnames(Min) <- colnames(Max) <- colnames(data_d)

data_d <- bind_rows(Max, Min, data_d)

data_d <- data.frame(data_d)

rownames(data_d) <- c("Max", "Min", dhs_rgns)

library(zoo)
data_d <- na.aggregate(data_d)

colnames(data_d) <- c("Climate", "Infrastr. & assets", "Social & thermal ineq.", "Health", "Edu. & working stds.")

###

# legend

data_d_legend <- data_d[c(1:3),]
data_d_legend[3,] <- 1

create_beautiful_radarchart <- function(data_d, color = "#00AFBB", 
                                        vlabels = colnames(data_d), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data_d, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )}

pdf(paste0("figures/legend_variables_radar.pdf"), width = 6.5, height = 5)
par(mar = c(1, 1, 1, 1))
create_beautiful_radarchart(
  data_d = data_d_legend[c(1, 2, 3), ], caxislabels = seq(0, 1, by=0.25),
  color = "lightgrey", title = ""
)

dev.off()

###################################
###################################

# Define colors and titles
library(randomcoloR)
colors <- RColorBrewer::brewer.pal(6, "Set2")[c(2, 5, 4, 1, 3, 6)]
titles <- paste0(dhs_rgns, ", ", round(cities_norm$SCPI[match(dhs_rgns, cities_norm$DHSREGEN)], 2))

# Reduce plot margin using par()
# Split the screen in 3 parts

pdf(paste0("figures/", ctry, "_radar.pdf"), width = 3.4*3, height = 2*3)

op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(ifelse(c(nrow(data_d)-2)<=6, 1, ceiling((nrow(data_d)-2))/6), ifelse(c(nrow(data_d)-2)<=6, c(nrow(data_d)-2), 6)))

create_beautiful_radarchart <- function(data_d, color = "#00AFBB", 
                                        vlabels = colnames(data_d), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data_d, axistype = 1,
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

for(i in 1:length(dhs_rgns)){
  create_beautiful_radarchart(
    data_d = data_d[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
    color = colors[i], title = titles[i]
  )
}

par(op)
dev.off()
}

#######################################
#######################################

data_d <- data

dhs_rgns <- unique(data$ISO)

data_d <- data_d %>% dplyr::group_by(ISO) %>% dplyr::summarise_at(vars(v1_wbt:v16_cooling_policy), funs(weighted.mean(., w=P15, na.rm=T)))

data <- cities_norm
data$geometry <- NULL
data$ordvar <- NULL

data <- as.data.frame(data)

data_d_scpi <- data %>% dplyr::group_by(ISO) %>% dplyr::summarise_at(vars(SCPI), funs(weighted.mean(., w=P15, na.rm=T)))

###

library(scales)
Min <- as.data.frame(t(colMins(as.matrix(data_d[,-1]), na.rm = T)))
Min[1,] <- 0 
Max <- as.data.frame(t(colMaxs(as.matrix(data_d[,-1]), na.rm = T)))
Max[1,] <- 1 
colnames(Min) <- colnames(Max) <- colnames(data_d[,-1])

data_d <- bind_rows(Max, Min, data_d)

data_d <- data.frame(data_d)
data_d$ISO <- NULL

rownames(data_d) <- c("Max", "Min", dhs_rgns)

library(zoo)
data_d <- na.aggregate(data_d)

# Define colors and titles
library(randomcoloR)
library(countrycode)
colors <- RColorBrewer::brewer.pal(6, "Set2")[c(2, 5, 4, 1, 3, 6)]

titles <- paste0(countrycode(dhs_rgns, 'iso2c', 'country.name'), ", ", round(data_d_scpi$SCPI[match(dhs_rgns, data_d_scpi$ISO)], 2))


# Reduce plot margin using par()
# Split the screen in 3 parts

pdf("figures/all_countries_radar.pdf", width = 4.5*4.5, height = 3*4)


op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(ifelse(c(nrow(data_d)-2)<=6, 1, ceiling((nrow(data_d)-2)/6)), ifelse(c(nrow(data_d)-2)<=6, c(nrow(data_d)-2), 6)))

create_beautiful_radarchart <- function(data_d, color = "#00AFBB", 
                                        vlabels = colnames(data_d), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data_d, axistype = 1,
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

for(i in dep_order){
  
  which_reg <-  match(first(cities_norm$GRGN_L1[which(cities_norm$ISO == rownames(data_d[i+2,]))]), unique(cities_norm$GRGN_L1))
  
  create_beautiful_radarchart(
    data_d = data_d[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
    color = colors[which_reg], title = titles[i]
  )
}

par(op)

dev.off()

###################
###################
###################

# by blocks of regions

library(randomcoloR)
colors <- RColorBrewer::brewer.pal(6, "Set2")[c(2, 5, 4, 1, 3, 6)]

data_d_s <- split(data_d[3:30,],cities_norm$GRGN_L1[match(row.names(data_d)[3:30], cities_norm$ISO)])
data_d_s <- lapply(data_d_s, function(X){bind_rows(data_d[1:2,], X)})

data_d_s <- data_d_s[match(unique(cities_norm$GRGN_L1)[c(2, 5, 4, 1, 3, 6)], names(data_d_s), )]

iter <- 0

titles <- paste0(countrycode(dhs_rgns, 'iso2c', 'country.name'), ", ", round(data_d_scpi$SCPI[match(dhs_rgns, data_d_scpi$ISO)], 2))

titles = split(titles,cities_norm$GRGN_L1[match(row.names(data_d)[3:30], cities_norm$ISO)])
titles <- titles[match(unique(cities_norm$GRGN_L1)[c(2, 5, 4, 1, 3, 6)], names(data_d_s), )]

for(data_d_sel in data_d_s){
  
  iter <- iter + 1
  
  pdf(paste0("figures/", names(data_d_s)[iter], "_radar.pdf"), width = 3.4*3, height = 3*2)
  
  op <- par(mar = c(1, 1, 1, 1))
  par(mfrow = c(ifelse(c(nrow(data_d_sel)-2)<=6, 1, ceiling((nrow(data_d_sel)-2)/6)), ifelse(c(nrow(data_d_sel)-2)<=6, c(nrow(data_d_sel)-2), 6)))
  
  create_beautiful_radarchart <- function(data_d_sel, color = "#00AFBB", 
                                          vlabels = "", vlcex = 0.7,
                                          caxislabels = NULL, title = NULL, ...){
    radarchart(
      data_d_sel, axistype = 1,
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
  
  for(i in 1:(nrow(data_d_sel)-2)){
    
    which_reg <-  match(first(cities_norm$GRGN_L1[which(cities_norm$ISO == rownames(data_d[i+2,]))]), unique(cities_norm$GRGN_L1))
    
    create_beautiful_radarchart(
      data_d_sel = data_d_sel[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
      color = colors[which_reg], title = titles[[names(data_d_s)[iter]]][i]
    )
  }
  
  par(op)
  
  dev.off()
  
  
}

###

data_d <- data

dhs_rgns <- unique(data$ISO)

data_d <- data_d %>% dplyr::group_by(ISO) %>% dplyr::summarise_at(vars(dim1_climate:dim5_edu_work_stds), funs(weighted.mean(., w=P15, na.rm=T)))

###

library(scales)
Min <- as.data.frame(t(colMins(as.matrix(data_d[,-1]), na.rm = T)))
Min[1,] <- 0 
Max <- as.data.frame(t(colMaxs(as.matrix(data_d[,-1]), na.rm = T)))
Max[1,] <- 1 
colnames(Min) <- colnames(Max) <- colnames(data_d[,-1])

data_d <- bind_rows(Max, Min, data_d)

data_d <- data.frame(data_d)
data_d$ISO <- NULL

rownames(data_d) <- c("Max", "Min", dhs_rgns)

library(zoo)
data_d <- na.aggregate(data_d)

colnames(data_d) <- c("Climate", "Infrastr. & assets", "Social & thermal ineq.", "Health", "Edu. & working stds.")

# Define colors and titles
colors <- RColorBrewer::brewer.pal(6, "Set2")[c(2, 5, 4, 1, 3, 6)]

titles <- paste0(countrycode(dhs_rgns, 'iso2c', 'country.name'), ", ", round(data_d_scpi$SCPI[match(dhs_rgns, data_d_scpi$ISO)], 2))

# Reduce plot margin using par()
# Split the screen in 3 parts

pdf("figures/all_countries_radar_simplif.pdf", width = 4.5*3.5, height = 3*3)


op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(ifelse(c(nrow(data_d)-2)<=6, 1, ceiling((nrow(data_d)-2)/6)), ifelse(c(nrow(data_d)-2)<=6, c(nrow(data_d)-2), 6)))

create_beautiful_radarchart <- function(data_d, color = "#00AFBB", 
                                        vlabels = colnames(data_d), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data_d, axistype = 1,
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

for(i in dep_order){
  
  which_reg <-  match(first(cities_norm$GRGN_L1[which(cities_norm$ISO == rownames(data_d[i+2,]))]), unique(cities_norm$GRGN_L1))
  
  create_beautiful_radarchart(
    data_d = data_d[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
    color = colors[which_reg], title = titles[i]
  )
}

par(op)

dev.off()

##################################

# legend

data_d_legend <- data_d[c(1:3),]
data_d_legend[3,] <- 1

create_beautiful_radarchart <- function(data_d, color = "#00AFBB", 
                                        vlabels = colnames(data_d), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data_d, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )}

pdf(paste0("figures/legend_dimensions_radar.pdf"), width = 3.4*3, height = 3*3)

create_beautiful_radarchart(
  data_d = data_d_legend[c(1, 2, 3), ], caxislabels = seq(0, 1, by=0.25),
  color = "lightgrey", title = ""
)

dev.off()


# by blocks of regions

data_d_s <- split(data_d[3:30,],cities_norm$GRGN_L1[match(row.names(data_d)[3:30], cities_norm$ISO)])
data_d_s <- lapply(data_d_s, function(X){bind_rows(data_d[1:2,], X)})

data_d_s <- data_d_s[match(unique(cities_norm$GRGN_L1)[c(2, 5, 4, 1, 3, 6)], names(data_d_s), )]

iter <- 0

titles <- paste0(countrycode(dhs_rgns, 'iso2c', 'country.name'), ", ", round(data_d_scpi$SCPI[match(dhs_rgns, data_d_scpi$ISO)], 2))

titles = split(titles,cities_norm$GRGN_L1[match(row.names(data_d)[3:30], cities_norm$ISO)])
titles <- titles[match(unique(cities_norm$GRGN_L1)[c(2, 5, 4, 1, 3, 6)], names(data_d_s), )]

colors <- RColorBrewer::brewer.pal(6, "Set2")[c(2, 5, 4, 1, 3, 6)]

for(data_d_sel in data_d_s){
  
  iter <- iter + 1
  
  pdf(paste0("figures/", names(data_d_s)[iter], "_dimensions_radar.pdf"), width = 3.4*3, height = 3*2)
  
  op <- par(mar = c(1, 1, 1, 1))
  par(mfrow = c(ifelse(c(nrow(data_d_sel)-2)<=6, 1, ceiling((nrow(data_d_sel)-2)/6)), ifelse(c(nrow(data_d_sel)-2)<=6, c(nrow(data_d_sel)-2), 6)))
  
  create_beautiful_radarchart <- function(data_d_sel, color = "#00AFBB", 
                                          vlabels = "", vlcex = 0.7,
                                          caxislabels = NULL, title = NULL, ...){
    radarchart(
      data_d_sel, axistype = 1,
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
  
  for(i in 1:(nrow(data_d_sel)-2)){
    
    which_reg <-  match(first(cities_norm$GRGN_L1[which(cities_norm$ISO == rownames(data_d[i+2,]))]), unique(cities_norm$GRGN_L1))
    
    create_beautiful_radarchart(
      data_d = data_d_sel[c(1, 2, i+2), ], caxislabels = seq(0, 1, by=0.25),
      color = colors[which_reg], title = titles[[names(data_d_s)[iter]]][i]
    )
  }
  
  par(op)
  
  dev.off()
  
  
}
