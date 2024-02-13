############
############

# alternative: PCA (?)

library(factoextra)

cities_pca <- cities %>% dplyr::select(P15, wb_t_max, green_blue_infra_share) %>% mutate(green_blue_infra_share=-green_blue_infra_share)
cities_pca$geom <- NULL

res.pca <- prcomp(cities_pca, scale = TRUE)
print(res.pca)
summary(res.pca)

eig.val<-get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, col.var="blue")

var <- get_pca_var(res.pca)
var
head(var$cos2)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

library(gridExtra)

# Contributions of variables to PC1
a<-fviz_contrib(res.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
b<-fviz_contrib(res.pca, choice = "var", axes = 2)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')

ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

library(ggfortify)

autoplot(res.pca, loadings=TRUE, loadings.colour='darkorchid4', loadings.label=TRUE, loadings.label.size=3)

kmeans<-eclust(cities_pca, k=4)
autoplot(res.pca, data=kmeans, colour="cluster")

#####

cities_norm$PCA_SCPI <- as.numeric(res.pca$x[,1])

cities_norm$PCA_SCPI <- normalize_variable(-cities_norm$PCA_SCPI)

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="lightgrey", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(cities_norm), colour="white", size=2.5)+
  stat_sf_coordinates(data=st_centroid(cities_norm), aes(colour=PCA_SCPI), size=1.8)+
  scale_colour_viridis_c(name="SCPI", direction = 1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, 'cm'),  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"))+
  xlab("")+
  ylab("")
