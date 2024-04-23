## ---------------------------
##
## Script name: 07_cluster_analyses_evaluation
##
## Purpose of script: Evaluate results of cluster analyses. Calculate confusion matrix, make PCA plots,
##                    retrieve variance explained per dimension, retrieve variable contributions and plot them
##
## Project: Species profiles from Sentinel-2 time series 
##
## Author: Tiziana Li Koch
##
## Copyright (c) Tiziana Li Koch, 2023
## 
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
library(factoextra)
library(ggpubr)
library(factoextra)
library(ggplot2)
library(vctrs)
library(dplyr)
# go on with 6 clusters as derived from silhouette plot
# closest to 7, little peak in 2020, flattening after 6 in 2019


# confusion matrix
res <- readRDS("01_data/10_preprocessed/cluster/kmeans_2020_results_2_10.RDS")
infos <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2020.csv")

res_k6 <- res[[5]]

conf_tab <- table(res_k6$cluster, infos$species) |> as.data.frame.matrix()  
write.csv2(conf_tab, "01_data/10_preprocessed/cluster/confusion_matrix_2020.csv",
           row.names = FALSE)


# PCA plot
res.pca <- prcomp(infos[,c(4:1025)],  scale = FALSE)
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
ind.coord$true_values <- as.factor(infos$species)
ind.coord$cluster_groups <- as.factor(res_k6$cluster)

#vector_pca_var <- get_pca_var(res.pca)
var <- get_pca_var(res.pca)
coordinates <- as.data.frame(var$coord[, c(1:2)])



g_cluster_groups <- ggscatter(
  ind.coord,
  x = "Dim.1",
  y = "Dim.2",
  color = "cluster_groups",
  palette = c(
    "#E64B35FF",
    "#4DBBD5FF",
    "#00A087FF",
    "#3C5488FF",
    "#F39B7FFF",
    "#8491B4FF",
    "#91D1C2FF"
  ),
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.alpha = 0,
  shape = "true_values",
  size = 1.5,
  legend = "right",
  title = "Cluster groups"
) + theme_minimal() +
  scale_shape_manual(values = 1:length(unique(ind.coord$true_values)),
                     guide = guide_legend(label.theme = element_text(face = "italic"))) +
  coord_fixed() +
  xlab("Dimension 1") + ylab("Dimension 2") +
  labs(color = "Cluster groups", shape = "NFI records") +
  guides(fill = "none")


g_cluster_groups


g_true_values <- ggscatter(
  ind.coord,
  x = "Dim.1",
  y = "Dim.2",
  color = "true_values",
  fill = "true_values",
  palette = c(
    "#e7298a",
    "#666666",
    "#7570b3",
    "#1b9e77",
    "#d95f02",
    "#e6ab02",
    "#a6761d"
  ),
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.alpha = 0,
  shape = "true_values",
  size = 1.5,
  legend = "right",
  title = "NFI species records"
) + theme_minimal() +
  coord_fixed() +
  xlab("Dimension 1") + ylab("Dimension 2") +
  labs(color = "NFI records", shape = "NFI records") +
  guides(fill = "none",
         color = guide_legend(face = "italic"),
         shape = guide_legend(face = "italic"))

# Italicize legend titles
g_true_values <- g_true_values +
  theme(
    legend.text = element_text(face = "italic")
  )


g_true_values

cowplot::plot_grid(
  g_true_values,
  g_cluster_groups,
  labels = "AUTO",
  nrow = 2,
  hjust = -0.5,
  vjust = 1
)

ggsave(
  "00_documentation/figures/cluster/PCA_plot_2020.png",
  width = 8,
  height = 10,
  dpi = 900
)


# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
pca_contributing <- res.var$contrib        # Contributions to the PCs
pca_quality_repres <- res.var$cos2           # Quality of representation 


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

write.csv2(variance.percent, "01_data/10_preprocessed/cluster/explained_variance_dim_2020.csv",
           row.names = FALSE)


# visualize top contributing variables
fviz_cos2(res.pca, choice = "var", axes = 1, top = 20, title = "Contributing variables to first PCA dimension") +
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis1_2020.png",
  width = 5,
  height = 6,
  dpi = 900
)
fviz_cos2(res.pca, choice = "var", axes = 2, top = 20, title = "Contributing variables to second PCA dimension")+
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis2_2020.png",
  width = 5,
  height = 6,
  dpi = 900
)
fviz_cos2(res.pca, choice = "var", axes = 1:2, top = 20, title = "Contributing variables to \nfirst and second PCA dimensions")+
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis1_2_2020.png",
  width = 5,
  height = 6,
  dpi = 900
)




####################
### 2019
####################
# go on with 6 clusters
# closest to 7, little peak in 2020, flattening after 6 in 2019


# confusion matrix
res <- readRDS("01_data/10_preprocessed/cluster/kmeans_2019_results_2_10.RDS")
infos <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2019.csv")

res_k6 <- res[[5]]

conf_tab <- table(res_k6$cluster, infos$species) |> as.data.frame.matrix()  
write.csv2(conf_tab, "01_data/10_preprocessed/cluster/confusion_matrix_2019.csv",
           row.names = FALSE)

# PCA plot
res.pca <- prcomp(infos[,c(4:1025)],  scale = FALSE)
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
ind.coord$true_values <- as.factor(infos$species)
ind.coord$cluster_groups <- as.factor(res_k6$cluster)

#vector_pca_var <- get_pca_var(res.pca)
var <- get_pca_var(res.pca)
coordinates <- as.data.frame(var$coord[, c(1:2)])



g_cluster_groups <- ggscatter(
  ind.coord,
  x = "Dim.1",
  y = "Dim.2",
  color = "cluster_groups",
  palette = c(
    "#E64B35FF",
    "#4DBBD5FF",
    "#00A087FF",
    "#3C5488FF",
    "#F39B7FFF",
    "#8491B4FF",
    "#91D1C2FF"
  ),
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.alpha = 0,
  shape = "true_values",
  size = 1.5,
  legend = "right",
  title = "Cluster groups"
) + theme_minimal() +
  scale_shape_manual(values = 1:length(unique(ind.coord$true_values)),
                     guide = guide_legend(label.theme = element_text(face = "italic"))) +
  #stat_mean(aes(color = true_values), size = 4) +
  coord_fixed() +
  xlab("Dimension 1") + ylab("Dimension 2") +
  labs(color = "Cluster groups", shape = "NFI records") +
  guides(fill = "none")


g_cluster_groups


g_true_values <- ggscatter(
  ind.coord,
  x = "Dim.1",
  y = "Dim.2",
  color = "true_values",
  fill = "true_values",
  palette = c(
    "#e7298a",
    "#666666",
    "#7570b3",
    "#1b9e77",
    "#d95f02",
    "#e6ab02",
    "#a6761d"
  ),
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.alpha = 0,
  shape = "true_values",
  size = 1.5,
  legend = "right",
  title = "NFI species records"
) + theme_minimal() +
  coord_fixed() +
  xlab("Dimension 1") + ylab("Dimension 2") +
  labs(color = "NFI records", shape = "NFI records") +
  guides(fill = "none",
         color = guide_legend(face = "italic"),
         shape = guide_legend(face = "italic"))

# Italicize legend titles
g_true_values <- g_true_values +
  theme(
    legend.text = element_text(face = "italic")
  )


g_true_values

cowplot::plot_grid(
  g_true_values,
  g_cluster_groups,
  labels = "AUTO",
  nrow = 2,
  hjust = -0.5,
  vjust = 1
)

ggsave(
  "00_documentation/figures/cluster/PCA_plot_2019.png",
  width = 8,
  height = 10,
  dpi = 900
)


# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
pca_contributing <- res.var$contrib        # Contributions to the PCs
pca_quality_repres <- res.var$cos2           # Quality of representation 


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

write.csv2(variance.percent, "01_data/10_preprocessed/cluster/explained_variance_dim_2019.csv",
           row.names = FALSE)


# visualize top contributing variables
fviz_cos2(res.pca, choice = "var", axes = 1, top = 20, title = "Contributing variables to first PCA dimension") +
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis1_2019.png",
  width = 5,
  height = 6,
  dpi = 900
)
fviz_cos2(res.pca, choice = "var", axes = 2, top = 20, title = "Contributing variables to second PCA dimension")+
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis2_2019.png",
  width = 5,
  height = 6,
  dpi = 900
)
fviz_cos2(res.pca, choice = "var", axes = 1:2, top = 20, title = "Contributing variables to \nfirst and second PCA dimensions")+
  ylab("Quality of representation")
ggsave(
  "00_documentation/figures/cluster/Contributing_vars_axis1_2_2019.png",
  width = 5,
  height = 6,
  dpi = 900
)


