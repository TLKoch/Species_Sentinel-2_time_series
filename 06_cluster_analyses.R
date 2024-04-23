## ---------------------------
##
## Script name: 06_cluster_analyses
##
## Purpose of script: Perform kmeans cluster analyses. In addition, make silhouette plot.  
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
library(dplyr)
library(tidyr)
library(factoextra)
##################
# data preparation
# read data
files_list <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                         pattern = "*2020*",
                         full.names = TRUE)
files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                         pattern = "*2020*") |> substr(6,8)
files <- lapply(files_list, read.csv2)


################################################
## normalized
files <- lapply(files_list, read.csv2)
files_wider <- list()
for (i in 1:length(files)){
  files[[i]]$band_value_original <- NULL
  files[[i]]$band_value <- NULL
  files_wider[[i]] <- files[[i]] |>  pivot_wider(names_from = date, values_from = band_value_norm)
  colnames(files_wider[[i]]) <- gsub("2020", paste0(files_names[[i]], "_", "2020"), colnames(files_wider[[i]]))
}

combined_df <- Reduce(function(x, y) merge(x, y, by = c("species", "CLNR", "BANR")), files_wider)
write.csv2(combined_df, "01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2020_norm.csv",
           row.names = FALSE)
################################################
combined_df_bands <- combined_df[,c(4:1025)]
k_res <- list()
for(i in 2:10){
  set.seed(100)
  k_res[[i-1]] <- kmeans(combined_df_bands, centers = i, nstart = 50)
}
saveRDS(k_res, file = "01_data/10_preprocessed/cluster/kmeans_2020_results_2_10.RDS")


# optimal number of clusters
# elbow approach
# silhouette approach
fviz_nbclust(combined_df_bands, kmeans, method = "silhouette")
ggsave("00_documentation/figures/cluster/Silhouette_2020.png",
       dpi = 900,
       width = 5,
       height = 4)

# gap approach
#gap_stat <- cluster::clusGap(combined_df_bands, FUN = kmeans, nstart = 25,
#                    K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)


#####################################
## 2019
#####################################
##################
# data preparation
# read data
files_list <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                         pattern = "*2019*",
                         full.names = TRUE)
files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                          pattern = "*2019*") |> substr(6,8)
files <- lapply(files_list, read.csv2)

################################################
## normalized
files_wider <- list()
files <- lapply(files_list, read.csv2)
for (i in 1:length(files)){
  files[[i]]$band_value_original <- NULL
  files[[i]]$band_value <- NULL
  files_wider[[i]] <- files[[i]] |>  pivot_wider(names_from = date, values_from = band_value_norm)
  colnames(files_wider[[i]]) <- gsub("2019", paste0(files_names[[i]], "_", "2019"), colnames(files_wider[[i]]))
}

combined_df <- Reduce(function(x, y) merge(x, y, by = c("species", "CLNR", "BANR")), files_wider)
write.csv2(combined_df, "01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2019_norm.csv",
           row.names = FALSE)

################################################


combined_df_bands <- combined_df[,c(4:1025)]
k_res <- list()
for(i in 2:10){
  set.seed(100)
  k_res[[i-1]] <- kmeans(combined_df_bands, centers = i, nstart = 50)
  
}
saveRDS(k_res, file = "01_data/10_preprocessed/cluster/kmeans_2019_results_2_10.RDS")

fviz_nbclust(combined_df_bands, kmeans, method = "silhouette")
ggsave("00_documentation/figures/cluster/Silhouette_2019.png",
       dpi = 900,
       width = 5,
       height = 4)



