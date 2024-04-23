## ---------------------------
##
## Script name: 09_variability
##
## Purpose of script: Retrieve standard deviations. 
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
##        All species, evergreen and deciduous separately
##
##        intraspecific - sd per timepoint
##        interspecific - median difference per timepoint or median of species group and then difference
## 
##        intraspecific - mean of all intraspecific differences
##        interspecific - mean of all interspecific differences
##
## ---------------------------
library(dplyr)

# read data
dat_df <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2020_norm.csv")
band_names <- files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                                        pattern = "*2020*") |> substr(6,8)

evergreen_df <- dat_df[dat_df$species %in% c("Abies alba", "Picea abies", "Pinus sylvestris"),]
deciduous_df <- dat_df[dat_df$species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi"),]


sd_per_intra_group <- list()
mean_per_intra_time_point <- list()
median_per_intra_group <- list()
for (i in 1:length(band_names)) {
  all_df_subset <-    dat_df[, c(1, grep(band_names[[i]], colnames(dat_df)))]
  
  ## intraspecific
  sd_per_intra_group[[i]] <- all_df_subset |> group_by(species) |> summarise(across(everything(), sd, na.rm = TRUE))
  write.csv2(sd_per_intra_group[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                             "norm_2020_intraspecific_sd_", band_names[[i]], ".csv"), row.names = FALSE)
  
  mean_per_intra_time_point[[i]] <- summarise(sd_per_intra_group[[i]], across(-species, mean))
  write.csv2(mean_per_intra_time_point[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                                    "norm_2020_intraspecific_mean_sd_", band_names[[i]], ".csv"), row.names = FALSE)
  
  ## interspecific
  # median per group
  # differences 
  median_per_intra_group[[i]] <- all_df_subset |> group_by(species) |> summarise(across(everything(), median, na.rm = TRUE))
  write.csv2(median_per_intra_group[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                                 "norm_2020_intraspecific_median_", band_names[[i]], ".csv"), row.names = FALSE)
  
  # differences between the median of the groups
  difference_table <- data.frame()
  
  # Get the column names of the date columns
  date_columns <- grep("_2020", colnames(median_per_intra_group[[i]]), value = TRUE)
  
  # Generate all combinations of species
  species_combinations <- combn(unique(median_per_intra_group[[i]]$species), 2)
  
  # Iterate over each combination of species
  for (z in seq_len(ncol(species_combinations))) {
    # Extract species for the current combination
    species1 <- species_combinations[1, z]
    species2 <- species_combinations[2, z]
    
    # Subset data for the two species
    species_subset <- median_per_intra_group[[i]] %>%
      filter(species %in% c(species1, species2))
    
    # Calculate differences for each pair of dates
    differences <- apply(species_subset[date_columns], 2, function(x) abs(diff(x)))
    sd_differences <- apply(species_subset[date_columns], 2, function(x) abs(sd(x)))
    # Create a dataframe to store differences
    diff_df <- data.frame(Species1 = species1,
                          Species2 = species2,
                          Date = date_columns,
                          Difference = as.vector(differences),
                          sd_difference = as.vector(sd_differences))
    
    # Append differences to the difference_table
    difference_table <- bind_rows(difference_table, diff_df)
  }
  write.csv2(difference_table, paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                      "norm_2020_interspecific_differences_of_medians_per_group_", band_names[[i]], ".csv"), row.names = FALSE)
  
  
  diff_mean <- difference_table |> group_by(Date) |> summarise(mean(Difference))
  diff_mean$mean_sd <- difference_table |> group_by(Date) |> summarise(mean(sd_difference))
  
  write.csv2(diff_mean, paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                               "norm_2020_interspecific_mean_of_sd_of_medians_per_group_", band_names[[i]], ".csv"), row.names = FALSE)
}




###################################
#### 2019
###################################
# read data
dat_df <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2019_norm.csv")
band_names <- files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                                        pattern = "*2019*") |> substr(6,8)

evergreen_df <- dat_df[dat_df$species %in% c("Abies alba", "Picea abies", "Pinus sylvestris"),]
deciduous_df <- dat_df[dat_df$species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi"),]


sd_per_intra_group <- list()
mean_per_intra_time_point <- list()
median_per_intra_group <- list()
for (i in 1:length(band_names)) {
  all_df_subset <-    dat_df[, c(1, grep(band_names[[i]], colnames(dat_df)))]
  
  ## intraspecific
  sd_per_intra_group[[i]] <- all_df_subset |> group_by(species) |> summarise(across(everything(), sd, na.rm = TRUE))
  write.csv2(sd_per_intra_group[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                             "norm_2019_intraspecific_sd_", band_names[[i]], ".csv"), row.names = FALSE)
  
  mean_per_intra_time_point[[i]] <- summarise(sd_per_intra_group[[i]], across(-species, mean))
  write.csv2(mean_per_intra_time_point[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                                    "norm_2019_intraspecific_mean_sd_", band_names[[i]], ".csv"), row.names = FALSE)
  
  ## interspecific
  # median per group
  # differences 
  median_per_intra_group[[i]] <- all_df_subset |> group_by(species) |> summarise(across(everything(), median, na.rm = TRUE))
  write.csv2(median_per_intra_group[[i]], paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                                 "norm_2019_intraspecific_median_", band_names[[i]], ".csv"), row.names = FALSE)
  
  # differences between the median of the groups
  difference_table <- data.frame()
  
  # Get the column names of the date columns
  date_columns <- grep("_2019", colnames(median_per_intra_group[[i]]), value = TRUE)
  
  # Generate all combinations of species
  species_combinations <- combn(unique(median_per_intra_group[[i]]$species), 2)
  
  # Iterate over each combination of species
  for (z in seq_len(ncol(species_combinations))) {
    # Extract species for the current combination
    species1 <- species_combinations[1, z]
    species2 <- species_combinations[2, z]
    
    # Subset data for the two species
    species_subset <- median_per_intra_group[[i]] %>%
      filter(species %in% c(species1, species2))
    
    # Calculate differences for each pair of dates
    differences <- apply(species_subset[date_columns], 2, function(x) abs(diff(x)))
    sd_differences <- apply(species_subset[date_columns], 2, function(x) abs(sd(x)))
    # Create a dataframe to store differences
    diff_df <- data.frame(Species1 = species1,
                          Species2 = species2,
                          Date = date_columns,
                          Difference = as.vector(differences),
                          sd_difference = as.vector(sd_differences))
    
    # Append differences to the difference_table
    difference_table <- bind_rows(difference_table, diff_df)
  }
  write.csv2(difference_table, paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                                      "norm_2019_interspecific_differences_of_medians_per_group_", band_names[[i]], ".csv"), row.names = FALSE)
  
  
  diff_mean <- difference_table |> group_by(Date) |> summarise(mean(Difference))
  diff_mean$mean_sd <- difference_table |> group_by(Date) |> summarise(mean(sd_difference))
  
  write.csv2(diff_mean, paste0("01_data/10_preprocessed/time_points_separability_evaluation/",
                               "norm_2019_interspecific_mean_of_sd_of_medians_per_group_", band_names[[i]], ".csv"), row.names = FALSE)
}
