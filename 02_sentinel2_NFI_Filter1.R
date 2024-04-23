## ---------------------------
##
## Script name: 02_sentinel2_NFI_Filter1 
##
## Purpose of script: Apply Filter 2 to NFI-Sentinel-2 time series (using TSS - real values) to select pure species plots with adequate Sentinel-2 observations
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
## filter
# requirements
# A. one image from 11-03
# B. 04-10 one image every 30 days
##
## ---------------------------

########## 2019
library(dplyr)
tss_files_list <- list.files("01_data/10_preprocessed/extract_tss_trees/", full.names = T)
tss_files <- lapply(tss_files_list, read.csv2)


all_tss <- do.call(rbind, tss_files)

all_tss$species
all_tss$CLNR

tss_only <- all_tss[,grep("X20", colnames(all_tss))]


tss_only_reduced_ts <- tss_only[,c(which(colnames(tss_only) == "X2018.11.01"):
                                     which(colnames(tss_only) == "X2020.03.31"))]

tss_df <- data.frame(species = all_tss$species,
                     CLNR = all_tss$CLNR,
                     BANR = all_tss$BANR,
                     tss_only_reduced_ts)

# remove duplicates but keep BANR
tss_df<- distinct(tss_df, across(-BANR), .keep_all = TRUE)


# filter
# requirements
# A. one image from 11-03
# B. 04-10 one image every 30 days

# A. winter at least with one image
# Check if there are rows with all values being NA within winter period
tss_df$filter_winter18_19 <- apply(tss_df[,c(which(colnames(tss_df) == "X2018.11.01"):which(colnames(tss_df) == "X2019.03.31"))], 1, function(row) all(is.na(row)))
tss_df$filter_winter19_20 <- apply(tss_df[,c(which(colnames(tss_df) == "X2019.11.01"):which(colnames(tss_df) == "X2020.03.31"))], 1, function(row) all(is.na(row)))


# B. vegetation period an image at least every 30 days
# Extract colnames for each row where values are present
colnames_with_values <- apply(tss_df[,c(which(colnames(tss_df) == "X2019.04.01"):which(colnames(tss_df) == "X2019.10.31"))], 1, function(row) names(which(!is.na(row))))

# Convert strings to Date objects
date_objects <- list()
date_diffs_days <- list()
diff_max <- list()
for (i in 1:length(colnames_with_values)){
  date_objects[[i]] <- as.Date(sub("X", "", colnames_with_values[[i]]), format = "%Y.%m.%d")
  date_diffs_days[[i]] <- diff(date_objects[[i]], units = "days")
  diff_max[[i]] <- max(date_diffs_days[[i]])
  }

tss_df$filter_vegperiod <- unlist(diff_max)

# summarizing filtering
# TRUE winter --> only NA
# > 30 --> not good 
tss_df$filter <- ifelse(tss_df$filter_winter18_19 == FALSE & tss_df$filter_winter19_20 == FALSE & tss_df$filter_vegperiod < 30, "okay", "no_data")

sum(tss_df$filter == "okay")
sum(tss_df$filter == "no_data")

tss_df_filtered_2019 <- tss_df[which(tss_df$filter == "okay"),]

write.csv2(tss_df_filtered_2019, "01_data/10_preprocessed/filtered/tss_EVI_filtered_2019.csv", row.names = FALSE)



#######################
# select filtered samples for map making
trees <-
  st_read("01_data/10_preprocessed/lfi/data_selection_spatial_epsg3035.shp")
tss_df_filtered_2019_reduced <- tss_df_filtered_2019 |> dplyr::select(species, CLNR, BANR)
trees_subset <-
  inner_join(trees, tss_df_filtered_2019_reduced, by = c("species", "CLNR", "BANR"))

trees_subset_reduced <- trees_subset |> dplyr::select(species, CLNR)

# distinct CLNR, species
trees_subset_reduced_distinct <- distinct(trees_subset_reduced, across(-geometry), .keep_all = TRUE)

st_write(trees_subset_reduced_distinct, "01_data/10_preprocessed/map_data/plots_2019_filtered.gpkg")



#########################################################################
########## 2020
#########################################################################
library(dplyr)
tss_files_list <- list.files("01_data/10_preprocessed/extract_tss_trees/", full.names = T)
tss_files <- lapply(tss_files_list, read.csv2)


all_tss <- do.call(rbind, tss_files)

all_tss$species
all_tss$CLNR

tss_only <- all_tss[,grep("X20", colnames(all_tss))]


tss_only_reduced_ts <- tss_only[,c(which(colnames(tss_only) == "X2019.11.01"):
                                     which(colnames(tss_only) == "X2021.03.31"))]

tss_df <- data.frame(species = all_tss$species,
                     CLNR = all_tss$CLNR,
                     BANR = all_tss$BANR,
                     tss_only_reduced_ts)

# remove duplicates but keep BANR
tss_df<- distinct(tss_df, across(-BANR), .keep_all = TRUE)



# filter
# requirements
# A. one image from 11-03
# B. 04-10 one image every 30 days

# A. winter at least with one image
# Check if there are rows with all values being NA within winter period
tss_df$filter_winter19_20 <- apply(tss_df[,c(which(colnames(tss_df) == "X2019.11.01"):which(colnames(tss_df) == "X2020.03.31"))], 1, function(row) all(is.na(row)))
tss_df$filter_winter20_21 <- apply(tss_df[,c(which(colnames(tss_df) == "X2020.11.01"):which(colnames(tss_df) == "X2021.03.31"))], 1, function(row) all(is.na(row)))

# B. vegetation period an image at least every 30 days
# Extract colnames for each row where values are present
colnames_with_values <- apply(tss_df[,c(which(colnames(tss_df) == "X2020.04.01"):which(colnames(tss_df) == "X2020.10.31"))], 1, function(row) names(which(!is.na(row))))


# Convert strings to Date objects
date_objects <- list()
date_diffs_days <- list()
diff_max <- list()
for (i in 1:length(colnames_with_values)){
  date_objects[[i]] <- as.Date(sub("X", "", colnames_with_values[[i]]), format = "%Y.%m.%d")
  date_diffs_days[[i]] <- diff(date_objects[[i]], units = "days")
  diff_max[[i]] <- max(date_diffs_days[[i]])
}

tss_df$filter_vegperiod <- unlist(diff_max)

# summarizing filtering
# TRUE winter --> only NA
# > 30 --> not good 
tss_df$filter <- ifelse(tss_df$filter_winter20_21 == FALSE & tss_df$filter_winter19_20 == FALSE & tss_df$filter_vegperiod < 30, "okay", "no_data")

sum(tss_df$filter == "okay")
sum(tss_df$filter == "no_data")

tss_df_filtered_2020 <- tss_df[which(tss_df$filter == "okay"),]

write.csv2(tss_df_filtered_2020, "01_data/10_preprocessed/filtered/tss_EVI_filtered_2020.csv", row.names = FALSE)

#######################
# select filtered samples for map making
trees <-
  st_read("01_data/10_preprocessed/lfi/data_selection_spatial_epsg3035.shp")
tss_df_filtered_2020_reduced <- tss_df_filtered_2020 |> dplyr::select(species, CLNR, BANR)
trees_subset <-
  inner_join(trees, tss_df_filtered_2020_reduced, by = c("species", "CLNR", "BANR"))

trees_subset_reduced <- trees_subset |> dplyr::select(species, CLNR)

# distinct CLNR, species
trees_subset_reduced_distinct <- distinct(trees_subset_reduced, across(-geometry), .keep_all = TRUE)

st_write(trees_subset_reduced_distinct, "01_data/10_preprocessed/map_data/plots_2020_filtered.gpkg")
