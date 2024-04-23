## ---------------------------
##
## Script name: 03_sentinel2_extract_TSI_only_filtered 
##
## Purpose of script: Extract the pixels of Sentinel-2 time series (TSI - interpolated & smoothed) at filtered NFI positions
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

# Sentinel-2 extract
# all bands and indices
# use filtered CLNR & BANR already to reduce computation time

library(sf)
library(raster)
library(terra)
library(lubridate)
library(ggplot2)
library(dplyr)


trees <-
  st_read("01_data/10_preprocessed/lfi/data_selection_spatial_epsg3035.shp")

tss_filter_2019 <-
  read.csv2("01_data/10_preprocessed/filtered/tss_EVI_filtered_2019.csv")
tss_filter_2020 <-
  read.csv2("01_data/10_preprocessed/filtered/tss_EVI_filtered_2020.csv")

tss_filters <-
  full_join(tss_filter_2019,
            tss_filter_2020,
            by = c("species", "CLNR", "BANR"))


# reduce data frame
tss_filters_reduced <-
  tss_filters |> dplyr::select(species, CLNR, BANR)

# select trees from shapefile
trees_subset <-
  inner_join(trees, tss_filters_reduced, by = c("species", "CLNR", "BANR"))



trees_terra <- vect(trees_subset)

# Sentinel-2
# select tiles with tree data from grid
# 5-daily time-series data
grid <-
  st_read("//speedy11-12-fs/Data_23/USER_TIZIANA/FORCE_Kingslide/level2/shp/grid.shp")
grid_rows <- st_intersects(trees_subset, grid)
tiles_subset <- grid[as.numeric(grid_rows),]
tiles_subset_v <- unique(tiles_subset$Tile_ID)


n_bands <- length(list.files(path = paste0(
                          "//speedy11-12-fs/Data_23/USER_TIZIANA/FORCE_Kingslide/level2/tsa/tsi_2017_2023_5_day_10_20_30_50_max_100/",
                          tiles_subset_v[[22]]
                        ), full.names = TRUE, pattern = "*TSI.tif$"))

# load data from selected tiles and extract data
# All bands and indices
for (s in 1:n_bands) {
  for (j in 1:length(tiles_subset_v)) {
    path_location <-
      paste0(
        "//speedy11-12-fs/Data_23/USER_TIZIANA/FORCE_Kingslide/level2/tsa/tsi_2017_2023_5_day_10_20_30_50_max_100/",
        tiles_subset_v[[j]]
      )
    s2 <-
      list.files(path = path_location,
                 full.names = TRUE,
                 pattern = "*TSI.tif$")
    s2_names <-
      list.files(path = path_location,
                 full.names = FALSE,
                 pattern = "*TSI.tif$")
    
    
    if (j == 1) {
      r <- lapply(s2[[s]], function(x) {
        terra::rast(s2[[s]]) %>% terra::extract(trees_terra)
      })
    } else{
      r[j] <- lapply(s2[[s]], function(x) {
        terra::rast(s2[[s]]) %>% terra::extract(trees_terra)
      })
    }
    # only keeps rows, where values are present
    r[[j]] <-
      r[[j]][rowSums(is.na(r[[j]][, 2:ncol(r[[j]])])) < ncol(r[[j]]) - 1,]
  }
  
  
  # combine all extractions
  r_all <- do.call(rbind, r)
  r_all <- r_all[order(r_all$ID), ]
  # get dates
  # yday for doy
  dates <-
    as.character(seq(as.Date("2017/01/01"), as.Date("2020/02/25"), 5))
  dates <-
    c(dates, as.character(seq(
      as.Date("2020/03/02"), as.Date("2023/12/31"), 5
    )))
  
  doy <-
    yday(seq(as.Date("2017/01/01"), as.Date("2020/02/25"), 5))
  doy <-
    c(doy, yday(seq(
      as.Date("2020/03/02"), as.Date("2023/12/31"), 5
    )))
  
  # use dates as column names
  colnames(r_all) <- c("ID", dates)
  
  
  trees_subset$ID <- seq(1, nrow(trees_subset))
  
  # combine extracted values with tree information
  tab_all <- dplyr::inner_join(trees_subset, r_all, by = "ID")
  

  
  # save as csv
  write.csv2(
    tab_all,
    paste0(
      "01_data/10_preprocessed/extract_tsi_trees/",
      substr(s2_names[[s]], 32, 34),
      ".csv"
    ),
    row.names = FALSE
  )
  
}

