## ---------------------------
##
## Script name: 01_sentinel2_extract_TSS_multiyear 
##
## Purpose of script: Extract the pixels of Sentinel-2 time series (TSS - real values) at NFI positions
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
library(sf)
library(raster)
library(terra)
library(lubridate)
library(ggplot2)

# NFI samples (Filter 1 already applied)
trees <-
  st_read("01_data/10_preprocessed/lfi/data_selection_spatial_epsg3035.shp")


specs <-
  list(
    "Fagus sylvatica",
    "Fraxinus excelsior",
    "Castanea sativa",
    "Larix decidua, Larix kaempferi",
    "Picea abies",
    "Pinus sylvestris",
    "Abies alba"
  )


# loop for each tree species separately
for (i in 1:length(specs)) {
  # select subset
  trees_subset <- trees[which(trees$species %in% specs[[i]]),]
  trees_terra <- vect(trees_subset)
  
  # Sentinel-2
  # select tiles with tree data from grid
  # 5-daily time-series data
  grid <-
    st_read("//speedy11-12-fs/Data_23/USER_TIZIANA/FORCE_Kingslide/level2/shp/grid.shp")
  grid_rows <- st_intersects(trees_subset, grid)
  tiles_subset <- grid[as.numeric(grid_rows),]
  tiles_subset_v <- unique(tiles_subset$Tile_ID)
  
  # load data from selected tiles and extract data
  # Only EVI
  for (j in 1:length(tiles_subset_v)) {
    path_location <-
      paste0(
        "//speedy11-12-fs/Data_23/USER_TIZIANA/FORCE_Kingslide/level2/tsa/real_values_flagged/",
        tiles_subset_v[[j]]
      )
    s2 <-
      list.files(path = path_location,
                 full.names = TRUE,
                 pattern = "*EVI_TSS.tif$")
    
    
    if (j == 1) {
      r_2018 <- lapply(s2[[1]], function(x) {
        terra::rast(s2[[1]]) %>% terra::extract(trees_terra)
      })
      r_2019 <- lapply(s2[[2]], function(x) {
        terra::rast(s2[[2]]) %>% terra::extract(trees_terra)
      })
      r_2020 <- lapply(s2[[3]], function(x) {
        terra::rast(s2[[3]]) %>% terra::extract(trees_terra)
      })
      r_2021 <- lapply(s2[[4]], function(x) {
        terra::rast(s2[[4]]) %>% terra::extract(trees_terra)
      })
    } else{
      r_2018[j] <- lapply(s2[[1]], function(x) {
        terra::rast(s2[[1]]) %>% terra::extract(trees_terra)
      })
      r_2019[j] <- lapply(s2[[2]], function(x) {
        terra::rast(s2[[2]]) %>% terra::extract(trees_terra)
      })
      r_2020[j] <- lapply(s2[[3]], function(x) {
        terra::rast(s2[[3]]) %>% terra::extract(trees_terra)
      })
      r_2021[j] <- lapply(s2[[4]], function(x) {
        terra::rast(s2[[4]]) %>% terra::extract(trees_terra)
      })
    }
    # only keeps rows, where values are present
    r_2018[[j]] <-
      r_2018[[j]][rowSums(is.na(r_2018[[j]][, 2:ncol(r_2018[[j]])])) < ncol(r_2018[[j]]) - 1,]
    r_2019[[j]] <-
      r_2019[[j]][rowSums(is.na(r_2019[[j]][, 2:ncol(r_2019[[j]])])) < ncol(r_2019[[j]]) - 1,]
    r_2020[[j]] <-
      r_2020[[j]][rowSums(is.na(r_2020[[j]][, 2:ncol(r_2020[[j]])])) < ncol(r_2020[[j]]) - 1,]
    r_2021[[j]] <-
      r_2021[[j]][rowSums(is.na(r_2021[[j]][, 2:ncol(r_2021[[j]])])) < ncol(r_2021[[j]]) - 1,]
  }
  
  
  # combine all extractions
  # Careful not uniform dates
  # data.frame with all 365 days as columns and then fill in values
  doy_2018 <-
    as.character(seq(as.Date("2018/01/01"), as.Date("2018/12/31"), 1))
  doy_2019 <-
    as.character(seq(as.Date("2019/01/01"), as.Date("2019/12/31"), 1))
  doy_2020 <-
    as.character(seq(as.Date("2020/01/01"), as.Date("2020/12/31"), 1))
  doy_2021 <-
    as.character(seq(as.Date("2021/01/01"), as.Date("2021/12/31"), 1))
  
  # for each year separately
  df_initials_list <- list()
  for (r_year in 1:4){
    if (r_year == 1){
      r <- r_2018
      doy_all <- doy_2018
    }else if(r_year == 2){
      r <- r_2019
      doy_all <- doy_2019
    }else if(r_year == 3){
      r <- r_2020
      doy_all <- doy_2020
    }else if(r_year == 4){
      r <- r_2021
      doy_all <- doy_2021
    }
    
    for (p in 1:length(r))    {
      if (p == 1) {
        # Prepare initial df
        doy_df_1 <-
          data.frame(matrix(ncol = length(doy_all), nrow = nrow(r[[p]])))
        colnames(doy_df_1) <- doy_all
        
        # from r (S-2)
        dates_colnames <-
          colnames(r[[p]][2:ncol(r[[p]])]) %>% stringr::str_sub(1, 8)
        doy <- ymd(dates_colnames) %>% yday()
        
        # filling df
        for (fill in 1:length(doy)) {
          if (sum(is.na(r[[p]][, fill + 1])) < length(r[[p]][, fill + 1])) {
            doy_df_1[, doy[fill]] <- r[[p]][, fill + 1]
          }
        }
        # attach ID
        df_initial <- cbind(r[[p]]["ID"], doy_df_1)
        
      } else{
        doy_df_2 <-
          data.frame(matrix(ncol = length(doy_all), nrow = nrow(r[[p]])))
        colnames(doy_df_2) <- doy_all
        
        # from r (S-2)
        dates_colnames <-
          colnames(r[[p]][2:ncol(r[[p]])]) %>% stringr::str_sub(1, 8)
        doy <- ymd(dates_colnames) %>% yday()
        
        # filling df
        for (fill in 1:length(doy)) {
          if (sum(is.na(r[[p]][, fill + 1])) < length(r[[p]][, fill + 1])) {
            doy_df_2[, doy[fill]] <- r[[p]][, fill + 1]
          }
        }
        # attach ID
        df_2 <- cbind(r[[p]]["ID"], doy_df_2)
        
        # merge dfs
        df_initial <- union(df_initial, df_2)
      }
      
    }
    
    df_initials_list[[r_year]] <- df_initial
    
  }
  
  
  df_initials_all <- dplyr::left_join(df_initials_list[[1]], df_initials_list[[2]], by = "ID")
  df_initials_all <- dplyr::left_join(df_initials_all, df_initials_list[[3]], by = "ID")
  df_initials_all <- dplyr::left_join(df_initials_all, df_initials_list[[4]], by = "ID")
  
  df_initials_all <- df_initials_all[order(df_initials_all$ID), ]
  

  trees_subset$ID <- seq(1, nrow(trees_subset))
  
  # combine extracted values with tree information
  tab_all <-
    dplyr::inner_join(trees_subset, df_initials_all,  by = "ID")
  
  
  spec_names <-
    specs[[i]] |> stringr::str_replace_all(" ", "_") |> stringr::str_replace_all(",", "")
  

  # save as csv
  write.csv2(
    tab_all,
    paste0(
      "01_data/10_preprocessed/revisions/extract_tss_trees/",
      spec_names,
      "_EVI_2018_2021",
      ".csv"
    ),
    row.names = FALSE
  )
}
