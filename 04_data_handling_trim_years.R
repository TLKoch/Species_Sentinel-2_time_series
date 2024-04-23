## ---------------------------
##
## Script name: 04_data_handling_trim_years
##
## Purpose of script: Trim band/index time series to year of analysis (2019 and 2020) and calculate sample sizes
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

library(tidyr)
library(dplyr)

# data handling
# trim band/index time series to year of analysis
tsi_files_list <- list.files("01_data/10_preprocessed/extract_tsi_trees/", full.names = T, pattern = "*.csv")
tsi_files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/", full.names = F, pattern = "*.csv")
tsi_files <- lapply(tsi_files_list, read.csv2)

###################################################
### include filters again to separate 2019 & 2020
tss_filter_2019 <-
  read.csv2("01_data/10_preprocessed/filtered/tss_EVI_filtered_2019.csv")|> dplyr::select(species, CLNR, BANR)
tss_filter_2020 <-
  read.csv2("01_data/10_preprocessed/filtered/tss_EVI_filtered_2020.csv")|> dplyr::select(species, CLNR, BANR)

tsi_files_2020 <- list()
tsi_files_2019 <- list()
for (i in 1:length(tsi_files)){
  tsi_files_2020[[i]] <- tss_filter_2020 |> dplyr::inner_join(tsi_files[[i]], by = c("species", "CLNR", "BANR"))
  tsi_files_2019[[i]] <- tss_filter_2019 |> dplyr::inner_join(tsi_files[[i]], by = c("species", "CLNR", "BANR"))
}


###################################################
# identify complete cases across all bands/indices
tsi_list <- list()
for (i in 1:length(tsi_files_2020)){
  tsi_trimmed_2020 <- data.frame(CLNR = tsi_files_2020[[i]]$CLNR,
                                 species = tsi_files_2020[[i]]$species,
                                 BANR = tsi_files_2020[[i]]$BANR,
                                 tsi_files_2020[[i]][,grep("X2020", colnames(tsi_files_2020[[i]]))])
  tsi_list[[i]] <- tsi_trimmed_2020[!complete.cases(tsi_trimmed_2020), ]
  
}

df_lost <- do.call(rbind, tsi_list)



# trim year 2020
for (i in 1:length(tsi_files_2020)){
  tsi_trimmed_2020 <- data.frame(CLNR = tsi_files_2020[[i]]$CLNR,
                                 species = tsi_files_2020[[i]]$species,
                                 BANR = tsi_files_2020[[i]]$BANR,
                                 tsi_files_2020[[i]][,grep("X2020", colnames(tsi_files_2020[[i]]))])
  tsi_trimmed_2020 <- tsi_trimmed_2020[-which(tsi_trimmed_2020$BANR %in% df_lost$BANR), ]
  write.csv2(tsi_trimmed_2020, paste0("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/2020_",
                                      tsi_files_names[[i]]), row.names = FALSE)
}



## scale matrix
for (i in 1:length(tsi_files_2020)){
  tsi_trimmed_2020 <- data.frame(CLNR = tsi_files_2020[[i]]$CLNR,
                                 species = tsi_files_2020[[i]]$species,
                                 BANR = tsi_files_2020[[i]]$BANR,
                                 tsi_files_2020[[i]][,grep("X2020", colnames(tsi_files_2020[[i]]))])
  tsi_trimmed_2020 <- tsi_trimmed_2020[-which(tsi_trimmed_2020$BANR %in% df_lost$BANR), ]
  
  tsi_trimmed_2020_red <- as.data.frame(tsi_trimmed_2020[,grep("X2020", colnames(tsi_trimmed_2020))])

  
  band <- tsi_trimmed_2020_red
  band$species <- tsi_trimmed_2020$species
  band$CLNR <- tsi_trimmed_2020$CLNR
  band$BANR <- tsi_trimmed_2020$BANR
  band <- band[complete.cases(band), ]
  
  band_long <- band |> pivot_longer(!c(species, CLNR, BANR), names_to = "date", values_to = "band_value")
  band_long <- band_long |> mutate(date = as.Date(sub("X|_band", "", date), format = "%Y.%m.%d"))
  band_long$band_value_original <- band_long$band_value
  band_long$band_value <- scale(band_long$band_value, center = FALSE)
  band_long$band_value_norm <- scale(band_long$band_value_original, center = min(band_long$band_value_original), 
                                     scale = max(band_long$band_value_original) - min(band_long$band_value_original))
  

  
  write.csv2(band_long, paste0("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/2020_",
                                      tsi_files_names[[i]]), row.names = FALSE)
  
  
}


#######################################################
# trim year 2019
# identify complete cases across all bands/indices
tsi_list <- list()
for (i in 1:length(tsi_files_2019)){
  tsi_trimmed_2019 <- data.frame(CLNR = tsi_files_2019[[i]]$CLNR,
                                 species = tsi_files_2019[[i]]$species,
                                 BANR = tsi_files_2019[[i]]$BANR,
                                 tsi_files_2019[[i]][,grep("X2019", colnames(tsi_files_2019[[i]]))])
  tsi_list[[i]] <- tsi_trimmed_2019[!complete.cases(tsi_trimmed_2019), ]
  
}

df_lost <- do.call(rbind, tsi_list)


# trim year 2020
for (i in 1:length(tsi_files_2019)){
  tsi_trimmed_2019 <- data.frame(CLNR = tsi_files_2019[[i]]$CLNR,
                                 species = tsi_files_2019[[i]]$species,
                                 BANR = tsi_files_2019[[i]]$BANR,
                                 tsi_files_2019[[i]][,grep("X2019", colnames(tsi_files_2019[[i]]))])
  tsi_trimmed_2019 <- tsi_trimmed_2019[-which(tsi_trimmed_2019$BANR %in% df_lost$BANR), ]
  write.csv2(tsi_trimmed_2019, paste0("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/2019_",
                                      tsi_files_names[[i]]), row.names = FALSE)
}


## scale matrix
for (i in 1:length(tsi_files_2019)){
  tsi_trimmed_2019 <- data.frame(CLNR = tsi_files_2019[[i]]$CLNR,
                                 species = tsi_files_2019[[i]]$species,
                                 BANR = tsi_files_2019[[i]]$BANR,
                                 tsi_files_2019[[i]][,grep("X2019", colnames(tsi_files_2019[[i]]))])
  tsi_trimmed_2019 <- tsi_trimmed_2019[-which(tsi_trimmed_2019$BANR %in% df_lost$BANR), ]
  
  tsi_trimmed_2019_red <- as.data.frame(tsi_trimmed_2019[,grep("X2019", colnames(tsi_trimmed_2019))])

  
  band <- tsi_trimmed_2019_red
  band$species <- tsi_trimmed_2019$species
  band$CLNR <- tsi_trimmed_2019$CLNR
  band$BANR <- tsi_trimmed_2019$BANR
  band <- band[complete.cases(band), ]
  
  band_long <- band |> pivot_longer(!c(species, CLNR, BANR), names_to = "date", values_to = "band_value")
  band_long <- band_long |> mutate(date = as.Date(sub("X|_band", "", date), format = "%Y.%m.%d"))
  band_long$band_value_original <- band_long$band_value
  band_long$band_value <- scale(band_long$band_value, center = FALSE)
  band_long$band_value_norm <- scale(band_long$band_value_original, center = min(band_long$band_value_original), 
                                     scale = max(band_long$band_value_original) - min(band_long$band_value_original))
  

  write.csv2(band_long, paste0("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/2019_",
                               tsi_files_names[[i]]), row.names = FALSE)
}








#########################################
########### Calculate samples sizes
length(unique(tsi_trimmed_2020$CLNR))
# 200 plots, 766 pixels
length(unique(tsi_trimmed_2019$CLNR))
# 159 plots, 563 pixels
# 158

tsi_trimmed_2019 %>%
  group_by(species) %>%
  summarise(count=n())
#pixels:
# "Abies alba": 12
# "Castanea sativa": 15
# "Fagus sylvatica" 184
# "Fraxinus excelsior": 5
# "Larix decidua, Larix kaempferi": 44
# "Picea abies": 279
# "Pinus sylvestris": 24

#
subs_2019 <- tsi_trimmed_2019[tsi_trimmed_2019$species == "Picea abies",]
length(unique(subs_2019$CLNR))

# 2020
tsi_trimmed_2020 %>%
  group_by(species) %>%
  summarise(count=n())
#pixels:
# "Abies alba": 22
# "Castanea sativa": 51
# "Fagus sylvatica" 285
# "Fraxinus excelsior": 13
# "Larix decidua, Larix kaempferi": 34
# "Picea abies": 332
# "Pinus sylvestris": 29

#
