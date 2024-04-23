## ---------------------------
##
## Script name: 08_correlation_analyses
##
## Purpose of script: Perform correlation analyses. Make plots.
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
library(corrplot)
library(dplyr)
library(tidyr)
library(Hmisc)
library(cowplot)
library(ggcorrplot)
# data preparation
# read data
files_list <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/",
                         pattern = "*2020*",
                         full.names = TRUE)
files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/",
                          pattern = "*2020*") |> substr(6,8)
files <- lapply(files_list, read.csv2)


##############
# Median r for each species and band combination
species_uni <- unique(files[[1]]$species)
empty_df <- data.frame(matrix(ncol = length(species_uni), nrow = length(files_names)))
colnames(empty_df) <- species_uni
rownames(empty_df) <- files_names

for(i in 1:length(files)){
  for(j in 1:length(species_uni)){
    band <- files[[i]]
    art <- band[which(band$species == species_uni[[j]]),]
    art_ts <- art[, grep("^X2020", colnames(art), value = TRUE)]
    art_t <- t(art_ts)
    art_corr <- rcorr(art_t)

    median(art_corr$r)
    empty_df[i,j] <- median(art_corr$r)
  }
}
write.csv2(empty_df, "01_data/10_preprocessed/correlations/corr_intraspecific_median_2020.csv")

##############
# species wise
p_list <- list()
files_names_beauti <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                        "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                        "SWIR-1", "SWIR-2")

for(i in 1:length(files)) {
  band <- files[[i]]
  
  band$CLNR <- NULL
  band$BANR <- NULL
  median_ts <- band %>%
    group_by(species) %>%
    summarise_all(.funs = median, na.rm = TRUE)
  
  median_ts_t <- t(median_ts)
  colnames(median_ts_t) <- median_ts_t[1, ]
  median_ts_t <- median_ts_t[-1, ]
  median_ts_corr <- rcorr(median_ts_t)
  
  # replace 1 with median correlations for each species
  median_replacements <- empty_df[files_names[[i]], ]
  median_ts_corr$r["Abies alba", "Abies alba"] <-
    median_replacements$`Abies alba`
  median_ts_corr$r["Castanea sativa", "Castanea sativa"] <-
    median_replacements$`Castanea sativa`
  median_ts_corr$r["Fagus sylvatica", "Fagus sylvatica"] <-
    median_replacements$`Fagus sylvatica`
  median_ts_corr$r["Fraxinus excelsior", "Fraxinus excelsior"] <-
    median_replacements$`Fraxinus excelsior`
  median_ts_corr$r["Larix decidua, Larix kaempferi", "Larix decidua, Larix kaempferi"] <-
    median_replacements$`Larix decidua, Larix kaempferi`
  median_ts_corr$r["Picea abies", "Picea abies"] <-
    median_replacements$`Picea abies`
  median_ts_corr$r["Pinus sylvestris", "Pinus sylvestris"] <-
    median_replacements$`Pinus sylvestris`
  
  # change to Larix spp.
  colnames(median_ts_corr$r)[which(colnames(median_ts_corr$r) == "Larix decidua, Larix kaempferi")] <-
    "Larix spp."
  rownames(median_ts_corr$r)[which(rownames(median_ts_corr$r) == "Larix decidua, Larix kaempferi")] <-
    "Larix spp."
  p_list[[i]] <- median_ts_corr$r
  
}


################
generate_corrplot <- function(i, lab_pos = 'n') {

  
  # Generate corrplot with specified parameters
  # no labels: tl.pos = 'n',
  corrplot(
    p_list[[i]],
    order = 'alphabet',
    type = "lower",
    tl.col = "black",
    method = 'color',
    font = 3,
    cl.pos = 'n',
    tl.pos = lab_pos,
    title = paste(files_names_beauti[[i]]),
    mar = c(0, 0, 1, 0), 
    line = -1.25
  )
}


######
png(
    "00_documentation/figures/correlations/corr_comparison_2020.png",
  units = "in",
  width = 8.5,
  height = 4.5,
  res = 900
)
par(mfrow=c(3,5))
generate_corrplot(1)
generate_corrplot(2)
generate_corrplot(3)
generate_corrplot(4)
generate_corrplot(5)
generate_corrplot(6)
generate_corrplot(7)
generate_corrplot(8)
generate_corrplot(9)
generate_corrplot(10)
generate_corrplot(11)
generate_corrplot(12)
generate_corrplot(13)
generate_corrplot(14)
dev.off()


png(
  "00_documentation/figures/correlations/corr_legend_label_2020.png",
  units = "in",
  width = 10,
  height = 4.5,
  res = 900
)

corrplot(
  p_list[[i]],
  order = 'alphabet',
  type = "lower",
  tl.col = "black",
  method = 'color',
  font = 3,
  cl.pos = 'r',
  tl.pos = 'lw',
  title = paste(files_names_beauti[[i]]),
  mar = c(0, 0, 1, 0)
)

dev.off()

##
png(
  "00_documentation/figures/correlations/corr_legend_label_small_2020.png",
  units = "in",
  width = 7,
  height = 2,
  res = 900
)

corrplot(
  p_list[[i]],
  order = 'alphabet',
  type = "lower",
  tl.col = "black",
  method = 'color',
  font = 3,
  cl.pos = 'r',
  tl.pos = 'lw',
  title = paste(files_names_beauti[[i]]),
  mar = c(0, 0, 1, 0)
)
dev.off()




#########################
## 2019
#########################
# data preparation
# read data
files_list <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/",
                         pattern = "*2019*",
                         full.names = TRUE)
files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/",
                          pattern = "*2019*") |> substr(6,8)
files <- lapply(files_list, read.csv2)


##############
# Median r for each species and band combination
species_uni <- unique(files[[1]]$species)
empty_df <- data.frame(matrix(ncol = length(species_uni), nrow = length(files_names)))
colnames(empty_df) <- species_uni
rownames(empty_df) <- files_names

for(i in 1:length(files)){
  for(j in 1:length(species_uni)){
    band <- files[[i]]
    art <- band[which(band$species == species_uni[[j]]),]
    art_ts <- art[, grep("^X2019", colnames(art), value = TRUE)]
    art_t <- t(art_ts)
    art_corr <- rcorr(art_t)
    median(art_corr$r)
    empty_df[i,j] <- median(art_corr$r)
  }
}
write.csv2(empty_df, "01_data/10_preprocessed/correlations/corr_intraspecific_median_2019.csv")

##############
# species wise
p_list <- list()
files_names_beauti <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                        "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                        "SWIR-1", "SWIR-2")

for(i in 1:length(files)) {
  band <- files[[i]]
  
  band$CLNR <- NULL
  band$BANR <- NULL
  median_ts <- band %>%
    group_by(species) %>%
    summarise_all(.funs = median, na.rm = TRUE)
  
  median_ts_t <- t(median_ts)
  colnames(median_ts_t) <- median_ts_t[1, ]
  median_ts_t <- median_ts_t[-1, ]
  median_ts_corr <- rcorr(median_ts_t)
  
  # replace 1 with median correlations for each species
  median_replacements <- empty_df[files_names[[i]], ]
  median_ts_corr$r["Abies alba", "Abies alba"] <-
    median_replacements$`Abies alba`
  median_ts_corr$r["Castanea sativa", "Castanea sativa"] <-
    median_replacements$`Castanea sativa`
  median_ts_corr$r["Fagus sylvatica", "Fagus sylvatica"] <-
    median_replacements$`Fagus sylvatica`
  median_ts_corr$r["Fraxinus excelsior", "Fraxinus excelsior"] <-
    median_replacements$`Fraxinus excelsior`
  median_ts_corr$r["Larix decidua, Larix kaempferi", "Larix decidua, Larix kaempferi"] <-
    median_replacements$`Larix decidua, Larix kaempferi`
  median_ts_corr$r["Picea abies", "Picea abies"] <-
    median_replacements$`Picea abies`
  median_ts_corr$r["Pinus sylvestris", "Pinus sylvestris"] <-
    median_replacements$`Pinus sylvestris`
  
  # change to Larix spp.
  colnames(median_ts_corr$r)[which(colnames(median_ts_corr$r) == "Larix decidua, Larix kaempferi")] <-
    "Larix spp."
  rownames(median_ts_corr$r)[which(rownames(median_ts_corr$r) == "Larix decidua, Larix kaempferi")] <-
    "Larix spp."
  p_list[[i]] <- median_ts_corr$r
  
}

###
generate_corrplot <- function(i, lab_pos = 'n') {

  # Generate corrplot with specified parameters
  # no labels: tl.pos = 'n',
  corrplot(
    p_list[[i]],
    order = 'alphabet',
    type = "lower",
    tl.col = "black",
    method = 'color',
    font = 3,
    cl.pos = 'n',
    tl.pos = lab_pos,
    title = paste(files_names_beauti[[i]]),
    mar = c(0, 0, 1, 0), 
#    addgrid.col = 'white',
    line = -1.25
  )
}

png(
  "00_documentation/figures/correlations/corr_comparison_2019.png",
  units = "in",
  width = 8.5,
  height = 4.5,
  res = 900
)


par(mfrow=c(3,5))
generate_corrplot(1)
generate_corrplot(2)
generate_corrplot(3)
generate_corrplot(4)
generate_corrplot(5)
generate_corrplot(6)
generate_corrplot(7)
generate_corrplot(8)
generate_corrplot(9)
generate_corrplot(10)
generate_corrplot(11)
generate_corrplot(12)
generate_corrplot(13)
generate_corrplot(14)
dev.off()
