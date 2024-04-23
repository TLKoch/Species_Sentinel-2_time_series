## ---------------------------
##
## Script name: 05_figures_species_profiles
##
## Purpose of script: Create figures for the species profiles. Spectral time series for species. 
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
##  plot lines per band/index and species
##
## ---------------------------
library(tidyr)
library(ggplot2)
library(dplyr)

##############################################################
### 2020
##############################################################
scaled_files <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                           full.names = T,
                           pattern = "*2020*")
scaled_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                           full.names = F,
                           pattern = "*2020*")
scaled_names_extended <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                           "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                           "SWIR-1", "SWIR-2")
species <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/2020_EVI.csv")  
scaled_list <- lapply(scaled_files, read.csv2)

####################################################################################################
### all bands separately
for (i in 1: length(scaled_list)){
  band_long <- scaled_list[[i]]
  band_long$date <- as.Date(band_long$date)
  
  band_long <- band_long %>%
    mutate(type = case_when(
      species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi") ~ "deciduous",
      species %in% c("Abies alba", "Picea abies", "Pinus sylvestris") ~ "evergreen",
      TRUE ~ NA_character_  # For any other cases
    ))
  
  ## plot
  ggplot(band_long, aes(x = date, y = band_value_norm, group = species, col = species, linetype = type)) + 
    geom_smooth(se=FALSE)+
    geom_smooth(show.legend=FALSE)+
    scale_color_manual(values = c(
      "#e7298a",
      "#666666",
      "#7570b3",
      "#1b9e77",
      "#d95f02",
      "#e6ab02",
      "#a6761d"
    )) +
    scale_linetype_discrete(name = "type")+
    ggpubr::theme_pubclean() +
    ylab(paste("normalized ", scaled_names_extended[[i]])) +
    theme(strip.background = element_rect(colour = "black", fill = "white"), 
          strip.placement = "outside",
          strip.text = element_text(face = "bold"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(face = "italic"),
          axis.text.x = element_text(angle = 0))+
    guides(linetype = guide_legend(override.aes = list(color = "black")))

  ggsave(paste0("00_documentation/figures/curve_lines/norm_", stringr::str_sub(scaled_names[[i]],1,8),".jpeg"), 
         width= 10,
         height= 4.5, dpi = 900) 
  
}

########################################
#### All bands - facet-wrap
for (i in 1: length(scaled_list)){
  band_long <- scaled_list[[i]]
  band_long$date <- as.Date(band_long$date)
  band_long$band_name <- scaled_names_extended[[i]]
  if (i == 1){
    df_all <- band_long
  }else{
    df_all <- rbind(df_all, band_long)
  }
}


Sys.setlocale(category = "LC_TIME", locale="eng")
p <- ggplot(df_all, aes(x = date, y = band_value_norm, group = species, col = species)) + 
  geom_smooth(se=FALSE)+
  geom_smooth(show.legend=FALSE)+
  scale_color_manual(values = c(
    "#e7298a",
    "#666666",
    "#7570b3",
    "#1b9e77",
    "#d95f02",
    "#e6ab02",
    "#a6761d"
  )) +
  ggpubr::theme_pubclean() +
  ylab("normalized values") +
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))
p + facet_wrap(vars(band_name), nrow = 3)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 12),
        axis.text.x = element_text(angle = 0))

ggsave(paste0("00_documentation/figures/curve_lines/norm_", "2020_facet_all_bands",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 


########################
### Different line types

df_all <- df_all %>%
  mutate(type = case_when(
    species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi") ~ "deciduous",
    species %in% c("Abies alba", "Picea abies", "Pinus sylvestris") ~ "evergreen",
    TRUE ~ NA_character_  # For any other cases
  ))



p <- ggplot(df_all, aes(x = date, y = band_value_norm, group = species, col = species, linetype = type)) + 
  geom_smooth(se=FALSE)+
  geom_smooth(show.legend=FALSE)+
  scale_color_manual(values = c(
    "#e7298a",
    "#666666",
    "#7570b3",
    "#1b9e77",
    "#d95f02",
    "#e6ab02",
    "#a6761d"
  )) +
  scale_linetype_discrete(name = "type")+
  ggpubr::theme_pubclean() +
  ylab("normalized values") +
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  guides(linetype = guide_legend(override.aes = list(color = "black")))
p + facet_wrap(vars(band_name), nrow = 3)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 12),
        axis.text.x = element_text(angle = 0))

ggsave(paste0("00_documentation/figures/curve_lines/norm_", "2020_facet_all_bands_diff_linetypes",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 




##############################################################
### 2019
##############################################################
scaled_files <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                           full.names = T,
                           pattern = "*2019*")
scaled_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                           full.names = F,
                           pattern = "*2019*")
scaled_names_extended <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                           "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                           "SWIR-1", "SWIR-2")
species <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/2019_EVI.csv")  
scaled_list <- lapply(scaled_files, read.csv2)

####################################################################################################
### all bands separately
for (i in 1: length(scaled_list)){
  band_long <- scaled_list[[i]]
  band_long$date <- as.Date(band_long$date)
  
  band_long <- band_long %>%
    mutate(type = case_when(
      species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi") ~ "deciduous",
      species %in% c("Abies alba", "Picea abies", "Pinus sylvestris") ~ "evergreen",
      TRUE ~ NA_character_  # For any other cases
    ))

  ## plot
  ggplot(band_long, aes(x = date, y = band_value_norm, group = species, col = species, linetype = type)) + 
    geom_smooth(se=FALSE)+
    geom_smooth(show.legend=FALSE)+
    scale_color_manual(values = c(
      "#e7298a",
      "#666666",
      "#7570b3",
      "#1b9e77",
      "#d95f02",
      "#e6ab02",
      "#a6761d"
    )) +
    scale_linetype_discrete(name = "type")+
    ggpubr::theme_pubclean() +
    ylab(paste("normalized ", scaled_names_extended[[i]])) +
    theme(strip.background = element_rect(colour = "black", fill = "white"), 
          strip.placement = "outside",
          strip.text = element_text(face = "bold"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(face = "italic"),
          axis.text.x = element_text(angle = 0))+
    guides(linetype = guide_legend(override.aes = list(color = "black")))
  
 
  
  ggsave(paste0("00_documentation/figures/curve_lines/norm_", stringr::str_sub(scaled_names[[i]],1,8),".jpeg"), 
         width= 10,
         height= 4.5, dpi = 900) 
  
}

########################################
#### All bands - facet-wrap
for (i in 1: length(scaled_list)){
  band_long <- scaled_list[[i]]
  band_long$date <- as.Date(band_long$date)
  band_long$band_name <- scaled_names_extended[[i]]
  if (i == 1){
    df_all <- band_long
  }else{
    df_all <- rbind(df_all, band_long)
  }
}


df_all <- df_all %>%
  mutate(type = case_when(
    species %in% c("Castanea sativa", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua, Larix kaempferi") ~ "deciduous",
    species %in% c("Abies alba", "Picea abies", "Pinus sylvestris") ~ "evergreen",
    TRUE ~ NA_character_  # For any other cases
  ))


Sys.setlocale(category = "LC_TIME", locale="eng")
p <- ggplot(df_all, aes(x = date, y = band_value_norm, group = species, col = species, linetype = type)) + 
  geom_smooth(se=FALSE)+
  geom_smooth(show.legend=FALSE)+
  scale_color_manual(values = c(
    "#e7298a",
    "#666666",
    "#7570b3",
    "#1b9e77",
    "#d95f02",
    "#e6ab02",
    "#a6761d"
  )) +
  scale_linetype_discrete(name = "type")+
  ggpubr::theme_pubclean() +
  ylab("normalized values") +
  xlab("2019") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2019-01-01"),as.Date("2019-12-14")))+
  guides(linetype = guide_legend(override.aes = list(color = "black")))
p + facet_wrap(vars(band_name), nrow = 3)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 12),
        axis.text.x = element_text(angle = 0))

ggsave(paste0("00_documentation/figures/curve_lines/norm_", "2019_facet_all_bands_diff_linetypes",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 


