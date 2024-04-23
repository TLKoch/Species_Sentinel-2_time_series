## ---------------------------
##
## Script name: 15_variability_tree_species_classification_comparison
##
## Purpose of script: Compare variability plots with selected variables of tree species classification.
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
##        Tree species classification - DOI: 10.16904/envidat.506
##   
##
## ---------------------------
library(tidyr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(dplyr)
library(ggh4x)

# read data
files_intra_mean_sd <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                  full.names = TRUE,
                                  pattern = "norm_2020_intraspecific_mean_sd") 

files_intra_mean_sd <- files_intra_mean_sd[grep("CCI|CRE|EVI|NDM|NDV", files_intra_mean_sd)]
intra_mean_sd <- lapply(files_intra_mean_sd, read.csv2)


files_inter_mean_diff <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                    full.names = TRUE,
                                    pattern = "norm_2020_interspecific_mean_of_sd") 
files_inter_mean_diff <- files_inter_mean_diff[grep("CCI|CRE|EVI|NDM|NDV", files_inter_mean_diff)]
inter_mean_diff <- lapply(files_inter_mean_diff, read.csv2)

names_extended <- c("CCI", "CIre", "EVI", "NDMI", "NDVI")


################################
species <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year/2020_EVI.csv")  

for (i in 1: length(inter_mean_diff)){
  band_long <- inter_mean_diff[[i]]
  band_long$intra_mean_sd <- as.numeric(intra_mean_sd[[i]][1,])
  
  band_long$mean_sd.Date <- NULL
  colnames(band_long) <- c("Date", "Interspecific_mean_med_diff", "Interspecific_mean_sd_diff","Intraspecific")
  band_long$Interspecific_mean_med_diff <- as.numeric(band_long$Interspecific_mean_med_diff)
  band_long$Interspecific_mean_sd_diff <- as.numeric(band_long$Interspecific_mean_sd_diff)
  
  band_long$Date <-  as.Date(substr(band_long$Date,5,20), format = "%Y.%m.%d")
  band_long$band_name <- names_extended[[i]]
  
  
  if (i == 1){
    df_all <- band_long
  }else{
    df_all <- rbind(df_all, band_long)
  }
}


############
# All species
############
p <- ggplot(df_all, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  # stat_difference() from ggh4x package applies the conditional fill
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  geom_vline(data=filter(df_all, band_name == "EVI"), aes(xintercept= as.Date("2020-05-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-03-12")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-04-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-07-05")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-05-11")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-08-14")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-24")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-12-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-04-21")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-11-17")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-03-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-11-02")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-06-10")), linewidth = 1.2)+
  ylab("all species")+
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))



p + facet_wrap(vars(band_name), nrow = 1)


#############################
### Deciduous 
#############################

# read data
files_intra_mean_sd <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                  full.names = TRUE,
                                  pattern = "norm_2020_deciduous_intraspecific_mean_sd")
files_intra_mean_sd <- files_intra_mean_sd[grep("CCI|CRE|EVI|NDM|NDV", files_intra_mean_sd)]
intra_mean_sd <- lapply(files_intra_mean_sd, read.csv2)


files_inter_mean_diff <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                    full.names = TRUE,
                                    pattern = "norm_2020_deciduous_interspecific_mean_of_sd")
files_inter_mean_diff <- files_inter_mean_diff[grep("CCI|CRE|EVI|NDM|NDV", files_inter_mean_diff)]
inter_mean_diff <- lapply(files_inter_mean_diff, read.csv2)

names_extended <- c("CCI", "CIre", "EVI", "NDMI", "NDVI")


######### Facet grid
for (i in 1: length(inter_mean_diff)){
  band_long <- inter_mean_diff[[i]]
  band_long$intra_mean_sd <- as.numeric(intra_mean_sd[[i]][1,])
  
  band_long$mean_sd.Date <- NULL
  colnames(band_long) <- c("Date", "Interspecific_mean_med_diff", "Interspecific_mean_sd_diff","Intraspecific")
  band_long$Interspecific_mean_med_diff <- as.numeric(band_long$Interspecific_mean_med_diff)
  band_long$Interspecific_mean_sd_diff <- as.numeric(band_long$Interspecific_mean_sd_diff)
  
  band_long$Date <-  as.Date(substr(band_long$Date,5,20), format = "%Y.%m.%d")
  band_long$band_name <- names_extended[[i]]
  
  
  if (i == 1){
    df_all <- band_long
  }else{
    df_all <- rbind(df_all, band_long)
  }
}


p_deciduous <-ggplot(df_all, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  # stat_difference() from ggh4x package applies the conditional fill
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  geom_vline(data=filter(df_all, band_name == "EVI"), aes(xintercept= as.Date("2020-05-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-03-12")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-04-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-07-05")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-05-11")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-08-14")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-24")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-12-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-04-21")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-11-17")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-03-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-11-02")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-06-10")), linewidth = 1.2)+
  ylab("deciduous")+
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))



p_deciduous + facet_wrap(vars(band_name), nrow = 1)


#############################
### Evergreen 
#############################
# read data
files_intra_mean_sd <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                  full.names = TRUE,
                                  pattern = "norm_2020_evergreen_intraspecific_mean_sd") 
files_intra_mean_sd <- files_intra_mean_sd[grep("CCI|CRE|EVI|NDM|NDV", files_intra_mean_sd)]
intra_mean_sd <- lapply(files_intra_mean_sd, read.csv2)


files_inter_mean_diff <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                    full.names = TRUE,
                                    pattern = "norm_2020_evergreen_interspecific_mean_of_sd") 
files_inter_mean_diff <- files_inter_mean_diff[grep("CCI|CRE|EVI|NDM|NDV", files_inter_mean_diff)]
inter_mean_diff <- lapply(files_inter_mean_diff, read.csv2)

names_extended <- c("CCI", "CIre", "EVI", "NDMI", "NDVI")


######### Facet grid
for (i in 1: length(inter_mean_diff)){
  band_long <- inter_mean_diff[[i]]
  band_long$intra_mean_sd <- as.numeric(intra_mean_sd[[i]][1,])
  
  band_long$mean_sd.Date <- NULL
  colnames(band_long) <- c("Date", "Interspecific_mean_med_diff", "Interspecific_mean_sd_diff","Intraspecific")
  band_long$Interspecific_mean_med_diff <- as.numeric(band_long$Interspecific_mean_med_diff)
  band_long$Interspecific_mean_sd_diff <- as.numeric(band_long$Interspecific_mean_sd_diff)
  
  band_long$Date <-  as.Date(substr(band_long$Date,5,20), format = "%Y.%m.%d")
  band_long$band_name <- names_extended[[i]]
  
  
  if (i == 1){
    df_all <- band_long
  }else{
    df_all <- rbind(df_all, band_long)
  }
}


p_evergreen <-ggplot(df_all, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  # stat_difference() from ggh4x package applies the conditional fill
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  geom_vline(data=filter(df_all, band_name == "EVI"), aes(xintercept= as.Date("2020-05-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-03-12")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-04-16")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-07-05")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-05-11")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-08-14")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-24")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-12-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-04-21")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-11-17")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "NDMI"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-03-22")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-08-09")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CCI"), aes(xintercept= as.Date("2020-11-02")), linewidth = 1.2)+
  geom_vline(data=filter(df_all, band_name == "CIre"), aes(xintercept= as.Date("2020-06-10")), linewidth = 1.2)+
  ylab("evergreen")+
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))



p_evergreen + facet_wrap(vars(band_name), nrow = 1)

ggsave("00_documentation/figures/tree_species_classification/legend.jpeg",
       width= 12,
       height= 6, dpi = 900)


####################################################################
######## All three
all <- p + facet_wrap(vars(band_name), nrow = 1) + 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.position="none",
        axis.text.x = element_text(angle = 0))


all_deciduous <- p_deciduous + facet_wrap(vars(band_name), nrow = 1)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.position="none",
        axis.text.x = element_text(angle = 0))


all_evergreen <- p_evergreen + facet_wrap(vars(band_name), nrow = 1)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.position="none",
        axis.text.x = element_text(angle = 0))


plot_grid(all, all_deciduous, all_evergreen, nrow = 3)

ggsave("00_documentation/figures/tree_species_classification/comparison_selected_vars_variability.jpeg",
       width= 12,
       height= 6, dpi = 900)
