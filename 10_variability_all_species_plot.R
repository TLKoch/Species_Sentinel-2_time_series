## ---------------------------
##
## Script name: 10_variability_all_species_plot
##
## Purpose of script: Plot standard deviations. 
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
library(ggh4x)
library(ggplot2)
# read data
files_intra_mean_sd <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                  full.names = TRUE,
                                  pattern = "norm_2020_intraspecific_mean_sd") 
intra_mean_sd <- lapply(files_intra_mean_sd, read.csv2)


files_inter_mean_diff <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                    full.names = TRUE,
                                    pattern = "norm_2020_interspecific_mean_of_sd") 
inter_mean_diff <- lapply(files_inter_mean_diff, read.csv2)

names_extended <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                    "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                    "SWIR-1", "SWIR-2")

##############################
#### Only EVI for demonstration
all_df <- inter_mean_diff[[4]]
all_df$intra_mean_sd <- as.numeric(intra_mean_sd[[4]][1,])
all_df$mean_sd.Date <- NULL
colnames(all_df) <- c("Date", "Interspecific_mean_med_diff", "Interspecific_mean_sd_diff","Intraspecific")
all_df$Date <-  as.Date(substr(all_df$Date,5,20), format = "%Y.%m.%d")
all_df$Interspecific_mean_med_diff <- as.numeric(all_df$Interspecific_mean_med_diff)
all_df$Interspecific_mean_sd_diff <- as.numeric(all_df$Interspecific_mean_sd_diff)

ggplot(all_df, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  ylab("mean of standard deviations")+
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        #legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))

ggsave(paste0("00_documentation/figures/differences/", "norm_2020_mean_sd_differences_EVI",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 


#####################


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

# different order for facet_wrap
df_all <- df_all |>
  mutate(across(band_name, ~factor(., levels=c("CCI","CIre","EVI", "NDMI", "NDVI",
                                               "BLUE", "GREEN", "RED", "RED-EDGE-1", "RED-EDGE-2",
                                               "RED-EDGE-3", "NIR", "SWIR-1", "SWIR-2"))))

p <- ggplot(df_all, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  ylab("mean of standard deviations [from normalized values]")+
  xlab("2020") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))



p + facet_wrap(vars(band_name), nrow = 3)


ggsave(paste0("00_documentation/figures/differences/norm_2020_mean_sd_differences_facet_all_bands",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 



####################
## 2019
####################
# read data
files_intra_mean_sd <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                  full.names = TRUE,
                                  pattern = "norm_2019_intraspecific_mean_sd") 
intra_mean_sd <- lapply(files_intra_mean_sd, read.csv2)


files_inter_mean_diff <- list.files("01_data/10_preprocessed/time_points_separability_evaluation/",
                                    full.names = TRUE,
                                    pattern = "norm_2019_interspecific_mean_of_sd") 
inter_mean_diff <- lapply(files_inter_mean_diff, read.csv2)

names_extended <- c("BLUE", "CCI", "CIre", "EVI", "GREEN", "NDMI", "NDVI",
                    "NIR", "RED-EDGE-1", "RED-EDGE-2", "RED-EDGE-3", "RED",
                    "SWIR-1", "SWIR-2")


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

# different order for facet_wrap
df_all <- df_all |>
  mutate(across(band_name, ~factor(., levels=c("CCI","CIre","EVI", "NDMI", "NDVI",
                                               "BLUE", "GREEN", "RED", "RED-EDGE-1", "RED-EDGE-2",
                                               "RED-EDGE-3", "NIR", "SWIR-1", "SWIR-2"))))


p <- ggplot(df_all, aes(x = Date)) +
  geom_line(aes(y = Interspecific_mean_sd_diff, color = "interspecific variability"), linewidth = 1.2) +
  geom_line(aes(y = Intraspecific, color = "intraspecific variability"), linewidth = 1.2) +
  stat_difference(aes(ymin = Interspecific_mean_sd_diff, ymax = Intraspecific), alpha = 0.3) +
  scale_color_manual(values = c("seagreen", "sandybrown")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("sandybrown"), 
      colorspace::lighten("seagreen"), 
      "grey60"
    ),
    guide = "none")+
  ylab("mean of standard deviations [from normalized values]")+
  xlab("2019") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2019-01-01"),as.Date("2019-12-14")))+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0))

p + facet_wrap(vars(band_name), nrow = 3)


ggsave(paste0("00_documentation/figures/differences/norm_2019_mean_sd_differences_facet_all_bands",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 

