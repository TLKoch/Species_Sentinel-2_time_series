## ---------------------------
##
## Script name: 14_values_2019_2020_boxplots
##
## Purpose of script: Depicting the species profiles values for 2019 & 2020 in violin plots separating inside and outside of the growing season.
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
# AS VIOLIN PLOT
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
####################
## Not median values
####################
# 2020
dat_df_2020 <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2020_norm.csv")
band_names <- files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                                        pattern = "*2020*") |> substr(6,8)
dat_df_2020$CLNR <- NULL
dat_df_2020$BANR <- NULL

# 2019
dat_df_2019 <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2019_norm.csv")
dat_df_2019$CLNR <- NULL
dat_df_2019$BANR <- NULL

name_mapping <- c(
  "BLU" = "BLUE",
  "CCI" = "CCI",
  "CRE" = "CIre",
  "EVI" = "EVI",
  "GRN" = "GREEN",
  "NDM" = "NDMI",
  "NDV" = "NDVI",
  "NIR" = "NIR",
  "RE1" = "RED-EDGE-1",
  "RE2" = "RED-EDGE-2",
  "RE3" = "RED-EDGE-3",
  "RED" = "RED",
  "SW1" = "SWIR-1",
  "SW2" = "SWIR-2"
)
#########
## Long data frame
band_long_2020 <- dat_df_2020 |> pivot_longer(!c(species), names_to = "name_date", values_to = "band_value")
band_long_2020$band_name <- substr(band_long_2020$name_date,1,3) 
band_long_2020$band_name <- stringr::str_replace_all(band_long_2020$band_name, name_mapping)
band_long_2020$date <- as.Date(substr(band_long_2020$name_date, 5, 14), format = "%Y.%m.%d")
band_long_2020$year <- format(band_long_2020$date, format = "%Y")

band_long_2019 <- dat_df_2019 |> pivot_longer(!c(species), names_to = "name_date", values_to = "band_value")
band_long_2019$band_name <- substr(band_long_2019$name_date,1,3) 
band_long_2019$band_name <- stringr::str_replace_all(band_long_2019$band_name, name_mapping)
band_long_2019$date <- as.Date(substr(band_long_2019$name_date, 5, 14), format = "%Y.%m.%d")
band_long_2019$year <- format(band_long_2019$date, format = "%Y")
#########
band_long_both <- rbind(band_long_2019, band_long_2020)
band_long_both <- band_long_both %>%
  mutate(
    Veg_period = case_when(
      month(date) %in% 4:10 ~ paste("GS", year(date)),
      TRUE ~ paste("Non-GS", year(date))
    )
  )



pv <- ggplot(band_long_both, aes(x = Veg_period, y = band_value, group = Veg_period)) +
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    aes(fill = Veg_period),
    ## custom bandwidth
    adjust = 0.5, 
    ## adjust height
    width = 1.5, 
    ## move geom to the right
    justification = -0.2, 
    ## remove slab interval
    .width = 0
  ) + 
  geom_boxplot(outlier.shape = NA,
    width = 0.35,
    position = position_dodge(width = 0.8)  # Dodge the boxplots
  ) +
  scale_fill_manual(values = c("Non-GS 2019" = "#e7d4e8",
                               "Non-GS 2020" = "#af8dc3",
                               "GS 2019" = "#7fbf7b",
                               "GS 2020" = "#1b7837")) +  
  coord_cartesian(xlim = c(0.8, NA)) +  # Remove the right limit
  ggpubr::theme_pubclean() +
  labs(x = NULL) +  # Remove x-axis label
  labs(fill = "Timing") +  # Change legend title
  theme(
    legend.position = "bottom",  # Change the legend position
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.y = element_line(colour = "grey")
  ) + 
  ylab("")

pv + facet_wrap(vars(band_name), nrow = 3) + 
  theme(
    strip.background = element_rect(colour = "black", fill = "white"), 
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(face = "italic", size = 12),
    axis.text.x = element_blank(),
    legend.position = c(1, 0), 
    legend.justification = c(1, 0)# Remove x-axis labels
  )

ggsave(paste0("00_documentation/figures/curve_lines/norm_2019_2020_differences_facet_all_bands_violin_not_median_per_species_outliers_not_displayed",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 


################
#### For legend
pv <- ggplot(band_long_both, aes(x = Veg_period, y = band_value, group = Veg_period)) +
  ggdist::stat_halfeye(
    aes(fill = Veg_period),
    adjust = 0.5, 
    width = 1.5, 
    justification = -0.2, 
    .width = 0
  ) + 
  geom_boxplot(
    width = 0.35,
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = c("Non-GS 2019" = "#e7d4e8",
                               "Non-GS 2020" = "#af8dc3",
                               "GS 2019" = "#7fbf7b",
                               "GS 2020" = "#1b7837"),
                    name = "Timing") +
  coord_cartesian(xlim = c(0.8, NA)) +
  ggpubr::theme_pubclean() +
  labs(x = NULL) +
  theme(
    legend.position = "bottom",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey")
  ) + 
  ylab("")
pv + facet_wrap(vars(band_name), nrow = 3, scales = "free_x", strip.position = "bottom") +  # Allow free x-axis scales
  theme(
    strip.background = element_rect(colour = "black", fill = "white"), 
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 12),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(face = "italic", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust angle and justification of x-axis labels
    legend.position = c(1, 0), 
    legend.justification = c(1, 0)
  ) 
ggsave(paste0("00_documentation/revision/figures/curve_lines/norm_2019_2020_differences_facet_all_bands_violin_legend",".jpeg"), 
       width= 12,
       height= 6, dpi = 900)
