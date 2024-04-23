## ---------------------------
##
## Script name: 13_differences_in_values_2019_2020
##
## Purpose of script: Calculating differences of species profiles for 2019 & 2020 and plotting them.
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
library(dplyr)
# 2020
dat_df_2020 <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2020_norm.csv")
band_names <- files_names <- list.files("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled/",
                                        pattern = "*2020*") |> substr(6,8)
dat_df_2020$CLNR <- NULL
dat_df_2020$BANR <- NULL
dat_df_2020 <- dat_df_2020 |> group_by(species) |> summarise(across(everything(), median, na.rm = TRUE))

# 2019
dat_df_2019 <- read.csv2("01_data/10_preprocessed/extract_tsi_trees/trimmed_year_scaled_wide/2019_norm.csv")
dat_df_2019$CLNR <- NULL
dat_df_2019$BANR <- NULL
dat_df_2019 <- dat_df_2019 |> group_by(species) |> summarise(across(everything(), median, na.rm = TRUE))


# differences
dat_diff <- dat_df_2020[,c(2:1023)] - dat_df_2019[,c(2:1023)]
dat_diff$species <- dat_df_2019$species

########################################
#### All bands - facet-wrap
band_long <- dat_diff |> pivot_longer(!c(species), names_to = "name_date", values_to = "band_value")
band_long$band_name <- substr(band_long$name_date,1,3) 
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

band_long$band_name <- stringr::str_replace_all(band_long$band_name, name_mapping)
band_long$date <- as.Date(substr(band_long$name_date, 5, 14), format = "%Y.%m.%d")



Sys.setlocale(category = "LC_TIME", locale="eng")
p <- ggplot(band_long, aes(x = date, y = band_value, group = species, col = species)) + 
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
  ylab("differences between 2020 and 2019 [of normalized values]") +
  xlab("Year") + scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14")))
p + facet_wrap(vars(band_name), nrow = 3)+ 
  theme(strip.background = element_rect(colour = "black", fill = "white"), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 12),
        axis.text.x = element_text(angle = 0))

ggsave(paste0("00_documentation/figures/curve_lines/norm_2019_2020_differences_facet_all_bands",".jpeg"), 
       width= 12,
       height= 6, dpi = 900) 




####################################################################################################
### all bands separately

band_names_uni <- unique(band_long$band_name)
band_long_all <- band_long

for (i in 1: length(unique(band_long$band_name))){
  band_long <- band_long_all[which(band_long_all$band_name == band_names_uni[[i]]),]
  
  ## plot
  ggplot(band_long, aes(x = date, y = band_value, group = species, col = species)) + 
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
    xlab("Year")+
    ylab(paste("differences between 2020 and 2019 [of normalized values]"))+ scale_x_date(date_breaks="2 month", date_labels = "%m", limit=c(as.Date("2020-01-01"),as.Date("2020-12-14"))) +
    theme(strip.background = element_rect(colour = "black", fill = "white"), 
          strip.placement = "outside",
          strip.text = element_text(face = "bold"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(face = "italic"),
          axis.text.x = element_text(angle = 0))
  

  
  
  ggsave(paste0("00_documentation/figures/curve_lines/norm_2019_2020_differences_", band_names_uni[[i]],".jpeg"), 
         width= 8.5,
         height= 4.5, dpi = 900) 
  
}
