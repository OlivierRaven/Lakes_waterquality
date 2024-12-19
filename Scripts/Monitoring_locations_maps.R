# Make maps of the lakes and show the monitoring locations
# Explanation of this script------------------------------------------------


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("ggrepel","readxl","sf", "readr", "tidyverse", "dplyr", "ggplot2", "rLakeAnalyzer")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the Data sets ---------------------------------------------------------
Monitoring_sites <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Monitoring_sites/All_monitoring_sites.gpkg")
lakes_Shape <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/12_lakes.gpkg")

Rotorua_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotorua/DHT_rotorua.gpkg")
Rotoiti_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotoiti/DHT_Rotoiti_Dissolve.gpkg")
Rotoehu_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotoehu/DHT_Rotoehu_Dissolved.gpkg")
Rotomā_DHT  <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotomā/DHT_Rotoma_Dissolved.gpkg")
Ōkāreka_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Ōkāreka/DHT_Okaroka_Dissolve.gpkg")


# Create a list of lakes and corresponding data
lake_data <- list(
  Rotorua = list(
    sites = Monitoring_sites %>% filter(Lake == "Rotorua"),
    lines = lakes_Shape %>% filter(name_ascii == "Lake Rotorua"),
    dht = Rotorua_DHT),
  Rotoiti = list(
    sites = Monitoring_sites %>% filter(Lake == "Rotoiti"),
    lines = lakes_Shape %>% filter(name_ascii == "Lake Rotoiti"),
    dht = Rotoiti_DHT),
  Rotoehu = list(
    sites = Monitoring_sites %>% filter(Lake == "Rotoehu"),
    lines = lakes_Shape %>% filter(name_ascii == "Lake Rotoehu"),
    dht = Rotoehu_DHT),
  Rotomā = list(
    sites = Monitoring_sites %>% filter(Lake == "Rotomā"),
    lines = lakes_Shape %>% filter(name_ascii == "Lake Rotoma"),
    dht = Rotomā_DHT),
  Ōkāreka = list(
    sites = Monitoring_sites %>% filter(Lake == "Ōkāreka"),
    lines = lakes_Shape %>% filter(name_ascii == "Lake Okareka"),
    dht = Ōkāreka_DHT)
  #,Ōkaro = list(
    #sites = Monitoring_sites %>% filter(Lake == "Ōkaro"),
    #lines = lakes_Shape %>% filter(name_ascii == "Lake Okaro")
    # DHT for Ōkaro seems missing, update if needed
    )

custom_colors <- c("Muddy" = "tan", "Sandy" = "orange","Rocky" = "gray", "Raupo" = "green", "AR"= "red",  "Cliff"= "black", "Geo"= "blue") 


# Loop through each lake to create individual plots
for (lake_name in names(lake_data)) {
  
  lake_sites <- lake_data[[lake_name]]$sites
  lake_lines <- lake_data[[lake_name]]$lines
  lake_dht <- lake_data[[lake_name]]$dht
  
  p <- ggplot() +
    #geom_sf(data = lake_lines) +
    geom_sf(data = lake_dht, aes(col = DHT)) +  # Fill DHT areas with custom colors
    scale_color_manual(values = custom_colors) +  # Apply custom colors to DHTgeom_text_repel(data = lake_sites, aes(geometry = geom, label = as.numeric(ID)), stat = "sf_coordinates") +
    geom_sf(data = lake_sites, aes(fill = Study), shape= 21, col="black", size=3) +
    geom_text_repel(data = lake_sites, aes(geometry = geom, label = as.numeric(ID)), stat = "sf_coordinates") +
    ggtitle(paste("Monitoring sites", lake_name)) +
    theme_bw()
  
  # Save or print the plot
  print(p)
  # Optionally, save the plot as a file
  ggsave(filename = paste0("Figures/Lake_", lake_name, "_Map.png"), plot = p, width = 8, height = 6)
}



ggplot()+
  geom_sf(data=Rotorua_line)+
  geom_sf(data=Rotorua_sites,aes(col=Study))+
  geom_point()+
  geom_text_repel(data = Rotorua_sites, aes(geometry = geom, label = as.numeric(ID)), stat = "sf_coordinates") +
  theme_bw()









