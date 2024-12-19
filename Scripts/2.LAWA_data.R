# 2. LAWA DATA
# Explanation of this script----------------------------------------------------

# This script focuses on the waterquality trents in all lakes in NZ from the LAWA dataset

# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("sf", "lubridate","readr", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data --------------------------------------------------------------




# lawa_monitoring_data ---------------------------------------------------------
lawa_monitoring_data$DateImported <- dmy(lawa_monitoring_data$DateImported)
lawa_monitoring_data$SampleDateTime <- dmy_hm(lawa_monitoring_data$SampleDateTime)
lawa_monitoring_data <- lawa_monitoring_data %>%
  mutate(SampleDateTime = as.Date(SampleDateTime, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(SampleDateTime, "%Y"),
         Month = format(SampleDateTime, "%m"))


# Remove rows with missing values in Value columns
lawa_monitoring_data <- lawa_monitoring_data %>% filter(!is.na(Value))
lawa_monitoring_data <- lawa_monitoring_data %>% filter(!is.na(SampleDateTime))

# make histograms for all the indicators
indicators <- unique(lawa_monitoring_data$Indicator)
SiteID <- unique(lawa_monitoring_data$SiteID)
unique_locations <- nrow(unique(lawa_monitoring_data[, c("Latitude", "Longitude")]))

# Create histograms for all indicators
par(mfrow = c(3, 3))  
for (indicator in indicators) {
  subset_data <- lawa_monitoring_data %>% filter(Indicator == indicator)
  hist(subset_data$Value, main = indicator)
}

# Plot indicator values over time for all indicators
ggplot(lawa_monitoring_data, aes(SampleDateTime, Value, col=Region)) +
  #geom_line() +
  geom_point() +
  facet_wrap(~ Indicator, scales = "free") 

# Calculate mean and plot ------------------------------------------------------
# Calculate the mean value for each indicator for each year
ggplot(lawa_monitoring_data %>% 
         group_by(Year, Indicator) %>% 
         summarize(mean_value = mean(as.numeric(Value), na.rm = F)), aes(as.numeric(Year), mean_value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Indicator, scales = "free")












# Make a map -------------------------------------------------------------------
# Convert to sf object
lawa_sf <- st_as_sf(lawa_monitoring_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Import the coastline
NZ_Coastline <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Coastline/Coastline_line_2.gpkg")
NZ_Coastline <- NZ_Coastline %>% select(geom)

# Create the map
ggplot() +
  geom_sf(data = NZ_Coastline)+
  geom_sf(data = lawa_sf, col="red") +
  theme_bw() 
