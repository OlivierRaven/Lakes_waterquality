# Lake Rotoiti
# Explanation of this script------------------------------------------------

# This script focuses on the waterquality trents in Lake Rotoiti


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD")

# Define the list of packages
packages <- c("readxl","openair","viridis","rLakeAnalyzer", "gganimate","sf", "lubridate","readr", "tidyverse", "dplyr", "ggplot2", "magick")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data --------------------------------------------------------------
Meteorology <- read.csv("C:/1.PhD stuff/AEM3D/Rotoiti_wall_125/run/infiles/Meteorology_sans_cloud.dat", 
                        header = TRUE, skip = 14, sep = "", stringsAsFactors = FALSE, 
                        colClasses = c("character", rep("numeric", 7)))

# Split the TIME column into date and fractional day
Meteorology$DATE <- as.Date(as.character(floor(as.numeric(Meteorology$TIME))), format = "%Y%j")
Meteorology$TIME_OF_DAY <- as.numeric(Meteorology$TIME) %% 1 * 24

# import water table
lake_height_Rotoiti <- read_excel("~/PhD/Data/Lakes/Rotoiti/2024-08-28 Daily lake height Rotoiti.xlsx") %>%
  select(1, 2)

# Plot the Data ----------------------------------------------------------------
head(Meteorology)

plot(Meteorology$WIND_SPEED,Meteorology$WIND_DIR)

# plot the AIR temperature data
# Fit the linear model
linear_model <- lm(AIR_TEMP ~ DATE, data = Meteorology)
summary(linear_model)

# Create the plot with the formula annotation
ggplot(Meteorology, aes(DATE, AIR_TEMP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(
    x = Inf, y = Inf, 
    label = sprintf("y = %.2f + %.4f * x", coef(linear_model)[1], coef(linear_model)[2]),
    hjust = 1.1, vjust = 2, size = 5)

# Plot the WIND data
windRose(Meteorology, ws = "WIND_SPEED", wd = "WIND_DIR", 
         angle = 30, key.position = "right",
         paddle = FALSE, main = "Wind Rose Lake Rotoiti")


# Plot lake height
ggplot(lake_height_Rotoiti, aes(Date, `Lake height at 0000 hrs`)) +
  geom_line() 
