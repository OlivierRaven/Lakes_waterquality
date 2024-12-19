# Lake bathemetry script
# Explanation of this script------------------------------------------------

# This script continues form the LERNZmp one to make more bathemotry for all the lakes.


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("raster","sf","tmap","pak","readr","readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

pak::pak("limnotrack/AEME")
pak::pak("limnotrack/bathytools")


# Import the data --------------------------------------------------------------
All_sample_sites <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx", 
                               sheet = "Monitoring_Sites")
# Create an sf object for All_sample_sites
All_sample_sites_sf <- st_as_sf(All_sample_sites, coords = c("Lon", "Lat"), crs = 4326)  # Assuming WGS84 for lat/lon coords
All_sample_sites_sf <- st_transform(All_sample_sites_sf, crs = 2193)  # Reproject to NZGD2000

# Filter sites for each lake
Sites_Rotorua <- All_sample_sites_sf %>% filter(Lake == "Lake Rotorua")
Sites_Rotoiti <- All_sample_sites_sf %>% filter(Lake == "Lake Rotoiti")
Sites_Rotoehu <- All_sample_sites_sf %>% filter(Lake == "Lake Rotoehu")
Sites_Rotomā  <- All_sample_sites_sf %>% filter(Lake == "Lake Rotomā")
Sites_Ōkāreka <- All_sample_sites_sf %>% filter(Lake == "Lake Ōkāreka")

### Lake perimeter:
Rotorua <- st_read("Data_raw/Lakes_perimeter/Lake Rotorua.gpkg")
Rotoiti <- st_read("Data_raw/Lakes_perimeter/Lake Rotoiti.gpkg")
Rotoehu <- st_read("Data_raw/Lakes_perimeter/Lake Rotoehu.gpkg")
Rotomā  <- st_read("Data_raw/Lakes_perimeter/Lake Rotomā.gpkg")
Ōkāreka <- st_read("Data_raw/Lakes_perimeter/Lake Ōkāreka.gpkg")

# Extract boundaries
Rotorua_outline <- st_boundary(Rotorua)
Rotoiti_outline <- st_boundary(Rotoiti)
Rotoehu_outline <- st_boundary(Rotoehu)
Rotomā_outline  <- st_boundary(Rotomā)
Ōkāreka_outline <- st_boundary(Ōkāreka)

Rotorua_outline <- st_transform(Rotorua_outline, crs = 2193)
Rotoiti_outline <- st_transform(Rotoiti_outline, crs = 2193)
Rotoehu_outline <- st_transform(Rotoehu_outline, crs = 2193)
Rotomā_outline  <- st_transform(Rotomā_outline, crs = 2193)
Ōkāreka_outline <- st_transform(Ōkāreka_outline, crs = 2193)


### Lake Bathmetry -------------------------------------------------------------
# all lakes
Rotlakes_bathymetry <- read_excel("Data_raw/Lake_bathmetry/Rotlakes_bathymetry.xls")

# Rotorua ----------------------------------------------------------------------
# Rotoiti ----------------------------------------------------------------------
Bathymetry_Rotoiti <- read_excel("Data_raw/Lake_bathmetry/Bathymetry_Rotoiti.xlsx")

max_elevation <- max(Bathymetry_Rotoiti$Elevation, na.rm = TRUE)
Bathymetry_Rotoiti <- Bathymetry_Rotoiti %>% 
  select(-Source) %>% 
  mutate(depth = (max_elevation - Elevation)* -1)%>%
  rename(
    lat = Northing, 
    lon = Easting)

Bathymetry_Rotoiti_sf <- st_as_sf(Bathymetry_Rotoiti, coords = c("lon", "lat"), crs = 27200) # 27200 is the EPSG code for NZMG (New Zealand Map Grid)
Bathymetry_Rotoiti_sf <- st_transform(Bathymetry_Rotoiti_sf, crs = 4326)

ggplot() +
  geom_sf(data = Bathymetry_Rotoiti_sf, aes(color = depth), size = 0.5) +  # Plot bathymetry points
  geom_sf(data = Rotoiti_outline, size = 1, fill = NA) +  # Plot shoreline
  scale_color_viridis_c(option = "C")

# use the  bathytools::rasterise_bathy but its not working to make the bat map
bathy_Rotoiti <- bathytools::rasterise_bathy(shoreline = Rotoiti_outline, point_data = Bathymetry_Rotoiti_sf, crs = 2193,res = 8)


# use the already made Bathmetry interpolated in QGIS grid size 25m

# Step 1a: Create a normal data frame
Rotoiti_bath_25_df <- read_csv("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raster25.csv") %>%
  pivot_longer(cols = -Y, names_to = "X", values_to = "Z") %>%
  mutate(Z = round(Z), X = as.numeric(X), Y = as.numeric(Y)) %>%  # Ensuring X, Y, and Z are numeric
  filter(!is.na(Z))  # Remove rows with NA values

# Step 1b: Create an sf object
Rotoiti_bath_25_sf <- Rotoiti_bath_25_df %>%
  st_as_sf(coords = c("X", "Y"), crs = 27200) %>%  # Convert to sf object with X and Y as coordinates
  st_transform(crs = 4326) %>%  # Transform to WGS84 (EPSG:4326)
  mutate(Elevation = Z)  # Add Elevation column

# Step 2: Plot the bathymetry data (sf object)
ggplot(Rotoiti_bath_25_sf) +
  geom_sf(aes(color = Elevation)) +
  scale_color_viridis_c() +
  labs(title = "Rotoiti Bathymetry", color = "Elevation")

# Step 3: Create raster from bathymetry data (XYZ values)
xyz_data <- as.data.frame(Rotoiti_bath_25_df) 

xyz_data_clean <- xyz_data %>%
  filter(!is.na(Z)) %>%
  mutate(Elevation = as.numeric(Z))

# Create the raster from the XYZ data (using the data frame directly)
raster_bath <- rasterFromXYZ(xyz_data_clean[, c("X", "Y", "Elevation")])

# Step 4: Generate contours at desired elevation levels (e.g., every 10 meters)
depth_contours_Rotoiti <- rasterToContour(raster_bath, levels = seq(min(values(raster_bath), na.rm = TRUE), 
                                                                    max(values(raster_bath), na.rm = TRUE), 
                                                                    by = 1))
# Convert the SpatialLinesDataFrame to an sf object
depth_contours_Rotoiti_sf <- st_as_sf(depth_contours_Rotoiti)

# Step 5: Plot the contours using ggplot
ggplot() +
  geom_sf(data = depth_contours_Rotoiti_sf, aes(color = factor(level)), size = 1) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Contour Lines at 10m Intervals", color = "Contour Level")

depth_contours_Rotoiti_sf <- st_set_crs(depth_contours_Rotoiti_sf, 27200) 


# Step 2: Filter contours that are 20 meters below the highest point
filtered_contours <- depth_contours_Rotoiti_sf %>%
  filter(level == (259))

depth_contours_points_rotoiti <- st_cast(filtered_contours, "POINT")

ggplot() +
  geom_sf(data=Rotoiti_outline)+
  geom_sf(data = depth_contours_points_rotoiti)+
  geom_sf(data=Sites_Rotoiti, col= "red")

# Ensure the CRS is the same for both datasets (they should be in the same CRS to calculate distances)
depth_contours_points_rotoiti <- st_transform(depth_contours_points_rotoiti, st_crs(Sites_Rotoiti))

# Calculate the shortest distance from each point in Sites_Rotoiti to the depth contour points
Sites_Rotoiti$distance_to_depth_contour <- st_distance(Sites_Rotoiti, depth_contours_points_rotoiti) %>%
  apply(1, min)  # Apply min to get the shortest distance for each point

  
# Rotoehu ----------------------------------------------------------------------

# Rotomā -----------------------------------------------------------------------
depth_contours_Rotoma <- readRDS(system.file("extdata/depth_contours.rds",
                                             package = "bathytools"))


depth_contours_Rotoma_filtered <- depth_contours_Rotoma %>%
  filter(depth == -20)

depth_contours_points <- st_cast(depth_contours_Rotoma_filtered, "POINT")

ggplot() +
  geom_sf(data=Rotomā_outline)+
  geom_sf(data = depth_contours_points)+
  geom_sf(data=Sites_Rotomā, col= "red")

# Ensure the CRS is the same for both datasets (they should be in the same CRS to calculate distances)
depth_contours_points <- st_transform(depth_contours_points, st_crs(Sites_Rotomā))

# Calculate the shortest distance from each point in Sites_Rotomā to the depth contour points
Sites_Rotomā$distance_to_depth_contour <- st_distance(Sites_Rotomā, depth_contours_points) %>%
  apply(1, min)  # Apply min to get the shortest distance for each point

# Inspect the results
head(Sites_Rotomā)



# Ōkāreka ----------------------------------------------------------------------





