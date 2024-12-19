# LERNZmp script
# Explanation of this script------------------------------------------------

# This script focuses on the objects from the Lake Ecosystem Research New Zealand Model Platform (LERNZmp)


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("tmap","pak","devtools","readr","readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

devtools::install_github("limnotrack/AEME")
pak::pak("limnotrack/AEME")
pak::pak("limnotrack/aemetools")
pak::pak("limnotrack/bathytools")


# AEME -------------------------------------------------------------------------
library(AEME)
## basic example code
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("dy_cd", "glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, 
                 path = path, parallel = TRUE)


class(aeme)
aeme

p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")
p1

p2 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                  facet = FALSE)
p2


# AEME Inputs ------------------------------------------------------------------
utils::data("aeme_parameters")
aeme_parameters

parameters(aeme) <- aeme_parameters



# Getting started --------------------------------------------------------------
library(AEME)

# Define the location of the lake
lat <- -36.8898
lon <- 174.46898

# View the location of the lake in a map
library(leaflet)
leaflet() |> 
  addTiles() |> 
  addMarkers(lng = lon, lat = lat, popup = "Wainamu Lake")

# Set depth & area
depth <- 13.07 # Depth of the lake in metres
area <- 152343 # Area of the lake in m2

aemetools::create_linz_key()
# Add the LINZ API key to your .Renviron file
aemetools::add_linz_key(key = "your_key_here")


# Get the elevation of the lake
key <- Sys.getenv("LINZ_KEY")
elevation <- aemetools::get_dem_value(lat = lat, lon = lon, key = key)
elevation # in metres above sea level

elevation

# Define lake list
lake = list(
  name = "Wainamu",
  id = 45819,
  latitude = lat,
  longitude = lon,
  elevation = elevation,
  depth = depth,
  area = area
)


# Define start and stop times
start <- "2020-08-01 00:00:00"
stop <- "2021-06-30 00:00:00"

time <- list(
  start = start,
  stop = stop
)

# Get ERA5 meteorological data
met <- aemetools::get_era5_point(lat = lat, lon = lon, years = 2020:2021)

# Summary of meteorological data
summary(met)

# Set Kw
Kw <- 1.31 # Light extinction coefficient in m-1

# Generate a simple hypsograph
hypsograph_simple <- data.frame(area = c(area, 0), 
                                elev = c(elevation, elevation - depth),
                                depth = c(0, -depth))
hypsograph_simple

# Plot the hypsograph
library(ggplot2)

ggplot(hypsograph_simple, aes(x = area, y = elev)) +
  geom_line() +
  geom_point() +
  xlab("Area (m2)") +
  ylab("Elevation (m)") +
  theme_bw()


# Generate a hypsograph
hypsograph <- generate_hypsograph(max_depth = depth, surface_area = area,
                                  volume_development = 1.4, elev = elevation,
                                  ext_elev = 1)

ggplot(hypsograph, aes(x = area, y = elev)) +
  geom_line() +
  geom_point() +
  geom_line(data = hypsograph_simple, aes(x = area, y = elev), linetype = "dashed") +
  xlab("Area (m2)") +
  ylab("Elevation (m)") +
  theme_bw()

# Define input list
input = list(
  init_depth = depth,
  hypsograph = hypsograph,
  meteo = met,
  use_lw = TRUE,
  Kw = Kw
)

# Construct AEME object
aeme <- aeme_constructor(lake = lake, 
                         time = time,
                         input = input)
aeme


# Introduction to AEME ---------------------------------------------------------
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
slotNames(aeme)

# Load lake data
lke <- lake(aeme)
# Print lake data to console
print(lke)

# Change lake name
lke[["name"]] <- "AEME"

# reassign lake data to aeme object
lake(aeme) <- lke

aeme


plot(aeme, "lake")

plot(aeme, "input")


model_controls <- get_model_controls()
inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
model <- c("dy_cd", "glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls, inf_factor = inf_factor,
                   ext_elev = 5, use_bgc = TRUE)
aeme

cfg <- configuration(aeme)

# Using LERNZmp with AEME ------------------------------------------------------
metadata <- read_csv("Data_raw/LERNZmp/lernzmp_okaro/LERNZmp_lake_metadata.csv")


metadata <- metadata |> 
  dplyr::filter(aeme_file %in% c("LID11133", "LID54730", "LID40188","LID40102", "LID15325", "LID14290"))
metadata

aeme <- readRDS("Data_raw/LERNZmp/lernzmp_rotorua/LID11133.rds")
class(aeme)

aeme

str(aeme@lake)

list.files("aeme", recursive = TRUE)


aeme <- run_aeme(aeme = aeme, model = model, path = path, parallel = TRUE)
aeme

plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")





# Define lake IDs
lake_ids <- c("LID11133", "LID54730","LID54731", "LID40188", "LID40102", "LID15325", "LID14290")

# Initialize a list for basic bathymetric summaries
lake_summary <- list()

# Loop through each lake ID and extract data
for (lake_id in lake_ids) {
  # Construct file path
  file_path <- paste0("Data_raw/LERNZmp/Lakes_combined/", lake_id, ".rds")
  
  # Read the AEME object
  aeme <- readRDS(file_path)
  
  # Extract lake metadata: Depth and Area
  lake_info <- data.frame(
    Lake = aeme@lake$name,
    Depth = aeme@lake$depth,    # Maximum depth
    Area = aeme@lake$area / 1e6 # Convert area to km²
  )
  
  # Append to the summary list
  lake_summary[[lake_id]] <- lake_info
}

# Combine all lakes' data into a single data frame
bathymetry_summary <- do.call(rbind, lake_summary)

# Print the summary table
print(bathymetry_summary)




# Merge Bathymetry with DEM data -----------------------------------------------

shoreline <- readRDS(system.file("extdata/rotoma_shoreline.rds",
                                 package = "bathytools"))
catchment <- readRDS(system.file("extdata/rotoma_catchment.rds",
                                 package = "bathytools"))

tmap_mode("view")
tmap_options(basemaps = "Esri.WorldImagery")

tm_shape(shoreline) +
  tm_borders(col = "#8DA0CB", lwd = 2) +
  tm_shape(catchment) +
  tm_borders(col = "#E78AC3", lwd = 2) 

point_data <- readRDS(system.file("extdata/depth_points.rds",
                                  package = "bathytools"))
head(point_data)

dem_raster <- terra::rast(system.file("extdata/dem_32m.tif",
                                      package = "bathytools"))
tm_shape(dem_raster) +
  tm_raster(alpha = 0.5, style = "cont", palette = "-YlGnBu") +
  tm_shape(shoreline) +
  tm_borders(col = "#FC8D62", lwd = 2) +
  tm_shape(catchment) +
  tm_borders(col = "#A6D854", lwd = 2) 

bathy_raster <- bathytools::rasterise_bathy(shoreline = shoreline,
                                point_data = point_data, crs = 2193,
                                res = 8)

tm_shape(dem_raster) +
  tm_raster(alpha = 0.5, style = "cont", palette = "-YlGnBu") +
  tm_shape(bathy_raster) +
  tm_raster(style = "cont", palette = "-viridis", breaks = seq(-90, 0, by = 10)) +
  tm_shape(shoreline) +
  tm_borders(col = "#FC8D62", lwd = 2) +
  tm_shape(catchment) +
  tm_borders(col = "#A6D854", lwd = 2) 


dem_bath <- bathytools::merge_bathy_dem(shoreline = shoreline, bathy_raster = bathy_raster,
                            dem_raster = dem_raster, catchment = catchment)
dem_bath


# do extra shit with it
ggplot(point_data, aes(lon, lat, col=depth))+
  geom_point()


depth_contours_Rotoma <- readRDS(system.file("extdata/depth_contours.rds",
                                  package = "bathytools"))


ggplot(data = depth_contours_Rotoma) +
  geom_sf(aes(color = as.factor(depth)), size = 1) + # Use depth for color
  scale_color_viridis_d() +  # Apply a color palette
  labs(title = "Depth Contours of Rotoma",
       color = "Depth (m)") +
  theme_minimal() +
  theme(legend.position = "right")




