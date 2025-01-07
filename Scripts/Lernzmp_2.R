# Title: Data Analysis LERNZmp =================================================
# Author: Olivier Raven
# Date: 09-12-2024
# Description: Script to process, analyze, and visualize data
# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working directory
setwd("~/Documents/R/NZ_Projects")

# Define the list of packages
packages <- c("AEME", "pak", "readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Install from GitHub
pak::pak("limnotrack/AEME")

# Extract lake data for 6 lakes ------------------------------------------------
# Define the list of lake IDs
lid_files <- c("LID11133", "LID14290", "LID15325", "LID40102", "LID40188", "LID54730")

# Load metadata
metadata <- read.csv("Data_raw/lernzmp/LERNZmp_lake_metadata.csv")

# Filter metadata for selected lake IDs
metadata <- metadata |> dplyr::filter(aeme_file %in% lid_files)

# Load AEME files for each lake ID into a list
aeme_list <- lapply(lid_files, function(lid) {
  readRDS(paste0("Data_raw/lernzmp/", lid, ".rds"))
})

# Model setup and directory path
model <- c("glm_aed", "gotm_wet")  # Models to build
path <- "aeme"  # Directory for model configuration

# Initialize an empty list to store detailed lake data
lake_data_list <- list()

for (i in seq_along(aeme_list)) {
  lid <- lid_files[i]
  aeme <- aeme_list[[i]]
  
  # Build and run the AEME object
  aeme <- build_aeme(aeme = aeme, model = model, path = path,
                     use_aeme = TRUE, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path, parallel = TRUE)
  
  # Extract input data
  inp <- input(aeme)
  
  # Extract hypsograph data
  hyps <- inp$hypsograph
  hyps <- hyps |> dplyr::filter(depth <= 0)
  
  # Extract metadata for the lake
  lake_metadata <- metadata |> dplyr::filter(aeme_file == lid)
  
  # Prepare a list for all extracted data for this lake
  lake_details <- list(
    LID = lid,
    lake_name = lake_metadata$Name,
    region = lake_metadata$Region,
    geomorphic_type = lake_metadata$Geomorphic.type,
    max_depth = lake_metadata$Depth,
    depth_measurement = lake_metadata$Depth.measurement,
    data_quality = lake_metadata$Data,
    init_depth = inp$init_depth,
    light_attenuation_coefficient = inp$Kw,
    use_longwave_radiation = inp$use_lw,
    hypsograph = hyps,  # Detailed hypsograph data
    init_profile = inp$init_profile,  # Initial conditions (depth, temperature, salinity)
    meteo = inp$meteo  # Meteorological data
  )
  
  # Add lake details to the list
  lake_data_list[[lid]] <- lake_details
}

# Combine all extracted data into a single structured object
all_lake_data <- do.call(rbind, lapply(lake_data_list, function(x) {
  data.frame(
    LID = x$LID,
    lake_name = x$lake_name,
    region = x$region,
    geomorphic_type = x$geomorphic_type,
    max_depth = x$max_depth,
    depth_measurement = x$depth_measurement,
    data_quality = x$data_quality,
    init_depth = x$init_depth,
    light_attenuation_coefficient = x$light_attenuation_coefficient,
    use_longwave_radiation = x$use_longwave_radiation,
    hypsograph = I(list(x$hypsograph)),  # Store hypsograph as a nested dataframe
    init_profile = I(list(x$init_profile)),  # Store initial profile as a nested dataframe
    meteo = I(list(x$meteo))  # Store meteorological data as a nested dataframe
  )
}))


# Meteorological data ----------------------------------------------------------
# Initialize an empty data frame to store all meteorological data
meteo_data <- data.frame()

# Loop through each lake and process its meteorological data
for (i in seq_along(lake_data_list)) {
  lake_id <- names(lake_data_list)[i]
  lake_meteo <- lake_data_list[[lake_id]]$meteo
  
  if (!is.null(lake_meteo)) {
    # Dynamically select all variables except "Date"
    variables <- setdiff(names(lake_meteo), "Date")
    
    # Pivot to long format for all variables
    meteo_long <- lake_meteo %>%
      tidyr::pivot_longer(cols = all_of(variables), 
                          names_to = "variable", 
                          values_to = "value")
    
    # Add metadata columns
    meteo_long$ID <- lake_id
    lake_metadata <- metadata %>% dplyr::filter(ID == lake_id)
    #meteo_long$lake_name <- lake_metadata$Name
    #meteo_long$region <- lake_metadata$Region
    
    # Append to the combined data frame
    meteo_data <- rbind(meteo_data, meteo_long)
  }}

# Save the  meteorological data
#write.csv(meteo_data, "Data_mod/meteo_data.csv", row.names = FALSE)

# Read the saved meteorological data for plotting
meteo_data <- read_csv("Data_mod/meteo_data.csv")

# Plot meteorological data
ggplot(meteo_data %>% 
         dplyr::filter(variable %in% c("MET_tmpair")), 
       aes(x = Date, y = value, color = ID)) +
  geom_line() +
  facet_wrap(~ID, scales = "free") +
  theme_bw()


# Hypsograph data --------------------------------------------------------------
# Initialize an empty data frame to store all data 
hypsograph_data <- data.frame()

# Loop through each lake and process
for (i in seq_along(aeme_list)) {
  lid <- lid_files[i]
  aeme <- aeme_list[[i]]
  
  # Build and run the AEME object
  aeme <- build_aeme(aeme = aeme, model = model, path = path,
                     use_aeme = TRUE, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path, parallel = TRUE)
  
  # Extract hypsograph data
  inp <- input(aeme)
  hyps <- inp$hypsograph
  
  # Filter out depths above 0
  hyps <- hyps |> dplyr::filter(depth <= 0)
  
  # Add metadata columns
  lake_metadata <- metadata |> dplyr::filter(aeme_file == lid)
  hyps$LID <- lid
  hyps$lake_name <- lake_metadata$Name
  hyps$region <- lake_metadata$Region
  hyps$geomorphic_type <- lake_metadata$Geomorphic.type
  hyps$max_depth <- lake_metadata$Depth
  hyps$depth_measurement <- lake_metadata$Depth.measurement
  hyps$data_quality <- lake_metadata$Data
  
  # Append to the combined dataframe
  hypsograph_data <- rbind(hypsograph_data, hyps)
}

#hypsograph_data <- hypsograph_data %>%
mutate(area_ha = area / 10000) %>%
  group_by(lake_name) %>%
  summarize(
    x_null = ifelse(any(depth == 0), area_ha[which.min(abs(depth))], NA),
    y_null = ifelse(any(area_ha == 0), depth[which.min(abs(area_ha))], NA),
    .groups = "drop") %>% 
  right_join(hypsograph_data %>% 
               mutate(area_ha = area / 10000), by = "lake_name")

# Save the combined hypsograph data
write.csv(hypsograph_data, "Data_mod/hypsograph_data.csv", row.names = FALSE)

hypsograph_data <- read_csv("Data_mod/hypsograph_data.csv")


# Plot hypsograph data
ggplot(hypsograph_data, aes(area_ha, depth)) +
  geom_line() +
  facet_wrap(~lake_name, scales = "free") +
  geom_segment( aes(x = x_null, xend = 0, y = 0, yend = y_null), linetype = "dotted") +
  labs(x = "Area (ha)",y = "Depth (m)") +
  theme_bw()
