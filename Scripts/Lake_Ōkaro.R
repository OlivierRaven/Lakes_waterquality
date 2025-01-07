# Lake Ōkaro
# Explanation of this script------------------------------------------------

# This script focuses on the water quality trents in Lake Ōkaro


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("ggforce","metR","akima","magick","readxl","sf", "lubridate","readr", "tidyverse", "dplyr", "ggplot2", "rLakeAnalyzer")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data --------------------------------------------------------------
Dosings_Okaro_BOPRC <- read_csv("Data_raw/Dosings_Okaro_BOPRC.csv")

# Okro water qulity from all waq data bop
Okaro_wq <- read_excel("Data_mod/Okaro_wq.xlsx")

# Okro water qulity from all ctd data bop
Okaro_CTD <- read_excel("Data_mod/Okaro_CTD.xlsx")

Okaro_CTD_bouy <- list.files(path = "Data_raw/Okaro_bouy_profiles", pattern = "\\.csv$", full.names = TRUE)
Okaro_CTD_bouy <- bind_rows(lapply(Okaro_CTD_bouy, read.csv)) # make it into a DF

# Bathymetry_Okaro2.0
hypsograph_Okaro <- read_csv("Data_mod/hypsograph_data.csv")
hypsograph_Okaro <- hypsograph_Okaro %>%
  filter(lake_name == "Okaro")

Rotlakes_bathymetry <- read_excel("Data_raw/Lake_bathmetry/Rotlakes_bathymetry.xls")
Bathymetry_Okaro2.0 <- Rotlakes_bathymetry %>%
  filter(LAKE == "Okaro")%>%
  select(depths = `DEPTH (m)`, areas = `PLANAR SURFACE AREA(m2)`)
write.csv(Bathymetry_Okaro2.0, "Data_mod/Bathymetry_Okaro2.0.csv", row.names = FALSE)
Bathymetry_Okaro2.1 <- load.bathy("Data_mod/Bathymetry_Okaro2.0.csv")

# Okaro outline
lakes_Shape <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/12_lakes.gpkg")
Okaro_Line <- lakes_Shape %>%
  filter(name_ascii == "Lake Okaro") %>% 
  mutate(geom = st_boundary(geom)) 

# Bathemetry Okaro -------------------------------------------------------------
plot(Bathymetry_Okaro2.1$areas, Bathymetry_Okaro2.1$depths, type='l',
     ylab='Depths (m)', xlab='Areas (m^2)')

Bathymetry_Okaro2.1
# Create a data frame for plotting the circles (radii)
# Add a radius column to the Bathymetry_Okaro2.1 data frame
Bathymetry_Okaro2.1$radius <- sqrt(Bathymetry_Okaro2.1$areas / pi)

# Create a data frame for plotting the circles
circle_data <- data.frame(
  x = rep(0, nrow(Bathymetry_Okaro2.1)),   # All circles centered at (0, 0)
  y = rep(0, nrow(Bathymetry_Okaro2.1)),
  radius = Bathymetry_Okaro2.1$radius,
  depth = Bathymetry_Okaro2.1$depths,
  area = Bathymetry_Okaro2.1$areas
)

# Create the plot
ggplot(circle_data) +
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = depth), col = "black") +  
  geom_text(aes(x = x, y = radius -10, label = depth), col="white") +  
  coord_equal() +
  labs(title = "Lake Ōkaro") +
  #scale_fill_gradient(low = "blue", high = "red") +  # Adjust colors as needed
  theme_void()

ggplot()+
  geom_sf(data=Okaro_Line)

lake_centroid <- st_centroid(Okaro_Line)
centroid_coords <- st_coordinates(lake_centroid)



#
# Play with the monthly CTD data -----------------------------------------------
# make a sample ID for each of the casts
Okaro_CTD <- Okaro_CTD %>% group_by(Time, Lake, Site) %>% mutate(cast_id = group_indices())

# Separate the date and time components into two new columns
Okaro_CTD <- Okaro_CTD %>%
  mutate(Date = as.Date(Time),
         Time_of_day = format(Time, format = "%H:%M:%S"),
         Day = day(Date),
         Month = month(Time, label = TRUE),  
         Year = year(Time),  
         Season = case_when(Month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                            Month %in% c("Mar", "Apr", "May") ~ "Autumn", 
                            Month %in% c("Jun", "Jul", "Aug") ~ "Winter", 
                            Month %in% c("Sep", "Oct", "Nov") ~ "Spring"))

# explore
head(Okaro_CTD)

 # make plot of all the years DO
ggplot(Okaro_CTD %>%
         filter(`Depth (m)` >= 1),
                #year(Date) == 2021,
       aes(`DO (g/m^3)`, `Depth (m)`)) +
  geom_line(aes(col=Season), lwd=1) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red")+
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  xlab(expression("DO" ~ "(g"~m^-3*")"))+ 
  scale_y_reverse()

# make plot of all the years Temperature
ggplot(Okaro_CTD %>%
         filter(`Depth (m)` >= 1),
       aes(`Water Temp (degC)`, `Depth (m)`)) +
  geom_line(aes(col=Season), lwd=1) +
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  scale_y_reverse()

# make plot of all the years Chlorofil
ggplot(Okaro_CTD %>%
         filter(`Depth (m)` >= 1),
       aes(`Chlorophyll (ug/l)`, `Depth (m)`)) +
  geom_line(aes(col=Season), lwd=1) +
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  scale_y_reverse()


# Plot Bathymetry_Okaro2.1 and calculate Schmidt stability
plot(Bathymetry_Okaro2.1$areas, Bathymetry_Okaro2.1$depths, type='l',
     ylab='Depths (m)', xlab='Areas (m^2)')

calculate_schmidt <- function(data, bathymetry) {
  schmidt_values <- data %>%
    group_by(cast_id) %>%
    summarise(schmidt_value = schmidt.stability(
      wtr = `Water Temp (degC)`,
      depths = `Depth (m)`,
      bthA = bathymetry$areas,
      bthD = bathymetry$depths
    ), .groups = 'drop') # Added `.groups = 'drop'` to avoid issues with grouping
  return(schmidt_values)
}

# Apply the function to your dataset
schmidt_results <- calculate_schmidt(Okaro_CTD, Bathymetry_Okaro2.1)

# Merge the Schmidt Stability values back into the original data frame
Okaro_CTD <- Okaro_CTD %>%
  left_join(schmidt_results, by = "cast_id")

plot(Okaro_CTD$Date, Okaro_CTD$schmidt_value)


# Calculate termocline
# Step 1: Calculate the thermocline depth for each cast_id
thermo_Okaro_CTD <- Okaro_CTD %>%
  arrange(cast_id, `Depth (m)`) %>%
  distinct(cast_id, `Depth (m)`, `Water Temp (degC)`, .keep_all = TRUE) %>%
  group_by(cast_id) %>%
  summarise(thermo_depth = thermo.depth(wtr = `Water Temp (degC)`, depths = `Depth (m)`, seasonal = FALSE))

# Step 2: Join the thermo_depth back to the original data and separate into above and below thermocline
Okaro_CTD <- Okaro_CTD %>%
  left_join(thermo_Okaro_CTD, by = "cast_id") %>%
  mutate(Position = ifelse(`Depth (m)` <= thermo_depth, "Above", "Below"))

# Step 3: Calculate means and SEs above and below the thermocline
meanvalues_Okaro_CTD <- Okaro_CTD %>%
  group_by(cast_id, Position) %>%
  summarise(
    mean_TmpWtr = mean(`Water Temp (degC)`, na.rm = TRUE),
    se_TmpWtr = sd(`Water Temp (degC)`, na.rm = TRUE) / sqrt(n()),
    mean_FlChlr = mean(`Chlorophyll (ug/l)`, na.rm = TRUE),
    se_FlChlr = sd(`Chlorophyll (ug/l)`, na.rm = TRUE) / sqrt(n()),
    mean_DOconc = mean(`DO (g/m^3)`, na.rm = TRUE),
    se_DOconc = sd(`DO (g/m^3)`, na.rm = TRUE) / sqrt(n()),
    mean_DOpsat = mean(`DO sat (%)`, na.rm = TRUE),
    se_DOpsat = sd(`DO sat (%)`, na.rm = TRUE) / sqrt(n()),
    mean_SpCond = mean(`Sp Cond (uS/cm)`, na.rm = TRUE),
    se_SpCond = sd(`Sp Cond (uS/cm)`, na.rm = TRUE) / sqrt(n()),
    mean_TurbRT = mean(`Turbidity NTU (_NTU)`, na.rm = TRUE),
    se_TurbRT = sd(`Turbidity NTU (_NTU)`, na.rm = TRUE) / sqrt(n())) %>%
  left_join(Okaro_CTD %>% select(cast_id, Date, Year, Month, Season, thermo_depth) %>% distinct(), by = "cast_id")

summary(meanvalues_Okaro_CTD)

ggplot(meanvalues_Okaro_CTD, aes(Date, mean_TmpWtr)) +
  geom_point(aes(col= Position))


# group the values for 0, 5, 10, 15, 16
# Define the specific depths to filter
target_depths <- c(0, 5, 10, 15, 16)

# Filter data for those specific depths and calculate the mean temperature and DO by depth
Depth_group_Okaro_CTD <- Okaro_CTD %>%
  filter(`Depth (m)` %in% target_depths) %>%  # Filter for the required depths
  group_by(Date, `Depth (m)`) %>%             # Group by date and depth
  summarise(
    mean_temperature = mean(`Water Temp (degC)`, na.rm = TRUE),  # Mean temperature for each date and depth
    mean_oxygen = mean(`DO (g/m^3)`, na.rm = TRUE),              # Mean dissolved oxygen for each date and depth
    livable_habitat = mean(`DO (g/m^3)`, na.rm = TRUE) > 5       # Livable habitat condition (TRUE/FALSE)
  ) %>%
  ungroup()

# Filter data for 2003 May as a Test
May_2003 <- Depth_group_Okaro_CTD %>%
  filter( Date == "2003-05-29")%>%
  mutate(`Depth (m)` = -`Depth (m)`)

circle_data_May_2003 <- circle_data %>%
  left_join(May_2003, by = c("depth" = "Depth (m)")) %>%
  select(x, y, radius, depth, area, livable_habitat) %>%
  mutate(livable_habitat = ifelse(depth == -18, FALSE, livable_habitat))

color_map <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")

ggplot(circle_data_May_2003) +
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = livable_habitat), col = "black") +  
  geom_text(aes(x = x, y = radius -10, label = depth), col="white") +  
  coord_equal() +
  labs(title = "Lake Ōkaro") +
  scale_fill_manual(values = color_map) +  
  theme_void()



#
# Play with the CTD buoy data --------------------------------------------------
names(Okaro_CTD_bouy)
head(Okaro_CTD_bouy)

Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  mutate(Date = as.Date(DateTime),
         Time = format(as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
         Day = day(Date),
         Month = month(Date, label = TRUE, abbr = TRUE),
         Year = year(Date),
         Season = case_when(
           Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
           Month %in% c("Mar", "Apr", "May") ~ "Autumn",
           Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
           Month %in% c("Sep", "Oct", "Nov") ~ "Spring"))

# Remove rows without depth and temperature values
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  filter(!is.na(DptSns))%>%
  filter(!is.na(TmpWtr))

# Make cast ID for each cast based on the reset of the depth
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  arrange(Date, Time) %>%  # Ensure the data is ordered by Date and Time
  mutate(cast_id = cumsum(DptSns < lag(DptSns, default = first(DptSns)))+ 1)%>%
  mutate(cast_id = as.factor(cast_id))

# Calculate the number of casts per day
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  group_by(Date) %>%
  mutate(num_casts = n_distinct(cast_id)) %>%
  ungroup()

# remove days with min 5 and max 15 casts
hist(Okaro_CTD_bouy$num_casts)
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  filter(num_casts >= 5 & num_casts <= 15)


# Remove duplicates
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  distinct(cast_id, DptSns, .keep_all = TRUE)

# Plot Bathymetry_Okaro2.1 and calculate Schmidt stability 
plot(Bathymetry_Okaro2.1$areas, Bathymetry_Okaro2.1$depths, type='l',
     ylab='Depths (m)', xlab='Areas (m^2)')

calculate_schmidt <- function(data, bathymetry) {
  schmidt_values <- data %>%
    group_by(cast_id) %>%
    summarise(schmidt_value = schmidt.stability(
      wtr = TmpWtr,
      depths = DptSns,
      bthA = bathymetry$areas,
      bthD = bathymetry$depths
    ), .groups = 'drop') # Added `.groups = 'drop'` to avoid issues with grouping
  return(schmidt_values)
}

# Apply the function to your dataset
schmidt_results <- calculate_schmidt(Okaro_CTD_bouy, Bathymetry_Okaro2.1)

# Merge the Schmidt Stability values back into the original data frame
Okaro_CTD_bouy <- Okaro_CTD_bouy %>%
  left_join(schmidt_results, by = "cast_id")

plot(Okaro_CTD_bouy$Date, Okaro_CTD_bouy$schmidt_value)




# Calculate the mean values above and below the termocline for each cast_id 
  # Step 1: Calculate the thermocline depth for each cast_id
thermo_Okaro_bouy <- Okaro_CTD_bouy %>%
  arrange(cast_id, DptSns) %>%
  distinct(cast_id, DptSns, TmpWtr, .keep_all = TRUE) %>%
  group_by(cast_id) %>%
  summarise(thermo_depth = thermo.depth(wtr = TmpWtr, depths = DptSns, seasonal = FALSE))

# Step 2: Join the thermo_depth back to the original data and separate into above and below thermocline
Okaro_bouy <- Okaro_CTD_bouy %>%
  left_join(thermo_Okaro_bouy, by = "cast_id") %>%
  mutate(Position = ifelse(DptSns <= thermo_depth, "Above", "Below"))

# Step 3: Calculate means and SEs above and below the thermocline
meanvalues_Okaro <- Okaro_bouy %>%
  group_by(cast_id, Position) %>%
  summarise(
    mean_TmpWtr = mean(TmpWtr, na.rm = TRUE),
    mean_DOconc = mean(DOconc, na.rm = TRUE),
    mean_DOpsat = mean(DOpsat, na.rm = TRUE),
    mean_Cond = mean(Cond, na.rm = TRUE),
    mean_SpCond = mean(SpCond, na.rm = TRUE),
    mean_pH = mean(pH, na.rm = TRUE),
    mean_ORP = mean(ORP, na.rm = TRUE),
    mean_TurbRT = mean(TurbRT, na.rm = TRUE),
    mean_FlChlr = mean(FlChlr, na.rm = TRUE),
    mean_FlPhyc = mean(FlPhyc, na.rm = TRUE),
    mean_TmpChg = mean(TmpChg, na.rm = TRUE),
    mean_StrsTt = mean(StrsTt, na.rm = TRUE),
    se_TmpWtr = sd(TmpWtr, na.rm = TRUE) / sqrt(n()),
    se_DOconc = sd(DOconc, na.rm = TRUE) / sqrt(n()),
    se_DOpsat = sd(DOpsat, na.rm = TRUE) / sqrt(n()),
    se_Cond = sd(Cond, na.rm = TRUE) / sqrt(n()),
    se_SpCond = sd(SpCond, na.rm = TRUE) / sqrt(n()),
    se_pH = sd(pH, na.rm = TRUE) / sqrt(n()),
    se_ORP = sd(ORP, na.rm = TRUE) / sqrt(n()),
    se_TurbRT = sd(TurbRT, na.rm = TRUE) / sqrt(n()),
    se_FlChlr = sd(FlChlr, na.rm = TRUE) / sqrt(n()),
    se_FlPhyc = sd(FlPhyc, na.rm = TRUE) / sqrt(n()),
    se_TmpChg = sd(TmpChg, na.rm = TRUE) / sqrt(n()),
    se_StrsTt = sd(StrsTt, na.rm = TRUE) / sqrt(n()),) %>%
  left_join(Okaro_bouy %>% select(cast_id, Date, Year, Month, Season, thermo_depth) %>% distinct(), by = "cast_id")

summary(meanvalues_Okaro)

na_count <- meanvalues_Okaro %>%
  group_by(Year, Season) %>%
  summarise(na_count = sum(is.na(Position)))


ggplot(meanvalues_Okaro, aes(Date, thermo_depth)) +
  geom_point(aes(col= Month))+
  scale_y_reverse()

ggplot(meanvalues_Okaro, aes(Date, mean_TmpWtr,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)

ggplot(meanvalues_Okaro, aes(Date, mean_DOconc,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc - se_DOconc, ymax = mean_DOconc + se_DOconc), width = 0.2)+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") 


# Join the 2 data sets ---------------------------------------------------------
# join the 2 casts data setes first
Okaro_CTD_bouy

Okaro_CTD

# Select relevant columns from each data set
meanvalues_Okaro_CTD_selected <- meanvalues_Okaro_CTD %>% ungroup() %>%
  select( Date, Year,Month,Season,Position,thermo_depth,mean_TmpWtr,se_TmpWtr,mean_DOconc,se_DOconc,mean_DOpsat,se_DOpsat,mean_SpCond,se_SpCond,mean_TurbRT,se_TurbRT,mean_FlChlr,se_FlChlr)

meanvalues_buoy_selected <- meanvalues_Okaro %>% ungroup() %>%
  select( Date, Year,Month,Season,Position,thermo_depth,mean_TmpWtr,se_TmpWtr,mean_DOconc,se_DOconc,mean_DOpsat,se_DOpsat,mean_SpCond,se_SpCond,mean_TurbRT,se_TurbRT,mean_FlChlr,se_FlChlr)

# Combine the datasets, arrange by Date, and add a row number column
combined_data <- bind_rows(meanvalues_Okaro_CTD_selected, meanvalues_buoy_selected) %>%
  arrange(Date) %>%
  mutate(Number = row_number())

# make plots of the combined data
ggplot(combined_data, aes(x = Date, y = mean_TmpWtr,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)
  geom_line(col="black")

ggplot(combined_data, aes(x = Date, y = mean_DOconc,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc - se_DOconc, ymax = mean_DOconc + se_DOconc), width = 0.2)
  geom_line(col="black")

ggplot(combined_data, aes(Date, thermo_depth)) +
  geom_point(aes(col= Month))+
  #geom_line()+
  scale_y_reverse()




# Calculate mean of mean for each month each year
Mean_combined_data <- combined_data %>%
  group_by(Year, Month, Position) %>%
  summarise(
    mean_thermo_depth = mean(thermo_depth, na.rm = TRUE),
    se_thermo_depth = sd(thermo_depth, na.rm = TRUE) / sqrt(n()),
    mean_TmpWtr_overall = mean(mean_TmpWtr, na.rm = TRUE),
    se_TmpWtr_overall = sd(mean_TmpWtr, na.rm = TRUE) / sqrt(n()),
    mean_DOconc_overall = mean(mean_DOconc, na.rm = TRUE),
    se_DOconc_overall = sd(mean_DOconc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  arrange(Year, Month) %>%
  mutate(Month = as.character(Month),Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))


ggplot(Month_okaro_data, aes(Date, mean_DOconc, col= Position)) +
  geom_point(aes()) 
  geom_errorbar(aes(ymin = mean_DOconc_overall - sd_DOconc_overall, ymax = mean_DOconc_overall + sd_DOconc_overall), width = 0.2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# calculate Montly data from bouy and combine with CTD data
Mean_Okaro_Month <- Okaro_bouy %>%
  group_by(Year, Month, Position) %>%
  summarise(
    thermo_depth = mean(thermo_depth, na.rm = TRUE),
    mean_TmpWtr = mean(TmpWtr, na.rm = TRUE),
    se_TmpWtr = sd(TmpWtr, na.rm = TRUE) / sqrt(n()),
    mean_DOconc = mean(DOconc, na.rm = TRUE),
    se_DOconc = sd(DOconc, na.rm = TRUE) / sqrt(n()),
    .groups = "drop") %>%
  mutate(Month = as.character(Month),Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))


Month_okaro_data <- bind_rows(meanvalues_Okaro_CTD_selected, Mean_Okaro_Month) %>%
  arrange(Date) %>%
  mutate(Number = row_number())


# Map the Available habitat on the bathemetry ----------------------------------
# Plot Bathymetry_Okaro2.1 and calculate Schmidt stability
Bathymetry_Okaro2.1 <- Bathymetry_Okaro2.1 %>%
  mutate(depth = abs(depths))

# Filter data for 2003 May as a Test
May_2003_data <- Mean_combined_data %>%
  filter(Year == 2009, Month == "May")


ggplot(Bathymetry_Okaro2.1, aes(x = areas, y = depth)) +
  geom_area(fill = "#00BFC4") +
  geom_ribbon(aes(ymin = pmin(depth, May_2003_data$mean_thermo_depth[1]), ymax = depth), fill = "#F8766D") +
  geom_line(size=2) +
  geom_hline(yintercept = May_2003_data$mean_thermo_depth , color = "black", linetype = "dashed", size=1) +
  labs(y = "Depth (m)", x = expression(Area~(m^2))) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_bw() +
  labs(caption = "Data Source: Limnotrack & BOPRC") +
  ggtitle(paste(current_month, current_year))

  
Mean_combined_data

# Prepare your data
areas <- Bathymetry_Okaro2.1$areas
depth <- Bathymetry_Okaro2.1$depth
mean_thermo_depth <- May_2003_data$mean_thermo_depth[1]  # Assuming this is a single value

# Determine color based on Livable_habitat
color_above <- ifelse(May_2003_data$Livable_habitat[May_2003_data$Position == "Above"] == "True", "#00BFC4", "#F8766D")
color_below <- ifelse(May_2003_data$Livable_habitat[May_2003_data$Position == "Below"] == "True", "#00BFC4", "#F8766D")

# Set up the plotting area
plot(areas, depth, type = "n", ylim = rev(range(depth)), xlim = rev(range(areas)),
     xlab = expression(Area~(m^2)), ylab = "Depth (m)", main = paste(current_month, current_year))
polygon(c(areas, rev(areas)), 
        c(depth, rep(min(depth), length(depth))), 
        col = "#00BFC4", border = NA)
polygon(c(areas, rev(areas)), 
        c(depth, rep(min(depth), length(depth))), 
        col = color_above, border = NA)
polygon(c(areas, rev(areas)), 
        c(pmin(depth, mean_thermo_depth), rep(max(depth), length(depth))),
        col = color_below, border = NA)
lines(areas, depth, lwd = 2)
abline(h = mean_thermo_depth, col = "black", lty = "dashed", lwd = 2)
mtext("Data Source: Limnotrack & BOPRC", side = 1, line = 4, adj = 0)




# Define the livable habitat
Mean_combined_data <- Mean_combined_data %>%
  mutate(Livable_habitat = ifelse(mean_DOconc_overall <= 5, "False", "True"))

# Define consistent colors for TRUE and FALSE
color_map <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")


# Plot the thermo in each plot for each month and year
# Keep only one combination of Year and Month
Mean_combined_data_unique <- Mean_combined_data %>%
  distinct(Year, Month, .keep_all = TRUE)

Mean_combined_data_unique <- Mean_combined_data_unique %>%
  arrange(Date)

# Initialize a counter for numbering plots
counter <- 1

# Create a list to store plots
plots <- list()

# Loop over each combination of year and month
for (i in 1:nrow(Mean_combined_data_unique)) {
  # Extract the Year and Month for the current iteration
  current_year <- Mean_combined_data_unique$Year[i]
  current_month <- Mean_combined_data_unique$Month[i]
    
    # Filter data for the current Year and Month
    current_data <- Mean_combined_data_unique %>%
      filter(Year == current_year, Month == current_month)
    
    # Generate the plot for the current combination
    plot <- ggplot(Bathymetry_Okaro2.1, aes(x = areas, y = depth)) +
      geom_area(fill = "lightblue") +
      geom_line() +
      geom_hline(yintercept = current_data$mean_thermo_depth , color = "red", linetype = "dashed") +
      labs(y = "Depth (m)", x = expression(Area~(m^2))) +
      scale_y_reverse() +
      scale_x_reverse() +
      theme_bw() +
      labs(caption = "Data Source: Limnotrack & BOPRC") +
      ggtitle(paste(current_month, current_year))
    
    # Save plot to the figures folder with unique numbering
    ggsave(filename = paste0("Figures/Okaro_Thermocline_plots/plot_", sprintf("%04d", counter), "_",current_month , "_", current_year, ".png"), plot = plot)
    
    # Save plot to the list with unique key
    plots[[sprintf("%04d_%s_%s", counter, current_year, current_month)]] <- plot
    
    # Increment the counter
    counter <- counter + 1}


# animate the plots
image_paths <- list.files(path = "Figures/Okaro_Thermocline_plots", pattern = "\\.png$", full.names = TRUE)

# Sort the image paths if needed
image_paths <- sort(image_paths)

# Read images
images <- lapply(image_paths, image_read)

# Create an animation (video)
animation <- image_animate(image_join(images), fps = 2.5)

# Save as a GIF
image_write(animation, "Thermo_Okaro.gif")







# Plot the thermo in each plot for each month and year
# Filter to keep only "Below" positions or NA if that is the only position for the Year-Month combination
# Step 1: Identify Year-Month combinations that only have NA positions
na_only_combinations <- Mean_combined_data %>%
  group_by(Year, Month) %>%
  filter(all(is.na(mean_thermo_depth))) %>%
  ungroup()

# Step 2: Filter for "Below" positions and add the NA-only combinations
Below_combined_data <- Mean_combined_data %>%
  filter(Position == "Below") %>%
  bind_rows(na_only_combinations) %>%
  arrange(Year, Month) %>%
  mutate(mean_thermo_depth = ifelse(is.na(mean_thermo_depth), 100, mean_thermo_depth))

# step 3 
Above_combined_data <- Mean_combined_data %>%
  filter(Position == "Above") %>%
  bind_rows(na_only_combinations) %>%
  arrange(Year, Month) %>%
  mutate(mean_thermo_depth = ifelse(is.na(mean_thermo_depth), 100, mean_thermo_depth))


# Prepare your data
areas <- Bathymetry_Okaro2.1$areas
depth <- Bathymetry_Okaro2.1$depth

Below_combined_data <- Below_combined_data %>%
  arrange(Date)

# Initialize a counter for numbering plots
counter <- 1

# Loop over each combination of year and month
for (i in 1:nrow(Below_combined_data)) {
  # Extract the Year and Month for the current iteration
  current_year <- Below_combined_data$Year[i]
  current_month <- Below_combined_data$Month[i]
  
  # Filter data for the current Year and Month
  current_data <- Below_combined_data %>%
    filter(Year == current_year, Month == current_month)
  
  thermo_depth <- current_data$mean_thermo_depth[1]
  
  # Determine color based on Livable_habitat
  #color_above <- ifelse(current_data$Livable_habitat[current_data$Position == "Above"] == "True", "#00BFC4", "#F8766D")
  color_below <- ifelse(current_data$Livable_habitat == "True", "#00BFC4", "#F8766D")
  
  # Open a PNG device
  png(filename = paste0("Figures/Okaro_Thermocline_plots/plot_", sprintf("%04d", counter), "_",current_month , "_", current_year, ".png"))
  
  # Generate the plot for the current combination
  plot <- plot(areas, depth, type = "n", ylim = rev(range(depth)), xlim = rev(range(areas)),
               xlab = expression(Area~(m^2)), ylab = "Depth (m)", main = paste(current_month, current_year))
  polygon(c(areas, rev(areas)), 
          c(depth, rep(min(depth), length(depth))), 
          col = "#00BFC4", border = NA)
  #polygon(c(areas, rev(areas)), 
          #c(depth, rep(min(depth), length(depth))), 
          #col = color_above, border = NA)
  polygon(c(areas, rev(areas)), 
          c(pmin(depth, mean_thermo_depth), rep(max(depth), length(depth))),
          col = color_below, border = NA)
  lines(areas, depth, lwd = 2)
  abline(h = thermo_depth, col = "black", lty = "dashed", lwd = 2)
  mtext("Data Source: Limnotrack & BOPRC", side = 1, line = 4, adj = 0)
  
  # Close the PNG device
  dev.off()
  
  # Increment the counter
  counter <- counter + 1}







