# Lake Rotoiti
# Explanation of this script------------------------------------------------

# This script focuses on the waterquality trents in Lake Rotoiti


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("viridis","rLakeAnalyzer", "gganimate","sf", "lubridate","readr", "tidyverse", "dplyr", "ggplot2", "magick")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data --------------------------------------------------------------
# subtract the BoPRC data from WQ_II file
Rotoiti_wq <- read_excel("Data_mod/Rotoiti_wq.xlsx")
 
Rotoiti_CTD <- read_excel("Data_mod/Rotoiti_CTD.xlsx")

# Import bouy data
Rotoiti_CTD_bouy <- list.files(path = "Data_raw/Bouy_rotoiti", pattern = "\\.csv$", full.names = TRUE)
Rotoiti_CTD_bouy <- bind_rows(lapply(Rotoiti_CTD_bouy, read.csv)) # make it into a DF

# Bathymetry_Rotoiti
grid_df_bat_Rotoiti <- read.delim("~/PhD/Data/Lakes/Lakes_waterquality/Data_raw/grid_df_bat_Rotoiti.txt")
# Make the bathamotri of the lake from depth in grid size of 12.5*12.5.
grid_df_bat_Rotoiti <- grid_df_bat_Rotoiti %>%
  filter(Z != -9999.0000) %>%  # Filter out rows with Z value -9999.0000
  mutate(Z = round(Z),         # Round Z values to the nearest whole number
         Depth = 279 - Z)    # Calculate Depth relative to the surface level

# change codinate system to lon and lat in sf
Bathymetry_Rotoiti_sf <- st_as_sf(grid_df_bat_Rotoiti, coords = c("X", "Y"), crs = 27200) # 27200 is the EPSG code for NZMG (New Zealand Map Grid)
#grid_df_bat_Rotoiti_sf <- st_as_sf(grid_df_bat_Rotoiti, coords = c("X", "Y"), crs = 2193)
#grid_df_bat_Rotoiti_wgs84 <- st_transform(grid_df_bat_Rotoiti_sf, crs = 4326)

# get the outline of the lake
Rotoiti_Shape <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Lake Rotoiti shape.gpkg")
Rotoiti_Line <- Rotoiti_Shape %>%
  mutate(geom = st_boundary(geom)) 

# Bathymetry_Rotoiti2.0
#Rotlakes_bathymetry <- read_excel("Data_raw/Rotlakes_bathymetry.xls")
#Bathymetry_Rotoiti2.0 <- Rotlakes_bathymetry %>%
#  filter(LAKE == "Rotoiti")%>%
#  select(depths = `DEPTH (m)`, areas = `PLANAR SURFACE AREA(m2)`)
#write.csv(Bathymetry_Rotoiti2.0, "Data_mod/Bathymetry_Rotoiti2.0.csv", row.names = FALSE)
Bathymetry_Rotoiti2.1 <- load.bathy("Data_mod/Bathymetry_Rotoiti2.0.csv")

# Clean the CTD buoy data & Bathymetry -----------------------------------------
names(Rotoiti_CTD_bouy)
head(Rotoiti_CTD_bouy)

Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
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
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  filter(!is.na(DptSns))%>%
  filter(!is.na(TmpWtr))

# Make cast ID for each cast based on the reset of the depth
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  arrange(Date, Time) %>%  # Ensure the data is ordered by Date and Time
  mutate(cast_id = cumsum(DptSns < lag(DptSns, default = first(DptSns)))+ 1)%>%
  mutate(cast_id = as.factor(cast_id))

# Calculate the number of casts per day
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  group_by(Date) %>%
  mutate(num_casts = n_distinct(cast_id)) %>%
  ungroup()

# remove days with min 5 and max 10 casts
hist(Rotoiti_CTD_bouy$num_casts)
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  filter(num_casts >= 5 & num_casts <= 10)


# Remove duplicates
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  distinct(cast_id, DptSns, .keep_all = TRUE)


# plot all the profiles of temperature
ggplot(Rotoiti_CTD_bouy, aes(TmpWtr , DptSns )) +
  geom_line(aes(color = Day)) +
  facet_grid(Month ~ Year) +
  theme_bw() +
  scale_y_reverse() 

# Make the Bathymetry of the lake from depth in grid size of 12.5*12.5.
# Define the color ramp
color_ramp <- c("red","yellow","green", "blue", "purple")

# Plot the Bathymetry_Rotoiti
ggplot() +
  geom_sf(data = Bathymetry_Rotoiti_sf,aes(col= Depth)) +
  geom_point()+
  scale_color_gradientn(colors = color_ramp) +
  theme_bw()


# Plot Bathymetry_Rotoiti2.1 and calculate Schmidt stability -----------
plot(Bathymetry_Rotoiti2.1$areas, Bathymetry_Rotoiti2.1$depths, type='l',
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
schmidt_results <- calculate_schmidt(Rotoiti_CTD_bouy, Bathymetry_Rotoiti2.1)

# Merge the Schmidt Stability values back into the original data frame
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  left_join(schmidt_results, by = "cast_id")

plot(Rotoiti_CTD_bouy$Date, Rotoiti_CTD_bouy$schmidt_value)
################################################################################
# Calculate the mean values above and below the termocline for each day --------
head(Rotoiti_CTD_bouy)

# Round the depth to the nearest meter
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  mutate(DptSns_meter = round(DptSns))

# Calculate the number of casts per day
Rotoiti_CTD_bouy <- Rotoiti_CTD_bouy %>%
  group_by(Date) %>%
  mutate(num_casts = n_distinct(cast_id)) %>%
  ungroup()


# Group by Date and DptSns_meter, then calculate the average of each variable
Day_data <- Rotoiti_CTD_bouy %>%
  group_by(Date, DptSns_meter) %>%
  summarise(
    Date = first(Date), # or use any appropriate function if you want to handle duplicates
    Time = first(Time), # assuming Time is consistent within each group
    Day = first(Day),
    Month = first(Month),
    Year = first(Year),
    Season = first(Season),
    schmidt_value = mean(schmidt_value),
    num_casts = first(num_casts),
    across(TmpWtr:StrsTt, mean, na.rm = TRUE),
    .groups = 'drop')


ggplot(data = Day_data 
      # %>%filter(format(Date, "%Y-%m") == "2022-05")
       , aes(x = TmpWtr, y = DptSns_meter)) +
  geom_line(aes(col = as.factor(Day))) +
  facet_grid(Month ~ Year) +
  theme_bw() +
  scale_y_reverse()

ggplot(data = Day_data 
       # %>%filter(format(Date, "%Y-%m") == "2022-05")
       , aes(x = TmpWtr, y = DptSns_meter)) +
  geom_line(aes(col = as.factor(Day))) +
  facet_grid(Month ~ Year) +
  geom_line(aes(y = thermo_depth, col = as.factor(Day)), linetype = "dashed")+
  geom_vline(xintercept = 20, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_reverse()

ggplot(data = Day_data 
       # %>%filter(format(Date, "%Y-%m") == "2022-05")
       , aes(x = DOconc, y = DptSns_meter)) +
  geom_line(aes(col = as.factor(Day))) +
  facet_grid(Month ~ Year) +
  geom_line(aes(y = thermo_depth, col = as.factor(Day)), linetype = "dashed")+
  #geom_hline(yintercept = thermo_depth, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_reverse()


# Step 1: Calculate the thermocline depth for each Date
thermo_date <- Day_data %>%
  arrange(Date, DptSns_meter) %>%
  distinct(Date, DptSns_meter, TmpWtr, .keep_all = TRUE) %>%
  group_by(Date) %>%
  summarise(thermo_depth = thermo.depth(wtr = TmpWtr, depths = DptSns_meter, seasonal = FALSE))

# Step 2: Join the thermo_depth back to the original data and separate into above and below thermocline
Day_data <- Day_data %>%
  left_join(thermo_date, by = "Date") %>%
  mutate(Position = ifelse(DptSns_meter <= thermo_depth, "Above", "Below"))

# Step 3: Calculate means and SEs above and below the thermocline for each day
meanvalues_Day_data <- Day_data %>%
  group_by(Date, Position) %>%
  summarise(
    mean_TmpWtr = mean(TmpWtr, na.rm = TRUE),
    mean_DOconc = mean(DOconc, na.rm = TRUE),
    mean_DOpsat = mean(DOpsat, na.rm = TRUE),
    mean_Cond   = mean(Cond, na.rm = TRUE),
    mean_SpCond = mean(SpCond, na.rm = TRUE),
    mean_pH     = mean(pH, na.rm = TRUE),
    mean_ORP    = mean(ORP, na.rm = TRUE),
    mean_TurbRT = mean(TurbRT, na.rm = TRUE),
    mean_FlChlr = mean(FlChlr, na.rm = TRUE),
    mean_FlPhyc = mean(FlPhyc, na.rm = TRUE),
    mean_TmpChg = mean(TmpChg, na.rm = TRUE),
    mean_StrsTt = mean(StrsTt, na.rm = TRUE),
    mean_schmidt_value = mean(schmidt_value, na.rm = T),
    se_TmpWtr   = sd(TmpWtr, na.rm = TRUE) / sqrt(n()),
    se_DOconc   = sd(DOconc, na.rm = TRUE) / sqrt(n()),
    se_DOpsat   = sd(DOpsat, na.rm = TRUE) / sqrt(n()),
    se_Cond     = sd(Cond, na.rm = TRUE) / sqrt(n()),
    se_SpCond   = sd(SpCond, na.rm = TRUE) / sqrt(n()),
    se_pH       = sd(pH, na.rm = TRUE) / sqrt(n()),
    se_ORP      = sd(ORP, na.rm = TRUE) / sqrt(n()),
    se_TurbRT   = sd(TurbRT, na.rm = TRUE) / sqrt(n()),
    se_FlChlr   = sd(FlChlr, na.rm = TRUE) / sqrt(n()),
    se_FlPhyc   = sd(FlPhyc, na.rm = TRUE) / sqrt(n()),
    se_TmpChg   = sd(TmpChg, na.rm = TRUE) / sqrt(n()),
    se_StrsTt   = sd(StrsTt, na.rm = TRUE) / sqrt(n()),
    se_schmidt_value = sd(schmidt_value, na.rm = T) / sqrt(n()),) %>%
  left_join(Day_data %>% select(Date, Year, Month,Day, Season, thermo_depth, num_casts) %>% distinct(), by = "Date")


# remove days with min 5 and max 10 casts
hist(meanvalues_Day_data$num_casts)
meanvalues_Day_data <- meanvalues_Day_data %>%
  filter(num_casts >= 5 & num_casts <= 10)


ggplot(meanvalues_Day_data, aes(Date, thermo_depth)) +
  geom_point(aes(col= Month))+
  geom_line()+
  scale_y_reverse()

ggplot(meanvalues_Day_data, aes(Date, mean_TmpWtr,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)

ggplot(meanvalues_Day_data, aes(Date,mean_DOconc ,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc - se_DOconc, ymax = mean_DOconc + se_DOconc), width = 0.2)+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") 

ggplot(meanvalues_Day_data, aes(Date, mean_schmidt_value  )) +
  geom_point(aes())+
  geom_errorbar(aes(ymin = mean_schmidt_value - se_schmidt_value, ymax = mean_schmidt_value + se_schmidt_value), width = 0.2)



# Calculate livable habitat based on (temperature) and oxygen from day values ----
# optimum Temperature = 10-21 degree
# optimum DO = >5.0 mg/l
head(meanvalues_Day_data)
  
  # Calculate the mean and standard error for each group
  Montly_summary_Rotoiti <- Day_data %>%
         group_by(Year, Month, Position) %>%
         summarise(
             mean_thermo_depth = mean(thermo_depth, na.rm = TRUE),
             mean_TmpWtr = mean(TmpWtr, na.rm = TRUE),
             mean_DOconc = mean(DOconc, na.rm = TRUE),
             mean_DOpsat = mean(DOpsat, na.rm = TRUE),
             mean_Cond   = mean(Cond, na.rm = TRUE),
             mean_SpCond = mean(SpCond, na.rm = TRUE),
             mean_pH     = mean(pH, na.rm = TRUE),
             mean_ORP    = mean(ORP, na.rm = TRUE),
             mean_TurbRT = mean(TurbRT, na.rm = TRUE),
             mean_FlChlr = mean(FlChlr, na.rm = TRUE),
             mean_FlPhyc = mean(FlPhyc, na.rm = TRUE),
             mean_TmpChg = mean(TmpChg, na.rm = TRUE),
             mean_StrsTt = mean(StrsTt, na.rm = TRUE),
             mean_schmidt_value = mean(schmidt_value, na.rm = T),
             sd_thermo_depth = sd(thermo_depth, na.rm = TRUE)/ sqrt(n()),
             se_TmpWtr   = sd(TmpWtr, na.rm = TRUE) / sqrt(n()),
             se_DOconc   = sd(DOconc, na.rm = TRUE) / sqrt(n()),
             se_DOpsat   = sd(DOpsat, na.rm = TRUE) / sqrt(n()),
             se_Cond     = sd(Cond, na.rm = TRUE) / sqrt(n()),
             se_SpCond   = sd(SpCond, na.rm = TRUE) / sqrt(n()),
             se_pH       = sd(pH, na.rm = TRUE) / sqrt(n()),
             se_ORP      = sd(ORP, na.rm = TRUE) / sqrt(n()),
             se_TurbRT   = sd(TurbRT, na.rm = TRUE) / sqrt(n()),
             se_FlChlr   = sd(FlChlr, na.rm = TRUE) / sqrt(n()),
             se_FlPhyc   = sd(FlPhyc, na.rm = TRUE) / sqrt(n()),
             se_TmpChg   = sd(TmpChg, na.rm = TRUE) / sqrt(n()),
             se_StrsTt   = sd(StrsTt, na.rm = TRUE) / sqrt(n()),
             se_schmidt_value = sd(schmidt_value, na.rm = T) / sqrt(n()),) %>%
         ungroup()
  
  
ggplot(Montly_summary_Rotoiti, aes(interaction(Month,Year, sep = "-"), mean_thermo_depth)) +
  geom_point(aes(col= Month)) +
  geom_errorbar(aes(ymin = mean_thermo_depth - sd_thermo_depth, ymax = mean_thermo_depth + sd_thermo_depth, col= Month), width = 0.2)+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_reverse()

ggplot(mean_day_summary_Rotoiti, aes(interaction(Month,Year, sep = "-"), mean_TmpWtr, col= Month)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(mean_day_summary_Rotoiti, aes(interaction(Month,Year, sep = "-"), mean_DOconc_overall, col= Month)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc_overall - sd_DOconc_overall, ymax = mean_DOconc_overall + sd_DOconc_overall), width = 0.2)+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



################################################################################
# Calculate the mean values above and below the termocline for each cast_id ----
# Step 1: Calculate the thermocline depth for each cast_id
thermo_Rotoiti_bouy <- Rotoiti_CTD_bouy %>%
  arrange(cast_id, DptSns) %>%
  distinct(cast_id, DptSns, TmpWtr, .keep_all = TRUE) %>%
  group_by(cast_id) %>%
  summarise(thermo_depth = thermo.depth(wtr = TmpWtr, depths = DptSns, seasonal = FALSE))

# Step 2: Join the thermo_depth back to the original data and separate into above and below thermocline
Rotoiti_bouy <- Rotoiti_CTD_bouy %>%
  left_join(thermo_Rotoiti_bouy, by = "cast_id") %>%
  mutate(Position = ifelse(DptSns <= thermo_depth, "Above", "Below"))

# Step 3: Calculate means and SEs above and below the thermocline
meanvalues_Rotoiti <- Rotoiti_bouy %>%
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
  left_join(Rotoiti_bouy %>% select(cast_id, Date, Year, Month, Season, thermo_depth) %>% distinct(), by = "cast_id")

summary(meanvalues_Rotoiti)

na_count <- meanvalues_Rotoiti %>%
  group_by(Year, Season) %>%
  summarise(na_count = sum(is.na(Position)))


ggplot(meanvalues_Rotoiti, aes(Date, thermo_depth)) +
  geom_point(aes(col= Month))+
  scale_y_reverse()

ggplot(meanvalues_Rotoiti, aes(Date, mean_TmpWtr,col = Month)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)

ggplot(meanvalues_Rotoiti, aes(Date, mean_DOconc,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc - se_DOconc, ymax = mean_DOconc + se_DOconc), width = 0.2)+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") 


# Calculate mean of mean for each month each year
Month_summary_Rotoiti <- Rotoiti_bouy %>%
  group_by(Year, Month, Position) %>%
  summarise(
    mean_thermo_depth = mean(thermo_depth, na.rm = TRUE),
    se_thermo_depth = sd(thermo_depth, na.rm = TRUE)/ sqrt(n()),
    mean_TmpWtr_overall = mean(TmpWtr, na.rm = TRUE),
    se_TmpWtr_overall = sd(TmpWtr, na.rm = TRUE)/ sqrt(n()),
    mean_DOconc_overall = mean(DOconc, na.rm = TRUE),
    se_DOconc_overall = sd(DOconc, na.rm = TRUE)/ sqrt(n()),
    .groups = "drop") %>%
  mutate(Month = as.character(Month),Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))



ggplot(Month_summary_Rotoiti, aes(Date, mean_thermo_depth, col= Month)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_thermo_depth - se_thermo_depth, ymax = mean_thermo_depth + se_thermo_depth), width = 0.2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(Month_summary_Rotoiti, aes(interaction(Month,Year, sep = "-"), mean_TmpWtr_overall, col= Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr_overall - se_TmpWtr_overall, ymax = mean_TmpWtr_overall + se_TmpWtr_overall), width = 0.2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(Month_summary_Rotoiti, aes(interaction(Month,Year, sep = "-"), mean_DOconc_overall, col= Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_DOconc_overall - se_DOconc_overall, ymax = mean_DOconc_overall + se_DOconc_overall), width = 0.2)+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Filter to keep only "Below" positions or NA if that is the only position for the Year-Month combination
# Step 1: Identify Year-Month combinations that only have NA positions
na_only_combinations <- mean_summary_Rotoiti %>%
  group_by(Year, Month) %>%
  filter(all(is.na(mean_thermo_depth))) %>%
  ungroup()

# Step 2: Filter for "Below" positions and add the NA-only combinations
Below_summary_Rotoiti <- mean_summary_Rotoiti %>%
  filter(Position == "Below") %>%
  bind_rows(na_only_combinations) %>%
  arrange(Year, Month) %>%
  mutate(mean_thermo_depth = ifelse(is.na(mean_thermo_depth), 100, mean_thermo_depth))



# Make maps of livable habitat based on oxygen in deep water -------------------

# Filter data for April 2022 as a Test
april_2022_data <- Below_summary_Rotoiti %>%
  filter(Year == 2022, Month == "Apr")

head(Below_summary_Rotoiti)
head(grid_df_bat_RotoitiII)

# add collum that indicates livable habitat (habitat is livable when grid_df_bat_Rotoiti$Depth is above the april_2022_data$mean_thermo_depth or when april_2022_data$mean_DOconc_overall is higher then value 5)
grid_df_bat_RotoitiII <- Bathymetry_Rotoiti_sf %>%
  mutate(Liveable_habitat = Depth < april_2022_data$mean_thermo_depth | april_2022_data$mean_DOconc_overall > 5)

# plot
ggplot() +
  geom_sf(data = Bathymetry_Rotoiti_sf, aes(color = factor(Liveable_habitat)), size = .5, show.legend = TRUE) +
  scale_color_manual(values = color_map, name = "Liveable Habitat", guide = guide_legend(override.aes = list(size = 2))) +
  ggtitle(paste(current_month,current_year, "- Livable Habitat:", round(percentage_livable, 0), "%"))+
  geom_sf(data =Rotoiti_Line)+
  theme_bw()+
  labs(caption = "Data Source: Limnotrack & BOPRC")


############################################################################## -

# Loop through each Year-Month combination in Below_summary_Rotoiti

# Initialize a data frame to store counts of TRUE and FALSE values
count_df <- data.frame(Year = integer(), Month = character(), TRUE_count = integer(), FALSE_count = integer(), stringsAsFactors = FALSE)

plots <- list()

# Define consistent colors for TRUE and FALSE
color_map <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")

for (i in 1:nrow(Below_summary_Rotoiti)) {
  # Extract the Year and Month for the current iteration
  current_year <- Below_summary_Rotoiti$Year[i]
  current_month <- Below_summary_Rotoiti$Month[i]
  
  # Filter data for the current Year and Month
  current_data <- Below_summary_Rotoiti %>%
    filter(Year == current_year, Month == current_month)
  
  # Calculate livable habitat for the current Year-Month
  Bathymetry_Rotoiti_sfII <- Bathymetry_Rotoiti_sf %>%
    mutate(Liveable_habitat = Depth < current_data$mean_thermo_depth | current_data$mean_DOconc_overall > 5)
  
  # Calculate counts of TRUE and FALSE
  true_count <- sum(Bathymetry_Rotoiti_sfII$Liveable_habitat, na.rm = TRUE)
  false_count <- sum(!Bathymetry_Rotoiti_sfII$Liveable_habitat, na.rm = TRUE)
  
  # Calculate percentage of livable habitat
  percentage_livable <- (true_count / 216066) * 100
  
  # Store counts and percentage in the data frame
  count_df <- rbind(count_df, data.frame(Year = current_year, Month = current_month, TRUE_count = true_count, FALSE_count = false_count, Percentage_Livable = percentage_livable, stringsAsFactors = FALSE))
  
  # Plot for the current Year-Month
  plot <- ggplot() +
    geom_sf(data = Bathymetry_Rotoiti_sfII, aes(color = factor(Liveable_habitat)), size = .5, show.legend = TRUE) +
    scale_color_manual(values = color_map, name = "Liveable Habitat", guide = guide_legend(override.aes = list(size = 2))) +
    ggtitle(paste(current_month,current_year, "- Livable Habitat:", round(percentage_livable, 0), "%", "- DOconc:",current_data$mean_DOconc_overall, "Mg/L - Thermo depth:", current_data$mean_thermo_depth))+
    geom_sf(data =Rotoiti_Line)+
    theme_bw()+
    labs(caption = "Data Source: Limnotrack & BOPRC")
  
  # Save plot to the figures folder with numbering
  ggsave(filename = paste0("Figures/Livable_habitat_ox/plot_", sprintf("%02d", i), "_", current_year, "_", current_month, ".png"), plot = plot)
  
  # Save plot to the list
  plots[[paste(current_month, current_year)]] <- plot
}

# Display the plots
plots[["Jul 2022"]]  # Example: to display the plot for April 2022

############################################################################## -
# Make it into a video using "magick"
# List all PNG files in the 'figures' folder
image_paths <- list.files(path = "Figures/Livable_habitat_ox", pattern = "\\.png$", full.names = TRUE)

# Sort the image paths if needed
image_paths <- sort(image_paths)

# Read images
images <- lapply(image_paths, image_read)

# Create an animation (video)
animation <- image_animate(image_join(images), fps = 1)

# Save as a GIF
image_write(animation, "Livable_habitat_Rotoiti.gif")
############################################################################## -


# gganimate


# Plot the temperature on the bottom of the lake for each day ------------------
# There are 840 days between 15 April 2022, and 1 August 2024.
# Total days in the data 712

# Filter data for April 2022 as a Test
April_15_2022 <- averaged_data %>%
  filter(Date == "2022-04-15")

head(April_15_2022)
head(Bathymetry_Rotoiti_sf)

Bathymetry_Rotoiti_sf2 <- Bathymetry_Rotoiti_sf %>%
  filter(Depth > 0 & Depth < 57)

# Join temperature data with bathymetry data
map_data <- Bathymetry_Rotoiti_sf2 %>%
  left_join(April_15_2022, by = c("Depth" = "DptSns_meter"))


ggplot(map_data) +
  geom_sf(aes(color = TmpWtr, geometry = geometry)) +
  scale_color_viridis_c(option = "plasma", name = "Temperature (°C)") +
  labs(title = "Lake Rotoiti",
       subtitle = "Temperature Distribution by depth on April 15, 2022",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()




# Get the unique dates in your dataset
unique_dates <- unique(averaged_data$Date)

# Create an empty list to store the plots (not used in final animation, but kept for reference)
plot_list <- list()

# Loop over each unique date to create a plot
for (i in seq_along(unique_dates)) {
  # Filter the data for the current date
  daily_data <- averaged_data %>%
    filter(Date == unique_dates[i])
  
  # Join temperature data with bathymetry data
  map_data <- Bathymetry_Rotoiti_sf2 %>%
    left_join(daily_data, by = c("Depth" = "DptSns_meter"))
  
  # Create the plot for the current date (for reference or separate saving)
  p <- ggplot(map_data) +
    geom_sf(aes(color = TmpWtr, geometry = geometry)) +
    scale_color_viridis_c(option = "plasma", name = "Temperature (°C)") +
    labs(title = "Lake Rotoiti",
         subtitle = paste("Temperature distribution by depth on", unique_dates[i]),
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
  
  # Store the plot in the list (optional for reference)
  plot_list[[i]] <- p
}

















#####
# Filter data for April 2022
april_2022_data <- mean_summary_Rotoiti %>%
  filter(Year == 2022, Month == "Apr")

# Add a column to define the color based on thermocline depth relative to the Depth value
grid_df_bat_Rotoiti <- grid_df_bat_Rotoiti %>%
  mutate(Stratification = ifelse(april_2022_data$mean_thermo_depth >= Depth, "Stratified", "Mixed"))

# Plot the data with colored points
ggplot(grid_df_bat_Rotoiti, aes(X, Y, col = Stratification)) +
  geom_point() +
  theme_bw()





# Define a function to calculate the color based on thermocline depth
calculate_color <- function(df, month, year, position_df) {
  month_data <- position_df %>%
    filter(Year == year, Month == month)
  
  if (nrow(month_data) > 0) {
    df <- df %>%
      mutate(Stratified = ifelse(month_data$mean_thermo_depth < Depth, "Yes", "No"))
  } else {
    df <- df %>%
      mutate(Stratified = NA) # If no data is available, set Stratified to NA
  }
  
  # Remove rows with NA thermo values
  df <- df %>%
    filter(!is.na(Stratified))
  
  return(df)
}

# Initialize an empty list to store plots
plot_list <- list()

# Loop over each unique year and month in the summary data
for (yr in unique(mean_summary_Rotoiti$Year)) {
  for (mo in unique(mean_summary_Rotoiti$Month)) {
    # Calculate the Stratified for the current month and year
    colored_df <- calculate_color(grid_df_bat_Rotoiti, mo, yr, mean_summary_Rotoiti)
    
    # Generate the plot for the current month and year
    p <- ggplot(colored_df, aes(X, Y, col = Stratified)) +
      geom_point() +
      ggtitle(paste(mo, yr)) +
      theme_bw()
    
    # Store the plot in the list
    plot_list[[paste(yr, mo, sep = "-")]] <- p
  }
}





# Define a function to calculate habitat suitability based on oxygen concentration at the bottom
calculate_habitat <- function(df, month, year, position_df) {
  month_data <- position_df %>%
    filter(Year == year, Month == month)
  
  if (nrow(month_data) > 0) {
    df <- df %>%
      # Join the oxygen data for the specified month and year
      left_join(month_data %>% select(mean_thermo_depth, Year, Month, mean_DOconc_overall), by = c("Year", "Month")) %>%
      # Calculate suitability based on oxygen concentration
      mutate(Suitable_Habitat = ifelse(mean_DOconc_overall >= 5, "Yes", "No"))
  } else {
    df <- df %>%
      mutate(Suitable_Habitat = NA) # If no data is available, set Suitable_Habitat to NA
  }
  
  # Remove rows with NA in Suitable_Habitat
  df <- df %>%
    filter(!is.na(Suitable_Habitat))
  
  return(df)
}

# Initialize an empty list to store plots
plot_list <- list()

# Loop over each unique year and month in the summary data
for (yr in unique(mean_summary_Rotoiti$Year)) {
  for (mo in unique(mean_summary_Rotoiti$Month)) {
    # Calculate the habitat suitability for the current month and year
    habitat_df <- calculate_habitat(grid_df_bat_Rotoiti, mo, yr, mean_summary_Rotoiti)
    
    # Generate the plot for the current month and year
    p <- ggplot(habitat_df, aes(X, Y, col = Suitable_Habitat)) +
      geom_point() +
      scale_color_manual(values = c("Yes" = "green", "No" = "red")) +
      ggtitle(paste(mo, yr)) +
      theme_bw() +
      labs(color = "Suitable Habitat")
    
    # Store the plot in the list
    plot_list[[paste(yr, mo, sep = "-")]] <- p
  }
}
