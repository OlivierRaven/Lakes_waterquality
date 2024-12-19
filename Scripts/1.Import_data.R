# 1. Import data Water quality lakes in NZ
# Explanation of this script------------------------------------------------
 
# Import the 4 data sets that i have available
# All_WQ                   = combindation of lawa data+ WQ master + Okaro data
# lawa_monitoring_data     = from the LAWA Site (2004-2022)
# WQ_master_dataframe_2021 = Water quality BOP Lakes (2000-2016)
# WQ_Okaro_BOPRC           = waterquality data Lake okaro (2018-2024)
# lawa_TLI                 = TLI From the LAWA Site (2008-2022)
# CTD_profile_Data_BOPRC   = CTD Data from the BOP lakes (2003-2022)



# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Lakes/Lakes_waterquality")

# Define the list of packages
packages <- c("writexl","readxl","sf", "lubridate","readr", "tidyverse", "dplyr", "ggplot2", "rLakeAnalyzer")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the Data sets ---------------------------------------------------------
#All_WQ <- read_csv("Data_raw/All_WQ.csv")
All_WQ <- read_excel("Data_raw/All_WQ.xlsx")

lawa_monitoring_data <- read.csv("Data_raw/lawa-monitoring-data.csv")

WQ_master_dataframe_2021 <- read_csv("Data_raw/WQ_master_dataframe_2021.csv")

WQ_Okaro_BOPRC <- read_csv("Data_raw/WQ_Okaro_BOPRC.csv")

lawa_TLI <- read_csv("Data_raw/lawa-TLI.csv")

#CTD_profile_Data_BOPRC <- read_csv("Data_raw/CTD_profile_Data_BOPRC.csv")
CTD_profile_Data_BOPRC <- read_excel("Data_raw/CTD_profile_Data_BOPRC.xlsx")

# Visualize and summaries the data ---------------------------------------------
# Inspect the structure and summary of the data sets
str(lawa_monitoring_data)
summary(lawa_monitoring_data)
head(lawa_monitoring_data)

str(WQ_master_dataframe_2021)
summary(WQ_master_dataframe_2021)
head(WQ_master_dataframe_2021)

str(lawa_TLI)
summary(lawa_TLI)
head(lawa_TLI)

str(CTD_profile_Data_BOPRC)
summary(CTD_profile_Data_BOPRC)
head(CTD_profile_Data_BOPRC)

# Check for missing values
colSums(is.na(lawa_monitoring_data))
colSums(is.na(WQ_master_dataframe_2021))
colSums(is.na(lawa_TLI))
colSums(is.na(CTD_profile_Data_BOPRC))


# Fix the ALL_WQ data set ------------------------------------------------------
WQ <- All_WQ
# Convert Date to Date format & Extract months and years
WQ$Date <- as.Date(WQ$Date , format = "%d/%m/%Y")
WQ$Month <- month(WQ$Date, label = TRUE)
WQ$Year <- year(WQ$Date)

names(WQ)
unique(WQ$Parameter)

# rename Lakes
WQ <- WQ %>%
  mutate(Lake = case_when(
    Lake == "Lake Rotorua"        ~ "Lake Rotorua",
    Lake == "Lake Rotoiti"        ~ "Lake Rotoiti",
    Lake == "Lake Rotoehu"        ~ "Lake Rotoehu",
    Lake == "Lake Rotoma"         ~ "Lake Rotomā",
    Lake == "Lake Okataina"       ~ "Lake Ōkataina",
    Lake == "Lake Okareka"        ~ "Lake Ōkāreka",
    Lake == "Lake Tikitapu"       ~ "Lake Tikitapu",
    Lake == "Lake Tarawera"       ~ "Lake Tarawera",
    Lake == "Lake Rotokakahi"     ~ "Lake Rotokākahi",
    Lake == "Lake Rotomahana"     ~ "Lake Rotomāhana",
    Lake == "Lake Okaro"          ~ "Lake Ōkaro",
    Lake == "Lake Rerewhakaaitu"  ~ "Lake Rerewhakaaitu",
    TRUE                          ~ Lake))

# rename Site
WQ <- WQ %>%
  mutate(Site = case_when(Site == "Okawa Bay"  ~ "Ōkawa Bay",TRUE~ Site))

# Rename Parameters
WQ <- WQ %>%
  mutate(
    Value = case_when(
      Parameter == "TP (g/m3)" ~ Value * 1000,
      Parameter == "TN (g/m3)" ~ Value * 1000,
      Parameter == "NH4N (mg/L)"  ~ Value * 1000,
      TRUE ~ Value
    ),
    Parameter = case_when(
      Parameter == "NNN (mg/m3)"       ~ "NNN (mg/m³)",
      Parameter == "TP (mg/m3)"        ~ "TP (mg/m³)",
      Parameter == "Chla (mg/m3)"      ~ "Chla (mg/m³)",
      Parameter == "NH4-N (mg/m3)"     ~ "NH4-N (mg/m³)",
      Parameter == "pH"                ~ "pH",
      Parameter == "TN (mg/m3)"        ~ "TN (mg/m³)",
      Parameter == "Turb (NTU)"        ~ "Turb (NTU)",
      Parameter == "DRP (mg/m3)"       ~ "DRP (mg/m³)",
      Parameter == "SecchiDepth (m)"   ~ "Secchi (m)",
      Parameter == "Secchi (m)"        ~ "Secchi (m)",
      Parameter == "pH (-log[H+])"     ~ "pH",
      Parameter == "NH4N (mg/L)"       ~ "NH4-N (mg/m³)",  # After conversion
      Parameter == "ECOLI (#/100 mL)"  ~ "E. coli (#/100 mL)",
      Parameter == "CHLA (mg/m3)"      ~ "Chla (mg/m³)",
      Parameter == "TSS (g/m3)"        ~ "TSS (g/m³)",
      Parameter == "VC - SD (m)"       ~ "Secchi (m)",
      Parameter == "TP (g/m3)"         ~ "TP (mg/m³)",  # After conversion
      Parameter == "Depth (m)"         ~ "Depth (m)",
      Parameter == "TN (g/m3)"         ~ "TN (mg/m³)",  # After conversion
      Parameter == "Turb (NTU)"        ~ "Turb (NTU)",
      Parameter == "Turbidity (NTU)"   ~ "Turb (NTU)",
      TRUE                             ~ Parameter))

# Remove duplicates based on specified columns
WQ_II <- WQ %>%
  distinct(Lake, Site, Date, Parameter, Value, .keep_all = TRUE)


# Save lake specific data --------------------------

# Okaro
Okaro_wq <- WQ_II%>% filter(Lake == 'Lake Ōkaro')
write_xlsx(Okaro_wq, "Data_mod/Okaro_wq.xlsx")

Okaro_CTD <- CTD_profile_Data_BOPRC%>% filter(Lake == 'Lake Ōkaro')
write_xlsx(Okaro_CTD, "Data_mod/Okaro_CTD.xlsx")


# Rotoiti
Rotoiti_wq <- filter(WQ_II, Lake == "Lake Rotoiti")
write_xlsx(Rotoiti_wq, "Data_mod/Rotoiti_wq.xlsx")

Rotoiti_CTD <- CTD_profile_Data_BOPRC%>% filter(Lake == 'Lake Rotoiti')
write_xlsx(Rotoiti_CTD, "Data_mod/Rotoiti_CTD.xlsx")







