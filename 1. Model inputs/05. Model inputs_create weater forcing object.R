###################################################################################################
######## Living on the Edge: Modeling daily leaf energy and cabon balance #########################
############################ of Mediterranean Shrublands ##########################################
###################################################################################################

#R.version.string

rm(list=ls())#remove all global environment

library(readxl)
library(dplyr)
library(ggplot2)
library(terra)
library(tibble)
library(tidyr)
library(broom)
library(meteoland)
library(medfate)
library(medfateland)
library(sass)

### LOAD THE CREATED FINAL INPUTS OBJECT ###########################################################

load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/04. Model inputs_save final model inputs object.RData")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 5. Weather forcing ############################################################################
### Import the weather data from ERA5 2023-2024 ###################################################

# Load the GRIB file as a SpatRaster object
#Maximum and minimum temperature, precipitation, maximum and minimum relative humidity, radiation, wind speed are necessary
#The variable to download on the ERA5 database are the following: 
#var_names <- c("u_wind", "v_wind", "dewpoint", "temp", "precip", "solar")
#10m u-component of wind, 10m v-component of wind, 2m dewpoint temperature, 2m temperature, Total precipitation, Surface solar radiation downwards
#Solar radiation: Surface solar radiation downward: includes direct + diffuse
#For the year 2023-2024
#For the sub region of: 0.48, 2.26, 41.03, 42.29 (xmin, xmax, ymin, ymax)

meteo <- rast("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/meteorological data ERA5/data2023.grib")
meteo

meteo2 <- rast("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/meteorological data ERA5/data2024.grib")
meteo2

### Extract the meteorological data for our coordinates until March 2024 ##########################

library(terra)

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

# Convert to spatial points
coords_vect <- vect(coords_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
coords_vect

# Reproject the vector data to match the raster
coords_proj <- project(coords_vect, crs(meteo))

# Get time stamps from raster
timestamps <- time(meteo)

# Confirm length
length(timestamps)

# Set those as names
names(meteo) <- as.character(timestamps)

ext(meteo)

# Now extract without warning
extracted_vals <- terra::extract(meteo, coords_proj)

# ID is the first column, then every 2 columns are Precip/PET
hourly_data <- extracted_vals

#hourly_data <- hourly_data[, !grepl("^2024", names(hourly_data))]

### Calculation and cleaning of the table for the Medfate requirement _ hourly ####################

# Example: Suppose your file has 6 repeating variables per time step
#var_names <- c("u_wind", "v_wind", "dewpoint", "temp", "precip", "solar")
#10m u-component of wind, 10m v-component of wind, 2m dewpoint temperature, 2m temperature, Total precipitation, Surface solar radiation downwards
#Solar radiation: Surface solar radiation downward: includes direct + diffuse

long_hourly <- extracted_vals %>%
  pivot_longer(cols = -ID, names_to = "DateTime", values_to = "Value") %>%
  arrange(ID, DateTime) %>%
  mutate(Variable = rep(c("u_wind","v_wind", "dewpoint", "temp", "precip", "solar"), length.out = n()))

long_hourly$DateTime <- sub("\\.[0-9]+$", "", long_hourly$DateTime)

# Substract the month values
long_hourly$Time <- substr(long_hourly$DateTime, 12, 19)

long_hourly$Time[long_hourly$Time == ""] <- "00:00:00"

long_hourly <- long_hourly[!is.na(long_hourly$Time),]

wide_hourly <- long_hourly %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  arrange(ID, DateTime)

#Wind speed: don't forget to adjust from 10m to 2m from the ERA5 copernius satellite data: 
#eg. u2 <- u10 * log(2/1) / log(10/1)  # rough estimate
#And calculate the speed between both U and V wind

wide_hourly <- wide_hourly %>%
  mutate(u_wind2m = u_wind * log(2/1) / log(10/1)) %>%
  mutate(v_wind2m = v_wind * log(2/1) / log(10/1)) %>%
  mutate(wind_speed = sqrt(u_wind2m^2 + v_wind2m^2))

library(lubridate)

wide_hourly <- wide_hourly %>%
  mutate(dewpoint_C =dewpoint - 273.15, temp_C = temp - 273.15,precip_mm = precip * 1000, solar_MJ = (solar / 1e6) * 24,
         Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Date = date(DateTime))

wide_hourly <- wide_hourly %>%
  filter(Year != 2024)

### Extract the meteorological data for our coordinates for the whole 2024 ##########################

# Reproject the vector data to match the raster
coords_proj <- project(coords_vect, crs(meteo2))

# Get time stamps from raster
timestamps <- time(meteo2)

# Confirm length
length(timestamps)

# Set those as names
names(meteo2) <- as.character(timestamps)

ext(meteo2)
# Now extract without warning
extracted_vals <- terra::extract(meteo2, coords_proj)

# ID is the first column, then every 2 columns are Precip/PET
hourly_data <- extracted_vals

### Calculation and cleaning of the table for the Medfate requirement _ hourly ####################

# Example: Suppose your file has 6 repeating variables per time step
#var_names <- c("u_wind", "v_wind", "dewpoint", "temp", "precip", "solar")
#10m u-component of wind, 10m v-component of wind, 2m dewpoint temperature, 2m temperature, Total precipitation, Surface solar radiation downwards
#Solar radiation: Surface solar radiation downward: includes direct + diffuse

long_hourly <- extracted_vals %>%
  pivot_longer(cols = -ID, names_to = "DateTime", values_to = "Value") %>%
  arrange(ID, DateTime) %>%
  mutate(Variable = rep(c("u_wind","v_wind", "dewpoint", "temp", "precip", "solar"), length.out = n()))

# Substract the month values
long_hourly$Time <- substr(long_hourly$DateTime, 12, 19)

long_hourly$Time[long_hourly$Time == ""] <- "00:00:00"

long_hourly <- long_hourly[!is.na(long_hourly$Time),]

wide_hourly2 <- long_hourly %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  arrange(ID, DateTime)

#Wind speed: don't forget to adjust from 10m to 2m from the ERA5 copernius satellite data: 
#eg. u2 <- u10 * log(2/1) / log(10/1)  # rough estimate
#And calculate the speed between both U and V wind

wide_hourly2 <- wide_hourly2 %>%
  mutate(u_wind2m = u_wind * log(2/1) / log(10/1)) %>%
  mutate(v_wind2m = v_wind * log(2/1) / log(10/1)) %>%
  mutate(wind_speed = sqrt(u_wind2m^2 + v_wind2m^2))

library(lubridate)

wide_hourly2 <- wide_hourly2 %>%
  mutate(dewpoint_C =dewpoint - 273.15, temp_C = temp - 273.15, precip_mm = precip * 1000, solar_MJ = (solar / 1e6) * 24,
         Year = year(DateTime), Month = month(DateTime), Day = day(DateTime), Date = date(DateTime))

### Merge values until March 2024 and from March 2024 #############################################

str(wide_hourly)
str(wide_hourly2)

safety <- wide_hourly

#wide_hourly <- wide_hourly2
wide_hourly <- bind_rows(wide_hourly, wide_hourly2)

# Returns TRUE for any duplicated rows (after the first occurrence)
any(duplicated(wide_hourly))

colSums(is.na(wide_hourly))
summary(wide_hourly)

#ERA5_copernicus satellite, daily values + air temperature have to be corrected for elevation: e.g. 0.6°c per 100m height meter

#library(ecmwfr)
#library(raster)

#orog <- raster("ERA5_orography.nc")
#site_elev <- extract(orog, cbind(lon, lat))

#Additional variables (atmospheric pressure or CO2 concentration) are optional.

### Import the elevation of each tile from ERA5 ###################################################

#The elevation is in m²/s²: gravitational energy per unit mass, it's related to height but not height yet
#Values have to be divided by Gravity: 9.80665

elevationERA5 <- rast("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/elevationERA5.grib")
elevationERA5

### Extract the elevation data for our coordinates ################################################

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

# Convert to spatial points
coords_vect <- vect(coords_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
coords_vect

# Reproject the vector data to match the raster
coords_proj <- project(coords_vect, crs(elevationERA5))

# Get time stamps from raster
timestamps <- time(elevationERA5)

# Confirm length
length(timestamps)

# Set those as names
names(elevationERA5) <- as.character(timestamps)

ext(elevationERA5)

# Now extract without warning
extracted_vals <- terra::extract(elevationERA5, coords_proj)

extracted_vals <- extracted_vals[, 1:2]

elevationERA5 <- as.data.frame(extracted_vals)

elevationERA5

names(elevationERA5)[names(elevationERA5) == '2023-01-01'] <- 'elevationERA5'

elevationERA5 <- elevationERA5 %>%
  mutate(elevationERA5 = elevationERA5 / 9.80665)

### Correct the temperature for the elevation of our site #########################################

# Create a data frame of your coordinates
elevation_site <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  elevationsite = c(106.22, 431.31, 584.48, 638.92, 1298.37, 1525.65))

elevation_site <- merge(elevation_site, elevationERA5, by=c("ID"), all= TRUE, na="NA")

str(elevation_site)

# Calculate elevation difference
elevation_site$elevation_diff <- elevation_site$elevationsite - elevation_site$elevationERA5

# Apply lapse rate correction (0.6°C per 100 m)
lapse_rate <- 0.6 / 100  # °C per meter

wide_hourly <- merge(elevation_site, wide_hourly, by=c("ID"), all= TRUE, na="NA")

wide_hourly <- wide_hourly %>%
  mutate(temp = temp + elevation_diff * lapse_rate, 
         temp_C = temp_C + elevation_diff * lapse_rate)

str(wide_hourly)

### Import the surface pressure of each tile from ERA5 ############################################

surfacepressureERA5 <- rast("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/SurfacepressureERA5.grib")
surfacepressureERA5

### Extract the surface pressure for our coordinates ##############################################
#Values appear to be in Pa approx 98998.19 Pa which would be approx. 990 hPa

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

# Convert to spatial points
coords_vect <- vect(coords_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
coords_vect

# Reproject the vector data to match the raster
coords_proj <- project(coords_vect, crs(surfacepressureERA5))

# Get time stamps from raster
timestamps <- time(surfacepressureERA5)

# Confirm length
length(timestamps)

# Set those as names
names(surfacepressureERA5) <- as.character(timestamps)

#ext(surfacepressureERA5)

# Now extract without warning
extracted_vals <- terra::extract(surfacepressureERA5, coords_proj)

long_extracted_vals <- extracted_vals %>%
  pivot_longer(cols = -ID, names_to = "DateTime", values_to = "Value") %>%
  arrange(ID, DateTime) %>%
  mutate(Variable = rep(c("Surfacepressure"), length.out = n()))

# Substract the month values
long_extracted_vals$Time <- substr(long_extracted_vals$DateTime, 12, 19)

long_extracted_vals$Time[long_extracted_vals$Time == ""] <- "00:00:00"

long_extracted_vals <- long_extracted_vals[!is.na(long_extracted_vals$Time),]

names(long_extracted_vals)[names(long_extracted_vals) == 'Value'] <- 'Surfacepressure'

long_extracted_vals <- long_extracted_vals[, -c(4)]

wide_hourly <- merge(long_extracted_vals, wide_hourly, by=c("ID", "DateTime", "Time"), all= TRUE, na="NA")

### Correct dewpoint and relative humidity ########################################################

wide_hourly <- wide_hourly %>%
  mutate(e_grid = 611.2 * exp((17.67 * dewpoint_C) / (dewpoint_C + 243.5)),
         q = 0.622 * e_grid / (Surfacepressure - 0.378 * e_grid),
         pressure_site = Surfacepressure * exp(-elevation_diff / 8400))

wide_hourly <- wide_hourly %>%
  mutate(q = case_when(q <= 0 | is.na(q) ~ NA_real_, TRUE ~ q))

wide_hourly <- wide_hourly %>%
  mutate(e_site = (q * pressure_site) / (0.622 + 0.378 * q),
         dewpoint_corr_C = (243.5 * log(e_site / 611.2)) / (17.67 - log(e_site / 611.2)))

wide_hourly <- wide_hourly %>%
  mutate(dewpoint_corr_C = ifelse(is.na(dewpoint_corr_C), dewpoint_C, dewpoint_corr_C))

### Calculation of dew point and relative humidity with corrected T values ########################

#ERA5_copernicus satellite, daily values + air temperature are already corrected for elevation: e.g. 0.6°c per 100m height meter

wide_hourly <- wide_hourly %>%
  mutate(es = 0.6108 * exp((17.27 * temp_C) / (temp_C + 237.3)),
         ea = 0.6108 * exp((17.27 * dewpoint_corr_C) / (dewpoint_corr_C + 237.3)), 
         RelativeHumidity = pmin(100, 100 * ea / es))

wide_hourly %>%
  summarise(max_RH = max(RelativeHumidity, na.rm = TRUE),
            max_Td_minus_T = max(dewpoint_corr_C - temp_C, na.rm = TRUE))

str(wide_hourly)

wide_hourly <- wide_hourly[!is.na(wide_hourly$elevationsite),]

#wide_hourly$Year <- substr(wide_hourly$DateTime, 1, 4)

wide_daily <- wide_hourly %>%
  group_by(ID, Date, Year) %>%
  summarise(MinTemperature = min(temp_C),
            MinTemp_hour = DateTime[which.min(temp_C)][1],            
            MaxTemperature = max(temp_C),
            MaxTemp_hour = DateTime[which.max(temp_C)][1],
            Precipitation = sum(precip_mm),
            Radiation = mean(solar_MJ),
            Windspeed = mean(wind_speed),
            MinRelativeHumidity = min(RelativeHumidity),
            MaxRelativeHumidity = max(RelativeHumidity)) %>%
  rename(dates = Date)

#ERA5_copernicus satellite, daily values + air temperature have to be corrected for elevation: e.g. 0.6°c per 100m height meter

#library(ecmwfr)
#library(raster)

#orog <- raster("ERA5_orography.nc")
#site_elev <- extract(orog, cbind(lon, lat))

#Additional variables (atmospheric pressure or CO2 concentration) are optional.

### Correct the precipitation of our site #########################################################

wide_daily <- wide_daily %>%
  mutate(Precipitation = ifelse(Precipitation < 0.1, 0, rep(Precipitation, each = 1)))

wide_daily

### Save the merge data from this database ########################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/"

# SAVE OUTPUT FILES 

#write.csv(wide_daily, file = file.path(dir_out, "Meteodata.csv"), row.names = FALSE)

### Save the weather forcing using the selected data ##############################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "05. Model inputs_save weather forcing.RData")
