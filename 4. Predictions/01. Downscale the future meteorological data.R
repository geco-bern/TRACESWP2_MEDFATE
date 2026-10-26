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

### LOAD THE CREATED FINAL INPUTS OBJECT ##########################################################

source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/03. Model_Evaluating model performance.R")

best_TAW_info <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/best_TAW_info.xlsx")
#View(best_TAW_info)

###################################################################################################
### VI. Predictions ###############################################################################
###################################################################################################

### 2. Down-scale the future meteorological data ##################################################

#Future regional future climate data was download from CORDEX regional climate model data on single levels (https://cds.climate.copernicus.eu/datasets/projections-cordex-domains-single-levels?tab=download#temporal_resolution)
#Selection of the parameters are the following: 
#Domain: Europe; Experiment: RCP 8.5; Horizontal resolution: 0.11 degree * 0.11 degree
#Temporal resolution: Daily mean; 
#Variable: 2m surface specific humidity - 10m wind speed - Maximum 2m temperature in the last 24 hours = Minimum 2h temperature in the last 24 hours
#Mean precipitation flux - surface pressure - surface solar radiation downwards
#Global climate model: MPI-M-MPI-ESM-LR (Germany)
#Regional climate model: MPI-CSC-REMO2009 (Germany)
#Ensemble member: r2i1p1
#Start year: 2066 and 2071
#End year: 2070 and 2075

library(ncdf4)

### Open the file for the maximum temperature and extract values for the sites ####################

FutureTmax <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/tasmax.nc")

names(FutureTmax$var)

lat <- ncvar_get(FutureTmax, "lat")
lon <- ncvar_get(FutureTmax, "lon")
time <- ncvar_get(FutureTmax, "time")
tmax <- ncvar_get(FutureTmax, "tasmax")

time_units <- ncatt_get(FutureTmax, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- tmax[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Tmax = values
  )
})

Future_final_Tmax <- do.call(rbind, results)

head(Future_final_Tmax)

### Open the file for the minimum temperature and extract values for the sites ####################

FutureTim <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/tasmin.nc")

names(FutureTim$var)

lat <- ncvar_get(FutureTim, "lat")
lon <- ncvar_get(FutureTim, "lon")
time <- ncvar_get(FutureTim, "time")
tmin <- ncvar_get(FutureTim, "tasmin")

time_units <- ncatt_get(FutureTim, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- tmin[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Tmin = values
  )
})

Future_final_Tmin <- do.call(rbind, results)

head(Future_final_Tmin)

### Open the file for the precipitation temperature and extract values for the sites ##############

FuturePrecip <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/precip.nc")

names(FuturePrecip$var)

lat <- ncvar_get(FuturePrecip, "lat")
lon <- ncvar_get(FuturePrecip, "lon")
time <- ncvar_get(FuturePrecip, "time")
precip <- ncvar_get(FuturePrecip, "pr")

time_units <- ncatt_get(FuturePrecip, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- precip[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Precip = values
  )
})

Future_final_Precip <- do.call(rbind, results)

head(Future_final_Precip)

### Merge the file together #######################################################################

Future_final <- merge(Future_final_Tmax, Future_final_Tmin, by=c("Date", "Site"), all= TRUE, na="NA")
Future_final <- merge(Future_final, Future_final_Precip, by=c("Date", "Site"), all= TRUE, na="NA")

head(Future_final)

Future_final <- Future_final %>%
  mutate(Tmax = Tmax - 273.15, #Transform from K to °C
         Tmin = Tmin - 273.15, 
         Precip = Precip * 86400, 
         Tmean = (Tmax + Tmin) /2) #Transform from flux to mm

### Correct for elevation and for precipitation ###################################################

Future_final <- Future_final %>%
  mutate(Precip = ifelse(Precip < 0.1, 0, rep(Precip, each = 1)))

### Select only the year 2069 and 2070 which are the extrems ######################################

Future_final <- Future_final %>%
  mutate(Year = year(Date))

Future_final <- Future_final %>%
  filter(Year %in% c(2069, 2070))

Future_final_t <- Future_final

### Merge with the current weather data ###########################################################

meteo2 <- meteo[, -c(4,5,7,9:12)]

meteo2 <- meteo2 %>%
  mutate(Site = case_when(ID == 1 ~ "PAS", ID == 2 ~ "ALO", ID == 3 ~ "GAR",
      ID == 4 ~ "CIN", ID == 5 ~ "MON", ID == 6 ~ "PEN", TRUE ~ NA_character_))

names(meteo2)[names(meteo2) == 'dates'] <- 'Date'
names(meteo2)[names(meteo2) == 'MaxTemperature'] <- 'Tmax'
names(meteo2)[names(meteo2) == 'Precipitation'] <- 'Precip'

meteo2 <- meteo2 %>%
  mutate(Scenario = "Observed")

Future_final <- Future_final %>%
  mutate(Scenario = "Future")

Future_final <- merge(Future_final, meteo2, by=c("Date", "Year", "Site", "Tmax", "Precip", "Scenario"), all= TRUE, na="NA")

### Create a graph with the data ##################################################################

order <- c("PAS", "ALO", "GAR", "CIN", "MON", "PEN")
Future_final$Site <- factor(Future_final$Site, levels = order)

order2 <- c("Observed", "Future")
Future_final$Scenario <- factor(Future_final$Scenario, levels = order2)

Future_final2 <- Future_final %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  mutate(TimeIndex = row_number(), TimeIndex = ifelse(Scenario == "Future",
         TimeIndex + 100,   # small gap between observed & future
         TimeIndex)) %>%
  ungroup()

Weather <- ggplot(Future_final2, aes(x = TimeIndex)) +
  geom_rect(aes(xmin = 171, xmax = 263, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 537, xmax = 629, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 1002, xmax = 1094, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 1367, xmax = 1459, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_col(aes(y = Precip / 4, fill = Scenario), alpha = 0.8) +
  geom_line(aes(y = Tmax, color = Scenario), linewidth = 0.2, alpha = 0.8) +
  facet_wrap(~ Site) +
  scale_y_continuous(name = "Temperature (°C)", limits = c(0, NA),
                     sec.axis = sec_axis(~ . * 4, name = "Precipitation (mm)")) +
  scale_fill_manual(values = c("#5c52cbff", "#5c52cbff"), label = c("Observed", "Future")) +
  scale_color_manual(values = c("#737373", "#737373"), label = c("Observed", "Future")) +
  scale_x_continuous(breaks = c(217, 583, 1048, 1413), labels = c(2023, 2024, 2069, 2070)) +
  theme_bw() +
  theme(text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none", 
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_blank(), 
        panel.spacing = unit(0.5, "lines"),
        strip.text = element_text(face = "bold", size = 18),  # <- bigger
        strip.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(fill = NA, color = "black"))

Weather

#ggsave("Weather.pdf", plot = Weather, device = cairo_pdf, width = 35, height = 18, units = "cm")

### Open the file for the radiation and extract values for the sites ##############################

FutureRad <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/rad.nc")

names(FutureRad$var)

lat <- ncvar_get(FutureRad, "lat")
lon <- ncvar_get(FutureRad, "lon")
time <- ncvar_get(FutureRad, "time")
rad <- ncvar_get(FutureRad, "rsds")

time_units <- ncatt_get(FutureRad, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- rad[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Radiation = values
  )
})

Future_final_Rad <- do.call(rbind, results)

head(Future_final_Rad)

#Future_final_Rad <- Future_final_Rad %>%
 #mutate(Radiation_corr = (Radiation / 1e6) * 24)

### Open the file for the specific humidity and extract values for the sites ######################

gc()

FutureSpHum <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/specifichumidity.nc")

names(FutureSpHum$var)

lat <- ncvar_get(FutureSpHum, "lat")
lon <- ncvar_get(FutureSpHum, "lon")
time <- ncvar_get(FutureSpHum, "time")
sphum <- ncvar_get(FutureSpHum, "huss")

time_units <- ncatt_get(FutureSpHum, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- sphum[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Specific_humidity = values
  )
})

Future_final_SpecificHumidity <- do.call(rbind, results)

head(Future_final_SpecificHumidity)

### Open the file for the surface pressure and extract values for the sites ######################

gc()

FuturePressure <- nc_open("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/futureweather2066_2070/pressure.nc")

names(FuturePressure$var)

lat <- ncvar_get(FuturePressure, "lat")
lon <- ncvar_get(FuturePressure, "lon")
time <- ncvar_get(FuturePressure, "time")
pressure <- ncvar_get(FuturePressure, "ps")

time_units <- ncatt_get(FuturePressure, "time", "units")$value
time_origin <- sub("days since ", "", time_units)

dates <- as.Date(time, origin = time_origin)

get_point_index <- function(target_lon, target_lat, lon, lat) {
  dist <- (lon - target_lon)^2 + (lat - target_lat)^2
  which(dist == min(dist), arr.ind = TRUE)
}

# Create a data frame of your coordinates
coords_df <- data.frame(
  Name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  Longitude = c(0.607788, 0.952325, 1.887764, 2.250595, 0.768946, 1.111793), #TAKE CARE AT CINGLES, OFTEN MODEL DO NOT LIKE THE GPS POINT 
  Latitude = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.280445))

#Extract the time series for all sites
results <- lapply(1:nrow(coords_df), function(k) {
  
  idx <- get_point_index(
    coords_df$Longitude[k],
    coords_df$Latitude[k],
    lon, lat
  )
  
  i <- idx[1]
  j <- idx[2]
  
  values <- pressure[i, j, ]  # dimension order: [lon, lat, time] (most likely)
  
  data.frame(
    Site = coords_df$Name[k],
    Date = dates,
    Pressure = values
  )
})

Future_final_Pressure <- do.call(rbind, results)

head(Future_final_Pressure)

### Merge all the files together ##################################################################

gc()

Future_final <- merge(Future_final_Tmax, Future_final_Tmin, by=c("Date", "Site"), all= TRUE, na="NA")
Future_final <- merge(Future_final, Future_final_Precip, by=c("Date", "Site"), all= TRUE, na="NA")
Future_final <- merge(Future_final, Future_final_Rad, by=c("Date", "Site"), all= TRUE, na="NA")
Future_final <- merge(Future_final, Future_final_SpecificHumidity, by=c("Date", "Site"), all= TRUE, na="NA")
Future_final <- merge(Future_final, Future_final_Pressure, by=c("Date", "Site"), all= TRUE, na="NA")

head(Future_final)

Future_final <- Future_final %>%
  mutate(Tmax = Tmax - 273.15, #Transform from K to °C
         Tmin = Tmin - 273.15, 
         Precip = Precip * 86400) #Transform from flux to mm

Future_final <- Future_final %>%
  mutate(Precip = ifelse(Precip < 0.1, 0, rep(Precip, each = 1)))

names(Future_final)[names(Future_final) == 'Tmin'] <- 'MinTemperature'
names(Future_final)[names(Future_final) == 'Tmax'] <- 'MaxTemperature'
names(Future_final)[names(Future_final) == 'Precip'] <- 'Precipitation'

head(Future_final)

### Make the calculation to have all the data as I want ############################################

Future_final <- Future_final %>%
  mutate(MeanTemperature = (MaxTemperature + MinTemperature)/2)

Future_final <- Future_final %>%
  mutate(Pressure = Pressure / 100)

Future_final <- Future_final %>%
  mutate(e = (Specific_humidity * Pressure)/ 0.622 + (1 - 0.622) * Pressure, 
         es = 6.112 * exp((17.67 * MeanTemperature) / (MeanTemperature + 243.5)) * 100)

Future_final <- Future_final %>%
  mutate(RelativeHumidity =  100 * (e / es))

Future_final$RelativeHumidity <- pmin(pmax(Future_final$RelativeHumidity, 0), 100)

head(Future_final)

Future_final <- Future_final %>%
  mutate(MinRelativeHumidity = RelativeHumidity - 10,
         MaxRelativeHumidity = RelativeHumidity + 10)

Future_final$MaxRelativeHumidity <- pmin(pmax(Future_final$MaxRelativeHumidity, 0), 100)
Future_final$MinRelativeHumidity <- pmin(pmax(Future_final$MinRelativeHumidity, 0), 100)

### Save the merge data from this database ########################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"

# SAVE OUTPUT FILES 
write.csv(Future_final, file = file.path(dir_out, "Futuremeteo.csv"), row.names = FALSE)

