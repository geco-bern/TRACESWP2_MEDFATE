###################################################################################################
######## Living on the Edge: Modeling daily leaf energy and cabon balance #########################
############################ of Mediterranean Shrublands ##########################################
###################################################################################################

#R.version.string

rm(list=ls())#remove all global environment

library(readxl)
library(rsconnect, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")
#library(lifecycle, lib.loc = "/storage/software/epyc2.9/software/R/4.5.1-gfbf-2025a/lib64/R/library")
library(rlang)
library(dplyr, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")

library(ggplot2)
library(terra)
library(tibble)
library(tidyr)
library(broom)

library(meteoland)
library(medfate)
library(medfateland)
library(sass)

getwd()
setwd("/storage/homefs/md26i579/GitHub/geco-bern/TRACESWP2_MEDFATE")

### LOAD THE CREATED FOREST OBJECT ################################################################

load("1. Model inputs/01. Model inputs_save forest object.RData")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 2. Create a soil object with the soil data ####################################################
### Import soil data from Eurofins: 0 - 20 cm #####################################################

TRACES_soil_eurofins <- read_excel("0. Data gathering/2. Soil object/TRACES_soileurofins.xlsx")
#View(TRACES_soil_eurofins)

TRACES_soil_eurofins <- TRACES_soil_eurofins[, -c(4,6,8,9,11,12,14,15,17,18)]

### Import soil data from SoilGrid: 0 - 200 cm ####################################################

library(sf)

#httr::GET("https://rest.isric.org/soilgrids/v2.0/properties/query?lon=0.607788&lat=41.15909") #This path does not work: GATEAWAY Time out

#x <- st_sf(geometry = st_sfc(st_point(c(0.607788, 41.159089)), crs = 4326))

#x_soil <- add_soilgrids(x, soilgrids_path = NULL, widths = c(300, 700, 1000))
#x_soil

#PAS = 41.159089, 0.607788
#ALO = 41.907495, 0.952325
#GAR = 41.350724, 1.887764
#CIN = 41.720588, 2.250483
#MON = 42.032490, 0.768946
#PEN = 42.280445, 1.111793

# Define your sites (name, lat, lon)
sites <- data.frame(
  name = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
  lat  = c(41.159089, 41.907495, 41.350724, 41.720588, 42.032490, 42.28044),
  lon  = c(0.607788, 0.952325, 1.887764, 2.250483, 0.768946, 1.11179))

# Convert to sf points
sites_sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)

# Collect the data for each site separately
pas <- st_sf(geometry = st_sfc(st_point(c(0.607788, 41.159089)), crs = 4326))
pas_soil <- add_soilgrids(pas, soilgrids_path = NULL, widths = c(300, 700, 1000))

alo <- st_sf(geometry = st_sfc(st_point(c(0.952325, 41.907495)), crs = 4326))
alo_soil <- add_soilgrids(alo, soilgrids_path = NULL, widths = c(300, 700, 1000))

gar <- st_sf(geometry = st_sfc(st_point(c(1.887764, 41.350724)), crs = 4326))
gar_soil <- add_soilgrids(gar, soilgrids_path = NULL, widths = c(300, 700, 1000))

cin <- st_sf(geometry = st_sfc(st_point(c(2.250483, 41.720588)), crs = 4326))
cin_soil <- add_soilgrids(cin, soilgrids_path = NULL, widths = c(300, 700, 1000))

mon <- st_sf(geometry = st_sfc(st_point(c(0.768946, 42.032490)), crs = 4326))
mon_soil <- add_soilgrids(mon, soilgrids_path = NULL, widths = c(300, 700, 1000))

pen <- st_sf(geometry = st_sfc(st_point(c(1.111793, 42.280445)), crs = 4326))
pen_soil <- add_soilgrids(pen, soilgrids_path = NULL, widths = c(300, 700, 1000))

# Add a site name to each dataset
pas_soil$name <- "PAS_ASE"
alo_soil$name <- "ALOS"
gar_soil$name <- "GARRAF"
cin_soil$name <- "CINGLES"
mon_soil$name <- "MONTSEC"
pen_soil$name <- "PENTINA"

# Combine them all into one sf object
TRACES_soil_soilgrid <- bind_rows(pas_soil, alo_soil, gar_soil, cin_soil, mon_soil, pen_soil)

# Check the combined data
TRACES_soil_soilgrid

# Select only the variable #vars = c("bdod", "cfvo") : bulk density and fragement that I missing from our personnal measurement

library(purrr)
library(tidyr)
library(tidyverse)

TRACES_soil_soilgrid <- TRACES_soil_soilgrid %>%
  st_drop_geometry() %>%                     # remove geometry for a regular table
  mutate(soil = map2(soil, name, ~mutate(.x, name = .y))) %>%  # add site name inside each nested df
  dplyr::select(soil) %>%
  unnest(soil)                               # expand all nested data.frames

TRACES_soil_soilgrid <- TRACES_soil_soilgrid %>%
  dplyr::select(name, widths, bd, rfc)

TRACES_soil_soilgrid <- TRACES_soil_soilgrid %>%
  rename(Site_name = name)

TRACES_soil_soilgrid <- TRACES_soil_soilgrid %>%
  mutate(bd = bd) %>% #Change from  cg/cm3 (SoilGrids units) to g/cm3 (Medfate application)
  mutate(rfc = rfc) #change from cm³/dm³ (soilgrids units) in whole-soil volume (Medfate application)

### Merge Eurofins and soilGrid datas #############################################################

TRACES_soil <- merge(TRACES_soil_eurofins, TRACES_soil_soilgrid, by=c("Site_name"), all= TRUE, na="NA") #Does add all the layers (widths) for all parameters measured at eurofins: use same values for all layers

### Rename the soil textural class to Medfate writing style #######################################

str(TRACES_soil)

TRACES_soil <- TRACES_soil %>%
  mutate(`Texture Type` = ifelse(`Texture Type` == "Sandy Loam", "sandy loam", rep(`Texture Type`, each = 1))) %>%
  mutate(`Texture Type` = ifelse(`Texture Type` == "Sandy Clay Loam", "sandy clay loam", rep(`Texture Type`, each = 1))) %>%
  mutate(`Texture Type` = ifelse(`Texture Type` == "Loam", "loam", rep(`Texture Type`, each = 1)))

#Texture Type must be one of MEDFATE’s accepted textural classes (e.g., "sand", "loamy sand", "sandy loam", "loam", "clay loam", "sandy clay loam", "silty clay loam", "silty clay", "clay").

### Save the merge data from both databases #######################################################

#Check your working directory
getwd()

#SAVE OUTPUT FILES 

write.csv(TRACES_soil, file = "0. Data gathering/2. Soil object/TRACES_soil.csv")

### Conversion to soil and list ###################################################################

SoilParams <- data.frame(
  Code_site = TRACES_soil$Code_site,
  widths = TRACES_soil$widths,   # layer thickness (example: upper/lower soil layers)
  clay = TRACES_soil$Clay,
  sand = TRACES_soil$Sand,
  om = TRACES_soil$`Organic Carbon (%)`,   # organic matter (can also use Oxidizable Organic Matter)
  nitrogen = TRACES_soil$`Kjeldahl Nitrogen (%)`,
  usda = TRACES_soil$`Texture Type`,
  bd = TRACES_soil$bd,       # bulk density (example, you can replace with measured)
  rfc = TRACES_soil$rfc)         # relative field capacity (example values)

str(SoilParams)

### Put directly the Soil param to the initalisation ##############################################

examplesoil <- soil(SoilParams)
class(examplesoil)

### Create a list of soil() objects, one per site #################################################

SoilList <- lapply(unique(SoilParams$Code_site), function(site) {
  site_data <- SoilParams %>% dplyr::filter(Code_site == site)
  site_soil <- soil(site_data)
  return(site_soil)
})

# Name the list elements with site codes
names(SoilList) <- unique(SoilParams$Code_site)

str(SoilList)

### Put directly the Soil param to the initalisation, without soil(Soilparams) ####################

soilPAS <- SoilList$`1`
class(soilPAS)

soilALO <- SoilList$`2`
class(soilALO)

soilGAR <- SoilList$`3`
class(soilGAR)

soilCIN <- SoilList$`4`
class(soilCIN)

soilMON <- SoilList$`5`
class(soilMON)

soilPEN <- SoilList$`6`
class(soilPEN)

# Create a named soil list from SoilList
soil_names <- c("PAS", "ALO", "GAR", "CIN", "MON", "PEN")
soil_list <- setNames(SoilList, soil_names)

# Check classes
lapply(soil_list, class)

# Example access
soil_list$PAS  # equivalent to SoilList$`1`
soil_list$ALO  # equivalent to SoilList$`2`

### Save the soil object using the selected data ##################################################

#Check your working directory
getwd()

# Save everything in the current environment
save.image(file = "1. Model inputs/02. Model inputs_save soil object.RData")
