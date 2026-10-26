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

### LOAD THE CREATED FINAL INPUTS OBJECT & WEATHER FORCING ########################################

#load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/05. Model inputs_save weather forcing.RData")

#source("01. Model inputs_create forest object.R")
#source("02. Model inputs_create soil object.R")
#source("03. Model inputs_define default parameters.R")
#source("04. Model inputs_create input object.R")
#source("05. Model inputs_create weater forcing object.R")

#list.files("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/")

source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/05. Model inputs_create weater forcing object.R")

###################################################################################################
### II. Forest water and energy balance (practice) ################################################
###################################################################################################

### 1. Water balance input object #################################################################
### Check what’s inside a single forest object ####################################################

names(PAS_Ar)

PAS_Ar$soil
GAR_Ar$soil

PAS_Ar$cohorts
GAR_Ar$cohorts

PAS_Ar$above
GAR_Ar$above

PAS_Ar$below
GAR_Ar$below

PAS_Ar$paramsTranspiration #This parameters are the same for all sites among the same species, 
GAR_Ar$paramsTranspiration #there is just a difference between species

GAR_Bu$paramsTranspiration #See with the one above

### Adjust the Transpiration parameters using the collected data in 2024 ##########################
#### Subsection: Description of the parameters used from the model ################################

#Parameters which are used: 

#Gswmin: Leaf residual conductance; Gswmax: Maximum stomatal conductance; Gsw_AC_slope: slope gs–assimilation coupling;
#Gs_P50; Water potential at 50% loss of stomatal conductance; Gs_slope: slope stomatal response to water potential; 

#VCleaf_P50: Leaf xylem P50; VCleaf_slope: Slope of the vulnerability curve; 
#VCstem_P50: Stem xylem P50; VCstem_slope: Slope of the vulnerability curve; 
#VCroot_P50: Root xylem P50; VCroot_slope: Slope of the vulnerability curve; 

#Kmax_stemxylem: Max stem hydraulic conductivity; Kmax_rootxylem: Max root hydraulic conductivity;
#VCleaf_kmax: Leaf max conductivity; VCstem_kmax: Stem max conductivity; VCroot_kmax: Root max conductivity; Plant_kmax: Whole-plant conductance 

#VCleafapo_kmax-  VGrhizo_kmax: NO IDEA WHAT'S THAT

#Vmax298: Max Rubisco capacity; Jmax298: Max electron transport

#VCleaf_c & VCleaf_d: Leaf capacitance; VCstem_c  VCstem_d: Stem capacitance; VCroot_c  VCroot_d: root capacitance

#FR_leaf: SLA or Huber value; FR_stem: Stem specific density; FR_root: soil rooting context
#kleaf_symp & kstem_symp: Symplastic conductivity inferred from RWC and capacitance



### 2. Basic water balance ########################################################################
### Open the appropriate weather data frame #######################################################

str(wide_daily)

meteo <- as.data.frame(wide_daily)
str(meteo)
head(meteo)  # to preview the first few rows
#summary(meteo)  # get a summary of each column

### Create a meteo dataframe for each site ########################################################

meteo <- meteo[complete.cases(meteo), ]

# Mapping of ID to variable names
meteo_names <- c("1" = "meteoPAS", "2" = "meteoALO", "3" = "meteoGAR", "4" = "meteoCIN", "5" = "meteoMON", "6" = "meteoPEN")

# Loop over each ID to create separate data.frames
for(id in names(meteo_names)) {
  
  # Subset data for this ID
  meteo_subset <- subset(meteo, ID == as.numeric(id))
  
  # Remove the ID column
  meteo_subset$ID <- NULL
  # Or: meteo_subset <- meteo_subset[, -1] if ID is the first column
  
  # Assign to variable in global environment
  assign(meteo_names[[id]], meteo_subset)}

### Check that rhmin < rhmax ######################################################################

test <- meteoPAS %>%
  filter(MinRelativeHumidity > MaxRelativeHumidity)

### Call the spwb() function with the latitude and elevation for PAS and Ar #######################

#Elevation & Latitude

#PAS = 106 & 41.159089
#ALO = 431 & 41.907495
#GAR = 584 & 41.350724
#CIN = 639 & 41.720588
#MON = 1298 & 42.032490
#PEN = 1526 & 42.280445

colSums(is.na(meteoPAS))
summary(meteoPAS)

S_PAS_Ar <- spwb(PAS_Ar, meteoPAS, latitude = 41.159089, elevation = 106)

class(S_PAS_Ar)
names(S_PAS_Ar)

head(S_PAS_Ar$WaterBalance, 2)
names(S_PAS_Ar$Soil)
names(S_PAS_Ar$Plants)

summary(S_PAS_Ar, freq="months",FUN=sum, output="WaterBalance")
summary(S_PAS_Ar, freq="months",FUN=mean, output="PlantStress")

#extract(S, level = "forest")

plot(S_PAS_Ar, plot.title = "Pas de l'Ase - Arbustus unedo", xlab = "Date", ylab = expression("Values ("~L~m^-2~")"))

### Call the spwb() function with the latitude and elevation of one site for all species ##########
#### Subsection: for PAS ##########################################################################

# Vector of site names
species <- c("Ar", "Pi", "Rh", "Sa")

# Corresponding PAS objects
PASa_list <- list(PAS_Ar, PAS_Pi, PAS_Rh, PAS_Sa)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  PAS_data <- PASa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_PAS_", specie),
         spwb(PAS_data, meteoPAS, latitude = 41.159089, elevation = 106))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Arbutus unedo" = S_PAS_Ar,
               "Pistacia lentiscus"= S_PAS_Pi,
               "Rhamnus alaternus" = S_PAS_Rh,
               "Salvia rosmarinus" = S_PAS_Sa)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("PAS -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: for ALO ##########################################################################

# Vector of site names
species <- c("Am", "Bu", "Rh", "Sa")

# Corresponding PAS objects
ALOa_list <- list(ALO_Am, ALO_Bu, ALO_Rh, ALO_Sa)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  ALO_data <- ALOa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_ALO_", specie),
         spwb(ALO_data, meteoALO, latitude = 41.907495, elevation = 431))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Amelanchier ovalis" = S_ALO_Am,
               "Buxus sempervirens" = S_ALO_Bu,
               "Rhamnus alaternus"  = S_ALO_Rh,
               "Salvia rosmarinus"  = S_ALO_Sa)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("ALO -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: for GAR ##########################################################################

# Vector of site names
species <- c("Ar", "Bu", "Pi", "Rh", "Sa")

# Corresponding PAS objects
GARa_list <- list(GAR_Ar, GAR_Bu, GAR_Pi, GAR_Rh, GAR_Sa)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  GAR_data <- GARa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_GAR_", specie),
         spwb(GAR_data, meteoGAR, latitude = 41.350724, elevation = 584))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Amelanchier ovalis" = S_GAR_Ar,
               "Buxus sempervirens" = S_GAR_Bu,
               "Pistacia lentiscus" = S_GAR_Pi,
               "Rhamnus alaternus"  = S_GAR_Rh,
               "Salvia rosmarinus"  = S_GAR_Sa)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("GAR -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: for CIN ##########################################################################

# Vector of site names
species <- c("Ar", "Pi", "Sa")

# Corresponding PAS objects
CINa_list <- list(CIN_Ar, CIN_Pi, CIN_Sa)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  CIN_data <- CINa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_CIN_", specie),
         spwb(CIN_data, meteoCIN, latitude = 41.720588, elevation = 639))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Arbutus unedo" = S_CIN_Ar,
               "Pistacia lentiscus" = S_CIN_Pi,
               "Salvia rosmarinus"  = S_CIN_Sa)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("CIN -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: for MON ##########################################################################

# Vector of site names
species <- c("Am", "Bu", "Rh", "Sa")

# Corresponding PAS objects
MONa_list <- list(MON_Am, MON_Bu, MON_Rh, MON_Sa)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  MON_data <- MONa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_MON_", specie),
         spwb(MON_data, meteoMON, latitude = 42.032490, elevation = 1298))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Amelanchier ovalis" = S_MON_Am,
               "Buxus sempervirens" = S_MON_Bu,
               "Rhamnus alaternus"  = S_MON_Rh,
               "Salvia rosmarinus"  = S_MON_Sa)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("MON -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: for PEN ##########################################################################

# Vector of site names
species <- c("Am", "Bu")

# Corresponding PAS objects
PENa_list <- list(PEN_Am, PEN_Bu)

# Loop over species to get their spwb()
for(i in seq_along(species)) {
  specie <- species[i]
  PEN_data <- PENa_list[[i]]
  
  # Run spwb and assign to a variable like S_PAS_Ar
  assign(paste0("S_PEN_", specie),
         spwb(PEN_data, meteoPEN, latitude = 42.280445, elevation = 1526))
}

#Make a graph for each of the species (which should look the same as the same site and precipitation)

series <- list("Amelanchier ovalis" = S_PEN_Am,
               "Buxus sempervirens" = S_PEN_Bu)

lapply(names(series), \(n)
       plot(series[[n]],
            ylim = c(0,1),
            plot.title = paste("PEN -", n),
            xlab = "Date",
            ylab = expression("Values ("~L~m^-2~")")))

#### Subsection: make a graph with one species per site to see the weather inputs #################

gPAS <- plot(S_PAS_Ar, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("PAS") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)
gALO <- plot(S_ALO_Am, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("ALO") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)
gGAR <- plot(S_GAR_Ar, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("GAR") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)
gCIN <- plot(S_CIN_Ar, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("CIN") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)
gMON <- plot(S_MON_Am, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("MON") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)
gPEN <- plot(S_PEN_Am, type = "PET_Precipitation", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
  ggtitle("PEN") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylim(0, 50)

legend <- cowplot::get_legend(plot(S_PAS_Ar, type = "PET_Precipitation", plot.title = "PAS", xlab = "Date", ylab = expression("Values ("~L~m^-2~")")) +
                                theme(legend.position = "bottom"))

plots <- cowplot::plot_grid(gPAS, gALO, gGAR, gCIN, gMON, gPEN,ncol = 3)

plots <- cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.1)) + 
  theme(plot.title = element_text(size = 14, face = "bold"), plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))
plots

class(plots)
getwd()
capabilities()

dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance"

setwd(dir_out)

#ggsave("Meteo_site_2023-2024_Medfate.pdf", plot = plots, device = cairo_pdf, width = 30, height = 20, units = "cm")


### Look at the daily water balance outputs from the spwb() function ##############################
#### Subsection: example for PAS ##################################################################

class(S_PAS_Ar)
names(S_PAS_Ar)

head(S_PAS_Ar$WaterBalance, 2)
names(S_PAS_Ar$Soil)
names(S_PAS_Ar$Plants)

### Assessing and summarising the model results ###################################################
#### Subsection: example for PAS ##################################################################

summary(S_PAS_Ar, freq="months",FUN=sum, output="WaterBalance")
summary(S_PAS_Ar, freq="months",FUN=mean, output="PlantStress")

### Save the basic water balance ##################################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "01. Model_save basic water balance.RData")


