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

#load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/02. Model_save advance water balance.RData")

source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/03. Model_Evaluating model performance.R")

###################################################################################################
### III. Calibration of soil rock fragment  #######################################################
###################################################################################################

### 1. Calibration rock fragment ##################################################################

### Load the data and and filter one species per site to try first _ PAS_Ar #######################

#Examples for PAS and Arbustus unedo

test <- Final_data %>%
  filter(Site_name  == "PAS_ASE" & sp_name == "Arbutus_unedo")

test <- test %>%
  mutate(WP_Spr_PDWP = WP_Spr_PDWP / 10,
         WP_Spr_MDWP = WP_Spr_MDWP / 10,
         WP_Sum_PDWP = WP_Sum_PDWP / 10,
         WP_Sum_MDWP = WP_Sum_MDWP / 10)

test <- test[, -c(1,10:50)]

test2 <- test %>%
  rename(Species = sp_name, Site = Site_name) %>%
  pivot_longer(cols = starts_with("WP_"))

test2 <- test2 %>%
  rename(Date = name) %>%
  mutate(Species = "Ar",
         Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                       ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)))

test2 <- test2 %>%
  mutate(Site = "PAS",
         Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                           ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA)))

test2 <- test2[, -c(5)]

test2 <- test2 %>%
  filter(plant_code == "1PA_AU01")

### Create a vector with the different leaf water potential #########################################
#Length have to be the same than the values: n=4, predawn and midday from spring and summer: objective function

obs_LWP <- test2$value
obs_LWP2 <- test2[, -c(1:3)]

obs_LWP2 <- obs_LWP2 %>%
  mutate(Date = ifelse(Campaign == "Spring", "2024-04-23", "2024-07-16"))

obs_LWP2 <- as.data.frame(obs_LWP2)

#obs_LWP2 <- obs_LWP2 %>%
#  mutate(Date = as.Date(Date)) %>%
#  select(Date, Type, value) %>%
#  pivot_wider(names_from = Type, values_from = value) %>%
#  rename(LeafPsiMin_obs = Predawn_LWP, LeafPsiMax_obs = Midday_LWP)

obs_LWP2 <- obs_LWP2 %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, Campaign, Type) %>%
  mutate(plant_code = plant_code) %>%
  ungroup() %>%
  pivot_wider(names_from = Type, values_from = value) %>%
  rename(LeafPsiMin_obs = Midday_LWP, LeafPsiMax_obs = Predawn_LWP)

str(obs_LWP2)

str(obs_LWP)
is.vector(obs_LWP)

### Define the TAW (total amount of water) vector ###################################################
#Length have to be the same than for the soil: 300, 700 and 1000 mm

soilPAS

# Calculate layer thickness
layer_thickness <- soilPAS$widths  # in mm
layer_thickness # layer thickness are 300, 700 and 1000 mm = 2000 mm total soil depth

summary.soil(soilPAS) #For PAS, the total extractable water for plants is 267mm

#Calculate Vol.WP - Vol.current for each layer

#RESULTS: 
#Layer  1  [ 0  to  300 mm ] Vol. WP (mm): 35 Vol. FC (mm): 69 Vol. SAT (mm): 108 Vol. current (mm): 69 
#Layer  2  [ 300  to  1000 mm ] Vol. WP (mm): 82 Vol. FC (mm): 162 Vol. SAT (mm): 256 Vol. current (mm): 162 
#Layer  3  [ 1000  to  2000 mm ] Vol. WP (mm): 112 Vol. FC (mm): 221 Vol. SAT (mm): 350 Vol. current (mm): 221 

#Calculation of TAW for each layer: Vol. FC - Vol. WP = e.g., 69 - 35 = 34

#TAW_vec <- c(rep(69 - 35, 300), #34
#             rep(162 - 82, 700), #80
#             rep(221 - 112, 1000)) #109

TAW_vec <- seq(40, 50, by = 10)

TAW_vec <- unique(TAW_vec)

length(TAW_vec)  # 2000, matches total soil depth
is.vector(TAW_vec)

### Open the function of Miquel to modify the soil TAW ##############################################

modify_soil_taw <- function(s, target_taw, max_rocks = 97.5, soilFunctions = "VG") {
  f_sew_diff <- function(factor, sew_target, max_rocks, soil, soilFunctions) {
    soil_tmp <- soil
    soil_tmp$rfc <- pmax(pmin(soil_tmp$rfc*factor,max_rocks),0)
    sew <- sum(medfate::soil_waterExtractable(soil_tmp, model = soilFunctions))
    return(sew_target - sew)
  }
  r <- uniroot(f_sew_diff, c(0,10), target_taw, max_rocks, medfate::soil(s), soilFunctions)
  s$rfc <- pmax(pmin(s$rfc*r$root,max_rocks),0)
  return(s)
}

### Import the date where leaf water potential were measured ######################################

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")
#View(Leafwaterpotential_measurementdate)

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate[, -c(1)]

### Filter PAS site #################################################################################

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  filter(Site == "PAS")

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "PAS"]

### Open a loop (for), going through the position of the vector #####################################

#PAS_Ar <- spwbInput(`forest_1_Arbutus unedo`, soilPAS, SpParamsMED, control) #THIS WAS DONE L. 464
# control$soilDomains = "buckets"

control$soilDomains = "dual"
control$soilDomains
control$cavitationInducedDefoliation
control$leafCavitationRecovery

PAS_Ar <- spwbInput(`forest_1_Arbutus unedo`, soilPAS, SpParamsMED, control) #THIS WAS DONE L. 464
S_PAS_Ar <- spwb(PAS_Ar, meteoPAS, latitude = 41.159089, elevation = 106)

#shinyplot(S_PAS_Ar)

#control$cavitationInducedDefoliation = TRUE

#S_PAS_Ar$Plants$LeafPsiMin
#S_PAS_Ar$Plants$LeafPsiMax

head(S_PAS_Ar$WaterBalance$Date)
head(S_PAS_Ar$weather$dates)
head(obs_dates)

### Create a function to extract modelled leaf water potential ################################

extract_LWP_Ar <- function(S_PAS_Ar, obs_dates) {
  
  obs_dates <- as.Date(obs_dates)
  dates <- as.Date(S_PAS_Ar$weather$dates)
  
  psi_min <- S_PAS_Ar$Plants$LeafPsiMin[,1]
  psi_max <- S_PAS_Ar$Plants$LeafPsiMax[,1]
  
  idx <- match(obs_dates, dates)
  valid <- !is.na(idx)
  
  data.frame(
    Date = obs_dates[valid],
    LeafPsiMin = psi_min[idx[valid]],
    LeafPsiMax = psi_max[idx[valid]]
  )
}

### Check at the soil values - rock fragment #################################################

soilPAS

#safety2 <- soilPAS

#Change it for higher values 
soilPAS$rfc <- c(25, 50, 75)

### Create a vector to store objective function values #######################################

soil_list <- vector("list", length(TAW_vec))
obj_values <- numeric(length(TAW_vec))
results_list <- vector("list", length(TAW_vec))

control$leafCavitationRecovery = "rate"
control$cavitationInducedDefoliation = TRUE

for (i in seq_along(TAW_vec)) {
  
  soil_i <- modify_soil_taw(soilPAS, TAW_vec[i])
  soil_list[[i]] <- soil_i
  
  PAS_Ar <- spwbInput(`forest_1_Arbutus unedo`, soil_i, SpParamsMED, control)
  sim <- spwb(PAS_Ar, meteoPAS, latitude = 41.159089, elevation = 106)
  
  sim_LWP <- extract_LWP_Ar(sim, obs_dates)
  
  merged <- merge(obs_LWP2, sim_LWP, by = "Date", all = TRUE)
  merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
  results_list[[i]] <- merged
  
  obj_values[i] <- mean(abs(merged$diff), na.rm = TRUE)
  
  cat("TAW:", TAW_vec[i], " -> error:", obj_values[i], "\n")
  
  if (i %% 100 == 0) cat("Done:", i, "\n")
}

sim_LWP
obs_LWP2

TAW_vec
obj_values

TAW_PAS_Ar <- bind_rows(results_list, .id = "TAW_id") %>%
  mutate(TAW = TAW_vec[as.numeric(TAW_id)])

### Find the TAW scenario that gives the minimum objective function ##################################

best_index <- which.min(obj_values)
best_TAW <- TAW_vec[best_index]
best_TAW

plot(TAW_vec, obj_values, type = "b")

### Create a function to extract LWP for all sites and species #######################################

extract_LWP <- function(sim, obs_dates) {
  
  obs_dates <- as.Date(obs_dates)
  dates <- as.Date(sim$weather$dates)
  
  psi_min <- sim$Plants$LeafPsiMin[,1]
  psi_max <- sim$Plants$LeafPsiMax[,1]
  
  idx <- match(obs_dates, dates)
  valid <- !is.na(idx)
  
  data.frame(
    Date = obs_dates[valid],
    LeafPsiMin = psi_min[idx[valid]],
    LeafPsiMax = psi_max[idx[valid]]
  )
}

### Load the data and and filter for ALL species in PAS ##############################################

TAW_vec <- seq(25, 120, by = 5)

species_list <- Final_data %>%
  filter(Site_name == "PAS_ASE") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "PAS"]

print(obs_dates)

unique(species_list)

#test %>% distinct(sp_name, plant_code)

meteoPAS <- meteoPAS %>% mutate(Date = dates)

run_calibration_PAS <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "PAS_ASE" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
  test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                  ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                      ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    mutate(Date = ifelse(Campaign == "Spring", "2024-04-23", "2024-07-16")) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
  # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_1_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilPAS, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoPAS, latitude = 41.159089, elevation = 106)
    
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_PAS <- lapply(species_list, run_calibration_PAS)
results_PAS <- bind_rows(results_PAS)

unique(results_PAS$Date)

results_PAS <- results_PAS %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_PAS <- results_PAS %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_PAS <- results_PAS %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_PAS <- results_PAS %>%
  filter(!is.na(Mean_diff))


results_PAS2 <- results_PAS %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_PAS2 <- results_PAS2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

is.na(results_PAS2$Mean_diff_T)

results_PAS2 <- results_PAS2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
      !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_PAS2_s <- results_PAS2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_PAS2_s <- results_PAS2_s %>% 
  filter(is_best == 'TRUE')

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration"
setwd(dir_out)

library(writexl)
#write_xlsx(results_PAS2, "results_PAS2.xlsx")
#write_xlsx(results_PAS2_s, "results_PAS2_s.xlsx")

### Load the data and and filter for ALL species in ALO ##############################################

TAW_vec <- seq(25, 120, by = 5)

library(furrr)
plan(multisession, workers = 6)

species_list <- Final_data %>%
  filter(Site_name == "ALOS_BALAGUER") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "ALO"]

print(obs_dates)

unique(species_list)

meteoALO <- meteoALO %>% mutate(Date = dates)

run_calibration_ALO <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "ALOS_BALAGUER" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
    test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                         ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                             ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    mutate(Date = ifelse(Campaign == "Spring", "2024-05-08", "2024-07-20")) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
    # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_2_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilALO, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoALO, latitude = 41.907495, elevation = 431)
  
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_ALO <- lapply(species_list, run_calibration_ALO)
results_ALO <- bind_rows(results_ALO)

unique(results_ALO$Date)

results_ALO <- results_ALO %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_ALO <- results_ALO %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_ALO <- results_ALO %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_ALO <- results_ALO %>%
  filter(!is.na(Mean_diff))

results_ALO2 <- results_ALO %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_ALO2 <- results_ALO2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

results_ALO2 <- results_ALO2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
                                 !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_ALO2_s <- results_ALO2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_ALO2_s <- results_ALO2_s %>% 
  filter(is_best == 'TRUE')

#write_xlsx(results_ALO2, "results_ALO2.xlsx")
#write_xlsx(results_ALO2_s, "results_ALO2_s.xlsx")

### Load the data and and filter for ALL species in GAR ##############################################

TAW_vec <- seq(25, 120, by = 5)

library(furrr)
plan(multisession, workers = 6)

species_list <- Final_data %>%
  filter(Site_name == "GARRAF") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

unique(species_list)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "GAR"]

print(obs_dates)

unique(species_list)

#range(meteoGAR$Date)
#head(meteoGAR)
#range(meteoGAR$dates)

meteoGAR <- meteoGAR %>% mutate(Date = dates)

run_calibration_GAR <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "GARRAF" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
  test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                         ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                             ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    #mutate(Date = ifelse(Campaign == "Spring", "2024-05-15", "2024-07-25")) %>%
    mutate(Date = case_when(Campaign == "Spring" ~ as.Date("2024-05-14"), Campaign == "Summer" ~ as.Date("2024-07-25"))) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
  # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_3_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilGAR, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoGAR, latitude = 41.350724, elevation = 584)
    
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_GAR <- lapply(species_list, run_calibration_GAR)
results_GAR <- bind_rows(results_GAR)

results_GAR <- results_GAR %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_GAR <- results_GAR %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_GAR <- results_GAR %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_GAR <- results_GAR %>%
  filter(!is.na(Mean_diff))

results_GAR2 <- results_GAR %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_GAR2 <- results_GAR2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

results_GAR2 <- results_GAR2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
                                 !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_GAR2_s <- results_GAR2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_GAR2_s <- results_GAR2_s %>% 
  filter(is_best == 'TRUE')

write_xlsx(results_GAR2, "results_GAR2.xlsx")
write_xlsx(results_GAR2_s, "results_GAR2_s.xlsx")

### Load the data and and filter for ALL species in CIN ##############################################

TAW_vec <- seq(25, 120, by = 5)

library(furrr)
plan(multisession, workers = 6)

species_list <- Final_data %>%
  filter(Site_name == "CINGLES_BERTI") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

unique(species_list)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "CIN"]

print(obs_dates)

unique(species_list)

meteoCIN <- meteoCIN %>% mutate(Date = dates)

run_calibration_CIN <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "CINGLES_BERTI" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
  test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                         ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                             ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    mutate(Date = ifelse(Campaign == "Spring", "2024-05-21", "2024-08-01")) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
  # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_4_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilCIN, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoCIN, latitude = 41.720588, elevation = 639)
    
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_CIN <- lapply(species_list, run_calibration_CIN)
results_CIN <- bind_rows(results_CIN)

results_CIN <- results_CIN %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_CIN <- results_CIN %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_CIN <- results_CIN %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_CIN <- results_CIN %>%
  filter(!is.na(Mean_diff))

results_CIN2 <- results_CIN %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_CIN2 <- results_CIN2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

results_CIN2 <- results_CIN2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
                                 !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_CIN2_s <- results_CIN2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_CIN2_s <- results_CIN2_s %>% 
  filter(is_best == 'TRUE')

#write_xlsx(results_CIN2, "results_CIN2.xlsx")
#write_xlsx(results_CIN2_s, "results_CIN2_s.xlsx")

### Load the data and and filter for ALL species in MON ##############################################

TAW_vec <- seq(25, 120, by = 5)

library(furrr)
plan(multisession, workers = 6)

species_list <- Final_data %>%
  filter(Site_name == "MONTSEC") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

unique(species_list)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "MON"]

print(obs_dates)

unique(species_list)

meteoMON <- meteoMON %>% mutate(Date = dates)

run_calibration_MON <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "MONTSEC" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
  test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                         ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                             ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    mutate(Date = ifelse(Campaign == "Spring", "2024-05-28", "2024-08-03")) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
  # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_5_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilMON, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoMON, latitude = 42.032490, elevation = 1298)
    
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_MON <- lapply(species_list, run_calibration_MON)
results_MON <- bind_rows(results_MON)

results_MON <- results_MON %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_MON <- results_MON %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_MON <- results_MON %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_MON <- results_MON %>%
  filter(!is.na(Mean_diff))

results_MON2 <- results_MON %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_MON2 <- results_MON2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

results_MON2 <- results_MON2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
                                 !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_MON2_s <- results_MON2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_MON2_s <- results_MON2_s %>% 
  filter(is_best == 'TRUE')

write_xlsx(results_MON2, "results_MON2.xlsx")
write_xlsx(results_MON2_s, "results_MON2_s.xlsx")

### Load the data and and filter for ALL species in PEN ##############################################

TAW_vec <- seq(25, 120, by = 5)

library(furrr)
plan(multisession, workers = 6)

species_list <- Final_data %>%
  filter(Site_name == "PENTINA") %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name)) %>%
  distinct(sp_name) %>%
  pull(sp_name)

unique(species_list)

Leafwaterpotential_measurementdate <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Leafwaterpotential_measurementdate.xlsx")

str(Leafwaterpotential_measurementdate)

Leafwaterpotential_measurementdate$Date <- as.Date(Leafwaterpotential_measurementdate$Date)

Leafwaterpotential_measurementdate <- Leafwaterpotential_measurementdate %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                       ifelse(Site == "ALOS", "ALO",
                              ifelse(Site == "GARRAF", "GAR",
                                     ifelse(Site == "CINGLES", "CIN",
                                            ifelse(Site == "MONTSEC", "MON",
                                                   ifelse(Site == "PENTINA", "PEN", Site)))))))

print(obs_dates)

obs_dates <- Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site == "PEN"]

print(obs_dates)

unique(species_list)

meteoPEN <- meteoPEN %>% mutate(Date = dates)

run_calibration_PEN <- function(sp_name, plant_code) {
  
  test <- Final_data %>%
    filter(Site_name == "PENTINA" & sp_name == sp_name & plant_code  == plant_code) %>%
    mutate(across(starts_with("WP_"), ~ . / 10)) %>%
    select(-c(1,10:50))
  
  test2 <- test %>%
    rename(Site = Site_name, sp_name = sp_name, plant_code = plant_code) %>%
    pivot_longer(cols = starts_with("WP_")) %>%
    rename(Date = name) %>%
    mutate(Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                         ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)),
           Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                             ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA))) %>%
    select(-Date) %>%
    mutate(Date = ifelse(Campaign == "Spring", "2024-06-04", "2024-08-08")) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_wider(names_from = Type, values_from = value) %>%
    rename(LeafPsiMin_obs = Predawn_LWP,
           LeafPsiMax_obs = Midday_LWP)
  
  # ---- FIX forest object naming depending on your case ----
  forest_obj <- get(paste0("forest_6_", gsub("_", " ", sp_name)))
  
  # ---- Store ALL results ----
  results_list <- vector("list", length(TAW_vec))
  
  for (i in seq_along(TAW_vec)) {
    
    soil_i <- modify_soil_taw(soilPEN, TAW_vec[i])
    
    input <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
    sim <- spwb(input, meteoPEN, latitude = 42.280445, elevation = 1526)
    
    sim_LWP <- extract_LWP(sim, obs_dates)
    
    merged <- merge(test2, sim_LWP, by = "Date", all = TRUE)
    merged <- merged %>% mutate(diff = LeafPsiMin_obs - LeafPsiMin)
    
    results_list[[i]] <- merged
  }
  
  # ---- THIS is what you want ----
  TAW_species <- bind_rows(results_list, .id = "TAW_id") %>%
    mutate(
      TAW = TAW_vec[as.numeric(TAW_id)],
      sp_name = sp_name,
      plant_code = plant_code
    )
  
  return(TAW_species)
}

results_PEN <- lapply(species_list, run_calibration_PEN)
results_PEN <- bind_rows(results_PEN)

results_PEN <- results_PEN %>% 
  rename(LeafPsiMin_obs = LeafPsiMax_obs,
         LeafPsiMax_obs = LeafPsiMin_obs)

results_PEN <- results_PEN %>% 
  mutate(diffMin = LeafPsiMin_obs - LeafPsiMin,
         diffMax = LeafPsiMax_obs - LeafPsiMax)

results_PEN <- results_PEN %>%
  mutate(Mean_diff = (diffMin + diffMax) / 2)

results_PEN <- results_PEN %>%
  filter(!is.na(Mean_diff))

results_PEN2 <- results_PEN %>%
  group_by(plant_code, TAW) %>%
  mutate(id = row_number()) %>%  # optional, keeps rows unique if needed
  ungroup() %>%
  pivot_wider(names_from = Campaign,
              values_from = Mean_diff) %>%
  group_by(plant_code, TAW) %>%
  fill(Spring, Summer, .direction = "downup") %>%
  ungroup()

results_PEN2 <- results_PEN2 %>%
  mutate(Mean_diff_T = (Spring + Summer) / 2)

results_PEN2 <- results_PEN2 %>%
  mutate(Mean_diff_T = case_when(!is.na(Spring) & !is.na(Summer) ~ (Spring + Summer) / 2,
                                 !is.na(Spring) ~ Spring, !is.na(Summer) ~ Summer, TRUE ~ NA_real_))

results_PEN2_s <- results_PEN2 %>% 
  group_by(plant_code) %>%
  mutate(best_diff = Mean_diff_T[which.min(abs(Mean_diff_T))],
         is_best = abs(Mean_diff_T) == min(abs(Mean_diff_T), na.rm = TRUE),
         best_TAW = ifelse(is_best, TAW, NA)) %>%
  ungroup()

results_PEN2_s <- results_PEN2_s %>% 
  filter(is_best == 'TRUE')

write_xlsx(results_PEN2, "results_PEN2.xlsx")
write_xlsx(results_PEN2_s, "results_PEN2_s.xlsx")

### Merge the different files together ###############################################################

names(results_PAS2_s)
names(results_ALO2_s)

#best_TAW_info <- merge(results_PAS_s, results_ALO_s, by = c("TAW_id", "Date", "Site_code", "Site", "sp_name", "plant_code", "Campaign", "LeafPsiMax_obs",
#                       "LeafPsiMin_obs", "LeafPsiMin", "LeafPsiMax", "diff", "TAW", "diffMin", "diffMax", "best_TAW",      
#                       "Mean_diff", "best_diff", "is_best"), all = TRUE)

best_TAW_info <- bind_rows(results_PAS2_s, results_ALO2_s)
best_TAW_info <- bind_rows(best_TAW_info, results_GAR2_s)
best_TAW_info <- bind_rows(best_TAW_info, results_CIN2_s)
best_TAW_info <- bind_rows(best_TAW_info, results_MON2_s)
best_TAW_info <- bind_rows(best_TAW_info, results_PEN2_s)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration"
setwd(dir_out)

write_xlsx(best_TAW_info, "best_TAW_info.xlsx")

### Save the rock fragment calibration ###############################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "01. Model_save rock fragment calibration.RData")

#Take the value of the TAW and modifie the soil using your function (Miquel): according to the target TAW it will change the rock fragment
#Store it and do not rewrite it 
#Run the simulation of spwb, with the modifie soil and access the water potential of the corresponding days
#Calculate the absolute value of the difference (SUM, n=4)
#Store the values in the corresponding vector defined _ call again my function
#Select the select the one which is lower objective function, and check TAW vector which correspond to those and change the coefficient of rock fragment _ mY function again

#SIMULATION BEFORE AND AFTER TO SEE THE DIFFERENCE