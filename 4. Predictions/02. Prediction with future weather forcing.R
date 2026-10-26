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

library(readr)
Futuremeteo <- read_csv("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/Futuremeteo.csv")

###################################################################################################
### VI. Predictions ###############################################################################
###################################################################################################

### 3. Run the water balance with calibrated rfc and future weather ###############################
### Create the appropriate weather data frame #####################################################

#Futuremeteo <- Future_final
str(Futuremeteo)

futuremeteo <- as.data.frame(Futuremeteo)
str(futuremeteo)
head(futuremeteo)  # to preview the first few rows
#summary(meteo)  # get a summary of each column

futuremeteo$Year <- as.numeric(format(as.Date(futuremeteo$Date), "%Y"))

futuremeteo <- futuremeteo %>%
  filter(Year %in% c(2069, 2070))

futuremeteo <- futuremeteo %>%
  mutate(Radiation = Radiation/10)

### Create a meteo dataframe for each site ########################################################

futuremeteo <- futuremeteo[complete.cases(futuremeteo), ]

# Mapping of ID to variable names
meteo_names <- c("PAS" = "meteoPAS_f", "ALO" = "meteoALO_f", "GAR" = "meteoGAR_f", "CIN" = "meteoCIN_f", "MON" = "meteoMON_f", "PEN" = "meteoPEN_f")

# Loop over each site ID
for (id in names(meteo_names)) {
  
  # Correct filtering by Site
  meteo_subset <- subset(futuremeteo, Site == id)
  
  # Optional: remove ID column if it exists
  # meteo_subset$ID <- NULL
  
  # Save into separate object
  assign(meteo_names[[id]], meteo_subset)
}

### Open the file from the calibration ############################################################

results_PAS2_s <- best_TAW_info %>% filter(Site == "PAS_ASE")
results_ALO2_s <- best_TAW_info %>% filter(Site == "ALOS_BALAGUER")
results_GAR2_s <- best_TAW_info %>% filter(Site == "GARRAF")
results_CIN2_s <- best_TAW_info %>% filter(Site == "CINGLES_BERTI")
results_MON2_s <- best_TAW_info %>% filter(Site == "MONTSEC")
results_PEN2_s <- best_TAW_info %>% filter(Site == "PENTINA")
  
### Re-load the function of Miquel ################################################################

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


### Check control of the model follow loop ########################################################

soilPAS

control$soilDomains
control$cavitationInducedDefoliation
control$leafCavitationRecovery

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL PAS ####

gc()

results_PAS2_sn <- results_PAS2_s 

results_PAS2_sn <- results_PAS2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_PAS2_sn))

summary(meteoPAS_f)
head(meteoPAS_f)

meteoPAS_f <- meteoPAS_f %>%
  mutate(MinRelativeHumidity = (MinRelativeHumidity * 1.5) + 7, 
         MaxRelativeHumidity = (MaxRelativeHumidity * 1.5) + 7)

meteoPAS_f$MaxRelativeHumidity <- pmin(pmax(meteoPAS_f$MaxRelativeHumidity, 0), 100)
meteoPAS_f$MinRelativeHumidity <- pmin(pmax(meteoPAS_f$MinRelativeHumidity, 0), 100)

ggplot(meteoPAS_f, aes(x = Date, y = MinRelativeHumidity, color = Site)) +
  geom_point()

for (i in seq_len(nrow(results_PAS2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_PAS2_sn$plant_code[i]
  taw_i <- results_PAS2_sn$best_TAW[i]
  sp_name_i <- results_PAS2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_1_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilPAS, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoPAS_f, latitude = 41.159089, elevation = 106)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_PAS_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^Sf_PAS_Ar_|^Sf_PAS_Pi_|^Sf_PAS_Rh_|^Sf_PAS_Sa_")
sim_list <- mget(sim_names)

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

#psi_df <- merge_leafpsi_range(sim_list) #It is the right TAW for each IND

### Check the simulation ##########################################################################

plot(Sf_PAS_Ar_1PAAU01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Ar_1PAAU11, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_PAS_Pi_1PAPL02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Pi_1PAPL11, type="LeafPsiRange", bySpecies = TRUE)

#plot(Sf_PAS_Rh_1PARA02, type="LeafPsiRange", bySpecies = TRUE)
#plot(Sf_PAS_Rh_1PARA03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Rh_1PARA06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Rh_1PARA07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Rh_1PARA08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Rh_1PARA09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Rh_1PARA10, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_PAS_Sa_1PASR02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PAS_Sa_1PASR12, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

#sim_names <- ls(pattern = "^Sf_PAS_Ar_|^Sf_PAS_Pi_")

spwb_list2 <- list(AU01 = Sf_PAS_Ar_1PAAU01, AU02 = Sf_PAS_Ar_1PAAU02, AU05 = Sf_PAS_Ar_1PAAU05, AU03 = Sf_PAS_Ar_1PAAU03,
                   AU06 = Sf_PAS_Ar_1PAAU06, AU07 = Sf_PAS_Ar_1PAAU07, AU11 = Sf_PAS_Ar_1PAAU11,
                   PL06 = Sf_PAS_Pi_1PAPL06, PL07 = Sf_PAS_Pi_1PAPL07, PL08 = Sf_PAS_Pi_1PAPL08, PL10 = Sf_PAS_Pi_1PAPL10, PL11 = Sf_PAS_Pi_1PAPL11,
                   RA02 = Sf_PAS_Rh_1PARA02, RA03 = Sf_PAS_Rh_1PARA03, RA06 = Sf_PAS_Rh_1PARA06, RA07 = Sf_PAS_Rh_1PARA07, RA09 = Sf_PAS_Rh_1PARA09,
                   SR02 = Sf_PAS_Sa_1PASR02, SR03 = Sf_PAS_Sa_1PASR03, SR05 = Sf_PAS_Sa_1PASR05, SR06 = Sf_PAS_Sa_1PASR06, SR07 = Sf_PAS_Sa_1PASR07, 
                   SR10 = Sf_PAS_Sa_1PASR10, SR12 = Sf_PAS_Sa_1PASR12)

df_all <- do.call(rbind, lapply(names(spwb_list2), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list2[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Species'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
#write_xlsx(df_mean, "gfPAS_data.xlsx")

gfPAS_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gfPAS_data.xlsx")

gfPAS_data <- gfPAS_data %>%
  filter(Date == as.Date("2070-07-16"))

GRAPH2 <- gfPAS_data %>%
  mutate(Site = "PAS")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL ALO #############

gc()

results_ALO2_sn <- results_ALO2_s 

results_ALO2_sn <- results_ALO2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_ALO2_sn))

summary(meteoALO_f)
head(meteoALO_f)

meteoALO_f <- meteoALO_f %>%
  mutate(MinRelativeHumidity = (MinRelativeHumidity * 1.5) + 7, 
         MaxRelativeHumidity = (MaxRelativeHumidity * 1.5) + 7)

meteoALO_f$MaxRelativeHumidity <- pmin(pmax(meteoALO_f$MaxRelativeHumidity, 0), 100)
meteoALO_f$MinRelativeHumidity <- pmin(pmax(meteoALO_f$MinRelativeHumidity, 0), 100)

for (i in seq_len(nrow(results_ALO2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_ALO2_sn$plant_code[i]
  taw_i <- results_ALO2_sn$best_TAW[i]
  sp_name_i <- results_ALO2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_2_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilALO, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoALO_f, latitude = 41.907495, elevation = 431)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_ALO_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

gc()

sim_names <- ls(pattern = "^Sf_ALO_Am_|^Sf_ALO_Bu_|^Sf_ALO_Rh_|^Sf_ALO_Sa_")
sim_list <- mget(sim_names)

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

### Subsection: check the simulation #############################################################

plot(Sf_ALO_Am_2ABAO02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Am_2ABAO13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_ALO_Bu_2ABBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Bu_2ABBS13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_ALO_Rh_2ABRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Rh_2ABRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_ALO_Sa_2ABSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_ALO_Sa_2ABSR13, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

spwb_list2_ALO <- list(AO02 = Sf_ALO_Am_2ABAO02, AO03 = Sf_ALO_Am_2ABAO03, AO04 = Sf_ALO_Am_2ABAO04, AO05 = Sf_ALO_Am_2ABAO05,  
                       AO06 = Sf_ALO_Am_2ABAO06, AO07 = Sf_ALO_Am_2ABAO07, AO10 = Sf_ALO_Am_2ABAO10, AO11 = Sf_ALO_Am_2ABAO11,  AO13 = Sf_ALO_Am_2ABAO13, 
                       BS02 = Sf_ALO_Bu_2ABBS02, BS03 = Sf_ALO_Bu_2ABBS03, BS04 = Sf_ALO_Bu_2ABBS04, BS05 = Sf_ALO_Bu_2ABBS05,  BS06 = Sf_ALO_Bu_2ABBS06,
                       BS09 = Sf_ALO_Bu_2ABBS09, BS11 = Sf_ALO_Bu_2ABBS11, BS12 = Sf_ALO_Bu_2ABBS12, BS13 = Sf_ALO_Bu_2ABBS13,
                       RA02 = Sf_ALO_Rh_2ABRA02, RA03 = Sf_ALO_Rh_2ABRA03, RA04 = Sf_ALO_Rh_2ABRA04, RA08 = Sf_ALO_Rh_2ABRA08,  RA09 = Sf_ALO_Rh_2ABRA09,
                       RA11 = Sf_ALO_Rh_2ABRA11, RA12 = Sf_ALO_Rh_2ABRA12, RA13 = Sf_ALO_Rh_2ABRA13, 
                       SR02 = Sf_ALO_Sa_2ABSR02, SR03 = Sf_ALO_Sa_2ABSR03, SR04 = Sf_ALO_Sa_2ABSR04, SR05 = Sf_ALO_Sa_2ABSR05,
                       SR06 = Sf_ALO_Sa_2ABSR06, SR11 = Sf_ALO_Sa_2ABSR11, SR12 = Sf_ALO_Sa_2ABSR12, SR13 = Sf_ALO_Sa_2ABSR13)

#names(spwb_list2_ALO)[sapply(spwb_list2_ALO, is.null)]
#sapply(spwb_list2_ALO, is.null)

df_all <- do.call(rbind, lapply(names(spwb_list2_ALO), function(sp) {
  p <- plot(spwb_list2_ALO[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Site'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
write_xlsx(df_mean, "gALOf_data.xlsx")

gALOf_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gALOf_data.xlsx")

gALOf_data <- gALOf_data %>%
  filter(Date == as.Date("2070-07-20"))

gALOf_data <- gALOf_data %>%
  mutate(Site = "ALO")

GRAPH2 <- rbind(GRAPH2, gALOf_data)

#write_xlsx(gSiteData_t, "gSiteData_t.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL GAR #############

gc()

results_GAR2_sn <- results_GAR2_s 

results_GAR2_sn <- results_GAR2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_GAR2_sn))

summary(meteoGAR_f)
head(meteoGAR_f)

meteoGAR_f <- meteoGAR_f %>%
  mutate(MinRelativeHumidity = (MinRelativeHumidity * 1.5) + 7, 
         MaxRelativeHumidity = (MaxRelativeHumidity * 1.5) + 7)

meteoGAR_f$MaxRelativeHumidity <- pmin(pmax(meteoGAR_f$MaxRelativeHumidity, 0), 100)
meteoGAR_f$MinRelativeHumidity <- pmin(pmax(meteoGAR_f$MinRelativeHumidity, 0), 100)


for (i in seq_len(nrow(results_GAR2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_GAR2_sn$plant_code[i]
  taw_i <- results_GAR2_sn$best_TAW[i]
  sp_name_i <- results_GAR2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_3_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilGAR, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoGAR_f, latitude = 41.350724, elevation = 584)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_GAR_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^Sf_GAR_Ar_|^Sf_GAR_Bu_|^Sf_GAR_Pi_|^Sf_GAR_Rh_|^Sf_GAR_Sa_")
sim_list <- mget(sim_names) 

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

### Subsection: check the simulation #############################################################

plot(Sf_GAR_Ar_3GFAU01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Ar_3GFAU12, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_GAR_Bu_3GFBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Bu_3GFBS11, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_GAR_Pi_3GFPL04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Pi_3GFPL12, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_GAR_Rh_3GFRA01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Rh_3GFRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_GAR_Sa_3GFSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_GAR_Sa_3GFSR09, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

spwb_list2_GAR <- list(AU12 = Sf_GAR_Ar_3GFAU12,
                       BS01 = Sf_GAR_Bu_3GFBS01, BS03 = Sf_GAR_Bu_3GFBS03, BS04 = Sf_GAR_Bu_3GFBS04, BS05 = Sf_GAR_Bu_3GFBS05,
                       BS06 = Sf_GAR_Bu_3GFBS06, BS07 = Sf_GAR_Bu_3GFBS07, BS08 = Sf_GAR_Bu_3GFBS08, BS11 = Sf_GAR_Bu_3GFBS11,
                       PL05 = Sf_GAR_Pi_3GFPL05, PL06 = Sf_GAR_Pi_3GFPL06, PL12 = Sf_GAR_Pi_3GFPL12, RA03 = Sf_GAR_Rh_3GFRA03,
                       RA04 = Sf_GAR_Rh_3GFRA04, RA06 = Sf_GAR_Rh_3GFRA06, RA07 = Sf_GAR_Rh_3GFRA07, RA12 = Sf_GAR_Rh_3GFRA12,
                       RA13 = Sf_GAR_Rh_3GFRA13, SR01 = Sf_GAR_Sa_3GFSR01, SR02 = Sf_GAR_Sa_3GFSR02, SR03 = Sf_GAR_Sa_3GFSR03,
                       SR04 = Sf_GAR_Sa_3GFSR04, SR06 = Sf_GAR_Sa_3GFSR06, SR08 = Sf_GAR_Sa_3GFSR08, SR09 = Sf_GAR_Sa_3GFSR09)

#names(spwb_list2_ALO)[sapply(spwb_list2_ALO, is.null)]
#sapply(spwb_list2_ALO, is.null)

df_all <- do.call(rbind, lapply(names(spwb_list2_GAR), function(sp) {
  p <- plot(spwb_list2_GAR[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Site'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
write_xlsx(df_mean, "gfGAR_data.xlsx")

gfGAR_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gfGAR_data.xlsx")

gfGAR_data <- gfGAR_data %>%
  filter(Date == as.Date("2070-07-25"))

gfGAR_data <- gfGAR_data %>%
  mutate(Site = "GAR")

GRAPH2 <- rbind(GRAPH2, gfGAR_data)

#write_xlsx(gSiteData_t, "gSiteData_t.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL CIN #############

gc()

results_CIN2_sn <- results_CIN2_s 

results_CIN2_sn <- results_CIN2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_CIN2_sn))

for (i in seq_len(nrow(results_CIN2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_CIN2_sn$plant_code[i]
  taw_i <- results_CIN2_sn$best_TAW[i]
  sp_name_i <- results_CIN2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_4_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilCIN, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoCIN_f, latitude = 41.720588, elevation = 639)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_CIN_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^Sf_CIN_Ar_|^Sf_CIN_Pi_|^Sf_CIN_Sa_")
sim_list <- mget(sim_names)

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

### Subsection: check the simulation #############################################################

plot(Sf_CIN_Ar_4CBAU01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Ar_4CBAU09, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_CIN_Pi_4CBPL01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Pi_4CBPL11, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_CIN_Sa_4CBSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_CIN_Sa_4CBSR12, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

spwb_list2_CIN <- list(AU01 = Sf_CIN_Ar_4CBAU01, AU02 = Sf_CIN_Ar_4CBAU02, AU03 = Sf_CIN_Ar_4CBAU03,
                       AU05 = Sf_CIN_Ar_4CBAU05, AU07 = Sf_CIN_Ar_4CBAU07, AU08 = Sf_CIN_Ar_4CBAU08, AU09 = Sf_CIN_Ar_4CBAU09,
                       SR01 = Sf_CIN_Sa_4CBSR01, SR02 = Sf_CIN_Sa_4CBSR02, SR03 = Sf_CIN_Sa_4CBSR03, SR04 = Sf_CIN_Sa_4CBSR04,
                       SR05 = Sf_CIN_Sa_4CBSR05, SR06 = Sf_CIN_Sa_4CBSR06, SR08 = Sf_CIN_Sa_4CBSR08, SR12 = Sf_CIN_Sa_4CBSR12)

df_all <- do.call(rbind, lapply(names(spwb_list2_CIN), function(sp) {
  p <- plot(spwb_list2_CIN[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Site'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
write_xlsx(df_mean, "gfCIN_data.xlsx")

gfCIN_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gfCIN_data.xlsx")

gfCIN_data <- gfCIN_data %>%
  filter(Date == as.Date("2070-08-01"))

gfCIN_data <- gfCIN_data %>%
  mutate(Site = "CIN")

GRAPH2 <- rbind(GRAPH2, gfCIN_data)

#write_xlsx(gSiteData_t, "gSiteData_t.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL MON #############

gc()

results_MON2_sn <- results_MON2_s 

results_MON2_sn <- results_MON2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_MON2_sn))

summary(meteoMON_f)
head(meteoMON_f)

meteoMON_f <- meteoMON_f %>%
  mutate(MinRelativeHumidity = (MinRelativeHumidity * 1.5) + 7, 
         MaxRelativeHumidity = (MaxRelativeHumidity * 1.5) + 7)

meteoMON_f$MaxRelativeHumidity <- pmin(pmax(meteoMON_f$MaxRelativeHumidity, 0), 100)
meteoMON_f$MinRelativeHumidity <- pmin(pmax(meteoMON_f$MinRelativeHumidity, 0), 100)

ggplot(meteoMON_f, aes(x = Date, y = MinRelativeHumidity, color = Site)) +
  geom_point()

for (i in seq_len(nrow(results_MON2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_MON2_sn$plant_code[i]
  taw_i <- results_MON2_sn$best_TAW[i]
  sp_name_i <- results_MON2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_5_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilMON, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoMON_f, latitude = 42.032490, elevation = 1298)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_MON_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^Sf_MON_Am_|^Sf_MON_Bu_|^Sf_MON_Rh_|^Sf_MON_Sa_")
sim_list <- mget(sim_names)

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

### Subsection: check the simulation #############################################################

plot(Sf_MON_Am_5MCAO02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Am_5MCAO13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_MON_Bu_5MCBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Bu_5MCBS12, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_MON_Rh_5MCRA01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA08, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA10, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA11, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Rh_5MCRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_MON_Sa_5MCSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_MON_Sa_5MCSR08, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

spwb_list2_MON <- list(BS01 = Sf_MON_Bu_5MCBS01, BS02 = Sf_MON_Bu_5MCBS02, BS07 = Sf_MON_Bu_5MCBS07, BS08 = Sf_MON_Bu_5MCBS08,
                       BS10 = Sf_MON_Bu_5MCBS10, BS11 = Sf_MON_Bu_5MCBS11, BS12 = Sf_MON_Bu_5MCBS12, RA01 = Sf_MON_Rh_5MCRA01,
                       RA02 = Sf_MON_Rh_5MCRA02, RA08 = Sf_MON_Rh_5MCRA08, RA09 = Sf_MON_Rh_5MCRA09, RA10 = Sf_MON_Rh_5MCRA10,
                       RA11 = Sf_MON_Rh_5MCRA11, RA12 = Sf_MON_Rh_5MCRA12, RA13 = Sf_MON_Rh_5MCRA13, SR01 = Sf_MON_Sa_5MCSR01,
                       SR02 = Sf_MON_Sa_5MCSR02, SR03 = Sf_MON_Sa_5MCSR03, SR04 = Sf_MON_Sa_5MCSR04, SR05 = Sf_MON_Sa_5MCSR05,
                       SR06 = Sf_MON_Sa_5MCSR06, SR07 = Sf_MON_Sa_5MCSR07, SR08 = Sf_MON_Sa_5MCSR08)

df_all <- do.call(rbind, lapply(names(spwb_list2_MON), function(sp) {
  p <- plot(spwb_list2_MON[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Site'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
write_xlsx(df_mean, "gfMON_data.xlsx")

gfMON_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gfMON_data.xlsx")

gfMON_data <- gfMON_data %>%
  filter(Date == as.Date("2070-08-03"))

gfMON_data <- gfMON_data %>%
  mutate(Site = "MON")

GRAPH2 <- rbind(GRAPH2, gfMON_data)

#write_xlsx(gSiteData_t, "gSiteData_t.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL PEN #############

gc()

results_PEN2_sn <- results_PEN2_s 

results_PEN2_sn <- results_PEN2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_PEN2_sn))

summary(meteoPEN_f)
head(meteoPEN_f)

meteoPEN_f <- meteoPEN_f %>%
  mutate(MinRelativeHumidity = (MinRelativeHumidity * 1.5) + 7, 
         MaxRelativeHumidity = (MaxRelativeHumidity * 1.5) + 7)

meteoPEN_f$MaxRelativeHumidity <- pmin(pmax(meteoPEN_f$MaxRelativeHumidity, 0), 100)
meteoPEN_f$MinRelativeHumidity <- pmin(pmax(meteoPEN_f$MinRelativeHumidity, 0), 100)

ggplot(meteoPEN_f, aes(x = Date, y = MinRelativeHumidity, color = Site)) +
  geom_point()

for (i in seq_len(nrow(results_PEN2_sn))) {
  
  # Extract plant specific info
  plant_id <- results_PEN2_sn$plant_code[i]
  taw_i <- results_PEN2_sn$best_TAW[i]
  sp_name_i <- results_PEN2_sn$sp_name[i]
  
  # Get correct forest object
  #forest_obj <- get("forest_1_Arbutus unedo")
  forest_obj <- get(paste0("forest_6_", gsub("_", " ", sp_name_i)))
  
  # Modify soil
  soil_i <- modify_soil_taw(soilPEN, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoPEN_f, latitude = 42.280445, elevation = 1526)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("Sf_PEN_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}


### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^Sf_PEN_Am_|^Sf_PEN_Bu_")
sim_list <- mget(sim_names)

merge_leafpsi_range <- function(sim_list) {
  
  psi_min_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMin)
  psi_max_list <- lapply(sim_list, function(sim) sim$Plants$LeafPsiMax)
  
  # combine all individuals + simulations
  psi_min_all <- do.call(cbind, psi_min_list)
  psi_max_all <- do.call(cbind, psi_max_list)
  
  data.frame(
    Date = sim_list[[1]]$weather$dates,  # ✅ FIXED HERE
    Ymin = apply(psi_min_all, 1, min, na.rm = TRUE),
    Ymax = apply(psi_max_all, 1, max, na.rm = TRUE),
    Ymean = rowMeans((psi_min_all + psi_max_all)/2, na.rm = TRUE)
  )
}

### Subsection: check the simulation #############################################################

plot(Sf_PEN_Am_6PTAO01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO09, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Am_6PTAO10, type="LeafPsiRange", bySpecies = TRUE)

plot(Sf_PEN_Bu_6PTBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(Sf_PEN_Bu_6PTBS08, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

spwb_list2_PEN <- list(AO01 = Sf_PEN_Am_6PTAO01, AO02 = Sf_PEN_Am_6PTAO02, AO03 = Sf_PEN_Am_6PTAO03, AO04 = Sf_PEN_Am_6PTAO04,
                       AO05 = Sf_PEN_Am_6PTAO05, AO06 = Sf_PEN_Am_6PTAO06, AO07 = Sf_PEN_Am_6PTAO07, AO09 = Sf_PEN_Am_6PTAO09,
                       AO10 = Sf_PEN_Am_6PTAO10, BS01 = Sf_PEN_Bu_6PTBS01, BS02 = Sf_PEN_Bu_6PTBS02, BS03 = Sf_PEN_Bu_6PTBS03,
                       BS04 = Sf_PEN_Bu_6PTBS04, BS05 = Sf_PEN_Bu_6PTBS05, BS06 = Sf_PEN_Bu_6PTBS06, BS07 = Sf_PEN_Bu_6PTBS07,
                       BS08 = Sf_PEN_Bu_6PTBS08)

df_all <- do.call(rbind, lapply(names(spwb_list2_PEN), function(sp) {
  p <- plot(spwb_list2_PEN[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'
names(df_all)[names(df_all) == 'Site'] <- 'plant_code'

df_mean <- df_all
df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/"
setwd(dir_out)

library(writexl)
write_xlsx(df_mean, "gfPEN_data.xlsx")

gfPEN_data <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/4. Predicitions/gfPEN_data.xlsx")

gfPEN_data <- gfPEN_data %>%
  filter(Date == as.Date("2070-08-08"))

gfPEN_data <- gfPEN_data %>%
  mutate(Site = "PEN")

GRAPH2 <- rbind(GRAPH2, gfPEN_data)

#write_xlsx(gSiteData_t, "gSiteData_t.xlsx")


Boxplot <- Final_data %>%
  select(Site_name, sp_name, plant_code, WP_Sum_PDWP, WP_Sum_MDWP)

names(Boxplot)[names(Boxplot) == 'Site_name'] <- 'Site'

Boxplot <- Boxplot %>%
  mutate(Site = case_when(Site == "PAS_ASE" ~ "PAS", Site == "ALOS_BALAGUER" ~ "ALO", 
                          Site == "GARRAF" ~ "GAR", Site == "CINGLES_BERTI" ~ "CIN", 
                          Site == "MONTSEC" ~ "MON", Site == "PENTINA" ~ "PEN"))

Boxplot <- Boxplot %>%
  mutate(plant_code = substr(plant_code, 5, nchar(plant_code)))

Boxplot <- merge(Boxplot, GRAPH2, by=c("Site", "sp_name", "plant_code"), all= TRUE, na="NA")

Boxplot <- Boxplot %>%
  mutate(diff_PD = Y1 - (WP_Sum_PDWP/10), 
         diff_MD = Y2 - (WP_Sum_MDWP/10))


# reshape to long format
df_long <- Boxplot %>%
  pivot_longer(
    cols = c(diff_PD, diff_MD),
    names_to = "metric",
    values_to = "value"
  )

df_long <- df_long %>%
  mutate(value = ifelse(value <= 20, "DEAD", rep(value, each = 1)))

DEAD <- df_long %>%
  filter(value == "DEAD")

df_long <- df_long %>%
  filter(value != "DEAD")

str(df_long)
df_long$value <- as.numeric(df_long$value)

ggplot(df_long, aes(x = Site, y = value, color = sp_name, shape = metric)) +
  geom_point(position = position_dodge(width = 0.6)) +
  #facet_wrap(~ Site) +
  theme_bw()

order <- c("PAS", "ALO", "GAR", "CIN", "MON", "PEN")
df_long$Site <- factor(df_long$Site, levels = order)

order <- c("diff_PD", "diff_MD")
df_long$metric <- factor(df_long$metric, levels = order)

df_long2 <- df_long %>%
  mutate(metric = recode(metric,
                         diff_PD = "Delta*Psi[PD]",
                         diff_MD = "Delta*Psi[MD]"))

ggplot(df_long2, aes(x = Site, y = value, fill = sp_name)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#e57e89ff", "#cc5285ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens",  "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#e57e89ff", "#cc5285ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens",  "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  facet_wrap(~ metric, labeller = label_parsed) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(y = "Value (MPa)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.title.x = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))

ggsave("FuturepredictionLWP.pdf", width = 25, height = 14, units = "cm")

meteoVALUE <- meteo %>%
  group_by(Year, ID) %>%
  summarise(Temp = mean((MinTemperature + MaxTemperature)/2),
           Prec = sum(Precipitation))

meteoVALUE <- meteoVALUE %>%
  group_by(ID) %>%
  summarise(Temp = mean(Temp),
            Prec = sum(Prec))

meteoVALUE <- meteoVALUE %>%
  mutate(Site = case_when(
    ID == 1 ~ "PAS",
    ID == 2 ~ "ALO",
    ID == 3 ~ "GAR",
    ID == 4 ~ "CIN",
    ID == 5 ~ "MON",
    ID == 6 ~ "PEN",
    TRUE ~ NA_character_
  ))

meteoVALUE2 <- futuremeteo %>%
  group_by(Year, Site) %>%
  summarise(Temp2 = mean((MinTemperature + MaxTemperature)/2),
            Prec2 = sum(Precipitation))


meteoVALUE2 <- meteoVALUE2 %>%
  group_by(Site) %>%
  summarise(Temp2 = mean(Temp2),
            Prec2 = sum(Prec2))

meteoVALUE <- merge(meteoVALUE, meteoVALUE2, by=c("Site"), all= TRUE, na="NA")


meteoVALUE <- meteoVALUE %>%
  mutate(diffT = Temp2 - Temp,
         diffP = Prec2 - Prec)

meteoVALUE <- meteoVALUE %>%
  summarise(diffTm = mean(diffT),
            diffPm = mean(diffP))
