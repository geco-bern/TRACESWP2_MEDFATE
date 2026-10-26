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

### LOAD THE CREATED FOREST OBJECT ################################################################

load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/03. Model inputs_save default parameters.RData")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 4. Create a simulation input object ###########################################################

### Combining forest, soil, control inputs and species parameters to an single input object #######

SpParamsMED_f %>%  count(Name) %>%  filter(n > 1)

#SpParamsMED_f %>% filter(Code_site == 1, Name == "Arbutus unedo") %>%  nrow()

#PAS_AU <- spwbInput(`forest_1_Arbutus unedo`, soilPAS, SpParamsMED_f %>% filter(Code_site == 1, Name == 	"Arbutus unedo"), control)
#PAS_PL <- spwbInput(`forest_1_Pistacia lentiscus`, soilPAS, SpParamsMED, control)
#PAS_RS <- spwbInput(`forest_1_Pistacia lentiscus`, soilPAS, SpParamsMED, control)
#PAS_SR <- spwbInput(`forest_1_Pistacia lentiscus`, soilPAS, SpParamsMED, control)

#ALO_AO <- spwbInput(`forest_2_Amelanchier ovalis`, soilALO, SpParamsMED, control)
#ALO_BS <- spwbInput(`forest_2_Buxus sempervirens`, soilALO, SpParamsMED, control)
#ALO_RS <- spwbInput(`forest_2_Rhamnus spp_`, soilALO, SpParamsMED, control)
#ALO_SR <- spwbInput(`forest_2_Salvia rosmarinus`, soilALO, SpParamsMED, control)

### Create this input for each site and species ###################################################

# Example for forest creation for all the sites and species together
#forest_list <- list()

#for(name in names(plant_split)) {
#f <- emptyforest()
#  f$shrubData <- as.data.frame(plant_split[[name]][, -1])

# Extract site from the split name
# site_num <- as.numeric(strsplit(name, "\\.")[[1]][1])
# soil <- soil_list[[site_num]]  # use the new soil_list

# f <- spwbInput(f, soil, SpParamsMED, control)

# forest_list[[name]] <- f
#}

#names(forest_list)

# Define a mapping from site number to soil object
soil_map <- list("1" = soilPAS, "2" = soilALO, "3" = soilGAR, "4" = soilCIN, "5" = soilMON, "6" = soilPEN)

# Optional: map site number to site code
site_codes <- c("1" = "PAS", "2" = "ALO", "3" = "GAR", "4" = "CIN", "5" = "MON", "6" = "PEN")

# Get all forest objects
forest_names <- ls(pattern = "^forest_")

for (fname in forest_names) {
  
  # --- Parse forest name ---
  parts <- unlist(strsplit(fname, "_"))
  site <- parts[2]
  species <- paste(parts[3:length(parts)], collapse = "_")
  species_name <- gsub("_", " ", species)  # spwb expects spaces
  
  # --- Select soil ---
  soil_used <- soil_map[[site]]
  
  # --- Filter species parameters (CRITICAL PART) ---
  SpParams_used <- SpParamsMED_f %>%
    mutate(Name = trimws(as.character(Name))) %>%
    filter(Code_site == as.numeric(site), Name == species_name) %>%
    distinct(Name, .keep_all = TRUE)
  
  # Safety check (highly recommended)
  if (nrow(SpParams_used) != 1) {
    warning(paste("Skipping", fname,
                  "- expected 1 parameter row, got", nrow(SpParams_used)))
    next
  }
  
  # --- Create output variable name ---
  species_letters <- paste(substr(unlist(strsplit(species, "_")), 1, 2), collapse = "")
  site_code <- site_codes[[site]]
  var_name <- paste0(site_code, "_", species_letters)
  
  # --- Create spwbInput object ---
  assign(var_name, spwbInput(get(fname), soil_used, SpParams_used, control))
}


### Save the model input object using the selected data ###########################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "04. Model inputs_save final model inputs object.RData")

