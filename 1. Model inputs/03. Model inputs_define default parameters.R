###################################################################################################
######## Living on the Edge: Modeling daily leaf energy and cabon balance #########################
############################ of Mediterranean Shrublands ##########################################
###################################################################################################

#R.version.string

rm(list=ls())#remove all global environment

library(pkgbuild)
#pkgbuild::check_build_tools(debug = TRUE)

library(remotes)
library(S7)

library(readxl)
library(rsconnect, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")
library(rlang)
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

getwd()
setwd("/storage/homefs/md26i579/GitHub/geco-bern/TRACESWP2_MEDFATE")

### LOAD THE CREATED FOREST AND SOIL OBJECT #######################################################

load("1. Model inputs/02. Model inputs_save soil object.RData")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 3. Control the simulation using the Defaultparameters #########################################
### Control the simulation using the Defaultparameters ############################################

names(control) # Control parameters should be left to their default values until their effect on simulations is fully understood!

control <- defaultControl(transpirationMode = "Sureau", soilDomains = "single")

#Option for transpirationMode = "Granier", "Sperry" or "Sureau"
#Option for soilDomains = "buckets" (for multi-bucket model), "single" (for single-domain Richards model) and "dual" (for dual-permeability model)
control$transpirationMode
control$soilDomains

control$bareSoilEvaporation #= "TRUE" evaporation from the soil surface is simulated
control$thermalCapacityLAI #= "1000000" Canopy thermal capacitance per LAI unit
control$segmentedXylemVulnerability #If FALSE leaf and root vulnerability curves will be equal to those of stem

control$leafCavitationRecovery= "rate"
control$stemCavitationRecovery= "rate"
#Option of leafCavitationRecovery / stemCavitationRecovery = "none" (no recovery), "annual" (complete recovery every first day of the year), "total" (instantaneous complete recovery), "rate" (following a rate of new leaf or sapwood formation)

control$defoliationParams$cavitationInducedDefoliation #= "TRUE" leaf hydraulic impairment induces defoliation

control$plantCapacitance #= "TRUE" effect of (symplasmic or apoplasmic) plant water compartments is considered in simulations
control$leafCuticularTranspiration #= "TRUE" effect of leaf cuticular transpiration is considered in simulations
control$stemCuticularTranspiration #= "FALSE" effect of stem cuticular transpiration is considered in simulations

control$stomatalSubmodel #either “Jarvis” or “Baldocchi”

### Save the Defaultparameters ####################################################################

# Define your working directory
getwd()

# Save everything in the current environment
save.image(file = "1. Model inputs/03. Model inputs_save default parameters.RData")
