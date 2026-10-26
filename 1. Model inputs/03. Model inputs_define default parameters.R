###################################################################################################
######## Living on the Edge: Modeling daily leaf energy and cabon balance #########################
############################ of Mediterranean Shrublands ##########################################
###################################################################################################

#R.version.string

rm(list=ls())#remove all global environment

library(pkgbuild)
#pkgbuild::check_build_tools(debug = TRUE)

library(remotes)
library(Rtools)
#install.packages("S7", type = "binary", force = TRUE)
#remotes::install_local("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/0. Medfate code/medfate-main/medfate-main", force = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(terra)
library(tibble)
library(tidyr)
library(broom)

library(meteoland)
library(medfate)

#packageDescription("medfate")
#find.package("medfate")

library(medfateland)
library(sass)
library(S7)

### LOAD THE CREATED FOREST AND SOIL OBJECT #######################################################

load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/02. Model inputs_save soil object.RData")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 3. Control the simulation using the Defaultparameters #########################################
### Control the simulation using the Defaultparameters ############################################

control <- defaultControl(transpirationMode = "Sureau", soilDomains = "dual")

names(control) # Control parameters should be left to their default values until their effect on simulations is fully understood!

control$transpirationMode
control$soilDomains

#control$leafCavitationRecovery = "rate"

control$cavitationInducedDefoliation

### Save the Defaultparameters ####################################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "03. Model inputs_save default parameters.RData")
