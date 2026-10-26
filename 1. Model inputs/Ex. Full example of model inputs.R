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

data("SpParamsMED")
data("SpParamsDefinition")

###################################################################################################
### Example on how to apply MEDFATE model - MODEL INPUTS ##########################################
###################################################################################################

forest2 <- emptyforest()

### 1. Create a forest object with the individuals data ###########################################

forest <- medfate::exampleforest

class(forest)

plant_basalArea(forest, SpParamsMED)
stand_basalArea(forest)

plant_LAI(forest, SpParamsMED)
stand_LAI(forest, SpParamsMED)

above <- forest2aboveground(forest, SpParamsMED)
above

vprofile_leafAreaDensity(forest, SpParamsMED)
vprofile_leafAreaDensity(forest, SpParamsMED, byCohorts = TRUE, bySpecies = TRUE)

vprofile_PARExtinction(forest, SpParamsMED)
vprofile_SWRExtinction(forest, SpParamsMED)

vprofile_rootDistribution(forest, SpParamsMED)

#shinyplot(forest, SpParamsMED)

### 2. Create a soil object with the soil data ####################################################

spar <- defaultSoilParams(2)
spar

examplesoil <- soil(spar)
class(examplesoil)

#Would be important to get some values of soil hydraulic for future modelling parameters: like soil retention curves
#Representated as the relationship between soil water content and soil water potential: model for simulation using the Van Genuchten mdoel, but could also use the Saxton equation

soil_retentionCurvePlot(examplesoil, model="both")

### 3. Control the simulation using the Defaultparameters #########################################

control <- defaultControl()

names(control) # Control parameters should be left to their default values until their effect on simulations is fully understood!

### 4. Create a simulation input object ###########################################################

#Combining forest, soil, control inputs and species parameters to an single input object

x <- spwbInput(forest, examplesoil, SpParamsMED, control)

### 5. Weather forcing ############################################################################

data(examplemeteo)
head(examplemeteo, 2)


