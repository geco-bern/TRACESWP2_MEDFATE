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

###################################################################################################
### Example on how to apply MEDFATE model #########################################################
###################################################################################################

### 1. Water balance input object #################################################################

names(x)

x$soil
x$cohorts
x$above
x$below

x$paramsTranspiration

### 2. Basic water balance ########################################################################

str(examplemeteo)
data(examplemeteo)

#The call to function spwb() needs the water balance input object, the weather data frame, latitude and elevation:

S <- spwb(x, examplemeteo, latitude = 41.82592, elevation = 100)

class(S)
names(S)

head(S$WaterBalance, 2)
names(S$Soil)
names(S$Plants)

summary(S, freq="months",FUN=sum, output="WaterBalance")
summary(S, freq="months",FUN=mean, output="PlantStress")

#extract(S, level = "forest")

plot(S, type = "PET_Precipitation")

### 3. Evaluating model performance ###############################################################

evaluation_metric(S, exampleobs, type = "SWC", metric = "MAE")
evaluation_stats(S, exampleobs, type = "SWC")

evaluation_plot(S, exampleobs, type = "SWC", plotType = "dynamics")

### 4. Advanced water/energy balance ##############################################################

control <- defaultControl("Sperry")
control$subdailyResults <- TRUE
x_adv <- spwbInput(exampleforest, examplesoil, SpParamsMED, control)

names(x_adv)

g1 <- hydraulics_vulnerabilityCurvePlot(x_adv, type="stem", speciesNames = TRUE)
g2 <- hydraulics_vulnerabilityCurvePlot(x_adv, type="leaf", speciesNames = TRUE)
cowplot::plot_grid(g1, g2, ncol = 2)

d = 100
meteovec <- unlist(examplemeteo[d,-1])
meteovec

#sd1 <- spwb_day(x_adv, date, meteovec, 
#              latitude = 41.82592, elevation = 100, 
#              slope= 0, aspect = 0)

#plot(sd1, type = "LeafTranspiration", bySpecies = TRUE)







