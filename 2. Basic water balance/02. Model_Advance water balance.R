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
library(Rcpp)

### LOAD THE CREATED FINAL INPUTS OBJECT ###########################################################

#load("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/01. Model_save basic water balance.RData")

#setwd("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs")

#source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/01. Model inputs_create forest object.R")
#source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/02. Model inputs_create soil object.R")
#source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/03. Model inputs_define default parameters.R")
#source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/04. Model inputs_create input object.R")
#source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs/05. Model inputs_create weater forcing object.R")

source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/01. Model_Basic water balance.R")

###################################################################################################
### II. Forest water and energy balance (practice) ################################################
###################################################################################################

### 4. Advanced water/energy balance ##############################################################
### Control the simulation using the Defaultparameters again (but not necessary) ##################

control <- defaultControl(transpirationMode = "Sureau", soilDomains = "dual")
control$subdailyResults <- TRUE # But we don't want to do that, is that correct?

control$leafCavitationRecovery
control$cavitationInducedDefoliation

### Used the input object for STEM hydraulics vulnerability Curves ################################
#### Subsection: one graph per species for PAS ####################################################

#PAS_Ar, PAS_Pi, #PAS_Rh, #PAS_Sa, #ALO_Am, #ALO_Bu, #ALO_Rh, #ALO_Sa

#PAS_Ar <- spwbInput(`forest_1_Arbutus unedo`, soilPAS, SpParamsMED, control) #THIS WAS DONE L. 464

gPAS_Ar_stem <- hydraulics_vulnerabilityCurvePlot(PAS_Ar, type="stem", speciesNames = TRUE)
gPAS_Pi_stem <- hydraulics_vulnerabilityCurvePlot(PAS_Pi, type="stem", speciesNames = TRUE)
gPAS_Rh_stem <- hydraulics_vulnerabilityCurvePlot(PAS_Rh, type="stem", speciesNames = TRUE)
gPAS_Sa_stem <- hydraulics_vulnerabilityCurvePlot(PAS_Sa, type="stem", speciesNames = TRUE)

cowplot::plot_grid(gPAS_Ar_stem, gPAS_Pi_stem, gPAS_Rh_stem, gPAS_Sa_stem, ncol = 4)

gGAR_Ar_stem <- hydraulics_vulnerabilityCurvePlot(GAR_Ar, type="stem", speciesNames = TRUE)
gGAR_Pi_stem <- hydraulics_vulnerabilityCurvePlot(GAR_Pi, type="stem", speciesNames = TRUE)
gGAR_Rh_stem <- hydraulics_vulnerabilityCurvePlot(GAR_Rh, type="stem", speciesNames = TRUE)
gGAR_Sa_stem <- hydraulics_vulnerabilityCurvePlot(GAR_Sa, type="stem", speciesNames = TRUE)

cowplot::plot_grid(gPAS_Ar_stem, gPAS_Pi_stem, gPAS_Rh_stem, gPAS_Sa_stem,
                   gGAR_Ar_stem, gGAR_Pi_stem, gGAR_Rh_stem, gGAR_Sa_stem,
                   ncol = 4)

#### Subsection: one graph for ALL species for PAS ################################################

# List your inputs
pas_list <- list(PAS_Ar = PAS_Ar, PAS_Pi = PAS_Pi, PAS_Rh = PAS_Rh, PAS_Sa = PAS_Sa)

df_all <- do.call(rbind, lapply(names(pas_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(pas_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gPAS <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "grey", "blue"), 
                     label = c("Ar", "Pi", "Rh", "Sa")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("PAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPAS

#### Subsection: one graph for ALL species for ALO ################################################

# List your inputs
alo_list <- list(ALO_Am = ALO_Am, ALO_Bu = ALO_Bu, ALO_Rh = ALO_Rh, ALO_Sa = ALO_Sa)

df_all <- do.call(rbind, lapply(names(alo_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(alo_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gALO <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("ALO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gALO

#### Subsection: one graph for ALL species for GAR ################################################

# List your inputs
gar_list <- list(GAR_Ar = GAR_Ar, GAR_Bu = GAR_Bu, GAR_Pi = GAR_Pi, GAR_Rh = GAR_Rh, GAR_Sa = GAR_Sa)

df_all <- do.call(rbind, lapply(names(gar_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(gar_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gGAR <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "purple", "black", "grey", "blue"), 
                     label = c("Ar", "Bu", "Pi", "Rh", "Sa")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("GAR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gGAR

#### Subsection: one graph for ALL species for CIN ################################################

# List your inputs
cin_list <- list(CIN_Ar = CIN_Ar, CIN_Pi = CIN_Pi, CIN_Sa = CIN_Sa)

df_all <- do.call(rbind, lapply(names(cin_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(cin_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gCIN <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "blue"), 
                     label = c("Ar", "Pi", "Sa")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("CIN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gCIN

#### Subsection: one graph for ALL species for MON ################################################

# List your inputs
mon_list <- list(MON_Am = MON_Am, MON_Bu = MON_Bu, MON_Rh = MON_Rh, MON_Sa = MON_Sa)

df_all <- do.call(rbind, lapply(names(mon_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(mon_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gMON <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("MON") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gMON

#### Subsection: one graph for ALL species for PEN ################################################

# List your inputs
pen_list <- list(PEN_Am = PEN_Am, PEN_Bu = PEN_Bu)

df_all <- do.call(rbind, lapply(names(pen_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(pen_list[[sp]], #List of the different species in PAS
                                           type = "stem",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gPEN <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple"), 
                     label = c("Am", "Bu")) +
  xlab(expression("Stem pressure (-MPa)")) +
  ylab(expression("Stem conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("PEN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPEN

range(df_all$Psi, na.rm = TRUE)

#### Subsection: make a graph with all species per site to see the hydraulics inputs ##############

gPAS <- gPAS + theme(legend.position = "none") +  ylim(0, 90)
gALO <- gALO + theme(legend.position = "none") +  ylim(0, 90)
gGAR <- gGAR + theme(legend.position = "none") +  ylim(0, 90)
gCIN <- gCIN + theme(legend.position = "none") +  ylim(0, 90)
gMON <- gMON + theme(legend.position = "none") +  ylim(0, 90)
gPEN <- gPEN + theme(legend.position = "none") +  ylim(0, 90)

all_species <- c("Ar", "Am", "Bu", "Pi", "Rh", "Sa")

species_colors <- c("Ar" = "red", "Am" = "#61D04F", "Bu" = "purple", "Pi" = "black", "Rh" = "grey", "Sa" = "blue")

legend_plot <- ggplot(data.frame(Psi = 1, Conductance = 1, Species = factor(all_species, levels = all_species)),
                      aes(x = Psi, y = Conductance, color = Species)) +
  geom_line() +
  scale_color_manual(values = species_colors, drop = FALSE) +          #KEEP ALL SPECIES
  guides(
    color = guide_legend(title = "Species", nrow = 1)) +
  theme_bw() + 
  theme(legend.position = "bottom")

legend <- cowplot::get_legend(legend_plot)

plots <- cowplot::plot_grid(gPAS, gALO, gGAR, gCIN, gMON, gPEN, ncol = 3)

cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.1)) + 
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggsave("Stem_vulnerabilitycurves_Medfate.pdf", width = 30, height = 20, units = "cm")

PAS_Ar$paramsTranspiration$VCleaf_P50
PAS_Ar$paramsTranspiration$VCstem_P50
PAS_Ar$paramsTranspiration$VCroot_P50




### Used the input object for LEAF hydraulics vulnerability Curves ################################
#### Subsection: one graph for ALL species for PAS ################################################

# List your inputs
pas_list <- list(PAS_Ar = PAS_Ar, PAS_Pi = PAS_Pi, PAS_Rh = PAS_Rh, PAS_Sa = PAS_Sa)

df_all <- do.call(rbind, lapply(names(pas_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(pas_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gPAS <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "grey", "blue"), 
                     label = c("Ar", "Pi", "Rh", "Sa")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("PAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPAS

#### Subsection: one graph for ALL species for ALO ################################################

# List your inputs
alo_list <- list(ALO_Am = ALO_Am, ALO_Bu = ALO_Bu, ALO_Rh = ALO_Rh, ALO_Sa = ALO_Sa)

df_all <- do.call(rbind, lapply(names(alo_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(alo_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gALO <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("ALO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gALO

#### Subsection: one graph for ALL species for GAR ################################################

# List your inputs
gar_list <- list(GAR_Ar = GAR_Ar, GAR_Bu = GAR_Bu, GAR_Pi = GAR_Pi, GAR_Rh = GAR_Rh, GAR_Sa = GAR_Sa)

df_all <- do.call(rbind, lapply(names(gar_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(gar_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gGAR <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "purple", "black", "grey", "blue"), 
                     label = c("Ar", "Bu", "Pi", "Rh", "Sa")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("GAR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gGAR

#### Subsection: one graph for ALL species for CIN ################################################

# List your inputs
cin_list <- list(CIN_Ar = CIN_Ar, CIN_Pi = CIN_Pi, CIN_Sa = CIN_Sa)

df_all <- do.call(rbind, lapply(names(cin_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(cin_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gCIN <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "blue"), 
                     label = c("Ar", "Pi", "Sa")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("CIN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gCIN

#### Subsection: one graph for ALL species for MON ################################################

# List your inputs
mon_list <- list(MON_Am = MON_Am, MON_Bu = MON_Bu, MON_Rh = MON_Rh, MON_Sa = MON_Sa)

df_all <- do.call(rbind, lapply(names(mon_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(mon_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gMON <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("MON") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gMON

#### Subsection: one graph for ALL species for PEN ################################################

# List your inputs
pen_list <- list(PEN_Am = PEN_Am, PEN_Bu = PEN_Bu)

df_all <- do.call(rbind, lapply(names(pen_list), function(sp) {
  # Get the vulnerability curve matrix
  mat <- hydraulics_vulnerabilityCurvePlot(pen_list[[sp]], #List of the different species in PAS
                                           type = "leaf",
                                           speciesNames = TRUE,
                                           draw = FALSE)
  
  # Convert to a tidy data frame
  df <- as_tibble(mat)
  df$Psi <- as.numeric(rownames(mat))  # psi values are in the rownames
  
  # Pivot to long format
  df_long <- pivot_longer(df, -Psi, names_to = "Cohort", values_to = "Conductance")
  
  # Add species label
  df_long$Species <- sp
  df_long
}))

# Now plot all curves
gPEN <- ggplot(df_all, aes(x = -Psi, y = Conductance, color = Species)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple"), 
                     label = c("Am", "Bu")) +
  xlab(expression("Leaf pressure (-MPa)")) +
  ylab(expression("Leaf conductance "~"(mmol"~m^-2~s^-1~MPa^-1~")")) +
  theme_bw() +
  ggtitle("PEN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPEN

#### Subsection: make a graph with all species per site to see the hydraulics inputs ##############

gPAS <- gPAS + theme(legend.position = "none") +  ylim(0, 6)
gALO <- gALO + theme(legend.position = "none") +  ylim(0, 6)
gGAR <- gGAR + theme(legend.position = "none") +  ylim(0, 6)
gCIN <- gCIN + theme(legend.position = "none") +  ylim(0, 6)
gMON <- gMON + theme(legend.position = "none") +  ylim(0, 6)
gPEN <- gPEN + theme(legend.position = "none") +  ylim(0, 6)

all_species <- c("Ar", "Am", "Bu", "Pi", "Rh", "Sa")

species_colors <- c("Ar" = "red", "Am" = "#61D04F", "Bu" = "purple", "Pi" = "black", "Rh" = "grey", "Sa" = "blue")

legend_plot <- ggplot(data.frame(Psi = 1, Conductance = 1, Species = factor(all_species, levels = all_species)),
                      aes(x = Psi, y = Conductance, color = Species)) +
  geom_line() +
  scale_color_manual(values = species_colors, drop = FALSE) +          #KEEP ALL SPECIES
  guides(
    color = guide_legend(title = "Species", nrow = 1)) +
  theme_bw() + 
  theme(legend.position = "bottom")

legend <- cowplot::get_legend(legend_plot)

plots <- cowplot::plot_grid(gPAS, gALO, gGAR, gCIN, gMON, gPEN, ncol = 3)

cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.1)) + 
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggsave("Leaf_vulnerabilitycurves_Medfate.pdf", width = 30, height = 20, units = "cm")






### Run water balance for multiple day - year 2023 and 2024 #######################################
#### Subsection: one graph per species for PAS ####################################################

plot(S_PAS_Ar, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Rh, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa, type="LeafPsiRange", bySpecies = TRUE)

#### Subsection: one graph for ALL species for PAS ################################################

spwb_list <- list(Ar = S_PAS_Ar, Pi = S_PAS_Pi, Rh = S_PAS_Rh, Sa = S_PAS_Sa)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))


gPAS <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "grey", "blue"),
                     labels = c("Ar", "Pi", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("PAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPAS

#### Subsection: one graph for ALL species for ALO ################################################

spwb_list <- list(Am = S_ALO_Am, Bu = S_ALO_Bu, Rh = S_ALO_Rh, Sa = S_ALO_Sa)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))


gALO <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("ALO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gALO

#### Subsection: one graph for ALL species for GAR ################################################

spwb_list <- list(Ar = S_GAR_Ar, Bu = S_GAR_Bu, Pi = S_GAR_Pi, Rh = S_GAR_Rh, Sa = S_GAR_Sa)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))

gGAR <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("red", "purple", "black", "grey", "blue"), 
                     label = c("Ar", "Bu", "Pi", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("GAR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gGAR

#### Subsection: one graph for ALL species for CIN ################################################

spwb_list <- list(Ar = S_CIN_Ar, Pi = S_CIN_Pi, Sa = S_CIN_Sa)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))

gCIN <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("red", "black", "blue"), 
                     label = c("Ar", "Pi", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("CIN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gCIN

#### Subsection: one graph for ALL species for MON ################################################

spwb_list <- list(Am = S_MON_Am, Bu = S_MON_Bu, Rh = S_MON_Rh, Sa = S_MON_Sa)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))


gMON <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple", "grey", "blue"),
                     label = c("Am", "Bu", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("MON") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gMON

#### Subsection: one graph for ALL species for PEN ################################################

spwb_list <- list(Am = S_PEN_Am, Bu = S_PEN_Bu)

df_all <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  # Get ggplot object WITHOUT drawing
  p <- plot(spwb_list[[sp]],type = "LeafPsiRange",bySpecies = TRUE, draw = FALSE)
  # Extract the data used by ggplot
  df <- p$data
  # Add PAS label
  df$Species <- sp
  df
}))


gPEN <- ggplot(df_all, aes(x = Date, color = Species)) +
  geom_line(aes(y = Y2), linewidth = 0.6) +
  scale_color_manual(values = c("#61D04F", "purple"), 
                     label = c("Am", "Bu")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("PEN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
gPEN

#### Subsection: make a graph with all species per site to see the hydraulics inputs ##############

gPAS <- gPAS + theme(legend.position = "none") + ylim(-10, 0)
gALO <- gALO + theme(legend.position = "none") + ylim(-10, 0)
gGAR <- gGAR + theme(legend.position = "none") + ylim(-10, 0)
gCIN <- gCIN + theme(legend.position = "none") + ylim(-10, 0)
gMON <- gMON + theme(legend.position = "none") + ylim(-10, 0)
gPEN <- gPEN + theme(legend.position = "none") + ylim(-10, 0)

all_species <- c("Ar", "Am", "Bu", "Pi", "Rh", "Sa")

species_colors <- c("Ar" = "red", "Am" = "#61D04F", "Bu" = "purple", "Pi" = "black", "Rh" = "grey", "Sa" = "blue")

legend_plot <- ggplot(data.frame(Psi = 1, Conductance = 1, Species = factor(all_species, levels = all_species)),
                      aes(x = Psi, y = Conductance, color = Species)) +
  geom_line() +
  scale_color_manual(values = species_colors, drop = FALSE) +          #KEEP ALL SPECIES
  guides(
    color = guide_legend(title = "Species", nrow = 1)) +
  theme_bw() + 
  theme(legend.position = "bottom")

legend <- cowplot::get_legend(legend_plot)

plots <- cowplot::plot_grid(gPAS, gALO, gGAR, gCIN, gMON, gPEN, ncol = 3)

cowplot::plot_grid(plots, legend, ncol = 1, rel_heights = c(1, 0.1)) + 
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggsave("Waterbalance2023-2024.pdf", width = 30, height = 20, units = "cm")


### Save the advanve water balance ##################################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "02. Model_save advance water balance.RData")

