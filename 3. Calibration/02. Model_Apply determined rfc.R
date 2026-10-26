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

###################################################################################################
### III. Calibration of soil rock fragment  #######################################################
###################################################################################################

### 2. Load the determine TAW for each individual #################################################

best_TAW_info <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/best_TAW_info.xlsx")
#View(best_TAW_info)

results_PAS2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")
results_ALO2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")
results_GAR2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")
results_CIN2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")
results_MON2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")
results_PEN2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PAS2_s.xlsx")

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

### Re-run the simulation for each individual with its determine TAW from calibration #############

results_PAS_s_test <- results_PAS2_s %>%
  filter(sp_name == "Arbutus_unedo")

results_list <- vector("list", nrow(results_PAS_s_test))

for (i in seq_len(nrow(results_PAS_s_test))) {
  
  # Extract plant specific info
  plant_id <- results_PAS_s_test$plant_code[i]
  taw_i <- results_PAS_s_test$best_TAW[i]
  sp_name_i <- results_PAS_s_test$sp_name[i]
  
  # Get correct forest object
  forest_obj <- get("forest_1_Arbutus unedo")
  
  # Modify soil
  soil_i <- modify_soil_taw(soilPAS, taw_i)
  
  # Build input
  input_i <- spwbInput(forest_obj, soil_i, SpParamsMED, control)
  
  # Run simulation
  sim_i <- spwb(input_i, meteoPAS, latitude = 41.159089, elevation = 106)

  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_PAS_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Check if TAW used if the new sim are correct ##################################################

TAW_summary <- data.frame(
  plant_code = sapply(results_list, function(x) x$plant),
  TAW_used   = sapply(results_list, function(x) x$TAW))

### Run the new water balance for multiple day - year 2023 and 2024 _ ALL SITES AND SP ############
#### Subsection: one graph per species for PAS ####################################################

plot(S_PAS_Ar_1PAAU01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Ar_1PAAU02, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Ar_1PAAU03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Ar_1PAAU05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Ar_1PAAU06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Ar_1PAAU07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Ar_1PAAU11, type="LeafPsiRange", bySpecies = TRUE)

#### Subsection: keep only the graph which have a proper leaf water potential range ###############

str(S_PAS_Ar_1PAAU01, max.level = 2)

sim_list <- list(S_PAS_Ar_1PAAU01, S_PAS_Ar_1PAAU02, #S_PAS_Ar_1PAAU03,
                 S_PAS_Ar_1PAAU05, S_PAS_Ar_1PAAU06, S_PAS_Ar_1PAAU07, S_PAS_Ar_1PAAU11)

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

psi_df <- merge_leafpsi_range(sim_list) #It is the right TAW for each IND

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL PAS #############

results_PAS2_sn <- results_PAS2_s 

results_PAS2_sn <- results_PAS2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_PAS2_sn))

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
  sim_i <- spwb(input_i, meteoPAS, latitude = 41.159089, elevation = 106)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_PAS_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Check if TAW used if correct ##################################################################

TAW_summary <- data.frame(
  plant_code = sapply(results_list, function(x) x$plant),
  TAW_used   = sapply(results_list, function(x) x$TAW))

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_PAS_Ar_|^S_PAS_Pi_|^S_PAS_Rh_|^S_PAS_Sa_")
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

#plot(S_PAS_Pi_1PAPL02, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Pi_1PAPL03, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Pi_1PAPL04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi_1PAPL06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi_1PAPL07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi_1PAPL08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi_1PAPL10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Pi_1PAPL11, type="LeafPsiRange", bySpecies = TRUE)

plot(S_PAS_Rh_1PARA02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Rh_1PARA03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Rh_1PARA06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Rh_1PARA07, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Rh_1PARA08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Rh_1PARA09, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Rh_1PARA10, type="LeafPsiRange", bySpecies = TRUE)

plot(S_PAS_Sa_1PASR02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR03, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_PAS_Sa_1PASR04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PAS_Sa_1PASR12, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_PAS_Ar_|^S_PAS_Pi_")

spwb_list2 <- list(AU01 = S_PAS_Ar_1PAAU01, AU02 = S_PAS_Ar_1PAAU02, AU05 = S_PAS_Ar_1PAAU05, AU03 = S_PAS_Ar_1PAAU03,
                   AU06 = S_PAS_Ar_1PAAU06, AU07 = S_PAS_Ar_1PAAU07, AU11 = S_PAS_Ar_1PAAU11,
                   PL06 = S_PAS_Pi_1PAPL06, PL07 = S_PAS_Pi_1PAPL07, PL08 = S_PAS_Pi_1PAPL08, PL10 = S_PAS_Pi_1PAPL10, PL11 = S_PAS_Pi_1PAPL11,
                   RA02 = S_PAS_Rh_1PARA02, RA03 = S_PAS_Rh_1PARA03, RA06 = S_PAS_Rh_1PARA06, RA07 = S_PAS_Rh_1PARA07, RA09 = S_PAS_Rh_1PARA09,
                   SR02 = S_PAS_Sa_1PASR02, SR03 = S_PAS_Sa_1PASR03, SR05 = S_PAS_Sa_1PASR05, SR06 = S_PAS_Sa_1PASR06, SR07 = S_PAS_Sa_1PASR07, 
                   SR10 = S_PAS_Sa_1PASR10, SR12 = S_PAS_Sa_1PASR12)

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

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier_ovalis", "Arbutus_unedo", "Buxus_sempervirens", "Pistacia_lentiscus", "Rhamnus_alaternus", "Salvia_rosmarinus")
df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "PAS_ASE") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
         sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()
         
Finalsum <- Finalsum %>%
  filter(Site_name == "PAS_ASE") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
    Date = case_when(Campaign == "Spring" ~ as.Date("2024-04-23"), Campaign == "Summer" ~ as.Date("2024-07-16")))

library(writexl)

#write_xlsx(df_mean, "gPAS_data.xlsx")
df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gPAS_data.xlsx")

gPAS <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 3) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#cc5285ff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Arbutus unedo", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#cc5285ff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Arbutus unedo", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("PAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x =  element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))
gPAS

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL ALO #############

results_ALO2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_ALO2_s.xlsx")

results_ALO2_sn <- results_ALO2_s 

results_ALO2_sn <- results_ALO2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_ALO2_sn))

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
  sim_i <- spwb(input_i, meteoALO, latitude = 41.907495, elevation = 431)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_ALO_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_ALO_Am_|^S_ALO_Bu_|^S_ALO_Rh_|^S_ALO_Sa_")
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

plot(S_ALO_Am_2ABAO02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Am_2ABAO13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_ALO_Bu_2ABBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS09, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS12, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Bu_2ABBS13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_ALO_Rh_2ABRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA09, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Rh_2ABRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_ALO_Sa_2ABSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR12, type="LeafPsiRange", bySpecies = TRUE)
plot(S_ALO_Sa_2ABSR13, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_ALO_Am_|^S_ALO_Bu_|^S_ALO_Rh_|^S_ALO_Sa_")
sim_list <- mget(sim_names)

spwb_list2_ALO <- list(AO02 = S_ALO_Am_2ABAO02, AO03 = S_ALO_Am_2ABAO03, AO04 = S_ALO_Am_2ABAO04, AO05 = S_ALO_Am_2ABAO05,  
                       AO06 = S_ALO_Am_2ABAO06, AO07 = S_ALO_Am_2ABAO07, AO10 = S_ALO_Am_2ABAO10, AO11 = S_ALO_Am_2ABAO11,  AO13 = S_ALO_Am_2ABAO13, 
                       BS02 = S_ALO_Bu_2ABBS02, BS03 = S_ALO_Bu_2ABBS03, BS04 = S_ALO_Bu_2ABBS04, BS05 = S_ALO_Bu_2ABBS05,BS06 = S_ALO_Bu_2ABBS06,
                       BS09 = S_ALO_Bu_2ABBS09, BS11 = S_ALO_Bu_2ABBS11, BS12 = S_ALO_Bu_2ABBS12, BS13 = S_ALO_Bu_2ABBS13,
                       RA02 = S_ALO_Rh_2ABRA02, RA03 = S_ALO_Rh_2ABRA03, RA04 = S_ALO_Rh_2ABRA04, RA08 = S_ALO_Rh_2ABRA08, RA09 = S_ALO_Rh_2ABRA09,
                       RA11 = S_ALO_Rh_2ABRA11, RA12 = S_ALO_Rh_2ABRA12, RA13 = S_ALO_Rh_2ABRA13, 
                       SR02 = S_ALO_Sa_2ABSR02, SR03 = S_ALO_Sa_2ABSR03, SR04 = S_ALO_Sa_2ABSR04, SR05 = S_ALO_Sa_2ABSR05,
                       SR06 = S_ALO_Sa_2ABSR06, SR11 = S_ALO_Sa_2ABSR11, SR12 = S_ALO_Sa_2ABSR12, SR13 = S_ALO_Sa_2ABSR13)

#names(spwb_list2_ALO)[sapply(spwb_list2_ALO, is.null)]
#sapply(spwb_list2_ALO, is.null)

df_all <- do.call(rbind, lapply(names(spwb_list2_ALO), function(sp) {
  p <- plot(spwb_list2_ALO[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")
#df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "ALOS_BALAGUER") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
           sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()

Finalsum <- Finalsum %>%
  filter(Site_name == "ALOS_BALAGUER") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
         Date = case_when(Campaign == "Spring" ~ as.Date("2024-05-08"), Campaign == "Summer" ~ as.Date("2024-07-20")))

df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gALO_data.xlsx")

gALO <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 3) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("ALO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text.x =  element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y =  element_blank(),         
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))
gALO

#write_xlsx(df_mean, "gALO_data.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL GAR #############

gc()

results_GAR2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_GAR2_s.xlsx")

results_GAR2_sn <- results_GAR2_s 

results_GAR2_sn <- results_GAR2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_GAR2_sn))

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
  sim_i <- spwb(input_i, meteoGAR, latitude = 41.350724, elevation = 584)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_GAR_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_GAR_Ar_|^S_GAR_Bu_|^S_GAR_Pi_|^S_GAR_Rh_|^S_GAR_Sa_")
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

#plot(S_GAR_Ar_3GFAU01, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU04, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU06, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU07, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU08, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU09, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Ar_3GFAU10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Ar_3GFAU12, type="LeafPsiRange", bySpecies = TRUE)

plot(S_GAR_Bu_3GFBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Bu_3GFBS11, type="LeafPsiRange", bySpecies = TRUE)

#plot(S_GAR_Pi_3GFPL04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Pi_3GFPL05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Pi_3GFPL06, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Pi_3GFPL07, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Pi_3GFPL09, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Pi_3GFPL10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Pi_3GFPL12, type="LeafPsiRange", bySpecies = TRUE)

#plot(S_GAR_Rh_3GFRA01, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_GAR_Rh_3GFRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Rh_3GFRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_GAR_Sa_3GFSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_GAR_Sa_3GFSR09, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_GAR_Ar_|^S_GAR_Bu_|^S_GAR_Pi_|^S_GAR_Rh_|^S_GAR_Sa_")
sim_list <- mget(sim_names)

spwb_list2_GAR <- list(AU12 = S_GAR_Ar_3GFAU12,
  BS01 = S_GAR_Bu_3GFBS01, BS03 = S_GAR_Bu_3GFBS03, BS04 = S_GAR_Bu_3GFBS04, BS05 = S_GAR_Bu_3GFBS05,
  BS06 = S_GAR_Bu_3GFBS06, BS07 = S_GAR_Bu_3GFBS07, BS08 = S_GAR_Bu_3GFBS08, BS11 = S_GAR_Bu_3GFBS11,
  PL05 = S_GAR_Pi_3GFPL05, PL06 = S_GAR_Pi_3GFPL06, PL12 = S_GAR_Pi_3GFPL12, RA03 = S_GAR_Rh_3GFRA03,
  RA04 = S_GAR_Rh_3GFRA04, RA06 = S_GAR_Rh_3GFRA06, RA07 = S_GAR_Rh_3GFRA07, RA12 = S_GAR_Rh_3GFRA12,
  RA13 = S_GAR_Rh_3GFRA13, SR01 = S_GAR_Sa_3GFSR01, SR02 = S_GAR_Sa_3GFSR02, SR03 = S_GAR_Sa_3GFSR03,
  SR04 = S_GAR_Sa_3GFSR04, SR06 = S_GAR_Sa_3GFSR06, SR08 = S_GAR_Sa_3GFSR08, SR09 = S_GAR_Sa_3GFSR09)

#names(spwb_list2_ALO)[sapply(spwb_list2_ALO, is.null)]
#sapply(spwb_list2_ALO, is.null)

df_all <- do.call(rbind, lapply(names(spwb_list2_GAR), function(sp) {
  p <- plot(spwb_list2_GAR[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")
#df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "GARRAF") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
           sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()

Finalsum <- Finalsum %>%
  filter(Site_name == "GARRAF") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
         Date = case_when(Campaign == "Spring" ~ as.Date("2024-05-14"), Campaign == "Summer" ~ as.Date("2024-07-25")))

df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gGAR_data.xlsx")

gGAR <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 3) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#e57e89ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Amelanchier ovalis", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#e57e89ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Amelanchier ovalis", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("GAR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x =  element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y =  element_blank(),         
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),  
        panel.border = element_rect(fill = NA, color = "black"))
gGAR

#write_xlsx(df_mean, "gGAR_data.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL CIN #############

gc()

results_CIN2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_CIN2_s.xlsx")

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
  sim_i <- spwb(input_i, meteoCIN, latitude = 41.720588, elevation = 639)

  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_CIN_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_CIN_Ar_|^S_CIN_Pi_|^S_CIN_Sa_")
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

plot(S_CIN_Ar_4CBAU01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU03, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Ar_4CBAU04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU05, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Ar_4CBAU06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Ar_4CBAU09, type="LeafPsiRange", bySpecies = TRUE)

#plot(S_CIN_Pi_4CBPL01, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL02, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL03, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL04, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL05, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL06, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL07, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_CIN_Pi_4CBPL11, type="LeafPsiRange", bySpecies = TRUE)

plot(S_CIN_Sa_4CBSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_CIN_Sa_4CBSR12, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_CIN_Ar_|^S_CIN_Pi_|^S_CIN_Sa_")
sim_list <- mget(sim_names)

spwb_list2_CIN <- list(AU01 = S_CIN_Ar_4CBAU01, AU02 = S_CIN_Ar_4CBAU02, AU03 = S_CIN_Ar_4CBAU03,
  AU05 = S_CIN_Ar_4CBAU05, AU07 = S_CIN_Ar_4CBAU07, AU08 = S_CIN_Ar_4CBAU08, AU09 = S_CIN_Ar_4CBAU09,
  SR01 = S_CIN_Sa_4CBSR01, SR02 = S_CIN_Sa_4CBSR02, SR03 = S_CIN_Sa_4CBSR03, SR04 = S_CIN_Sa_4CBSR04,
  SR05 = S_CIN_Sa_4CBSR05, SR06 = S_CIN_Sa_4CBSR06, SR08 = S_CIN_Sa_4CBSR08, SR12 = S_CIN_Sa_4CBSR12)

df_all <- do.call(rbind, lapply(names(spwb_list2_CIN), function(sp) {
  p <- plot(spwb_list2_CIN[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")
#df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "CINGLES_BERTI") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
           sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()

Finalsum <- Finalsum %>%
  filter(Site_name == "CINGLES_BERTI") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
         Date = case_when(Campaign == "Spring" ~ as.Date("2024-05-21"), Campaign == "Summer" ~ as.Date("2024-08-01")))

Final_long <- Final_long %>%
  filter(sp_name != "Pistacia_lentiscus")

df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gCIN_data.xlsx")

mid_dates <- as.Date(c("2023-08-05", "2024-08-05"))

gCIN <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 3) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#cc5285ff", "#2db3b4ff"), 
                    label = c("Arbutus unedo", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#cc5285ff", "#2db3b4ff"), 
                     label = c("Arbutus unedo", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  scale_x_date(breaks = mid_dates, labels = c("2023", "2024")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("CIN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))
gCIN

#write_xlsx(df_mean, "gCIN_data.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL MON #############

gc()

results_MON2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_MON2_s.xlsx")

results_MON2_sn <- results_MON2_s 

results_MON2_sn <- results_MON2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_MON2_sn))

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
  sim_i <- spwb(input_i, meteoMON, latitude = 42.032490, elevation = 1298)
  
  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_MON_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_MON_Am_|^S_MON_Bu_|^S_MON_Rh_|^S_MON_Sa_")
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

#plot(S_MON_Am_5MCAO02, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO03, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO05, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO08, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO10, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO11, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO12, type="LeafPsiRange", bySpecies = TRUE)
#plot(S_MON_Am_5MCAO13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_MON_Bu_5MCBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Bu_5MCBS12, type="LeafPsiRange", bySpecies = TRUE)

plot(S_MON_Rh_5MCRA01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA08, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA09, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA10, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA11, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA12, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Rh_5MCRA13, type="LeafPsiRange", bySpecies = TRUE)

plot(S_MON_Sa_5MCSR01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_MON_Sa_5MCSR08, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_MON_Am_|^S_MON_Bu_|^S_MON_Rh_|^S_MON_Sa_")
sim_list <- mget(sim_names)

spwb_list2_MON <- list(BS01 = S_MON_Bu_5MCBS01, BS02 = S_MON_Bu_5MCBS02, BS07 = S_MON_Bu_5MCBS07, BS08 = S_MON_Bu_5MCBS08,
                       BS10 = S_MON_Bu_5MCBS10, BS11 = S_MON_Bu_5MCBS11, BS12 = S_MON_Bu_5MCBS12, RA01 = S_MON_Rh_5MCRA01,
                       RA02 = S_MON_Rh_5MCRA02, RA08 = S_MON_Rh_5MCRA08, RA09 = S_MON_Rh_5MCRA09, RA10 = S_MON_Rh_5MCRA10,
                       RA11 = S_MON_Rh_5MCRA11, RA12 = S_MON_Rh_5MCRA12, RA13 = S_MON_Rh_5MCRA13, SR01 = S_MON_Sa_5MCSR01,
                       SR02 = S_MON_Sa_5MCSR02, SR03 = S_MON_Sa_5MCSR03, SR04 = S_MON_Sa_5MCSR04, SR05 = S_MON_Sa_5MCSR05,
                       SR06 = S_MON_Sa_5MCSR06, SR07 = S_MON_Sa_5MCSR07, SR08 = S_MON_Sa_5MCSR08)

df_all <- do.call(rbind, lapply(names(spwb_list2_MON), function(sp) {
  p <- plot(spwb_list2_MON[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")
#df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "MONTSEC") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
           sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()

Finalsum <- Finalsum %>%
  filter(Site_name == "MONTSEC") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
         Date = case_when(Campaign == "Spring" ~ as.Date("2024-05-28"), Campaign == "Summer" ~ as.Date("2024-08-03")))

Final_long <- Final_long %>%
  filter(sp_name != "Amelanchier_ovalis")

df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gMON_data.xlsx")

gMON <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 3) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  scale_x_date(breaks = mid_dates, labels = c("2023", "2024")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("MON") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))
gMON

#write_xlsx(df_mean, "gMON_data.xlsx")

### Re-run the simulation for each individual with its determine TAW from calibration _ ALL PEN #############

gc()

results_PEN2_s <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration/results_PEN2_s.xlsx")

results_PEN2_sn <- results_PEN2_s 

results_PEN2_sn <- results_PEN2_sn %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_alaternus", "Rhamnus_spp", sp_name))

results_list <- vector("list", nrow(results_PEN2_sn))

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
  sim_i <- spwb(input_i, meteoPEN, latitude = 42.280445, elevation = 1526)
  

  results_list[[i]] <- list(sim = sim_i, TAW = taw_i, soil = soil_i, plant = plant_id)
  
  # Create safe names
  specie_short <- substr(gsub(" ", "", sp_name_i), 1, 2)
  plant_safe <- gsub("[^A-Za-z0-9]", "", plant_id)
  
  obj_name <- paste0("S_PEN_", specie_short, "_", plant_safe)
  
  assign(obj_name, sim_i)
}

### Extract the data of the new simulation ########################################################

sim_names <- ls(pattern = "^S_PEN_Am_|^S_PEN_Bu_")
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

plot(S_PEN_Am_6PTAO01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO09, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Am_6PTAO10, type="LeafPsiRange", bySpecies = TRUE)

plot(S_PEN_Bu_6PTBS01, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS02, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS03, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS04, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS05, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS06, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS07, type="LeafPsiRange", bySpecies = TRUE)
plot(S_PEN_Bu_6PTBS08, type="LeafPsiRange", bySpecies = TRUE)

### Create the graph ##############################################################################

sim_names <- ls(pattern = "^S_PEN_Am_|^S_PEN_Bu_")
sim_list <- mget(sim_names)

spwb_list2_PEN <- list(AO01 = S_PEN_Am_6PTAO01, AO02 = S_PEN_Am_6PTAO02, AO03 = S_PEN_Am_6PTAO03, AO04 = S_PEN_Am_6PTAO04,
                       AO05 = S_PEN_Am_6PTAO05, AO06 = S_PEN_Am_6PTAO06, AO07 = S_PEN_Am_6PTAO07, AO09 = S_PEN_Am_6PTAO09,
                       AO10 = S_PEN_Am_6PTAO10, BS01 = S_PEN_Bu_6PTBS01, BS02 = S_PEN_Bu_6PTBS02, BS03 = S_PEN_Bu_6PTBS03,
                       BS04 = S_PEN_Bu_6PTBS04, BS05 = S_PEN_Bu_6PTBS05, BS06 = S_PEN_Bu_6PTBS06, BS07 = S_PEN_Bu_6PTBS07,
                       BS08 = S_PEN_Bu_6PTBS08)

df_all <- do.call(rbind, lapply(names(spwb_list2_PEN), function(sp) {
  p <- plot(spwb_list2_PEN[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  
  df <- p$data
  df$Site <- sp   # better name than Species
  
  df
}))

names(df_all)[names(df_all) == 'Cohort'] <- 'sp_name'

df_mean <- df_all %>%
  group_by(Date, sp_name) %>%
  summarise(meanY1 = mean(Y1, na.rm = TRUE),
            #seY1   = sd(Y1, na.rm = TRUE) / sqrt(sum(!is.na(Y1))),
            meanY2 = mean(Y2, na.rm = TRUE),
            #seY2   = sd(Y2, na.rm = TRUE) / sqrt(sum(!is.na(Y2))),
            mean_t = (meanY1 + meanY2) /2,
            .groups = "drop")

df_mean$sp_name <- gsub(" ", "_", df_mean$sp_name)

str(df_mean)

df_mean <- as.data.frame(df_mean)

df_mean$sp_name <- as.character(df_mean$sp_name)

df_mean <- df_mean %>%
  mutate(sp_name = ifelse(sp_name == "Rhamnus_spp", "Rhamnus_alaternus", rep(sp_name, each =1)))

#Plot species effects using ggplot2
order <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")
#df_mean$sp_name <- factor(df_mean$sp_name, levels = order)

Finalsum <- Final_data %>%
  filter(Site_name == "PENTINA") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            .groups = "drop")

Finalsum <- Finalsum %>%
  rowwise() %>%
  mutate(se = sd(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP), na.rm = TRUE) /
           sqrt(sum(!is.na(c(WP_Sum_MDWP, WP_Sum_PDWP, WP_Spr_MDWP, WP_Spr_PDWP))))) %>%
  ungroup()

Finalsum <- Finalsum %>%
  filter(Site_name == "PENTINA") %>%
  group_by(Site_name, sp_name) %>%
  mutate(WP_Sum = (WP_Sum_PDWP + WP_Sum_MDWP) / 2,
         WP_Spr = (WP_Spr_PDWP + WP_Spr_MDWP) / 2) %>%
  distinct(Site_name, sp_name, .keep_all = TRUE) %>%
  ungroup()

Finalsum <- Finalsum[, -c(3:6)]

Final_long <- Finalsum %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Type", values_to = "WP") %>%
  mutate(Campaign = case_when(grepl("Spr", Type) ~ "Spring", grepl("Sum", Type) ~ "Summer"),
         Date = case_when(Campaign == "Spring" ~ as.Date("2024-06-04"), Campaign == "Summer" ~ as.Date("2024-08-08")))

df_mean <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/gPEN_data.xlsx")

gPEN <- ggplot(df_mean, aes(x = Date, color = sp_name)) +
  geom_rect(aes(xmin = as.Date("2023-06-20"), xmax = as.Date("2023-09-20"),, ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-06-20"), xmax = as.Date("2024-09-20"), ymin = -Inf, ymax = Inf), fill = "#fafafa", alpha = 0.3, inherit.aes = FALSE) +
  geom_ribbon(data = df_mean, aes(ymin = meanY1, ymax = meanY2, color = sp_name, fill = sp_name), alpha = 0.2, color = NA) +
  geom_line(data = df_mean, aes(y = mean_t, color = sp_name), linewidth = 0.6) +
  #geom_line(aes(y = meanY1), linewidth = 0.6) +
  geom_errorbar(data = Final_long, aes(x = Date, ymin = WP - se, ymax = WP + se), color = "black", width = 2) +
  geom_point(data = Final_long, aes(x = Date, y = WP, fill = sp_name), color = "black", size = 2.2, shape = 21) +
  scale_fill_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                    label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  scale_color_manual(values = c("#e57e89ff", "#b752ccff", "#51a3cbff", "#2db3b4ff"), 
                     label = c("Amelanchier ovalis", "Buxus sempervirens", "Rhamnus alaternus", "Salvia rosmarinus")) +
  #scale_color_manual(values = c("red", "black", "grey", "blue"),
  #labels = c("Ar", "Pi", "Rh", "Sa")) +
  scale_x_date(breaks = mid_dates, labels = c("2023", "2024")) +
  ylab(expression("Leaf water potential (MPa)")) +
  xlab("Date") +
  theme_bw() +
  ggtitle("PEN") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        text = element_text(size=15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        axis.text  = element_text(color="black"),
        axis.ticks.length = unit(10, "pt"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        panel.border = element_rect(fill = NA, color = "black"))
gPEN

library(writexl)
#write_xlsx(df_mean, "gPEN_data.xlsx")

### Merge all the graph together #########################################################

gPAS <- gPAS + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)
gALO <- gALO + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)
gGAR <- gGAR + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)
gCIN <- gCIN + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)
gMON <- gMON + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)
gPEN <- gPEN + theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10)) + ylim(-10, 0)

all_species <- c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus")

species_colors <- c("Amelanchier ovalis" = "#e57e89ff", "Arbutus unedo" = "#cc5285ff", 
                    "Buxus sempervirens" = "#b752ccff", "Pistacia lentiscus" = "#5c52cbff", 
                    "Rhamnus alaternus" = "#51a3cbff", "Salvia rosmarinus" = "#2db3b4ff")

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
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))

#ggsave("Waterbalance_rfc_2023-2024.LEGEND.pdf", width = 35, height = 18, units = "cm")


library(gridExtra)
library(cowplot)

ggdraw() +
  draw_plot(gPAS + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.005, y= 0.53, width= 0.36, height= 0.42, scale = 1) +
  draw_plot(gALO + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.365, y= 0.53, width= 0.32, height= 0.42, scale = 1) + 
  draw_plot(gGAR + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.682, y= 0.53, width= 0.32, height= 0.42, scale = 1) +
  draw_plot(gCIN + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.005, y= 0.03, width= 0.36, height= 0.49, scale = 1) + 
  draw_plot(gMON + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.365, y= 0.03, width= 0.32, height= 0.49, scale = 1) + 
  draw_plot(gPEN + theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")), x= 0.682, y= 0.03, width= 0.32, height= 0.49, scale = 1)

ggsave("Waterbalance_rfc_2023-2024.2.pdf", width = 35, height = 18, units = "cm")

### Save the water balance after fragment calibration ###############################################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/3. Calibration"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "02. Model_save simulation with new fragment calibration.RData")
