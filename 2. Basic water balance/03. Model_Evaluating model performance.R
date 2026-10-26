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

source("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance/02. Model_Advance water balance.R")

###################################################################################################
### II. Forest water and energy balance (practice) ################################################
###################################################################################################

### 3. Evaluating model performance ###############################################################
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

#### Subsection: Extract simulated spring and summer leaf water potential for PAS #################

spwb_list <- list(Ar = S_PAS_Ar, Pi = S_PAS_Pi, Rh = S_PAS_Rh, Sa = S_PAS_Sa)

df_pas <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "PAS"
  df
  }))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_pas <- df_pas %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="PAS"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Subsection: Extract simulated spring and summer leaf water potential for ALO #################

spwb_list <- list(Am = S_ALO_Am, Bu = S_ALO_Bu, Rh = S_ALO_Rh, Sa = S_ALO_Sa)

df_alo <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "ALO"
  df
}))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_alo <- df_alo %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="ALO"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Subsection: Extract simulated spring and summer leaf water potential for GAR #################

spwb_list <- list(Ar = S_GAR_Ar, Bu = S_GAR_Bu, Pi = S_GAR_Pi, Rh = S_GAR_Rh, Sa = S_GAR_Sa)

df_gar <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "GAR"
  df
}))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_gar <- df_gar %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="GAR"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Subsection: Extract simulated spring and summer leaf water potential for CIN #################

spwb_list <- list(Ar = S_CIN_Ar, Pi = S_CIN_Pi, Sa = S_CIN_Sa)

df_cin <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "CIN"
  df
}))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_cin <- df_cin %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="CIN"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Subsection: Extract simulated spring and summer leaf water potential for MON #################

spwb_list <- list(Am = S_MON_Am, Bu = S_MON_Bu, Rh = S_MON_Rh, Sa = S_MON_Sa)

df_mon <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "MON"
  df
}))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_mon <- df_mon %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="MON"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Subsection: Extract simulated spring and summer leaf water potential for PEN #################

spwb_list <- list(Am = S_PEN_Am, Bu = S_PEN_Bu)

df_pen <- do.call(rbind, lapply(names(spwb_list), function(sp) {
  p <- plot(spwb_list[[sp]], type="LeafPsiRange", bySpecies=TRUE, draw=FALSE)
  df <- p$data
  df$Species <- sp
  df$Site <- "PEN"
  df
}))

#That gives us the date, Y1 (predawn), Y2 (midday), Species, Site

df_pen <- df_pen %>%
  filter(Date %in% Leafwaterpotential_measurementdate$Date[Leafwaterpotential_measurementdate$Site=="PEN"]) %>%
  select(Date, Species, Site, Predawn_LWP = Y1, Midday_LWP = Y2)

#### Merge with the measured leaf water potential of each site #####################################

df_pas2 <- Final_data %>%
  #filter(Site_name == "PAS_ASE") %>%
  group_by(Site_name, sp_name) %>%
  summarise(WP_Spr_PDWP = mean(WP_Spr_PDWP, na.rm = TRUE) / 10,
            WP_Spr_MDWP = mean(WP_Spr_MDWP, na.rm = TRUE) / 10,
            WP_Sum_PDWP = mean(WP_Sum_PDWP, na.rm = TRUE) / 10,
            WP_Sum_MDWP = mean(WP_Sum_MDWP, na.rm = TRUE) / 10)

# Pivot to long format
df_long <- df_pas2 %>%
  rename(Species = sp_name, Site = Site_name) %>%
  pivot_longer(cols = starts_with("WP_"), names_to = "Date", values_to = "Measured")

df_long <- df_long %>%
  mutate(Species = ifelse(Species == "Amelanchier_ovalis", "Am",
                          ifelse(Species == "Arbutus_unedo", "Ar",
                                 ifelse(Species == "Buxus_sempervirens", "Bu",
                                        ifelse(Species == "Pistacia_lentiscus", "Pi",
                                               ifelse(Species == "Rhamnus_alaternus", "Rh",
                                                      ifelse(Species == "Salvia_rosmarinus", "Sa", Species)))))),
         Type = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Sum_PDWP"), "Predawn_LWP",
                       ifelse(Date %in% c("WP_Spr_MDWP", "WP_Sum_MDWP"), "Midday_LWP", NA)))

df_long <- df_long %>%
  mutate(Site = ifelse(Site == "PAS_ASE", "PAS",
                          ifelse(Site == "ALOS_BALAGUER", "ALO",
                                 ifelse(Site == "CINGLES_BERTI", "CIN",
                                        ifelse(Site == "GARRAF", "GAR",
                                               ifelse(Site == "MONTSEC", "MON",
                                                      ifelse(Site == "PENTINA", "PEN", Site)))))))

df_long <- df_long %>%
  mutate(Campaign = ifelse(Date %in% c("WP_Spr_PDWP", "WP_Spr_MDWP"), "Spring",
                  ifelse(Date %in% c("WP_Sum_PDWP", "WP_Sum_MDWP"), "Summer", NA)))

df_long <- df_long[, -c(3)]

# Add the campaign of the modelled

df_pas <- df_pas %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))

df_alo <- df_alo %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))

df_gar <- df_gar %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))

df_cin <- df_cin %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))

df_mon <- df_mon %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))

df_pen <- df_pen %>%
  mutate(Campaign = ifelse(substr(Date, 7, 7) %in% c("4", "5", "6"), "Spring",
                           ifelse(substr(Date, 7, 7) %in% c("7", "8"), "Summer", NA)))


# Assuming your data frame is called df_pas2_long, merge all of them from each site

df_long2 <- merge(df_pas, df_alo, by=c("Site", "Species", "Campaign", "Date","Predawn_LWP", "Midday_LWP"), all= TRUE, na="NA")
df_long2 <- merge(df_long2, df_gar, by=c("Site", "Species", "Campaign", "Date", "Predawn_LWP", "Midday_LWP"), all= TRUE, na="NA")
df_long2 <- merge(df_long2, df_cin, by=c("Site", "Species", "Campaign", "Date", "Predawn_LWP", "Midday_LWP"), all= TRUE, na="NA")
df_long2 <- merge(df_long2, df_mon, by=c("Site", "Species", "Campaign", "Date", "Predawn_LWP", "Midday_LWP"), all= TRUE, na="NA")
df_long2 <- merge(df_long2, df_pen, by=c("Site", "Species", "Campaign", "Date", "Predawn_LWP", "Midday_LWP"), all= TRUE, na="NA")

df_long2 <- df_long2 %>%
  pivot_longer(cols = c(Predawn_LWP, Midday_LWP),  # columns to pivot
               names_to = "Type",                   # new column for the type
               values_to = "Modelled")               # new column for the values

str(df_long)
#df_long$Date <- as.Date(df_long$Date)

#Merge the modelled and measured data

df_long <- merge(df_long, df_long2, by=c("Site", "Species", "Campaign", "Type"), all= TRUE, na="NA")

### Graphically check the modelled vs. measured for PAS ###########################################

species_colors <- c("Ar" = "red", "Am" = "#61D04F", "Bu" = "purple", "Pi" = "black", "Rh" = "grey", "Sa" = "blue")

order <- c("PAS", "ALO", "GAR", "CIN", "MON", "PEN")  # Reverse order for plotting
df_long$Site <- factor(df_long$Site, levels = order)

graph <- ggplot(df_long, aes(x = Measured, y = Modelled)) +
  geom_point(aes(color = Species, shape = Type), size = 2) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 0.9) + 
  scale_color_manual(values = species_colors, drop = FALSE) +          #KEEP ALL SPECIES
  facet_wrap(~Site) +
  theme_bw() 
graph

### Remove outliners from the two modelled #########################################################

#df_long <- df_long %>%
  #filter(Modelled > -13)

### Graphically check the modelled vs. measured for PAS : BETTER ###################################

graph <- ggplot(df_long, aes(x = Measured, y = Modelled)) +
  geom_point(aes(color = Species, shape = Type), size = 2) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 0.9) + 
  scale_color_manual(values = species_colors, drop = FALSE) +          #KEEP ALL SPECIES
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +                   # General 1:1 linetheme_bw() +
  facet_wrap(~Site) +
  theme_bw() 
graph

#### Test the fit using linear regression for each site ###########################################

# Fit linear regression
lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "PAS"))
summary(lm_model)

lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "ALO"))
summary(lm_model)

lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "GAR"))
summary(lm_model)

lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "CIN"))
summary(lm_model)

lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "MON"))
summary(lm_model)

lm_model <- lm(Measured ~ Modelled, data = df_long %>% filter(Site == "PEN"))
summary(lm_model)

### Add number in graph ###########################################################################

dat_text <- data.frame(label = c("R² = 0.44 **", "R² = 0.67 ***", "R² = 0.00", "R² = 0.47 **", "R² = 0.59 ***", "R² = 0.42*"),
                       Site = c("PAS", "ALO", "GAR", "CIN", "MON", "PEN"),
                       x = c(-1, -1, -1, -1, -1, -1),
                       y = c(-12, -12, -12, -12, -12, -12))

graph <- graph + geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label, group= NULL), color = "black", size = 5, show.legend = F, hjust = 0.5, inherit.aes = FALSE)
graph

#ggsave("LeafWP_measuredvsmodelled.pdf", width = 40, height = 25, units = "cm")

### Save the evaluation of the model vs measured data ##############################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/2. Basic water balance"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "03. Model_save  weather forcing.RData")
