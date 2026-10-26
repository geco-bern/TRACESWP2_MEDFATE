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
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 1. Create a forest object with the individuals data ###########################################
### Import the shrubs details #####################################################################

TRACES_plant_info <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/1. TRACE field work/1. Protocols & Datasheets/1. Site description/TRACES_plant_final_selection_info2.xlsx")
#View(TRACES_plant_info)

TRACES_plant_info <- TRACES_plant_info[, -c(7)]

### Import plant cover, height and interaction data ###############################################

TRACES_plant_info2 <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/1. TRACE field work/2. Measurements/5. Additional data/TRACES_plant.info2.xlsx")
#View(TRACES_plant_height)

TRACES_plant_info <- merge(TRACES_plant_info, TRACES_plant_info2, by=c("Code_site", "Site_name", "Species_name", "Ind_code"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$Replicate),]

### Calculate mean plot individual shrub cover ####################################################
#### Subsection: calculation using the approximately elliptical in shape shrub equation ###########

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(Pl_cover = (pi * (PD1/100) * (PD2/100)) / 4) #IN m²

TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$Pl_cover),]

#### Subsection: Incorporate the data of Pol and keep only the necessary column ###################

Final_data <- readRDS("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Final_data_Pol.rds")
#View(Final_data)

summary(Final_data)
str(Final_data)

#Canopy cover, total leaf area (Total LA), and height are the simplest direct inputs for LAI.

Final_data <- Final_data %>%
  mutate(LeafArea_m2 = Total_LA / 10000)  # cm² → m²

Final_data2 <- Final_data[, -c(1,3,4,6:21,23:49)]

names(Final_data2)[names(Final_data2) == "Site_code"] <- "Code_site"
names(Final_data2)[names(Final_data2) == "plant_code"] <- "Ind_code"

TRACES_plant_info <- merge(TRACES_plant_info, Final_data2, by=c("Code_site", "Ind_code"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$Replicate),]
TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$ID),]

BranchlengthsforLA_Pol <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/BranchlengthsforLA_Pol.xlsx")
#View(BranchlengthsforLA_Pol)

TRACES_plant_info <- merge(TRACES_plant_info, BranchlengthsforLA_Pol, by=c("Site_name", "Species_name", "Ind_code"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info %>%
  mutate(Height_m = Height / 100,
         LeafArea_1m_m2 = (LeafArea_m2 *100) / TB_length) 

TRACES_plant_info <- TRACES_plant_info %>%
  mutate(LA = (LeafArea_1m_m2)* 1 * Height_m * N_stems)

#### Subsection: Estimation for LAI using optimal formula: NO POSSIBLE ############################

TRACES_plant_info <- TRACES_plant_info %>%
  mutate(LAIfrombranch = LA / Pl_cover) #The best way to determine the LAI would be this equation. BUT, we have leaf area for a m-branch, so the calculation is WRONG here

#### Subsection: calculation LAI using Medfate webpage, SpParams + De Caceres 2019 paper ##########

SpParamsMED2 <- SpParamsMED %>%
  filter(Name %in% c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens",
                     "Pistacia lentiscus", "Rhamnus spp.", "Salvia rosmarinus"))

# Create the data frame
shrubs_param <- data.frame(
  Species_name = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"),
  aarea = c(0.3508, 0.6284, 0.9734499, 0.0172867, 0.3508, 0.1544),
  barea = c(1.9641, 1.8804, 1.807175, 2.965406, 1.9641, 2.2532),
  afine = c(0.2068, 0.2375, 0.3032149, 1.1897118, 0.2068, 1.3196),
  bfine = c(0.8514, 0.8631, 0.7163349, 0.7337279, 0.8514, 0.8929))

TRACES_plant_info <- merge(TRACES_plant_info, shrubs_param, by=c("Species_name"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(Pl_cover_cm = (pi * (PD1) * (PD2)) / 4) #IN cm²  -- THIS WAS HOW AREA WAS CALCULATED ABOVE
#Pl_cover3 = aarea * Height^barea)    #IN cm², THIS IS HOW AREA IS CALCULATED IN MEDFATE

#### Subsection: calculation dry weight using Medfate webpage, SpParams + De Caceres 2019 paper ####

# Calculate volume using ellipsoid formula
TRACES_plant_info <- TRACES_plant_info %>%
  mutate(Dryweight = afine * (Height_m *(Pl_cover))^bfine) #IN m² for plant cover

#### Subsection: calculation density using Medfate webpage, SpParams + De Caceres 2019 paper #######

#Total cover calculation in % -- NOT NEEDED ANYMORE AS WE DON'T USE THE DENSITY IN THE MEDFATE EQUATION ANYMORE

#TRACES_total_cover <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/1. TRACE field work/2. Measurements/5. Additional data/TRACES_total cover.xlsx")
#View(TRACES_total_cover)

#TRACES_total_cover <- TRACES_total_cover %>%
# mutate(Cover = ifelse(Species_name %in% c("Soil", "Herbaceus", "Moss", "Rock"), NA, 2))

#TRACES_total_cover <- TRACES_total_cover[!is.na(TRACES_total_cover$Cover),]

#TRACES_total_cover <- TRACES_total_cover[!is.na(TRACES_total_cover$Net_cover),]

#TRACES_total_cover <- TRACES_total_cover %>%
# group_by(Site) %>%
# mutate(Cover2 = sum(Net_cover))

#TRACES_total_cover <- TRACES_total_cover %>%
# group_by(Species_name) %>%
# mutate(Cover_spmost = sum(Net_cover))

#TRACES_total_cover <- TRACES_total_cover %>%
# group_by(Site, Species_name) %>%
# mutate(Cover_spmostsi = sum(Net_cover))

#TRACES_total_cover <- TRACES_total_cover[, -c(1,3,5:10)]

#TRACES_total_cover <- TRACES_total_cover[, -c(1,3,5:9)]

#TRACES_total_cover <- TRACES_total_cover %>%
# group_by(Species_name) %>%
# mutate(Percentage_cover =  (100*Cover_spmostsi)/Cover2)

#TRACES_total_cover <- TRACES_total_cover[, -c(3:5)]

#TRACES_total_cover <- TRACES_total_cover[!duplicated(TRACES_total_cover), ]

#TRACES_total_cover <- TRACES_total_cover %>%
# filter(Species_name %in% c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", 
#                     "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"))

#names(TRACES_total_cover)[names(TRACES_total_cover) == 'Site'] <- 'Site_name'

#Merge with the old infos 

#TRACES_plant_info <- merge(TRACES_plant_info, TRACES_total_cover, by=c("Species_name", "Site_name"), all= TRUE, na="NA")

#TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$Ind_code),]

#TRACES_plant_info <- TRACES_plant_info %>%         
# mutate(Shrub_density = (Percentage_cover/100)/(Pl_cover2/10000), 
#        Shrub_density2 = (Percentage_cover/100)/(Pl_cover3/10000))

#TRACES_plant_info <- TRACES_plant_info %>% 
# mutate(Finefuelbiomass = Totaldryweight * Shrub_density,
#        Finefuelbiomass2 = Totaldryweight2 * Shrub_density2) 

#### Subsection: calculation foliar biomass using Medfate, SpParams + De Caceres 2019 paper #######

shrubs_param2 <- data.frame(
  Species_name = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"),
  r635 = c(3.709808, 1.669141, 1.667465, 2, 2.475149, 2)) #2 is added for Pistacia lentiscus and Salvia because no values are available

TRACES_plant_info <- merge(TRACES_plant_info, shrubs_param2, by=c("Species_name"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(Foliarbiomass = (Dryweight/r635)/Pl_cover)
#Foliarbiomass2 = Dryweight2/Pl_cover) #Using the recalculated ash and bsh total

#### Subsection: check SLA measured from Pol vs. SLA MEDFATE ######################################

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(SLA_m_kg = SLA / 10) #change from cm²/g to m²/kg

OutlineGraph <- ggplot(TRACES_plant_info, aes(x=Code_site, y=SLA_m_kg)) +
  geom_point(aes(color=Species_name), size = 2)
OutlineGraph

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(SLA_m_kg = ifelse(SLA_m_kg <= 1.5, NA, rep(SLA_m_kg, each = 1))) #change from cm²/g to m²/kg

TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$SLA_m_kg),]

#Check the average of SLA for each species overall, to compare with the SpParams of MEDFATE
test <-  TRACES_plant_info %>%
  group_by(Species_name) %>%
  summarise(SLA = mean(SLA_m_kg))

#Check the distribution using a boxplot, to see if there is outliners

# Ensure 'Code_site' is a factor with the correct levels in the desired order

# Create the data frame
shrubs_param3 <- data.frame(
  Species_name = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"),
  SLA_medfate = c(11.334074, 8.073095, 8.458495, 9.568382, 13.427982, 10.795498))

TRACES_plant_info <- merge(TRACES_plant_info, shrubs_param3, by=c("Species_name"), all= TRUE, na="NA")


order <- c("PAS_ASE", "ALOS", "GARRAF", "CINGLES", "MONTSEC", "PENTINA")  # Reverse order for plotting
TRACES_plant_info$Site_name <- factor(TRACES_plant_info$Site_name, levels = order)

site_labels <- c(PAS_ASE = "PAS", ALOS = "ALO", GARRAF = "GAR", CINGLES = "CIN", MONTSEC = "MON", PENTINA = "PEN")

SLAgraph <- ggplot(TRACES_plant_info, aes(x = Site_name, y = SLA_m_kg)) +
  geom_boxplot(aes(color=Species_name)) +
  geom_point(aes(y = SLA_medfate, color=Species_name), shape = 11, position = position_dodge(width = 0.75), size = 3) +
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  labs(x = "Site", y = "SLA (m²/kg)", color = "Species") +
  scale_x_discrete(labels = site_labels) +
  theme_bw() +
  theme(text = element_text(size = 18))
SLAgraph

#ggsave("SLAgraph.pdf", width = 25, height = 15, units = "cm")

#### Subsection: final calculation LAI using Medfate webpage, SpParams + De Caceres 2019 paper ####

TRACES_plant_info <- TRACES_plant_info %>% 
  mutate(LAI_medfate = Foliarbiomass * SLA_m_kg)

LAI <- ggplot(TRACES_plant_info, aes(x = Site_name, y = LAI_medfate)) +
  geom_boxplot(aes(color=Species_name)) +
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  labs(x = "Site", y = "LAI (m²/m²)", color = "Species") +
  scale_x_discrete(labels = site_labels) +
  theme_bw() +
  theme(text = element_text(size = 18))
LAI

#ggsave("LAIgraph.pdf", width = 25, height = 15, units = "cm")

#### Subsection: comparing the LAI medfate estimation with LAI branch factor ######################

Graph <- TRACES_plant_info %>%
  mutate(Site_name = case_when(Site_name == "PAS_ASE" ~ "PAS", Site_name == "ALOS" ~ "ALO", Site_name == "GARRAF" ~ "GAR",
                               Site_name == "CINGLES" ~ "CIN", Site_name == "MONTSEC" ~ "MON", Site_name == "PENTINA" ~ "PEN"))

# Fit linear regression
lm_model <- lm(LAI_medfate ~ LAIfrombranch, data = Graph)
summary(lm_model)

# Ensure 'Code_site' is a factor with the correct levels in the desired order
order <- c("PAS", "ALO", "GAR", "CIN", "MON", "PEN")  # Reverse order for plotting
Graph$Site_name <- factor(Graph$Site_name, levels = order)

Graph <- Graph %>%
  mutate(Species_name = case_when(Species_name == "Amelanchier ovalis" ~ "Am", Species_name == "Arbutus unedo" ~ "Ar", Species_name == "Buxus sempervirens" ~ "By",
                                  Species_name == "Pistacia lentiscus" ~ "Pi", Species_name == "Rhamnus alaternus" ~ "Rh", Species_name == "Salvia rosmarinus" ~ "Sa"))

LAIcomp <- ggplot(Graph, aes(x=LAI_medfate, y=LAIfrombranch)) +
  geom_point(aes(color=Species_name, fill = Species_name, shape = Site_name), size = 2, position = position_dodge(width = 0.7)) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +  # 1:1 line
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_fill_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 8)) +
  ylab(expression("LAI"["estimate from measured LA"], sep="")) +
  xlab(expression("LAI"["medfate, coeff modeled"], sep="")) +
  labs(color = "Species", fill = "Species", shape = "Site") +
  theme_bw() +
  theme(text = element_text(size = 18))
LAIcomp  

dat_text <- data.frame(label = c("R² = 0.00", "slope = 0.07", "p-value = 0.212"),
                       x = c(0.5, 0.5, 0.5), 
                       y = c(6, 5.7, 5.4))

LAIcomp <- LAIcomp + geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label, group= NULL), color = "black", size = 4.5, show.legend = F, hjust = 0.5, inherit.aes = FALSE)
LAIcomp

#ggsave("LAIcomp.pdf", width = 20, height = 15, units = "cm")

#### Subsection: check if the species parameters are correct for the missing species ###############

#Check in the aarea and barea determine from the De Caceres calculation are more or less good for the species which are not in the paper
#To remember what I did that, see Figure2_MD in the MEDFATE Figure folder powerpoint

LinEq <- ggplot(TRACES_plant_info, aes(x=log(Height_m), y=log(Pl_cover))) +
  geom_point(aes(color=Species_name, fill = Species_name, shape = Site_name), size = 2) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#61D04F", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Amelanchier ovalis")) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "red", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Arbutus unedo")) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "purple", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Buxus sempervirens")) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "black", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Pistacia lentiscus")) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "grey", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Rhamnus alaternus")) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "blue", linewidth = 0.7, data = TRACES_plant_info %>% filter(Species_name == "Salvia rosmarinus")) +
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_fill_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 8)) +
  ylab(expression("log P"["cover"], sep="")) +
  xlab(expression("log H", sep="")) +
  labs(color = "Species", fill = "Species", shape = "Site") +
  theme_bw() +
  theme(text = element_text(size = 18))
LinEq

#ggsave("LinEq.pdf", width = 20, height = 15, units = "cm")

#Fit the power-law model for Arbutus unedo and extract the parameters using the linear model built - lm(x ~ y)

model_sp <- lm(log(Height_m) ~ log(Pl_cover), data = TRACES_plant_info %>% filter(Species_name == "Arbutus unedo"))
summary(model_sp)

barea <- coef(model_sp)[2]
aarea <- exp(coef(model_sp)[1])

barea
aarea

#Now fit the power-law model for all the species and extract the parameters using the linear model built

params <- TRACES_plant_info %>%
  group_by(Species_name) %>%
  summarise(model = list(lm(log(Height_m) ~ log(Pl_cover), data = pick(everything())))) %>%
  mutate(barea_recalc = sapply(model, function(m) coef(m)[2]),
         aarea_recalc = sapply(model, function(m) exp(coef(m)[1])))

params <- merge(params, shrubs_param, by=c("Species_name"), all= TRUE, na="NA")

#Check the correlation between aarea from MEDFATE and the one extracted here

LinEq2 <- ggplot(params, aes(x=aarea, y=aarea_recalc)) +
  geom_point(aes(color=Species_name, fill = Species_name), size = 2) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_fill_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  ylab(expression("ash"["area"], sep="")) +
  xlab(expression("ash"["area recalculated"], sep="")) +
  labs(color = "Species", fill = "Species") +
  theme_bw() +
  theme(text = element_text(size = 18))
LinEq2

LinEq3 <- ggplot(params, aes(x=barea, y=barea_recalc)) +
  geom_point(aes(color=Species_name, fill = Species_name), size = 2) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  scale_fill_manual(values = c("#61D04F", "red", "purple", "black", "grey", "blue")) +
  ylab(expression("bsh"["area"], sep="")) +
  xlab(expression("bsh"["area recalculated"], sep="")) +
  scale_y_continuous(position = "right") +
  labs(color = "Species", fill = "Species") +
  theme_bw() +
  theme(text = element_text(size = 18))
LinEq3

library(gridExtra)
library(cowplot)

ggdraw() +
  draw_plot(LinEq2 + theme(legend.position="none", plot.margin = rep(unit(0,"null"),4)), 
            x= 0.035, y= 0.35, width= 0.432, height= 0.40, scale = 1) +
  draw_plot(LinEq3 + theme(legend.position=c(0,-0.3), legend.direction = "horizontal", plot.margin = rep(unit(0,"null"),4)), 
            x= 0.467, y= 0.35, width= 0.4327, height= 0.40, scale = 1) 

#ggsave("LinEq2.pdf", width = 40, height = 30, units = "cm")




### Rename column to fit the SpParams file ########################################################

#Create a summary of the data for each site and species, with mean Height etc.

TRACES_plant_info <- TRACES_plant_info %>%
  mutate(Species_name = ifelse(Species_name == "Rhamnus alaternus", "Rhamnus spp", rep(Species_name, each = 1)))

TRACES_plant_info <- TRACES_plant_info |>
  rename(Name = Species_name)

### Adjust the SpParams for our species: P12,P50,P80 ##############################################

SpParamsMED <- SpParamsMED %>%
  mutate(Name = ifelse(Name == "Rhamnus spp.", "Rhamnus spp", rep(Name, each = 1)))

SpParamsMED_f <- SpParamsMED %>%
  filter(Name %in% c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", 
                     "Pistacia lentiscus", "Rhamnus spp", "Salvia rosmarinus"))

Final_data3 <- Final_data

names(Final_data3)[names(Final_data3) == "sp_name"] <- "Name"
names(Final_data3)[names(Final_data3) == "Site_code"] <- "Code_site"

Final_data3 <- Final_data3 %>%
  mutate(Name = case_when(Name == "Amelanchier_ovalis" ~ "Amelanchier ovalis", Name == "Arbutus_unedo" ~ "Arbutus unedo",
                          Name == "Buxus_sempervirens" ~ "Buxus sempervirens", Name == "Pistacia_lentiscus" ~ "Pistacia lentiscus",
                          Name == "Rhamnus_alaternus" ~ "Rhamnus spp",  Name == "Salvia_rosmarinus" ~ "Salvia rosmarinus",
                          TRUE ~ NA_character_))

safety <- Final_data3

Final_data3 <- Final_data3 %>%
  group_by(Code_site, Name) %>%
  summarise(#Gswmin = mean(gres, na.rm = TRUE) / 1000,
    VCstem_P12_c = mean(VCs_P12, na.rm = TRUE),
    VCstem_P50_c = mean(VCs_P50, na.rm = TRUE),
    VCstem_P88_c = mean(VCs_P88, na.rm = TRUE),
    #VCstem_slope = mean(VCs_slope, na.rm = TRUE),
    #Kmax_stemxylem = mean(VCs_Ks, na.rm = TRUE),
    #VCstem_c = mean(C, na.rm = TRUE),
    .groups = "drop")

Final_data3 <- Final_data3 %>%
  mutate(VCstem_P12_c = ifelse(VCstem_P12_c == "NaN", NA, rep(VCstem_P12_c)),
         VCstem_P50_c = ifelse(VCstem_P50_c == "NaN", NA, rep(VCstem_P50_c)),
         VCstem_P88_c = ifelse(VCstem_P88_c == "NaN", NA, rep(VCstem_P88_c)))

str(SpParamsMED_f)

SpParamsMED_f <- merge(SpParamsMED_f, Final_data3, by=c("Name"), all= TRUE, na="NA")

SpParamsMED_f <- SpParamsMED_f %>%
  mutate(VCstem_P12_c = ifelse(is.na(VCstem_P12_c), VCstem_P12, VCstem_P12_c),
         VCstem_P50_c = ifelse(is.na(VCstem_P50_c), VCstem_P50, VCstem_P50_c),
         VCstem_P88_c = ifelse(is.na(VCstem_P88_c), VCstem_P88, VCstem_P88_c)) %>%
  dplyr::select(-c(VCstem_P12, VCstem_P50, VCstem_P88))

names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P12_c'] <- 'VCstem_P12'
names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P50_c'] <- 'VCstem_P50'
names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P88_c'] <- 'VCstem_P88'

### Adjust the SpParams for our species: Gswmin ###################################################

#Final_data3 <- safety

#Final_data3 <- Final_data3 %>%
#  group_by(Code_site, Name) %>%
# summarise(Gswmin = mean(gres, na.rm = TRUE) / 1000,
#     .groups = "drop")

#SpParamsMED_f <- SpParamsMED_f %>%
# full_join(Final_data3, by = c("Name", "Code_site"), suffix = c(".x", ".y")) %>%
# mutate(Gswmin = coalesce(Gswmin.y, Gswmin.x)) %>%
# dplyr::select(-ends_with(".x"), -ends_with(".y"))

#### Subsection: recalculate Gswmin using Abiel new code ###########################################

gmin_PS <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Gmin curves/gmin_PS.xlsx", 
                      sheet = 4)
#View(gmin_PS) 

#gmin_PS <- gmin_PS %>%
 # filter(Step %in% c("A", "B"))

# Get Step B values
stepB <- gmin_PS %>% filter(Step == "B") %>% 
  dplyr::select(plant_code, Temperature, RH) %>%
  rename(Temp_B = Temperature, RH_B = RH)

# Join and fill
gmin_PS <- gmin_PS %>%
  left_join(stepB, by="plant_code") %>%
  mutate(Temperature = ifelse(is.na(Temperature) & Step=="A", Temp_B, Temperature),
         RH = ifelse(is.na(RH) & Step=="A", RH_B, RH)) %>%
  dplyr::select(-Temp_B, -RH_B)

#calculate the gswmax using the gravimetric transpiration method

gmin_PS <- gmin_PS %>%
  group_by(plant_code) %>%
  mutate(time_min = cumsum(Dessication_time),
         time_s = time_min * 60)

gmin_PS <- gmin_PS %>%
  group_by(plant_code) %>%
  mutate(dW = FW - lag(FW),
         dt = time_s - lag(time_s), 
         slope = dW / dt)   # g/s

gmin_leafarea <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/1. Data gathering/Gmin curves/gmin_leafarea.xlsx")
#View(gmin_leafarea) 

str(gmin_PS)
str(gmin_leafarea)

gmin_PS <- merge(gmin_PS, gmin_leafarea, by=c("Site_code", "Site_name", "sp_name", "plant_code"), all= TRUE, na="NA")

gmin_PS <- gmin_PS %>%
  group_by(plant_code) %>%
  mutate(FW_sat = FW[which.min(time_s)],
    RWC = (FW - DW) / (FW_sat - DW) * 100) %>%
  ungroup()

gmin_PS <- gmin_PS %>%
  mutate(gleaf = (1000 * abs(slope.x) * 101.35) / (18.01528 * Leaf_area * VPDmean))

gmin_PS <- gmin_PS %>%
  mutate(date_time = as.POSIXct(time_s, origin = "2024-04-23"))

#### Subsection: Create a function for compute_gmin_sliding _ follow Abiel function ###############

compute_gmin_sliding <- function(df,
                                 n_points = 5,
                                 ratio = 1.5,
                                 consecutive = 2) {
  
  df <- df %>%
    filter(!is.na(gleaf), !is.na(RWC)) %>%
    arrange(date_time)
  
  n <- nrow(df)
  
  if (n < n_points) {
    return(tibble(gmin = NA, RWC_gmin = NA))
  }
  
  win_var <- rep(NA, n)
  
  for (i in 1:(n - n_points + 1)) {
    
    sel <- df[i:(i + n_points - 1), ]
    
    win_var[i] <- var(sel$gleaf, na.rm = TRUE)
  }
  
  win_var <- win_var[1:(n - n_points + 1)]
  
  if (length(win_var) < 3) {
    return(tibble(gmin = NA, RWC_gmin = NA))
  }
  
  base_var <- mean(tail(win_var, 3), na.rm = TRUE)
  
  stable <- win_var <= ratio * base_var
  
  run_start <- NA
  
  for (i in 1:(length(stable) - consecutive + 1)) {
    
    if (all(stable[i:(i + consecutive - 1)])) {
      run_start <- i
      break
    }
  }
  
  if (is.na(run_start)) {
    return(tibble(gmin = NA, RWC_gmin = NA))
  }
  
  start_row <- run_start
  end_row   <- run_start + n_points + consecutive - 2
  
  stable_window <- df[start_row:end_row, ]
  
  gmin_val <- median(stable_window$gleaf, na.rm = TRUE)
  
  idx <- which.min(abs(stable_window$gleaf - gmin_val))
  
  tibble(
    gmin = gmin_val,
    RWC_gmin = stable_window$RWC[idx]
  )
}

#### Subsection: use the function for compute_gmin_sliding #########################################

result <- gmin_PS %>%
  group_by(plant_code) %>%
  group_modify(~ compute_gmin_sliding(.x, n_points = 5, ratio = 1.5,  consecutive = 2)) %>%
  ungroup()

gmin_PS <- gmin_PS %>%
  left_join(result, by = "plant_code")

#### Subsection: plot the gmin curves created from the function ####################################

plot_one_id <- function(gmin_PS, id) {
  
  gmin_val <- unique(gmin_PS$gmin)
  rwc_val  <- unique(gmin_PS$RWC_gmin)
  
  ggplot(gmin_PS %>% arrange(desc(RWC)),
         aes(x = RWC, y = gleaf)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = gmin_val,
               linetype = "dashed",
               color = "red") +
    labs(
      title = paste("Plant:", id),
      subtitle = paste(
        "gmin =", round(gmin_val,4),
        "| RWC_gmin =", round(rwc_val,2)
      ),
      x = "RWC (%)",
      y = "gleaf"
    ) +
    theme_minimal()
}


plots <- gmin_PS %>%
  group_by(plant_code) %>%
  group_map(~ plot_one_id(.x, .y$plant_code[1]))


#### Subsection: average the gmin curves created from the function ###############################

Gswmin_final <- gmin_PS %>%
  group_by(plant_code) %>%
  summarise(gres = mean(gres, na.rm = TRUE),
            gmin = mean(gmin, na.rm = TRUE))

### Adjust the SpParams for our species: Gswmax ###################################################

Gswmin_final <- gmin_PS %>%
  group_by(Site_code, sp_name, plant_code) %>%
  summarise(gmin = mean(gmin, na.rm = TRUE), 
            gmax = max(gleaf, na.rm = TRUE))

#Gswmin_final <- Gswmin_final %>%
 # mutate(test = gmax - gmin)

Gswmin_final <- Gswmin_final %>%
  group_by(Site_code, sp_name) %>%
  summarise(Gswmin = mean(gmin, na.rm = TRUE) / 1000, 
            Gswmax = mean(gmax, na.rm = TRUE) / 1000)

names(Gswmin_final)[names(Gswmin_final) == 'Site_code'] <- 'Code_site'
names(Gswmin_final)[names(Gswmin_final) == 'sp_name'] <- 'Name'

Gswmin_final <- Gswmin_final %>%
  mutate(Name = case_when(Name == "Amelanchier_ovalis" ~ "Amelanchier ovalis", Name == "Arbutus_unedo" ~ "Arbutus unedo",
                          Name == "Buxus_sempervirens" ~ "Buxus sempervirens", Name == "Pistacia_lentiscus" ~ "Pistacia lentiscus",
                          Name == "Rhamnus_alaternus" ~ "Rhamnus spp",  Name == "Salvia_rosmarinus" ~ "Salvia rosmarinus",
                          TRUE ~ NA_character_))

SpParamsMED_f <- SpParamsMED_f %>%
  full_join(Gswmin_final, by = c("Name", "Code_site"), suffix = c(".x", ".y")) %>%
  mutate(Gswmin = coalesce(Gswmin.y, Gswmin.x), 
         Gswmax = coalesce(Gswmax.y, Gswmax.x)) %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"))

### Adjust the SpParams for our species: Gsw_AC_slope - BOF #######################################

#TRACES_Trc <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/1. TRACE field work/2. Measurements/2. Trc measurement/TRACES_Trc_final.xlsx")
#View(TRACES_Trc)

#Leafscans_results_MD <- read_excel("C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/1. TRACE field work/2. Measurements/2. Trc measurement/Leaf scans/Leafscans_results_MD.xlsx")
#View(Leafscans_results_MD)

#TRACES_Trc <- merge(TRACES_Trc, Leafscans_results_MD, by=c("Code_site", "Ind_code"), all= TRUE, na="NA")

#TRACES_Trc <- TRACES_Trc %>%
# mutate(LS = ifelse(is.na(LS), "2", rep(LS, each =1)))

#TRACES_Trc <- TRACES_Trc[, -c(27:30)]

#str(TRACES_Trc)

#TRACES_Trc$LS <- as.numeric(TRACES_Trc$LS)

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Acorr = (LS * Photo) / 2) %>%
# mutate(Cond = Cond * 1000) %>%
# mutate(Gscorr = (LS * Cond) / 2)

#TRACES_Trc <- TRACES_Trc[, -c(3:11,13:27)]

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Ind = substr(Ind_code, nchar(Ind_code) - 3, nchar(Ind_code)))

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Repetition = substr(Ind_code, 4, 4)) %>%
# mutate(Repetition = ifelse(Repetition == "_" & Code_site == 1, NA, 1))

#TRACES_Trc <- TRACES_Trc[!is.na(TRACES_Trc$Repetition),]

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Ind_code = gsub("1PA2", "1PA", Ind_code))

#TRACES_Trc <- merge(TRACES_Trc, TRACES_plant_info, by=c("Code_site", "Ind_code"), all= TRUE, na="NA")

#TRACES_Trc <- TRACES_Trc[, -c(12:35)]

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Gscorr = ifelse(Gscorr > 75, NA, rep(Gscorr, each = 1))) %>%
# mutate(Gscorr = ifelse(Gscorr < -15, NA, rep(Gscorr, each = 1))) %>%
# mutate(Gscorr = ifelse(Gscorr > 50 & Code_site == 6, NA, rep(Gscorr, each = 1)))

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Gscorr = ifelse(Gscorr < -5, 0, rep(Gscorr, each = 1)))

#TRACES_Trc <- TRACES_Trc[!is.na(TRACES_Trc$Acorr),]
#TRACES_Trc <- TRACES_Trc[!is.na(TRACES_Trc$Gscorr),]
#TRACES_Trc <- TRACES_Trc[!is.na(TRACES_Trc$Name),]

#species_colors <- c("Arbutus unedo" = "red", "Amelanchier ovalis" = "#61D04F", "Buxus sempervirens" = "purple", 
#                    "Pistacia lentiscus" = "black", "Rhamnus spp" = "grey", "Salvia rosmarinus" = "blue")

#OutlineGraph <- ggplot(TRACES_Trc, aes(x = Gscorr, y = Acorr)) +
#  geom_point(aes(fill = Name, color = Name, shape = Site_name), size = 2) +
# stat_smooth(aes(color = Name, fill = Name, group = interaction(Name, Code_site)),
#             method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.7, alpha = 0.2) +
# scale_color_manual(values = species_colors, drop = FALSE) +
# scale_fill_manual(values = species_colors, drop = FALSE) +
# scale_shape_manual(values = c(0, 1, 2, 5, 6, 8)) +
# facet_wrap(~Site_name, ncol = 2) +
# ylab(expression("A")) +
# xlab(expression("g"["s"])) +
# theme_bw()  +
#  guides(shape = "none")  # This line ensures only the color legend is shown

#OutlineGraph

#ggsave("Gsw_AC_slope.pdf", width = 30, height = 20, units = "cm")

#Gsw_AC_slope <- TRACES_Trc %>%
# group_by(Name, Code_site) %>%
# do(tidy(lm(Acorr ~ Gscorr, data = .))) %>%
# filter(term == "Gscorr") %>%
# select(Name, Code_site, Gsw_AC_slope = estimate)
#Gsw_AC_slope

#SpParamsMED_f <- SpParamsMED_f %>%
# full_join(Gsw_AC_slope, by = c("Name", "Code_site"), suffix = c(".x", ".y")) %>%
# mutate(Gsw_AC_slope = coalesce(Gsw_AC_slope.y, Gsw_AC_slope.x), 
#        Gsw_AC_slope = coalesce(Gsw_AC_slope.y, Gsw_AC_slope.x)) %>%
# dplyr::select(-ends_with(".x"), -ends_with(".y"))

#TRACES_Trc <- TRACES_Trc %>%
# mutate(Tleaf = ifelse(Tleaf <= 15, NA, rep(Tleaf, each = 1)))

#AvsT <- ggplot(TRACES_Trc, aes(x = Tleaf, y = Acorr)) +
# geom_point(aes(fill = Name, color = Name, shape = Site_name), size = 2) +
# geom_smooth(aes(color = Name, group = Name, fill = Name), 
#             method = "loess", se = TRUE, linewidth = 0.8, alpha = 0.2) +
# scale_color_manual(values = species_colors, drop = FALSE) +
# scale_fill_manual(values = species_colors, drop = FALSE) +
# scale_shape_manual(values = c(0, 1, 2, 5, 6, 8)) +
# facet_wrap(~Site_name, ncol = 2) +
# ylab(expression("A")) +
# xlab(expression("T"["leaf"])) +
# theme_bw()  +
# guides(shape = "none")  # This line ensures only the color legend is shown
#AvsT

#ggsave("AvsT.pdf", width = 30, height = 20, units = "cm")


### Adjust the SpParams for our species: VCstem_Kmax - CHECK AGAIN #################################

Final_data3 <- safety

Final_data3 <- Final_data3 %>%
  group_by(Code_site, Name) %>%
  summarise(Vcstem_kmax = mean(VCs_Ks, na.rm = TRUE), .groups = "drop")

Final_data3 <- Final_data3 %>%
  mutate(Vcstem_kmax = ifelse(Vcstem_kmax == "NaN", NA, rep(Vcstem_kmax)))

str(SpParamsMED_f)

Final_data3 <- Final_data3 %>%
  mutate(Vcstem_kmax = (Vcstem_kmax * 5.55e7)/1000)

str(SpParamsMED_f)

SpParamsMED$Vcstem_kmax

SpParamsMED_f <- SpParamsMED_f %>%
  full_join(Final_data3, by = c("Name", "Code_site"))

### Merge the SpParams with the plants info #######################################################

TRACES_plant_info <- merge(TRACES_plant_info, SpParamsMED_f, by=c("Code_site", "Name"), all= TRUE, na="NA")

TRACES_plant_info <- TRACES_plant_info[!is.na(TRACES_plant_info$Height),]

TRACES_plant_info2 <- TRACES_plant_info |>
  group_by(Code_site, Name) |>
  summarise(Height = mean(Height),
            LAI = mean(LAI_medfate),
            Z50 = mean(Z50),
            Z95 = mean(Z95)) |>
  #VCstem_P12 = mean(VCstem_P12),
  #VCstem_P50 = mean(VCstem_P50),
  #VCstem_P88 = mean(VCstem_P88)) 
  mutate(Cover = 100) |>
  #Z50 = NA,
  #Z95 = NA) |>
  #dplyr::select(-c(LeafArea, ProjectedArea)) |>
  rename(Species = Name)

str(TRACES_plant_info2)

### Create the forest object using the selected data ##############################################

#f <- emptyforest()
#f$shrubData <- as.data.frame(TRACES_plant_info2[1,])
#class(f)

#x <- spwbInput(f, examplesoil, SpParamsMED, control)

# Filter for each Code_site and species and remove the first column
# Now forest_list$PAS, forest_list$ALO, etc. contain your forest objects

forestPAS_AU <- emptyforest()

forestPAS_AU$shrubData <- TRACES_plant_info2[TRACES_plant_info2$Code_site == 1 & TRACES_plant_info2$Species == "Arbutus unedo" , -1]
class(forestPAS_AU)

# Split data by site and species
plant_split <- split(TRACES_plant_info2, list(TRACES_plant_info2$Code_site, TRACES_plant_info2$Species), drop = TRUE)

# Loop over each site-species combination
for(name in names(plant_split)) {
  
  # Create an empty forest
  f <- emptyforest()
  
  # Assign the shrubData, removing the first column (Code_site)
  f$shrubData <- as.data.frame(plant_split[[name]][, -1])
  
  # Make a clean variable name: replace '.' with '_' for site/species
  var_name <- paste0("forest_", gsub("\\.", "_", name))
  
  # Assign to global environment
  assign(var_name, f)
}

# Now I have separate objects like forest_1_Arbutus_unedo, forest_1_Pistacia lenticus, etc.

names(`forest_1_Arbutus unedo`)

#forest_list <- list()  # to store all forest objects together

#for(name in names(plant_split)) {

# Create an empty forest
#f <- emptyforest()

# Assign the shrubData, removing the first column (Code_site)
#f$shrubData <- as.data.frame(plant_split[[name]][, -1])

# Save to list
#forest_list[[name]] <- f
#}

# Now forest_list contains all forest objects
# Example access:
#names(forest_list)
#forest_list[["1.Arbutus unedo"]]  # forest for site 1, species "Arbutus unedo"


### Save the forest object using the selected data ##############################################

# Define your working directory
dir_out <- "C:/Users/Gency.WSL/Desktop/Margaux _ Post-Doc CREAF/22. MEDFATE model/2. R Scripts and data outputs/1. Model inputs"
setwd(dir_out)

# Save everything in the current environment
save.image(file = "01. Model inputs_save forest object.RData")
