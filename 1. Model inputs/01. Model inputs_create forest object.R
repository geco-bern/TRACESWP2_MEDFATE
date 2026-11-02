###################################################################################################
######## Living on the Edge: Modeling daily leaf energy and cabon balance #########################
############################ of Mediterranean Shrublands ##########################################
###################################################################################################

#R.version.string

rm(list=ls())#remove all global environment

library(readxl)

library(rsconnect, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")
#library(lifecycle, lib.loc = "/storage/software/epyc2.9/software/R/4.5.1-gfbf-2025a/lib64/R/library")
library(rlang)
library(dplyr, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")

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
data("SpParamsDefinition", package = "medfate")
#View(SpParamsDefinition)

# Set working directory to project root
getwd()
setwd("/storage/homefs/md26i579/GitHub/geco-bern/TRACESWP2_MEDFATE")

###################################################################################################
### I. MODEL INPUTS & Test on one species and one site ############################################
###################################################################################################

### 1. Create a forest object with the individuals data ###########################################
### Import the shrubs details #####################################################################

list.files("..")
list.files("0. Data gathering")
list.files("0. Data gathering/1. Forest object")

TRACES_plantselection <- readxl::read_excel("0. Data gathering/1. Forest object/TRACESWP2_Finalplantselectionxlsx")
#View(TRACES_plantselection)

TRACES_plantselection <- TRACES_plantselection[, -c(7)]

### Import plant cover, height and interaction data ###############################################

TRACES_plantinfo <- readxl::read_excel("0. Data gathering/1. Forest object/TRACESWP2_Plantinfo.xlsx")
#View(TRACES_plantinfo)

TRACES_plantinfo <- merge(TRACES_plantselection, TRACES_plantinfo, by=c("Code_site", "Site_name", "Species_name", "Ind_code"), all= TRUE, na="NA")

TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$Replicate),]

### Calculate mean plot individual shrub cover ####################################################
#### Subsection: calculation using the approximately elliptical in shape shrub equation ###########

TRACES_plantinfo <- TRACES_plantinfo %>% 
  mutate(Pl_cover = (pi * (PD1/100) * (PD2/100)) / 4) #IN m²

TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$Pl_cover),]

#### Subsection: Incorporate the data of Pol and keep only the necessary column ###################

TRACES_finaldata_Pol <- readRDS("0. Data gathering/1. Forest object/TRACESWP2_finaldata_Pol.rds")
#View(TRACESWP2_finaldata_Pol)

summary(TRACES_finaldata_Pol)
str(TRACES_finaldata_Pol)

#Canopy cover, total leaf area (Total LA), and height are the simplest direct inputs for LAI.

TRACES_finaldata_Pol <- TRACES_finaldata_Pol %>%
  mutate(LeafArea_m2 = Total_LA / 10000)  # cm² → m²

TRACES_finaldata_subset <- TRACES_finaldata_Pol[, -c(1,3,4,6:21,23:49)]

names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == "Site_code"] <- "Code_site"
names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == "plant_code"] <- "Ind_code"

TRACES_plantinfo <- merge(TRACES_plantinfo, TRACES_finaldata_subset, by=c("Code_site", "Ind_code"), all= TRUE, na="NA")

TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$Replicate),]
TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$ID),]

TRACES_plantinfo <- TRACES_plantinfo %>%
  mutate(Height_m = Height / 100) 

#### Subsection: calculation LAI using Medfate webpage, SpParams + De Caceres 2019 paper ##########

library(dplyr, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")

SpParamsMED2 <- SpParamsMED %>%
  dplyr::filter(Name %in% c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens",
                     "Pistacia lentiscus", "Rhamnus spp.", "Salvia rosmarinus"))

# Create the data frame
shrubs_param <- data.frame(
  Species_name = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"),
  aarea = c(0.3508, 0.6284, 0.9734499, 0.0172867, 0.3508, 0.1544),
  barea = c(1.9641, 1.8804, 1.807175, 2.965406, 1.9641, 2.2532),
  afine = c(0.2068, 0.2375, 0.3032149, 1.1897118, 0.2068, 1.3196),
  bfine = c(0.8514, 0.8631, 0.7163349, 0.7337279, 0.8514, 0.8929))

TRACES_plantinfo <- merge(TRACES_plantinfo, shrubs_param, by=c("Species_name"), all= TRUE, na="NA")

#### Subsection: calculation dry weight using Medfate webpage, SpParams + De Caceres 2019 paper ####

# Calculate volume using ellipsoid formula
TRACES_plantinfo <- TRACES_plantinfo %>%
  mutate(Dryweight = afine * (Height_m *(Pl_cover))^bfine) #IN m² for plant cover

#### Subsection: calculation foliar biomass using Medfate, SpParams + De Caceres 2019 paper #######

shrubs_param2 <- data.frame(
  Species_name = c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", "Pistacia lentiscus", "Rhamnus alaternus", "Salvia rosmarinus"),
  r635 = c(3.709808, 1.669141, 1.667465, 2, 2.475149, 2)) #2 is added for Pistacia lentiscus and Salvia because no values are available

TRACES_plantinfo <- merge(TRACES_plantinfo, shrubs_param2, by=c("Species_name"), all= TRUE, na="NA")

TRACES_plantinfo <- TRACES_plantinfo %>% 
  mutate(Foliarbiomass = (Dryweight/r635)/Pl_cover)

#### Subsection: final calculation LAI using Medfate webpage, SpParams + De Caceres 2019 paper ####

TRACES_plantinfo <- TRACES_plantinfo %>% 
  mutate(SLA_m_kg = SLA / 10) #change from cm²/g to m²/kg

OutlineGraph <- ggplot(TRACES_plantinfo, aes(x=Code_site, y=SLA_m_kg)) +
  geom_point(aes(color=Species_name), size = 2)
OutlineGraph

TRACES_plantinfo <- TRACES_plantinfo %>% 
  mutate(SLA_m_kg = ifelse(SLA_m_kg <= 1, NA, rep(SLA_m_kg, each = 1))) #change from cm²/g to m²/kg

TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$SLA_m_kg),]

TRACES_plantinfo <- TRACES_plantinfo %>% 
  mutate(LAI = Foliarbiomass * SLA_m_kg)


order <- c("PAS_ASE", "ALOS", "GARRAF", "CINGLES", "MONTSEC", "PENTINA")  # Reverse order for plotting
TRACES_plantinfo$Site_name <- factor(TRACES_plantinfo$Site_name, levels = order)

site_labels <- c(PAS_ASE = "PAS", ALOS = "ALO", GARRAF = "GAR", CINGLES = "CIN", MONTSEC = "MON", PENTINA = "PEN")


LAI <- ggplot(TRACES_plantinfo, aes(x = Site_name, y = LAI)) +
  geom_boxplot(aes(color=Species_name), fill = "grey85", alpha = 0.6) +
  scale_color_manual(values = c("#e57e89ff", "#cc5285ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff")) +
  labs(x = "Site", y = "LAI (m²/m²)", color = "Species") +
  scale_x_discrete(labels = site_labels) +
  theme_bw() +
  theme(text = element_text(size = 18), 
        panel.grid.minor = element_blank())
LAI

#ggsave("LAI.pdf", width = 25, height = 15, units = "cm")


### Rename column to fit the SpParams file ########################################################

#Create a summary of the data for each site and species, with mean Height etc.

TRACES_plantinfo <- TRACES_plantinfo %>%
  mutate(Species_name = ifelse(Species_name == "Rhamnus alaternus", "Rhamnus spp", rep(Species_name, each = 1)))

TRACES_plantinfo <- TRACES_plantinfo |>
  rename(Name = Species_name)

### Adjust the SpParams for our species: P12,P50,P80 ##############################################

SpParamsMED <- SpParamsMED %>%
  mutate(Name = ifelse(Name == "Rhamnus spp.", "Rhamnus spp", rep(Name, each = 1)))

SpParamsMED_f <- SpParamsMED %>%
  dplyr::filter(Name %in% c("Amelanchier ovalis", "Arbutus unedo", "Buxus sempervirens", 
                     "Pistacia lentiscus", "Rhamnus spp", "Salvia rosmarinus"))

TRACES_finaldata_subset <- TRACES_finaldata_Pol

names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == "sp_name"] <- "Name"
names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == "Site_code"] <- "Code_site"

TRACES_finaldata_subset <- TRACES_finaldata_subset %>%
  mutate(Name = case_when(Name == "Amelanchier_ovalis" ~ "Amelanchier ovalis", Name == "Arbutus_unedo" ~ "Arbutus unedo",
                          Name == "Buxus_sempervirens" ~ "Buxus sempervirens", Name == "Pistacia_lentiscus" ~ "Pistacia lentiscus",
                          Name == "Rhamnus_alaternus" ~ "Rhamnus spp",  Name == "Salvia_rosmarinus" ~ "Salvia rosmarinus",
                          TRUE ~ NA_character_))

safety <- TRACES_finaldata_subset

TRACES_finaldata_subset <- TRACES_finaldata_subset %>%
  group_by(Code_site, Name) %>%
  summarise(VCstem_P12_c = mean(VCs_P12, na.rm = TRUE),
            VCstem_P50_c = mean(VCs_P50, na.rm = TRUE),
            VCstem_P88_c = mean(VCs_P88, na.rm = TRUE),
    #VCstem_slope = mean(VCs_slope, na.rm = TRUE),
    #Kmax_stemxylem = mean(VCs_Ks, na.rm = TRUE),
    #VCstem_c = mean(C, na.rm = TRUE),
    .groups = "drop")

TRACES_finaldata_subset <- TRACES_finaldata_subset %>%
  mutate(VCstem_P12_c = ifelse(VCstem_P12_c == "NaN", NA, rep(VCstem_P12_c)),
         VCstem_P50_c = ifelse(VCstem_P50_c == "NaN", NA, rep(VCstem_P50_c)),
         VCstem_P88_c = ifelse(VCstem_P88_c == "NaN", NA, rep(VCstem_P88_c)))


str(SpParamsMED_f)

SpParamsMED_f <- merge(SpParamsMED_f, TRACES_finaldata_subset, by=c("Name"), all= TRUE, na="NA")

SpParamsMED_f <- SpParamsMED_f %>%
  mutate(VCstem_P12_c = ifelse(is.na(VCstem_P12_c), VCstem_P12, VCstem_P12_c),
         VCstem_P50_c = ifelse(is.na(VCstem_P50_c), VCstem_P50, VCstem_P50_c),
         VCstem_P88_c = ifelse(is.na(VCstem_P88_c), VCstem_P88, VCstem_P88_c)) %>%
  dplyr::select(-c(VCstem_P12, VCstem_P50, VCstem_P88))

names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P12_c'] <- 'VCstem_P12'
names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P50_c'] <- 'VCstem_P50'
names(SpParamsMED_f)[names(SpParamsMED_f) == 'VCstem_P88_c'] <- 'VCstem_P88'

### Adjust the SpParams for our species: Gswmin ###################################################
#### Subsection: recalculate Gswmin using Abiel new code ##########################################

TRACES_gmin <- readxl::read_excel("0. Data gathering/1. Forest object/TRACESWP2_gmin.xlsx", sheet = 4)
#View(TRACES_gmin) 

TRACES_gmin <- as.data.frame(TRACES_gmin)

TRACES_gmin <- TRACES_gmin %>%
  mutate(Dessication_time = if_else(Step == "A", 0, Dessication_time))

# Get Step B values
stepB <- TRACES_gmin %>% dplyr::filter(Step == "B") %>% 
  dplyr::select(plant_code, Temperature, RH) %>%
  rename(Temp_B = Temperature, RH_B = RH)

# Join and fill
TRACES_gmin <- TRACES_gmin %>%
  left_join(stepB, by="plant_code") %>%
  mutate(Temperature = ifelse(is.na(Temperature) & Step=="A", Temp_B, Temperature),
         RH = ifelse(is.na(RH) & Step=="A", RH_B, RH)) %>%
  dplyr::select(-Temp_B, -RH_B)

#TRACES_gmin <- TRACES_gmin %>%
 # group_by(plant_code) %>%
  #mutate(dW = FW - lag(FW),
   #      dt = time_s - lag(time_s), 
    #     slope = dW / dt)   # g/s

TRACES_gmin <- TRACES_gmin %>%
  group_by(plant_code) %>%
  mutate(time_min = cumsum(Dessication_time),
         time_s = time_min * 60)

library(hms, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")

TRACES_gmin <- TRACES_gmin %>%
  group_by(plant_code) %>%
  mutate(time_start = as_hms((time_min - first(time_min)) * 60)) %>%
  ungroup()

TRACES_gmin <- TRACES_gmin %>%
  mutate(time = time_start)

TRACES_gmin <- TRACES_gmin %>%
  mutate(time = format(as.POSIXct(time), "%H:%M:%S"))


ggplot(TRACES_gmin) +
  geom_point(aes(x = time, y = FW, color = sp_name), size = 2, alpha = 0.9) +
  facet_wrap(~ Site_name, ncol = 3) +
  geom_smooth(aes(x = time, y = FW, color = sp_name), method = "loess", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = c("#e57e89ff", "#cc5285ff", "#b752ccff", "#5c52cbff", "#51a3cbff", "#2db3b4ff")) +
  xlab(expression("Dessication time (min)", sep="")) +
  theme_bw() +
  theme(text = element_text(size = 16), 
        panel.grid.minor = element_blank())


TRACES_gmin <- TRACES_gmin %>%
  mutate(VPD = (613.75 * exp((17.502 * Temperature) / (240.9 + Temperature)) * 1e-3) * (100 - RH) / 100)


#### Subsection: create a function to interpolate temperature or humidity #########################

weighted_interpolate <- function(x, time) {
  na_idx <- which(is.na(x))
  
  for(i in na_idx) {
    t0 <- time[i]
    
    # previous non-NA
    prev_candidates <- which(time < t0 & !is.na(x))
    prev_idx <- if(length(prev_candidates) > 0) max(prev_candidates) else NA
    
    # next non-NA
    next_candidates <- which(time > t0 & !is.na(x))
    next_idx <- if(length(next_candidates) > 0) min(next_candidates) else NA
    
    # skip if no previous or next (cannot interpolate)
    if(is.na(prev_idx) | is.na(next_idx)) next
    
    # compute time differences in seconds
    t_prev <- as.numeric(difftime(t0, time[prev_idx], units = "secs"))
    t_next <- as.numeric(difftime(time[next_idx], t0, units = "secs"))
    
    # weighted mean
    x[i] <- (x[prev_idx] * t_next + x[next_idx] * t_prev) / (t_prev + t_next)
  }
  
  return(x)
}

#### Subsection: calculation of RWC ###############################################################

TRACES_gmin <- TRACES_gmin %>%
  group_by(plant_code) %>%
  mutate(TW = FW[which.min(time_s)],
         RWC = (FW - DW) / (TW - DW) * 100) %>%
  ungroup()

summary(TRACES_gmin)

#### Subsection: import leaf area #################################################################

TRACES_gminleafarea <- readxl::read_excel("0. Data gathering/1. Forest object/TRACESWP2_gminleafarea.xlsx")
#View(TRACES_gminleafarea) 

TRACES_gminleafarea <- TRACES_gminleafarea[, -c(5:8,10)]

str(TRACES_gmin)
str(TRACES_gminleafarea)

TRACES_gmin <- as.data.frame(TRACES_gmin)
TRACES_gminleafarea <- as.data.frame(TRACES_gminleafarea)

TRACES_gmin <- merge(TRACES_gmin, TRACES_gminleafarea, by=c("Site_code", "Site_name", "sp_name", "plant_code"), all= TRUE, na="NA")

#### Subsection: create a function to calculate gleaf per RWC: gres_compute_one ###################

gres_compute_one <- function(df, df_meta,
                             windows = 8, deg_pol = 3,
                             time_step = "10 mins",
                             pv_curve = TRUE) {
  
  # aggregate within sample
  df_mean <- df %>%
    group_by(date_time = floor_date(DateTimetrue, time_step)) %>%
    summarise(Raw_mass = mean(Leafweight, na.rm = TRUE), 
              Tamb = mean(Temperature, na.rm = TRUE),
              RH = mean(RH, na.rm = TRUE),
              RWC=mean(RWC, na.rm=TRUE),
              .groups = "drop")
  
  # regular time grid
  date_time <- seq(min(df_mean$date_time),
                   max(df_mean$date_time),
                   by = time_step)
  
  # interpolate
  Mass <- approx(df_mean$date_time, df_mean$Raw_mass, xout = date_time, rule = 2)$y
  Tamb <- approx(df_mean$date_time, df_mean$Tamb, xout = date_time, rule = 2)$y
  RH <- approx(df_mean$date_time, df_mean$RH, xout = date_time, rule = 2)$y
  RWC <- approx(df_mean$date_time, df_mean$RWC, xout = date_time, rule = 2)$y
  
  out <- tibble(date_time, Mass, Tamb, RH, RWC) %>%
    drop_na(Mass)
  
  # metadata
  meta <- df_meta %>% dplyr::filter(IDfinal == unique(df$IDfinal)) %>% dplyr::slice(1)
  
  # sgolay parameters
  n_sgolay <- windows + 1
  if (n_sgolay %% 2 == 0) n_sgolay <- n_sgolay + 1
  if (n_sgolay <= deg_pol) deg_pol <- n_sgolay - 1
  
  # check if enough points
  if (nrow(out) < n_sgolay) return(NULL)
  
  dt_seconds <- as.numeric(difftime(date_time[2], date_time[1], units = "secs"))
  
  out %>% mutate(slope = sgolayfilt(Mass, p = deg_pol, n = n_sgolay, m = 1) / dt_seconds,
                 VPD = (613.75 * exp((17.502 * Tamb) / (240.9 + Tamb)) * 1e-3) * (100 - RH) / 100,
                 gleaf = (1000 * abs(slope) * 101.35) / (18.01528 * meta$Leafarea * VPD),
                 sample_ID = meta$IDfinal)
}

#### Subsection: create a function to compute gmin: compute_gmin_sliding ##########################

compute_gmin_sliding <- function(df,
                                 n_points = 8,
                                 ratio = 1.5,
                                 consecutive = 2) {
  
  df <- df %>%
    dplyr::filter(!is.na(gleaf), !is.na(RWC)) %>%
    dplyr::arrange(date_time)
  
  n <- nrow(df)
  
  # Not enough data
  if (n < n_points) {
    return(tibble(gmin = NA, RWC_gmin = NA))
  }
  
  # --- Sliding variance computation ---
  win_var <- rep(NA, n)
  
  for (i in 1:(n - n_points + 1)) {
    
    sel <- df[i:(i + n_points - 1), ]
    
    win_var[i] <- var(sel$gleaf, na.rm = TRUE)
  }
  
  # Remove unused tail
  win_var <- win_var[1:(n - n_points + 1)]
  
  # Need baseline from last windows
  if (length(win_var) < 3) {
    return(tibble(gmin = NA, RWC_gmin = NA))
  }
  
  # Baseline variance = mean of last 3 sliding windows
  base_var <- mean(tail(win_var, 3), na.rm = TRUE)
  
  # Stable windows = variance close to baseline
  stable <- win_var <= ratio * base_var
  
  # --- Find first run of consecutive stable windows ---
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
  
  # --- Select data from the first stable run ---
  start_row <- run_start
  end_row   <- run_start + n_points + consecutive - 2
  
  stable_window <- df[start_row:end_row, ]
  
  gmin_val <- median(stable_window$gleaf, na.rm = TRUE)
  
  idx <- which.min(abs(stable_window$gleaf - gmin_val))
  
  tibble(
    gmin     = gmin_val,
    RWC_gmin = stable_window$RWC[idx]
  )
  
}

#### Subsection: create a function to plot gleaf and gmin by RWC ##################################

plot_one_id <- function(df, id) {
  
  gmin_val <- unique(df$gmin)
  rwc_val  <- unique(df$RWC_gmin)
  
  p <- ggplot(df %>% arrange(desc(RWC)), aes(x = RWC, y = gleaf)) +
    geom_point(size = 1.2) +
    #geom_line() +
    geom_smooth(method = "lm", formula = y ~ stats::poly(x, 3), se = FALSE, linewidth = 0.9, color = "grey") +
    labs(title = paste0("IDfinal = ", id),
         subtitle = paste0("gmin = ", round(gmin_val, 4)," | RWC_gmin = ", round(rwc_val, 2)),
         x = "RWC (%)",
         y = "gleaf") +
    theme_minimal()
  
  if (!is.na(gmin_val)) {
    p <- p +
      geom_hline(yintercept = gmin_val,
                 linetype = "dashed",
                 color = "red")
  }
  p
}

#### Subsection: assign new ID since we need integer IDs ##########################################

df_masse <- TRACES_gmin %>%
  mutate(IDfinal = match(plant_code, unique(plant_code)))

#### Subsection: rename leaf area #################################################################

names(df_masse)[names(df_masse) == 'Leaf_area'] <- 'Leafarea'

plot(df_masse$Leafarea)
hist(df_masse$Leafarea)

#### Subsection: calculate final time steps #######################################################

names(df_masse)[names(df_masse) == 'time'] <- 'RealTime'

df_masse$Hour <- hms::as_hms(df_masse$RealTime)

df_masse$Date <- as.Date("15.08.2024", format = "%d.%m.%Y") #Example of date, not real one here

# Combine into POSIXct
df_masse$DateTime <- as.POSIXct(paste(df_masse$Date, df_masse$Hour), format="%Y-%m-%d %H:%M:%S", tz="UTC")

summary(df_masse)
colSums(!is.na(df_masse))

names(df_masse)[names(df_masse) == 'FW'] <- 'Leafweight'
plot(df_masse$Leafweight)
hist(df_masse$Leafweight)

#### Subsection: interpolation of RH and temperature time steps ###################################

df_masse$DateTimetrue <- df_masse$DateTime

df_masse$temperature <- as.numeric(df_masse$Temperature)
df_masse$humidity <- as.numeric(df_masse$RH)

df_masse <- df_masse %>%
  arrange(DateTimetrue) %>%
  mutate(temperature = weighted_interpolate(temperature, DateTimetrue),
         humidity = weighted_interpolate(humidity, DateTimetrue))

#### Subsection: calculation of gleaf #############################################################

str(df_masse)

#df_masse$time_start <- as.POSIXct(df_masse$time_start)

#Group the data for each ID in individal tables 
df_nested <- df_masse %>%
  group_by(IDfinal) %>%
  nest() %>%
  mutate(data = purrr::map2(data, IDfinal, ~ mutate(.x, IDfinal = .y)))

library(lubridate, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")
library(purrr, lib.loc = "/storage/software/epyc2.9/software/R/4.5.1-gfbf-2025a/lib64/R/library")
library(signal, lib.loc = "/storage/software/epyc2.9/software/R-bundle-CRAN/2025.10-foss-2025a")

#compute function for one ID from the nested data : ID1 from the dataframe df_mass
gres_compute_one(df_nested$data[[1]], df_meta = df_masse)

gres_safe <- safely(gres_compute_one)

#Get the output for the calculation of gleaf 
OUTPUT <- df_nested %>%
  mutate(res = map(data, ~ gres_safe(.x, df_meta = df_masse))) %>%
  mutate(result = map(res, "result"), error  = map(res, "error"))

df_all <- OUTPUT %>%
  tidyr::unnest(result) %>%
  dplyr::select(IDfinal, gleaf, slope, RWC, date_time)

head(df_all)

df_all <- df_all %>%
  group_by(IDfinal) %>%  # replace with your curve identifier
  arrange(RWC, .by_group = TRUE) %>%
  dplyr::filter(row_number() <= which.max(gleaf)) %>%
  ungroup()

#### Subsection: use the function for compute_gmin_sliding #########################################

result_all <- df_all %>%
  group_by(IDfinal) %>%
  group_modify(~ compute_gmin_sliding(.x, n_points = 8, ratio = 1.2, consecutive = 2)) %>%
  ungroup()

df_all_gmin <- df_all %>%
  left_join(result_all, by = "IDfinal")

df_filtered <- df_all_gmin %>%
  dplyr::filter(!is.na(gmin))

df_filtered <- df_filtered %>%
  dplyr::filter(RWC_gmin <= 100)

#### Subsection: plot the gleaf vs. RWC and clean the data #########################################

#plots <- df_filtered %>%
#  dplyr::filter(IDfinal <= 45) %>%
#  group_by(IDfinal) %>%
#  group_map(~ plot_one_id(.x, id = .y$IDfinal[1]))
#plots

#plots <- df_filtered %>%
#  group_by(IDfinal) %>%
#  group_map(~ plot_one_id(.x, id = .y$IDfinal[1]))
#plots

df_filtered <- df_filtered %>% 
  dplyr::filter(!IDfinal %in% c(165, 84, 78, 75, 45, 13)) #Number 57 ID FINAL is beautiful

plot(df_filtered$gmin,df_filtered$RWC_gmin)
cor.test(df_filtered$gmin,df_filtered$RWC_gmin)

#### Subsection: create the final file with gmax and gmin values ##################################

df_filtered <- df_filtered %>%
  group_by(IDfinal) %>%
  slice_max(order_by = gleaf, n = 1, with_ties = FALSE) %>%
  ungroup()

df_filtered <- df_filtered[, -c(3:5)]

df_filtered <- merge(df_filtered, df_masse, by=c("IDfinal"), all= TRUE, na="NA")

df_filtered <- df_filtered[!is.na(df_filtered$gmin),]

str(df_filtered)

df_filtered <- df_filtered[, -c(1,4,9:29)]

df_filtered <- df_filtered %>%  distinct()

names(df_filtered)[names(df_filtered) == 'gmin'] <- 'Gswmin'
names(df_filtered)[names(df_filtered) == 'gleaf'] <- 'Gswmax'

df_filtered <- df_filtered %>%
  dplyr::filter(Gswmin <= 35)

#### Subsection: compare with the data of Pol #######################################################

TRACES_finaldata <- merge(df_filtered, TRACES_finaldata_Pol, by=c("Site_code", "Site_name", "sp_name", "plant_code"), all= TRUE, na="NA")

ggplot(TRACES_finaldata, aes(x = Gswmin, y = gres)) +
  geom_point(size = 2, alpha = 0.9) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "#666666ff", linewidth = 1) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed")                  # General 1:1 linetheme_bw() 

lm.model <- lm(Gswmin ~ gres, data=TRACES_finaldata)
summary(lm.model)

TRACES_finaldata <- TRACES_finaldata %>%
  mutate(Gswmin_predicted = predict(lm.model, newdata = TRACES_finaldata) )

TRACES_finaldata <- TRACES_finaldata %>%
  mutate(Gswmin = ifelse(is.na(Gswmin), predict(lm.model, newdata = TRACES_finaldata), Gswmin))

sum(is.na(TRACES_finaldata$Gswmin))
sum(is.na(TRACES_finaldata$gres))

TRACES_finaldata <- TRACES_finaldata[, -c(53)]

### Adjust the SpParams for our species: Gswmin and Gswmax #########################################

TRACES_gminfinal <- TRACES_finaldata %>%
  group_by(Site_code, sp_name, plant_code) %>%
  summarise(Gswmin = if (all(is.na(Gswmin))) NA_real_ else mean(Gswmin, na.rm = TRUE),
            Gswmax = if (all(is.na(Gswmax))) NA_real_ else max(Gswmax, na.rm = TRUE),
    .groups = "drop")

#test <- TRACES_gminfinal %>%
 # mutate(test = Gswmax - Gswmin)

TRACES_gminfinal <- TRACES_gminfinal %>%
  group_by(Site_code, sp_name) %>%
  summarise(Gswmin = mean(Gswmin, na.rm = TRUE) / 1000, 
            Gswmax = mean(Gswmax, na.rm = TRUE) / 1000)

names(TRACES_gminfinal)[names(TRACES_gminfinal) == 'Site_code'] <- 'Code_site'
names(TRACES_gminfinal)[names(TRACES_gminfinal) == 'sp_name'] <- 'Name'

TRACES_gminfinal <- TRACES_gminfinal %>%
  mutate(Name = case_when(Name == "Amelanchier_ovalis" ~ "Amelanchier ovalis", Name == "Arbutus_unedo" ~ "Arbutus unedo",
                          Name == "Buxus_sempervirens" ~ "Buxus sempervirens", Name == "Pistacia_lentiscus" ~ "Pistacia lentiscus",
                          Name == "Rhamnus_alaternus" ~ "Rhamnus spp",  Name == "Salvia_rosmarinus" ~ "Salvia rosmarinus",
                          TRUE ~ NA_character_))

SpParamsMED_f <- SpParamsMED_f %>%
  full_join(TRACES_gminfinal, by = c("Name", "Code_site"), suffix = c(".x", ".y")) %>%
  mutate(Gswmin = coalesce(Gswmin.y, Gswmin.x), 
         Gswmax = coalesce(Gswmax.y, Gswmax.x)) %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"))

### Adjust the SpParams for our species: Vcmax and Jmax ###########################################

TRACES_isotope <- readxl::read_excel("0. Data gathering/1. Forest object/TRACESWP2_isotope.xlsx")
#View(TRACESWP2_isotope)

TRACES_isotope <- TRACES_isotope[, -c(1:3)]

str(TRACES_isotope)
names(TRACES_isotope)[names(TRACES_isotope) == '% N'] <- 'N'
names(TRACES_isotope)[names(TRACES_isotope) == 'δ 15NAIR (‰)'] <- 'd15N'
names(TRACES_isotope)[names(TRACES_isotope) == '% C'] <- 'C'
names(TRACES_isotope)[names(TRACES_isotope) == 'δ 13CVPDV (‰)'] <- 'd13C'

names(TRACES_finaldata)[names(TRACES_finaldata) == 'C'] <- 'Capacitance'

names(TRACES_isotope)[names(TRACES_isotope) == 'Ind_code'] <- 'plant_code'

TRACES_finaldata <- merge(TRACES_finaldata, TRACES_isotope, by=c("plant_code"), all= TRUE, na="NA")

TRACES_finaldata <- TRACES_finaldata %>%
  mutate(SLA_m_g = SLA / 1000,
         N_area    = (N / 100) * SLA_m_g * 1000)

summary(TRACES_finaldata$SLA)
summary(TRACES_finaldata$SLA_m_g)
summary(TRACES_finaldata$N)
summary(TRACES_finaldata$N_area)

TRACES_finaldata <- TRACES_finaldata %>% # Vcmax and Jmax: Walker et al. (2014), Table 3, model 2 and 4
  mutate(ln_Vcmax = 1.993 + 2.555 * log(N_area) - 0.372 * log(SLA_m_g) + 0.422 * log(N_area) * log(SLA_m_g), 
         Vcmax = exp(ln_Vcmax), 
         ln_Jmax = 1.197 + 0.847 * log(Vcmax), 
         Jmax = exp(ln_Jmax))

summary(TRACES_finaldata$Vcmax)
summary(TRACES_finaldata$Jmax)

ggplot(TRACES_finaldata, aes(x = Vcmax, y = Jmax)) +
  geom_point(aes(color = sp_name)) +
  theme_classic() +
  labs(x = expression(V[cmax]), y = expression(J[max]))

TRACES_finaldata <- TRACES_finaldata %>%
  mutate(Jmax_Vcmax_ratio = Jmax / Vcmax) #Double check to see if the values are plausible

summary(TRACES_finaldata$Jmax_Vcmax_ratio)

TRACES_finaldata <- TRACES_finaldata[!is.na(TRACES_finaldata$Site_code),]

TRACES_finaldata_subset <- TRACES_finaldata %>%
  group_by(Site_code, sp_name) %>%
  summarise(Vmax298 = mean(Vcmax, na.rm = TRUE), 
            Jmax298 = mean(Jmax, na.rm = TRUE),
            Nleaf = mean(N, na.rm = TRUE) * 10,
            .groups = "drop")

names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == 'Site_code'] <- 'Code_site'
names(TRACES_finaldata_subset)[names(TRACES_finaldata_subset) == 'sp_name'] <- 'Name'

TRACES_finaldata_subset <- TRACES_finaldata_subset %>%
  mutate(Name = case_when(Name == "Amelanchier_ovalis" ~ "Amelanchier ovalis", Name == "Arbutus_unedo" ~ "Arbutus unedo",
                          Name == "Buxus_sempervirens" ~ "Buxus sempervirens", Name == "Pistacia_lentiscus" ~ "Pistacia lentiscus",
                          Name == "Rhamnus_alaternus" ~ "Rhamnus spp",  Name == "Salvia_rosmarinus" ~ "Salvia rosmarinus",
                          TRUE ~ NA_character_))

SpParamsMED_f <- SpParamsMED_f %>%
  full_join(TRACES_finaldata_subset, by = c("Name", "Code_site"), suffix = c(".x", ".y")) %>%
  mutate(Vmax298 = coalesce(Vmax298.y, Vmax298.x), 
         Jmax298 = coalesce(Jmax298.y, Jmax298.x),
         Nleaf = coalesce(Nleaf.y, Nleaf.x)) %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"))

### Merge the SpParams with the plants info #######################################################

TRACES_plantinfo <- merge(TRACES_plantinfo, SpParamsMED_f, by=c("Code_site", "Name"), all= TRUE, na="NA")

TRACES_plantinfo <- TRACES_plantinfo[!is.na(TRACES_plantinfo$Height),]

TRACES_plantinfo2 <- TRACES_plantinfo |>
  group_by(Code_site, Name) |>
  summarise(Height = mean(Height),
            LAI = mean(LAI),
            Z50 = mean(Z50),
            Z95 = mean(Z95), 
            VCstem_P12 = mean(VCstem_P12),
            VCstem_P50 = mean(VCstem_P50),
            VCstem_P88 = mean(VCstem_P88), 
            Gswmin = mean(Gswmin), 
            Gswmax = mean(Gswmax), 
            Vmax298 = mean(Vmax298),
            Jmax298 = mean(Jmax298),
            Nleaf = mean(Nleaf)) |>
  mutate(Cover = 100) |>
  #dplyr::select(-c(LeafArea, ProjectedArea)) |>
  rename(Species = Name)

str(TRACES_plantinfo2)

### Create the forest object using the selected data ##############################################

#f <- emptyforest()
#f$shrubData <- as.data.frame(TRACES_plantinfo2[1,])
#class(f)

#x <- spwbInput(f, examplesoil, SpParamsMED, control)

# Filter for each Code_site and species and remove the first column
# Now forest_list$PAS, forest_list$ALO, etc. contain your forest objects

forestPAS_AU <- emptyforest()

forestPAS_AU$shrubData <- TRACES_plantinfo2[TRACES_plantinfo2$Code_site == 1 & TRACES_plantinfo2$Species == "Arbutus unedo" , -1]
class(forestPAS_AU)

# Split data by site and species
plant_split <- split(TRACES_plantinfo2, list(TRACES_plantinfo2$Code_site, TRACES_plantinfo2$Species), drop = TRUE)

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

getwd()
list.files("..")
list.files("1. Model inputs")

# Save everything in the current environment
save.image(file = "1. Model inputs/01. Model inputs_save forest object.RData")
