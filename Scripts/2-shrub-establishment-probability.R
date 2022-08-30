# stsimMLRA42
# Sarah Chisholm (ApexRMS)
#
# This script models the probability of shrub establishment as a functiion of time in MLRA 42

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(tidyverse)
library(scales)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")
plotDir <- file.path("Plots")

# Load spatial data
# Soil type raster
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# Elevation raster
elevationRaster <- rast((file.path(spatialDataDir, "MLRA42_DEM1arcsec.tif")))

# Parameters
# List state class tif files 
stateClassFileList <- list.files(file.path(spatialDataDir, "RAP_processed"),
                                pattern = "masked.tif",
                                full.names = TRUE)

# Coordinate reference system
targetCrs <- "epsg:3083"

# Extent
targetExtent <- c(888895.6203079018741846,
                  896415.3864811647217721,
                  7535714.4531292086467147,
                  7543289.3091645464301109)

# Larger extent
# Cell count = 33499185 * 7 years
# c(896966.2851019165245816,
#   915889.6527247427729890,
#   7645343.2786588277667761,
#   7662889.3997297743335366)

targetResolution <- 27.54493

#List of pixel IDs
pixelID <- rep(c(1:75075), times = 7) %>% sort()

## Prep rasters ----
# State class rasters
# Create a template raster with the target extent
templateRaster <- rast()
ext(templateRaster) <- targetExtent
crs(templateRaster) <- targetCrs
res(templateRaster) <- targetResolution

# Load state class rasters and crop to test extent
stateClassRasters <- rast(stateClassFileList) %>% 
  crop(y = templateRaster)

# Reclassify state class rasters to generate shrub presence rasters
# 1 -> 0 
# 2 -> 1
# 3 -> 1
shrubPresenceMatrix <- matrix(data = c(1, 2, 3, 0, 1, 1), nrow = 3, ncol = 2)
shrubPresenceRasters <- rast()

for(i in seq(1,7)){
  shrubPresenceRaster <- stateClassRasters[[i]] %>% 
    classify(rcl = shrubPresenceMatrix)
  
  names(shrubPresenceRaster) <- names(shrubPresenceRaster) %>% 
    str_replace(pattern = "RAP_shrubtree", replacement = "shrub_presence") %>% 
    str_remove(pattern = "_categories")
  
  add(shrubPresenceRasters) <- shrubPresenceRaster
}

# Memory management
rm(stateClassRasters)

# Soil raster
# Crop to test extent
soilTypeRaster <- soilTypeRaster %>% 
  crop(y = templateRaster)

# Reclassify soil types
soilTypeReclassMatirx <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)
soilTypeReclassRaster <- soilTypeRaster %>% 
  classify(rcl = soilTypeReclassMatirx)

# Add soil type raster to stack of shrub presence rasters
add(shrubPresenceRasters) <- soilTypeReclassRaster

# Elevation raster
# Crop and project to test extent
elevationRaster <- elevationRaster %>% 
  project(y = templateRaster)

# Add elevation raster to stack of shrub presence rasters
add(shrubPresenceRasters) <- elevationRaster

## Generate data table ----
shrubPresenceTibble <- as.data.frame(shrubPresenceRasters) %>% 
  tibble %>%
  pivot_longer(cols = shrub_presence_1986_1990:shrub_presence_2016_2020,
               names_to = "timeStep",
               names_prefix = "shrub_presence_",
               values_to = "shrubPresence") %>% 
  mutate(pixelID = pixelID %>% as.factor,
         timeStep = timeStep %>% str_sub(start = 6, end = 9) %>% as.numeric,
         soilType = NM_ens_dsm_prediction_8 %>% as.factor,
         elevation = MLRA42_DEM1arcsec,
         elevationBinned = case_when(elevation <= 1500 ~ "Low Elevation",
                                     elevation > 1500 ~ "High Elevation") %>% as.factor,
         timeStepScaled = scales::rescale(timeStep, to=c(0,6)),
         elevationScaled = scales::rescale(elevation)) %>% 
  select(pixelID, timeStep, soilType, elevation, elevationBinned, timeStepScaled, elevationScaled, shrubPresence)

# Save shrubPresenceTibble to disk
write_csv(shrubPresenceTibble, file.path(tabularDataDir, "shrub-presence-data-frame.csv"))

# Memory management
rm(templateRaster, soilTypeRaster, soilTypeReclassRaster)

## Exploratory Analysis ----
# Limit to cells that had no shrub in 1990
shrubPresenceTibble %>% 
  filter(shrubPresence = )

# Plot response against each predictor variable
timePlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = timeStep, y = shrubPresence)) + 
  geom_jitter() +
  xlab("Timestep") +
  ylab("Shrub presence")+
  theme_bw()

soilPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = soilType, y = shrubPresence)) + 
  geom_boxplot() +
  geom_jitter() +
  xlab("Soil Type") +
  ylab("Shrub presence")+
  theme_bw()

elevPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = elevation, y = shrubPresence)) + 
  geom_jitter() +
  xlab("Elevation (m)") +
  ylab("Shrub presence")+
  theme_bw()

elevBinPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = elevationBinned, y = shrubPresence)) + 
  geom_boxplot() +
  geom_jitter() +
  xlab("Elevation Bins") +
  ylab("Shrub presence")+
  theme_bw()

# Save plots to disk
ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-vs-time.png"),
  plot = timePlot,
  device = "png", 
  dpi = 300
)

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-vs-soil.png"),
  plot = soilPlot,
  device = "png", 
  dpi = 300
)

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-vs-elevation.png"),
  plot = elevPlot,
  device = "png", 
  dpi = 300
)

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-vs-elevation-binned.png"),
  plot = elevBinPlot,
  device = "png", 
  dpi = 300
)

# Plot predictor variables against each other
soilElevPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = elevation, y = soilType)) + 
  geom_jitter() +
  xlab("Elevation") +
  ylab("Soil Type")+
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "soil-vs-elevation.png"),
  plot = soilElevPlot,
  device = "png", 
  dpi = 300
)

## Logistic regression model ----
# Load data frame
shrubPresenceTibble <- read_csv(file.path(tabularDataDir, "shrub-presence-data-frame-smaller-sample.csv"))

# Model shrub presence as a function of:
# Time step
shrubPresenceModel <- glm(shrubPresence ~ timeStepScaled, data = shrubPresenceTibble, family = binomial)
# Time step and soil type
shrubPresenceSoilModel <- glm(shrubPresence ~ timeStepScaled + soilType, data = shrubPresenceTibble, family = binomial)
# Time step and elevation (continuous)
shrubPresenceElevModel <- glm(shrubPresence ~ timeStepScaled + elevationScaled, data = shrubPresenceTibble, family = binomial)
# Time step and elevation (binned)
shrubPresenceElevBinModel <- glm(shrubPresence ~ timeStepScaled + elevationBinned, data = shrubPresenceTibble, family = binomial)
# Time step, soil type and elevation (continuous)
shrubPresenceSoilElevModel <- glm(shrubPresence ~ timeStepScaled + soilType + elevationScaled, data = shrubPresenceTibble, family = binomial)
# Time step, soil type and elevation (continuous)
shrubPresenceSoilEleBinvModel <- glm(shrubPresence ~ timeStepScaled + soilType + elevationBinned, data = shrubPresenceTibble, family = binomial)

# Check fit
AIC(shrubPresenceModel, 
    shrubPresenceSoilModel,
    shrubPresenceElevModel,
    shrubPresenceElevBinModel,
    shrubPresenceSoilElevModel,   
    shrubPresenceSoilEleBinvModel)

# Plot model prediction
timeSoilElevPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = timeStepScaled, y = shrubPresence, colour = elevation)) + 
  #geom_point() + 
  geom_jitter() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_wrap(~soilType) +
  xlab("Timestep") +
  ylab("Probability of shrub presence") +
  theme_bw()

timeSoilPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = timeStepScaled, y = shrubPresence)) + 
  #geom_point() + 
  geom_jitter() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_wrap(~soilType) +
  xlab("Timestep") +
  ylab("Probability of shrub presence") +
  theme_bw()

# Save plot to disk
ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-time-soil-elevation.png"),
  plot = timeSoilElevPlot,
  device = "png", 
  dpi = 300
)

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-time-soil.png"),
  plot = timeSoilPlot,
  device = "png", 
  dpi = 300
)

# Testing ---- 

# Check that soil values line up correctly with presence rasters
soilValues <- as.vector(soilTypeRaster %>% mask(mask = shrubPresenceRasters[[1]]))
soilValues <- soilValues[soilValues %in% c(1,2,3,4,5,6,7,8)]

soilValues2015 <- shrubPresenceTibble %>% 
  filter(timeStep == 2015) %>% 
  pull(soilType)

soilValues == soilValues2015

## Try shrub presence one year as a function of presence of the next year
shrubPresenceTibble <- as.data.frame(shrubPresenceRasters) %>% 
  tibble %>%
  mutate(presence2005 = shrub_presence_2001_2005,
         presence2010 = shrub_presence_2006_2010,
         soilType = NM_ens_dsm_prediction_8 %>% as.factor,
         elevation = MLRA42_DEM1arcsec,
         elevationBinned = case_when(elevation <= 1500 ~ "Low Elevation",
                                     elevation > 1500 ~ "High Elevation") %>% as.factor,
         elevationScaled = scales::rescale(elevation)) %>% 
  select(presence2005, presence2010, soilType, elevation, elevationBinned, elevationScaled)

# Plot presence2020 ~ presence2015
shrubPresenceTibble %>% 
  ggplot(aes(x = presence2015, y = presence2020)) +
  geom_jitter() +
  theme_bw()

shrubPresenceModel <- glm(presence2020 ~ presence2015, data = shrubPresenceTibble, family = binomial)
# Time step and soil type
shrubPresenceSoilModel <- glm(presence2020 ~ presence2015 + soilType, data = shrubPresenceTibble, family = binomial)
# Time step and elevation (continuous)
shrubPresenceElevModel <- glm(presence2020 ~ presence2015 + elevationScaled, data = shrubPresenceTibble, family = binomial)
# Time step and elevation (binned)
shrubPresenceElevBinModel <- glm(presence2020 ~ presence2015 + elevationBinned, data = shrubPresenceTibble, family = binomial)
# Time step, soil type and elevation (continuous)
shrubPresenceSoilElevModel <- glm(presence2020 ~ presence2015 + soilType + elevationScaled, data = shrubPresenceTibble, family = binomial)
# Time step, soil type and elevation (continuous)
shrubPresenceSoilEleBinvModel <- glm(presence2020 ~ presence2015 + soilType + elevationBinned, data = shrubPresenceTibble, family = binomial)

# Check fit
AIC(shrubPresenceModel, 
    shrubPresenceSoilModel,
    shrubPresenceElevModel,
    shrubPresenceElevBinModel,
    shrubPresenceSoilElevModel,   
    shrubPresenceSoilEleBinvModel)

# Plot model prediction
timeSoilElevPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = presence1990, y = presence1995, colour = elevation)) + 
  #geom_point() + 
  geom_jitter() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = FALSE) +
  facet_wrap(~soilType) +
  xlab("Shrub Presence 1990") +
  ylab("Shrub Presence 1995") +
  theme_bw()

timeSoilPlot <- shrubPresenceTibble %>% 
  ggplot(aes(x = presence2005, y = presence2010)) + 
  #geom_point() + 
  geom_jitter() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = FALSE) +
  facet_wrap(~soilType) +
  xlab("Shrub Presence 2005") +
  ylab("Shrub Presence 2010") +
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Logistic Regression", "shrub-time-soil-2005-2010.png"),
  plot = timeSoilPlot,
  device = "png", 
  dpi = 300
)
