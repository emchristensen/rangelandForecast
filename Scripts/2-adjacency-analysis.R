# a248
# Sarah Chisholm (ApexRMS)
#
# Adjacency analysis of shrub cover in MLA 42

## Workspace ----

# Load libraries
library(terra)
library(raster)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")
plotDir <- file.path("Plots")

# Load spatial data

# State class rasters
# 2011-2015
stateClass2015Raster <- rast(file.path(spatialDataDir, "RAP_processed", "State_Class_2011_2015_masked.tif"))
# 2016 - 2020
stateClass2020Raster <- rast(file.path(spatialDataDir, "RAP_processed", "State_Class_2016_2020_masked.tif"))

# Soil type raster
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# Parameters

# Coordinate reference system
targetCrs <- "epsg:3083"
# Extent
targetExtent <- c(896966.2851019165245816,
                  915889.6527247427729890,
                  7645343.2786588277667761,
                  7662889.3997297743335366)

# Radius of circular moving window in units of the crs (meters)
windowDiameter <- 720

## Crop state class rasters to target extent ----
# Create a template raster with the target extent
templateRaster <- rast()
ext(templateRaster) <- targetExtent
crs(templateRaster) <- targetCrs

# Crop the state class and soil type rasters to the template
stateClass2015CropRaster <- stateClass2015Raster %>% 
                            crop(y = templateRaster)

stateClass2020CropRaster <- stateClass2020Raster %>% 
                            crop(y = templateRaster)

soilTypeCropRaster <- soilTypeRaster %>% 
                      crop(y = templateRaster)

# Reclassify stateClass2015Raster 
# 1 -> 0 : grassland 
# 2 -> 1 : shrub encroachment
# 3 -> 1 : shrubland 
stateClass2015CropRaster[stateClass2015CropRaster == 1] <- 0
stateClass2015CropRaster[stateClass2015CropRaster == 2] <- 1
stateClass2015CropRaster[stateClass2015CropRaster == 3] <- 1

shrub2015Raster <- stateClass2015CropRaster

# Reclassify stateClass2020Raster 
# 1 -> 0 : grassland 
# 2 -> 1 : shrub encroachment
# 3 -> 1 : shrubland 
stateClass2020CropRaster[stateClass2020CropRaster == 1] <- 0
stateClass2020CropRaster[stateClass2020CropRaster == 2] <- 1
stateClass2020CropRaster[stateClass2020CropRaster == 3] <- 1

shrub2020Raster <- stateClass2020CropRaster

# Reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeCropRaster[soilTypeCropRaster == 1] <- 8
soilTypeCropRaster[soilTypeCropRaster == 2] <- 8

## Create the shrub focal window raster ----
# Define the window size and shape
movingWindow <- focalWeight(shrub2015Raster, windowDiameter, type = "circle")

# Generate focal window raster
shrub2015FocalWindowRaster <- raster::focal(shrub2015Raster, movingWindow, pad = TRUE, padValue = 0.0)

# Get cells that had no shrub in 2015 and have it in 2020
newShrub2020Raster <- shrub2020Raster - shrub2015Raster
newShrub2020Raster[newShrub2020Raster == -1] <- 0

# Create a stack of relevant rasters
names(soilTypeCropRaster) <- "SoilType"
names(stateClass2015CropRaster) <- "StateClass2015"
names(shrub2015Raster) <- "ShrubPresence2015"
names(shrub2015FocalWindowRaster) <- "ShrubFocalWindow2015"
names(newShrub2020Raster) <- "newShrub2020"

rasterStack <- c(soilTypeCropRaster, stateClass2015CropRaster, shrub2015Raster, shrub2015FocalWindowRaster, newShrub2020Raster)

# Generate data frame
shrubTibble <- as.data.frame(rasterStack) %>% 
               tibble %>% 
               filter(ShrubPresence2015 == 0) %>% 
               mutate(SoilType = as.factor(SoilType),
                      StateClass2015 = as.factor(StateClass2015))

# Visualize data
shrubPresencePlot <- shrubTibble %>% 
  ggplot(aes(x = ShrubFocalWindow2015, y = newShrub2020)) +
  geom_jitter() +
  xlab("Proportion of adjacent shrub cover in 2015") +
  ylab("Probability of shrub presence in 2020")
  
ggsave(
  filename = file.path(plotDir, str_c("shrub-presence-2020-vs-shrub-proportion-2015-", windowDiameter, "m-window.png")),
  plot = shrubPresencePlot,
  device = "png", 
  dpi = 300
)

# Model likelihood of shrub presence in 2020 as a function of proportion 
# of adjacent cells that are shrub in 2015, stratified by soil type
modelShrub <- glm(newShrub2020 ~ ShrubFocalWindow2015 + SoilType, data = shrubTibble, family = binomial)

# Get predicted values
transitionProbability <- tibble(
  TransitionType = "Shrub Establishment",
  ShrubFocalWindow2015 = c(seq(0, 1, 0.1), seq(0, 1, 0.1)),
  SoilType = c(as.factor(rep(6, times = 11)), as.factor(rep(7, times = 11)))) 

transitionProbability <- transitionProbability %>% 
  mutate(TransitionProbability = predict(modelShrub, transitionProbability, type = "response")) %>% 
  rename(ProportionShrubCover = ShrubFocalWindow2015)

# Write transition probabilities to disk
write_csv(transitionProbability, file.path(tabularDataDir, str_c("transition-multipliers-", windowDiameter, "m-window.csv")))

# Plot model prediction
transitionProbabilityPlot <- shrubTibble %>% 
  ggplot(aes(x = ShrubFocalWindow2015, y = newShrub2020)) + 
  geom_point() + 
  #geom_jitter() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_wrap(~SoilType) +
  xlab("Proportion of adjacent shrub cover in 2015") +
  ylab("Probability of shrub presence in 2020")

ggsave(
  filename = file.path(plotDir, str_c("grassland2015-to-shrubland2020-probability-", windowDiameter, "m-window.png")),
  plot = transitionProbabilityPlot,
  device = "png", 
  dpi = 300
)
  