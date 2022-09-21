## rangelandForecast
## Sarah Chisholm, ApexRMS
##
## Create spatial initial conditions at the full study area extent and at a 
## smaller test extent

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(tidyverse)
library(terra)

# Define directories
spatialDataDir <- file.path(getwd(), "Data", "Spatial")
spatialModelInputsDir <- file.path(getwd(), "Model-Inputs", "Spatial")

# Parameters
# Test extent
targetExtent <- ext(809000, 875000, 7600000, 7666000)

# Load spatial data
# Studay area mask
studyAreaMaskRaster <- rast(file.path(spatialDataDir, "study-area-mask.tif"))

# Soil type raster
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# State class map (1985)
stateClass1985Raster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_1985.tif"))

# State class map (2020)
stateClass2020Raster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_2020.tif"))

## Create Spatial Initial Conditions ----

## Crop, mask, and reclassify soil type raster ----
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeReclassMatrix <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)

# Soil type - Full Extent
soilTypeRaster <- soilTypeRaster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  classify(rcl = soilTypeReclassMatrix) %>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "soil-type.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

# Soil type - Test Extent
test <- soilTypeRaster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  crop(y = targetExtent)%>% 
  classify(rcl = soilTypeReclassMatrix)%>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "soil-type-test-extent.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

## Crop and mask the state class rasters ----

# Initial state class - 1985
stateClass1985Raster <- stateClass1985Raster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "state-class-1985.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

# Initial state class - 2020
stateClass2020Raster <- stateClass2020Raster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "state-class-2020.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

# Initial state class - 1985 - Test Extent
test <- stateClass1985Raster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  crop(y = targetExtent)%>%  
  writeRaster(filename = file.path(spatialModelInputsDir, "state-class-test-extent-1985.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

# Initial state class - 2020 - Test Extent
test <- stateClass2020Raster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  crop(y = targetExtent)%>%  
  writeRaster(filename = file.path(spatialModelInputsDir, "state-class-test-extent-2020.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)
