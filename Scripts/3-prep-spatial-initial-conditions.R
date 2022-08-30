# stsimMLRA42
# Sarah Chisholm (ApexRMS)
#
# Create spatial initial conditions

# Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(tidyverse)
library(terra)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
spatialModelInputsDir <- file.path("Model Inputs", "Spatial")

# Load spatial data
# Studay area mask
studyAreaMaskRaster <- rast(file.path(spatialDataDir, "study-area-mask.tif"))

# Soil type raster
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# State class map (1985)
stateClassRaster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_1985.tif"))

# Create Spatial Initial Conditions ----

# Crop, mask, and reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeReclassMatrix <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)

soilTypeRaster <- soilTypeRaster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  classify(rcl = soilTypeReclassMatrix) %>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "soil-type-study-extent.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)

# Crop and mask the state class raster
stateClassRaster <- stateClassRaster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  writeRaster(filename = file.path(spatialModelInputsDir, "state-class-study-extent.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = TRUE)