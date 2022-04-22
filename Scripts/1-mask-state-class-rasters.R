# stsimMLRA42
# Sarah Chisholm (ApexRMS)
#
# Mask state class rasters to soil type raster and to an elevation range of 1000-2000m 

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")

# Load spatial data
# Elevation raster
elevationRaster <- rast(file.path(spatialDataDir, "MLRA42_DEM1arcsec.tif"))

# Soil types
soilTypeRaster <- rast(file.path(spatialDataDir, "NM_ens_dsm_prediction.tif"))[[8]]

# State class raster file paths
stateClassFileList <- list.files(file.path(spatialDataDir, "RAP_processed"),
                                 pattern = "categories.tif",
                                 full.names = TRUE)

# Parameters
targetCrs <- "epsg:3083"

## Create mask raster ----
# Load a template raster on the correct grid
templateRaster <- rast(stateClassFileList[1]) %>% 
  project(y = targetCrs)

# Adjust the crs, resolution and extent of the elevation raster
# zzz: review that these are the correct steps to get onto common grid
elevationRaster <- elevationRaster %>%
  project(y = templateRaster) 

# Create elevation mask raster
elevationMaskRaster <- elevationRaster
elevationMaskRaster[elevationMaskRaster > 2000] <- NA
elevationMaskRaster[elevationMaskRaster < 1000] <- NA

# Adjust the crs, resolution and extent of the elevation raster
# zzz: review that these are the correct steps to get onto common grid
soilTypeRaster <- soilTypeRaster %>% 
  project(y = templateRaster)

# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeMaskRaster <- soilTypeRaster
soilTypeMaskRaster[soilTypeMaskRaster == 1] <- 8
soilTypeMaskRaster[soilTypeMaskRaster == 2] <- 8

# Combine elevation and soil type raster to make one mask raster
maskRaster <- elevationMaskRaster + soilTypeMaskRaster

## Apply elevation and soil mask ----
# Loop over state class files and apply elevation mask
for(stateClassFile in stateClassFileList){
  
  stateClassRaster <- rast(stateClassFile) %>% 
    project(y = targetCrs, method = "near") 
  
  stateClassMaskRaster <- stateClassRaster %>% 
    mask(mask = maskRaster)
  
  outputFile <- stateClassFile %>% 
    str_replace(pattern = "RAP_shrubtree", replacement = "State_Class") %>% 
    str_remove(pattern = "_categories")
  
  writeRaster(stateClassMaskRaster, 
              filename = outputFile,
              overwrite = TRUE)
}
