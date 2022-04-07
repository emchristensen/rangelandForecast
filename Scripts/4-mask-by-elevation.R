# Mask state class rasters to elevation between 1000-2000m

## Workspace ----

# Load libraries
library(terra)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")

# Load spatial data
# Elevation raster
elevationRaster <- rast(file.path(spatialDataDir, "MLRA42_DEM1arcsec.tif"))

# State class raster file paths
stateClassFileList <- list.files(file.path(spatialDataDir, "RAP_processed"),
                                 pattern = "categories.tif",
                                 full.names = TRUE)

# Parameters
targetCrs <- "epsg:3083"

## Mask by elevation ----
# Load a template raster on the correct grid
templateRaster <- rast(stateClassFileList[1]) %>% 
  project(y = targetCrs)

# Adjust the resolution and extent of the elevation raster
elevationRaster <- elevationRaster %>% 
  resample(y = templateRaster)

# Create elevation mask raster
maskRaster <- elevationRaster
maskRaster[maskRaster > 2000] <- NA
maskRaster[maskRaster < 1000] <- NA

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
