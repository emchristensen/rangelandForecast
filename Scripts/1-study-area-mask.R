# a248
# Sarah Chisholm, ApexRMS
#
# Crop RAP data to New Mexico boundary and extract BPS classes

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(sf)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")

# Load spatial data
# LF BPS masked to Shrub and Grassland, excluding anthropogenic
bpsRaster <- rast(file.path(spatialDataDir, "LF2016_BPS_200_CONUS", "Processed", "bps-no-anthropogenic-cells.tif"))

# Parameters

# Coordinate reference system
targetCrs <- "epsg:3083"

# Target resolution
targetResolution <- 30

#Target Extent
targetExtent <- c(658270, 1039720, 7497000, 7820100)

## Create template raster ----
templateRaster <- rast()
crs(templateRaster) <- targetCrs
ext(templateRaster) <- targetExtent
res(templateRaster) <- targetResolution

# Get layers onto template grid
bpsMaskRaster <- bpsRaster %>% 
  resample(y = templateRaster, method = "near")

bpsMaskRaster[!is.na(bpsMaskRaster)] <- 1

writeRaster(bpsMaskRaster, 
            file.path(spatialDataDir, "study-area-mask.tif"), 
            overwrite = TRUE)
