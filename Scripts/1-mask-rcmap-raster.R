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
# Elevation raster
# elevationRaster <- rast(file.path(spatialDataDir, "elevation-mask.tif"))

# Soil types
# soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# MLRA42 
mlra42Shapefile <- st_read(dsn = file.path(spatialDataDir, "nrcs142p2_052440"),
                           layer = "mlra_v42") %>% 
                   filter(MLRA_ID == 54)

mlra42File <- system.file(file.path(spatialDataDir, "nrcs142p2_052440", "mlra_v42.shp"), package = "terra")
v <- vect(mlra42File)

# US state boundaries
# Source: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
newMexicoShapefile <- st_read(dsn = file.path(spatialDataDir, "cb_2018_us_state_500k"),
                                    layer = "cb_2018_us_state_500k") %>% 
                      filter(NAME == "New Mexico")

# Land Fire Biophysical Settings (Remap 2016)
bpsRaster <- rast(file.path(spatialDataDir, 
                            "LF2016_BPS_200_CONUS", 
                            "LF2016_BPS_200_CONUS", 
                            "Tif", 
                            "LC16_BPS_200.tif"))

# Parameters
# State class raster file paths
shrubCoverFileList <- list.files(file.path(spatialDataDir, "RCMAP"),
                                 pattern = ".img$",
                                 full.names = TRUE,
                                 recursive = TRUE)

targetCrs <- "epsg:3083"
#targetExtent <- c(-1294425, -492135, 618525, 1461675)

## Create mask raster ----
# Load a template raster on the correct grid
templateRaster <- rast(shrubCoverFileList[1]) %>% 
  project(y = targetCrs)
  #crop(y = targetExtent)

# Adjust the crs, resolution and extent of the elevation raster
# zzz: review that these are the correct steps to get onto common grid
# elevationRaster <- elevationRaster %>%
#   project(y = templateRaster) 
# 
# Create elevation mask raster
# elevationMaskRaster <- elevationRaster
# elevationMaskRaster[elevationMaskRaster > 2000] <- NA
# elevationMaskRaster[elevationMaskRaster < 1000] <- NA

# Adjust the crs, resolution and extent of the elevation raster
# zzz: review that these are the correct steps to get onto common grid
# soilTypeRaster <- soilTypeRaster %>% 
#   project(y = templateRaster)

# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
# soilTypeMaskRaster <- soilTypeRaster
# soilTypeMaskRaster[soilTypeMaskRaster == 1] <- 8
# soilTypeMaskRaster[soilTypeMaskRaster == 2] <- 8

# Adjust the crs of the New Mexico state boundary shapefile
newMexicoShapefile <- newMexicoShapefile %>% 
  st_transform(crs = crs(templateRaster))

# Adjust the crs of the MLRA42 shapefile
mlra42Shapefile <- mlra42Shapefile %>% 
  st_transform(crs = crs(templateRaster)) %>% 
  as("Spatial") %>% 
  vect

# Combine elevation and soil type raster to make one mask raster
#maskRaster <- elevationMaskRaster + soilTypeMaskRaster

test <- templateRaster %>% 
  crop(y = mlra42Shapefile) %>% 
  mask(mask = mlra42Shapefile)

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

# May 13 2022
# Mask bps to grass ans shrubland cells
bps <- rast("./Data/Spatial/bps-mlra42-new-mexico-soil-elev.tif")

bpsTable <- read_csv("./Data/Tabular/bps-classes-mlra42-new-mexico-soil-elev.csv")

bpsShrubGrassValues <- bpsTable %>% 
  filter(GROUPVEG == "Shrubland" | GROUPVEG == "Grassland") %>% 
  pull(Value)

bpsMask <- bps

bpsMask[bpsMask %in% bpsShrubGrassValues] <- 1
bpsMask[bpsMask != 1] <- NA

bpsMaskGrassShrub <- bps %>% 
  mask(mask = bpsMask)

freqBpsGrassShrub <- freq(bpsMaskGrassShrub)

bpsTrim <- trim(bps)

writeRaster(bpsTrim, "./Data/Spatial//bps-mlra42-new-mexico-soil-elev-shrub-grass.tif",
            overwrite = TRUE)

# May 18 2022
# Mask out anthropogenic land cover 
bps <- rast(file.path(spatialDataDir, "bps-mlra42-new-mexico-soil-elev-shrub-grass.tif"))

landCoverRaster <- rast(file.path(spatialDataDir, "land-cover-cropped.tif"))

# Crop land cover raster to current study extent
# -zzz: this takes too long to project; crop manually in QGIS first. 
landCoverProjectRaster <- landCoverRaster %>% 
  project(y = bps, method = "near") 
  #crop(x = landCoverRaster, y = bps, snap = "out")

# Reclassify land cover to mask out anthropogenic classes
landCoverClasses <- unique(landCoverProjectRaster) %>% 
  pull(`land-cover-cropped`) %>% 
  append(c(0,11,NA,NA,NA,NA,31,41,42,43,52,71,NA,NA,90,95))

landCoverReclassMatrix <- matrix(data = landCoverClasses,
                                 nrow = 16,
                                 ncol = 2)

landCoverMask <- landCoverProjectRaster %>% 
  classify(rcl = landCoverReclassMatrix)

bpsMasked <- bps %>% 
  mask(mask = landCoverMask)

# Get unique BPS classes
cellCountBps <- freq(bpsMasked)

bpsClasses <- tibble(
  Value = cellCountBps[, c(2)],
  Count = cellCountBps[, c(3)])

write_csv(bpsClasses, file.path(tabularDataDir, "bps-classes-no-anthropogenic-cells.csv"))

# look at land cover types in area
landCoverCountRaster <- landCoverMask %>% 
  mask(mask = bpsMasked)

cellCountLandCover <- freq(landCoverCountRaster)

landCoverClasses <- tibble(
  Value = cellCountLandCover[, c(2)],
  Count = cellCountLandCover[, c(3)])
