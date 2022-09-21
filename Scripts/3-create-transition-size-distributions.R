## rangelandForecast
## Sarah Chisholm, ApexRMS
##
## Calculate transition size distributions and generate datasheets

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(raster)
library(tidyverse)

# Define directories
spatialDataDir <- file.path(getwd(), "Data", "Spatial")
tabularModelInputsDir <- file.path(getwd(), "Model-Inputs", "Tabular")

# Parameters
# Test extent
targetExtent <- ext(809000, 875000, 7600000, 7666000)

# Cell size to area in hectares
scaleFactor <- 0.09

# Transitions
transitionTypes <- c("Establishment", "Loss", "Decline", "In-filling")

# List transition size distribution files
transitionSizeDistributionFiles <- list.files(
  path = tabularModelInputsDir,
  pattern = "Transition Size Distribution", 
  full.names = TRUE)

# Load spatial data
# Study area mask
studyAreaMaskRaster <- rast(file.path(spatialDataDir, "study-area-mask.tif"))

# State Class 1985
stateClassXRaster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_1985.tif")) %>% 
  project(y = studyAreaMaskRaster, method = "near") %>%
  mask(mask = studyAreaMaskRaster) %>% 
  crop(y = targetExtent)

# State Class 1986
stateClassYRaster <- rast(file.path(spatialDataDir, "RCMAP", "Processed","rcmap_states_1986.tif")) %>% 
  project(y = studyAreaMaskRaster, method = "near")%>%
  mask(mask = studyAreaMaskRaster) %>% 
  crop(y = targetExtent)

## Caluclate transition size frequencies ----
for(transitionType in transitionTypes) {
  # Reclassify state class rasters to make binary transition layers
  if(transitionType == "Establishment") {
    # Establishment
    reclassMatrix <- matrix(data = c(1,2,3,0,1,1), nrow = 3, ncol = 2)
  } else if(transitionType == "Loss") {
    # Loss
    reclassMatrix <- matrix(data = c(1,2,3,1,0,0), nrow = 3, ncol = 2)
  } else if(transitionType == "Decline") {
    # Decline
    reclassMatrix <- matrix(data = c(1,2,3,1,1,0), nrow = 3, ncol = 2)
  } else if(transitionType == "In-filling") {
    # In-filling
    reclassMatrix <- matrix(data = c(1,2,3,0,0,1), nrow = 3, ncol = 2)
  }
  
  # Reclassify stateClassXRaster
  shrubXRaster <- stateClassXRaster %>%
    classify(rcl = reclassMatrix)
  
  # Reclassify stateClassYRaster
  shrubYRaster <- stateClassYRaster %>%
    classify(rcl = reclassMatrix)
  
  # Get cells that had no shrub in shrubXRaster and have it in shrubYRaster
  newShrubYRaster <- shrubYRaster - shrubXRaster
  newShrubYRaster[newShrubYRaster == -1] <- 0
  
  # Memory management 
  rm(shrubXRaster, shrubYRaster)
  
  # Generate Patches
  patchRaster <- patches(x = newShrubYRaster, 
                         directions = 8,
                         zeroAsNA = TRUE)
  
  # Get patch size frequency
  patchSizeFreq <- patchRaster %>% 
    freq %>% 
    as.tibble() %>% 
    select(count) %>%
    group_by(count) %>% 
    count() %>% 
    rename(PatchCellCount = count,
           Frequency = n) %>% 
    mutate(PatchAreaHa = PatchCellCount * scaleFactor) %>% 
    select(PatchCellCount, PatchAreaHa, Frequency)
  
  write_csv(patchSizeFreq, file.path(tabularModelInputsDir, 
                                     str_c("Transition Size Distribution - Shrub ", transitionType, ".csv")))
}

## Bin transition sizes ----
# Decline
declineDf <- transitionSizeDistributionFiles[[1]] %>% 
  read_csv() %>% 
  select(-PatchCellCount) %>% 
  mutate(MaximumArea = case_when(PatchAreaHa <= 0.1 ~ 0.1,
                                 PatchAreaHa <= 1 & PatchAreaHa > 0.1 ~ 1,
                                 PatchAreaHa <= 10 & PatchAreaHa > 1 ~ 10,
                                 PatchAreaHa <= 100 & PatchAreaHa > 10 ~ 100,
                                 PatchAreaHa <= max(PatchAreaHa) & PatchAreaHa > 100 ~ max(PatchAreaHa))) %>% 
  group_by(MaximumArea) %>% 
  summarise(RelativeAmount = sum(Frequency)) %>% 
  ungroup() %>% 
  mutate(TransitionType = "Shrub decline [Type]") %>% 
  select(TransitionType, MaximumArea, RelativeAmount) %>% 
  write_csv(file.path(tabularModelInputsDir, "Transition Size Distribution - Shrub Decline.csv"))

# Establishment
establishmentDf <- transitionSizeDistributionFiles[[2]] %>% 
  read_csv() %>% 
  select(-PatchCellCount) %>% 
  mutate(MaximumArea = case_when(PatchAreaHa <= 0.1 ~ 0.1,
                                 PatchAreaHa <= 1 & PatchAreaHa > 0.1 ~ 1,
                                 PatchAreaHa <= max(PatchAreaHa) & PatchAreaHa > 1 ~ max(PatchAreaHa))) %>% 
  group_by(MaximumArea) %>% 
  summarise(RelativeAmount = sum(Frequency)) %>% 
  ungroup() %>% 
  mutate(TransitionType = "Shrub establishment [Type]") %>% 
  select(TransitionType, MaximumArea, RelativeAmount)%>% 
  write_csv(file.path(tabularModelInputsDir, "Transition Size Distribution - Shrub Establishment.csv"))

# In-Filling
inFillingDf <- transitionSizeDistributionFiles[[3]] %>% 
  read_csv() %>% 
  select(-PatchCellCount) %>% 
  mutate(MaximumArea = case_when(PatchAreaHa <= 0.1 ~ 0.1,
                                 PatchAreaHa <= 1 & PatchAreaHa > 0.1 ~ 1,
                                 PatchAreaHa <= 10 & PatchAreaHa > 1 ~ 10,
                                 PatchAreaHa <= max(PatchAreaHa) & PatchAreaHa > 10 ~ max(PatchAreaHa))) %>% 
  group_by(MaximumArea) %>% 
  summarise(RelativeAmount = sum(Frequency)) %>% 
  ungroup() %>% 
  mutate(TransitionType = "Shrub in-filling [Type]") %>% 
  select(TransitionType, MaximumArea, RelativeAmount) %>% 
  write_csv(file.path(tabularModelInputsDir, "Transition Size Distribution - Shrub In-Filling.csv"))

# Loss
lossDf <- transitionSizeDistributionFiles[[4]] %>% 
  read_csv() %>% 
  select(-PatchCellCount) %>% 
  mutate(MaximumArea = case_when(PatchAreaHa <= 0.1 ~ 0.1,
                                 PatchAreaHa <= 1 & PatchAreaHa > 0.1 ~ 1,
                                 PatchAreaHa <= 10 & PatchAreaHa > 1 ~ 10,
                                 PatchAreaHa <= max(PatchAreaHa) & PatchAreaHa > 10 ~ max(PatchAreaHa))) %>% 
  group_by(MaximumArea) %>% 
  summarise(RelativeAmount = sum(Frequency)) %>% 
  ungroup() %>% 
  mutate(TransitionType = "Shrub loss [Type]") %>% 
  select(TransitionType, MaximumArea, RelativeAmount) %>% 
  write_csv(file.path(tabularModelInputsDir, "Transition Size Distribution - Shrub Loss.csv"))
