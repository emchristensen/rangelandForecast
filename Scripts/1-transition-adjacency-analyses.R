## rangelandForecast
## Sarah Chisholm, ApexRMS
##
## This script performs adjacency analyses for all transitions in the Rangeland 
## Forecast SyncroSim library

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(raster)
library(tidyverse)

# Define directories
spatialDataDir <- file.path(getwd(), "Data", "Spatial")
tabularDataDir <- file.path(getwd(), "Data", "Tabular")
tabularModelInputsDir <- file.path(getwd(), "Model-Inputs", "Tabular")
intermediatesDir <- file.path(getwd(), "Intermediates")

# Parameters
# Radius of circular moving window in units of the crs (meters)
windowRadius <- 90

# Timestep pairs
timeStepPairs <- tibble(x = 1985:2019, y = 1986:2020) %>% 
  filter(x != 2012, y!= 2012)

# File name of output data frame
# NB: change file name according to transition type
adjacencyFilename <- file.path(intermediatesDir, "in-filling-adjacency-output.csv")

# File name of random sampled data frame (sampling performed outside of R in bash)
# NB: change file name according to transition type
adjacencySampleFilename <- file.path(intermediatesDir, "in-filling-adjacency-output-sample.csv")

# Load spatial data
# Study area mask
studyAreaMaskRaster <- rast(file.path(spatialDataDir, "study-area-mask.tif"))

# Soil type
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# Load tabular data
# PDSI drought index
droughtYears <- read_csv(file.path(tabularDataDir, "PDSI_by_year.csv"))

## Prep drought data ----
droughtYearTypes <- droughtYears %>% 
  mutate(yearType = case_when(avg <= -2 ~ "Dry",
                              avg > -2 & avg < 2 ~ "Normal",
                              avg >= 2 ~ "Wet")) %>%
  select(year, yearType)

## Prep soil raster ----
# Crop, mask, and reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeReclassMatrix <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)

soilTypeReclassRaster <- soilTypeRaster %>% 
  project(y = studyAreaMaskRaster, method = "near") %>% 
  mask(mask = studyAreaMaskRaster) %>% 
  classify(rcl = soilTypeReclassMatrix)

# Rename soil type raster
names(soilTypeReclassRaster) <- "soilType"

## Generate data frame ----
# Write an emtpy output csv to disk
tibble(StartTime = integer(0),
       SoilType = character(0),
       yearType = character(0),
       ProportionStart = numeric(0),
       NewStateEnd = integer(0)) %>% 
  write_csv(adjacencyFilename)

# Loop over time step pairs to calculate proportion of no shrub cover
# Time how long the loop takes to run
start_time <- Sys.time()
for(i in seq_along(timeStepPairs$x))
{
  # Clear excess memory
  gc()
  
  # Start time step
  x <- timeStepPairs$x[i]
  # End time step
  y <- timeStepPairs$y[i]
  
  # Print out the start year of the time step pair that is currently being processed
  print(str_c("Currently processing time step pair with start time: ", x))

  ## Crop and mask the state class rasters to the template ----
  stateClassXRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", x, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = studyAreaMaskRaster, method = "near") %>%
                       mask(mask = studyAreaMaskRaster)
  
  stateClassYRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", y, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = studyAreaMaskRaster, method = "near")%>%
                       mask(mask = studyAreaMaskRaster) 
  
   
  ## Reclassify state class rasters to make binary transition layers ----
  # NB: Uncomment chunks for one transition type at a time
  
  # # Establishment
  # 1 (grassland) -> 0
  # 2 (low shrub) -> 1
  # 3 (shrubland) -> 1
  # stateReclassMatrix <- matrix(data = c(1,2,3,0,1,1), nrow = 3, ncol = 2)
 
  # # Loss
  # 1 (grassland) -> 1
  # 2 (low shrub) -> 0
  # 3 (shrubland) -> 0
  # stateReclassMatrix <- matrix(data = c(1,2,3,1,0,0), nrow = 3, ncol = 2)
 
  # # Decline
  # # 1 (grassland) -> 1
  # # 2 (low shrub) -> 1
  # # 3 (shrubland) -> 0
  # stateReclassMatrix <- matrix(data = c(1,2,3,1,1,0), nrow = 3, ncol = 2)
  
  # In-filling
  # 1 (grassland) -> 0
  # 2 (low shrub) -> 0
  # 3 (shrubland) -> 1
  # stateReclassMatrix <- matrix(data = c(1,2,3,0,0,1), nrow = 3, ncol = 2)

  # Reclassify stateClassXRaster
  transitionXRaster <- stateClassXRaster %>%
    classify(rcl = stateReclassMatrix)

  # Reclassify stateClassYRaster
  transitionYRaster <- stateClassYRaster %>%
    classify(rcl = stateReclassMatrix)

  # Get cells that experienced shrub decline (shrubland -> grassland/low shrub)
  newtransitionYRaster <- transitionYRaster - transitionXRaster
  newtransitionYRaster[newtransitionYRaster == -1] <- 0

  # Rename rasters
  names(transitionXRaster) <- "stateClassX"
  names(newtransitionYRaster) <- "newStateClassY"
  
  # Create a multiband raster of relevant layers
  rasterStack <- c(soilTypeReclassRaster, transitionXRaster, newtransitionYRaster)
  
  # Memory management
  rm(stateClassXRaster, stateClassYRaster, stateReclassMatrix, transitionYRaster, 
     newtransitionYRaster)
  
  ## Create focal window rasters ----
  # Define the window size and shape
  movingWindow <- focalWeight(transitionXRaster, windowRadius, type = "circle")
  
  # Generate focal window raster
  stateClassXFocalWindowRaster <- raster::focal(transitionXRaster, movingWindow, pad = TRUE, padValue = 0.0)
  
  # Add NoShrub proportion raster to stack
  add(rasterStack) <- stateClassXFocalWindowRaster
  
  # Memory management
  rm(transitionXRaster, movingWindow, stateClassXFocalWindowRaster)
  
  ## Generate data frame ----
  output <- as.data.frame(rasterStack) %>% 
    tibble %>% 
    mutate(StartTime = x %>% as.numeric,
           EndTime = y %>%  as.numeric,
           ProportionStart = focal_sum,
           PresenceStart = stateClassX,
           NewStateEnd = newStateClassY,
           SoilType = case_when(soilType == 3 ~ "Loamy-Clayey",
                                soilType == 4 ~ "Gravelly and Calcic",
                                soilType == 5 ~ "Bedrock and Colluvium",
                                soilType == 6 ~ "Gypsic",
                                soilType == 7 ~ "Bottomland",
                                soilType == 8 ~ "Sandy and Deep Sand") %>% as.factor) %>% 
    # Match the type of drought year based on the EndTime of the adjacency analysis
    # (i.e. is the probability of transition in present year influenced by drought conditions of present year)
    left_join(droughtYearTypes, by = c("EndTime" = "year")) %>% 
    filter(PresenceStart == 0) %>% 
    select(StartTime, SoilType, yearType, ProportionStart, NewStateEnd)
  
  # Write rows to disk
  write_csv(output, adjacencyFilename, append = TRUE)
}
end_time <- Sys.time()
end_time - start_time

# Memory management
rm(studyAreaMaskRaster, soilTypeRaster, soilTypeReclassRaster)

## Logisitic regression ----
# NB: Run for one transition at a time
# Model likelihood of a given transition for a given year as a function of the 
# proportion of adjacent cells that are covered by the 'end' state class in the 
# previous year, stratified by soil type and drought year type

# Read in a randomly sampled subset
transitionAdjacency <- read_csv(adjacencySampleFilename) %>%
  mutate(yearType = yearType %>% factor(order = TRUE, levels = c("Dry", "Normal", "Wet")),
         SoilType = SoilType %>% as.factor())

### Generate predicted transition probabilities ----
# Define function predictTransition
predictTransition <- function(data, SoilType, yearType, ProportionStartRange = seq(0,1,0.1)){
  modelFit <- glm(NewStateEnd ~ ProportionStart, data = data, family = binomial)
  newData <- expand_grid(SoilType = SoilType, yearType = yearType, ProportionStart = ProportionStartRange) 
  pred <- predict.glm(modelFit, newData, type = "response")
  newData %>% 
    mutate(pred = pred) %>% 
    return()
}

# Generate output data of predicted values and write to disk
output <- transitionAdjacency %>% 
  select(-StartTime) %>% 
  group_by(SoilType, yearType) %>% 
  nest %>% 
  pmap_dfr(predictTransition) %>% 
  write_csv(file.path(tabularModelInputsDir, "Transition Adjacency Multipliers.csv"))