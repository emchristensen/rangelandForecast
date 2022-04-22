# stsimMLRA42
# Sarah Chisholm (ApexRMS)
#
# Adjacency analysis of shrub cover in MLRA 42

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(terra)
library(raster)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")
plotDir <- file.path("Plots")

# Load spatial data
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
windowRadii <- c(90, 180, 360, 720)

# Timestep pairs
timeStepPairs <- list(list(x = 1990, y = 1995),
                      list(x = 1995, y = 2000), 
                      list(x = 2000, y = 2005), 
                      list(x = 2005, y = 2010), 
                      list(x = 2010, y = 2015), 
                      list(x = 2015, y = 2020))

## Prep soil raster ----
# Create a template raster with the target extent
templateRaster <- rast()
ext(templateRaster) <- targetExtent
crs(templateRaster) <- targetCrs

# Crop and reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeReclassMatrix <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)

soilTypeReclassRaster <- soilTypeRaster %>% 
  crop(y = templateRaster) %>% 
  classify(rcl = soilTypeReclassMatrix)

# Rename soil type raster
names(soilTypeReclassRaster) <- "soilType"

## Generate data frame ----
shrubTibble <- imap_dfr(timeStepPairs,
~{
  # .x <- timeStepPairs[[6]]
  
  # Crop the state class rasters to the template
  stateClassXRaster <- rast(list.files(file.path(spatialDataDir, "RAP_processed"),
                                       pattern = .x$x %>% str_c("_masked.tif"),
                                       full.names = TRUE)) %>% 
                       crop(y = templateRaster)
  
  stateClassYRaster <- rast(list.files(file.path(spatialDataDir, "RAP_processed"),
                                       pattern = .x$y %>% str_c("_masked.tif"),
                                       full.names = TRUE)) %>% 
                       crop(y = templateRaster)
  
  # Reclassify state class rasters to make binary shrub presence layers
  # 1 (grassland) -> 0  
  # 2 (shrub encroachment) -> 1 
  # 3 (shrubland) -> 1 
  shrubReclassMatrix <- matrix(data = c(1,2,3,0,1,1), nrow = 3, ncol = 2)
  shrubXRaster <- stateClassXRaster %>% 
    classify(rcl = shrubReclassMatrix)
  
  # Reclassify stateClass2020Raster 
  shrubYRaster <- stateClassYRaster %>% 
    classify(rcl = shrubReclassMatrix)
  
  # Get cells that had no shrub in 2015 and have it in 2020
  newShrubYRaster <- shrubYRaster - shrubXRaster
  newShrubYRaster[newShrubYRaster == -1] <- 0
  
  # Rename rasters
  names(shrubXRaster) <- "shrubPresenceX" 
  names(newShrubYRaster) <- "newShrubY" 
  
  # Create a multiband raster of relevant layers
  rasterStack <- c(soilTypeReclassRaster, shrubXRaster, newShrubYRaster)
  
  ## Create the shrub focal window rasters
  for(windowRadius in windowRadii){
    
    #windowRadius <- windowRadii[[1]]
    
    # Define the window size and shape
    movingWindow <- focalWeight(shrubXRaster, windowRadius, type = "circle")
    
    # Generate focal window raster
    shrubXFocalWindowRaster <- raster::focal(shrubXRaster, movingWindow, pad = TRUE, padValue = 0.0)
    
    # Rename focal window raster
    names(shrubXFocalWindowRaster) <- "ShrubProportion" %>% str_c(paste0(.x$x, "_", windowRadius, "m"))
    
    add(rasterStack) <- shrubXFocalWindowRaster
  }
    
  ShrubProportion90m <- "ShrubProportion" %>% str_c(paste0(.x$x, "_90m"))
  ShrubProportion720m <- "ShrubProportion" %>% str_c(paste0(.x$x, "_720m"))
  
  # Generate data frame
  output <- as.data.frame(rasterStack) %>% 
    tibble %>% 
    pivot_longer(cols = all_of(ShrubProportion90m):all_of(ShrubProportion720m),
                 names_to = "WindowRadius",
                 names_prefix = "ShrubProportion" %>% str_c(paste0(.x$x, "_")),
                 values_to = "ShrubProportionStart") %>% 
    mutate(StartTime = .x$x %>% as.factor,
           EndTime = .x$y %>%  as.factor,
           WindowRadius = WindowRadius %>% as.factor,
           ShrubPresenceStart = shrubPresenceX,
           NewShrubEnd = newShrubY,
           SoilType = case_when(soilType == 3 ~ "Loamy-Clayey",
                                soilType == 4 ~ "Gravelly and Calcic",
                                soilType == 5 ~ "Bedrock and Colluvium",
                                soilType == 6 ~ "Gypsic",
                                soilType == 7 ~ "Bottomland",
                                soilType == 8 ~ "Sandy or Deep Sand") %>% as.factor) %>% 
    select(StartTime, EndTime, WindowRadius, SoilType, ShrubPresenceStart, ShrubProportionStart, NewShrubEnd) %>% 
    filter(ShrubPresenceStart == 0)

})

# Save tabular summary to disk
write_csv(shrubTibble, file.path(tabularDataDir, "adjacency-analysis-raw-data.csv"))

## Logisitic regression ----
# Model likelihood of shrub presence in 2020 as a function of proportion 
# of adjacent cells that are shrub in 2015, stratified by soil type
modelShrub <- glm(newShrub2020 ~ ShrubFocalWindow2015, data = shrubTibble, family = binomial)

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

## Plot model prediction ----
transitionProbabilityPlot <- shrubTibble %>% 
  ggplot(aes(x = ShrubProportionStart, y = NewShrubEnd)) + 
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_grid(SoilType + EndTime ~ WindowRadius) +
  xlab("Proportion of adjacent shrub in previous year") +
  ylab("Probability of new shrub") + 
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Adjacency Analysis", str_c("grass-to-shrub-probability.png")),
  plot = transitionProbabilityPlot,
  device = "png", 
  dpi = 300
)
  
## Calculate pseudo R squared ----
modelFitTibble <- shrubTibble %>% 
  select(-StartTime, -ShrubPresenceStart) %>% 
  group_by(EndTime, WindowRadius, SoilType) %>% 
  group_nest() %>% 
  mutate(pseudoR = map_dbl(data,
    function(x) {
      # Fit the logistic regression model
      modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart, data = x, family = binomial)
      
      # Fit the null model
      modelNull <- glm(NewShrubEnd ~ 1, data = x, family = binomial)
      
      # Calculate McFadden's Pseudo R squared
      pseudoR <- 1-logLik(modelShrub)/logLik(modelNull)
      return(pseudoR)
    }
  )) %>% 
  select(-data) %>% 
  pivot_wider(names_from = WindowRadius,
              values_from = pseudoR)

# Save tabular output to disk
write_csv(modelFitTibble, file.path(tabularDataDir, "pseudo-r-adjacency-analysis.csv"))
  
# Summarize p-values ----
pValuesTibble <- shrubTibble %>% 
  select(-StartTime, -ShrubPresenceStart) %>% 
  group_by(EndTime, WindowRadius, SoilType) %>% 
  group_nest() %>% 
  mutate(pvalue = map_dbl(data,
                           function(x) {
                             # Fit the logistic regression model
                             modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart, data = x, family = binomial)
                             
                             # Extract the p-value for the ShrubProportionStart coefficient
                             pvalue <- coef(summary(modelShrub))[,4][[2]]
                             return(pvalue)
                           }
  )) %>% 
  select(-data) %>% 
  pivot_wider(names_from = WindowRadius,
              values_from = pvalue)

# Save tabular output to disk
write_csv(pValuesTibble, file.path(tabularDataDir, "p-values-adjacency-analysis.csv"))
