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

# Extent 1
#targetExtent <- c(893990, 974990, 7555546, 7632946)

# Extent 2
# targetExtent <- c(781745, 799745, 7606590, 7630590)

# Full Test Extent
targetExtent <- c(658270, 1039720, 7497000, 7820100)

targetResolution <- 30

# Radius of circular moving window in units of the crs (meters)
windowRadii <- c(90, 180, 360, 720)

# Timestep pairs
timeStepPairs <- list(list(x = 2016, y = 2017),
                      list(x = 2017, y = 2018), 
                      list(x = 2018, y = 2019), 
                      list(x = 2019, y = 2020))

## Prep soil raster ----
# Create a template raster with the target extent
templateRaster <- rast()
ext(templateRaster) <- targetExtent
crs(templateRaster) <- targetCrs
res(templateRaster) <- targetResolution

# Crop and reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeReclassMatrix <- matrix(data = c(1,2,3,4,5,6,7,8,8,3,4,5,6,7), nrow = 7, ncol = 2)

soilTypeReclassRaster <- soilTypeRaster %>% 
  project(y = templateRaster, method = "near") %>% 
  classify(rcl = soilTypeReclassMatrix)

# Rename soil type raster
names(soilTypeReclassRaster) <- "soilType"

## Generate data frame ----
start_time <- Sys.time()
shrubTibble <- imap_dfr(timeStepPairs,
~{
  #.x <- timeStepPairs[[1]]
  
  # Crop the state class rasters to the template
  stateClassXRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", .x$x, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = templateRaster, method = "near")
  
  stateClassYRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", .x$y, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = templateRaster, method = "near")
  
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
    mutate(WindowRadius = factor(WindowRadius, levels = c("90m", "180m", "360m", "720m"))) %>% 
    select(StartTime, EndTime, WindowRadius, SoilType, ShrubPresenceStart, ShrubProportionStart, NewShrubEnd) %>% 
    filter(ShrubPresenceStart == 0)

})
end_time <- Sys.time()
end_time - start_time
# Save tabular summary to disk
write_csv(shrubTibble, file.path(tabularDataDir, "adjacency-analysis-raw-data-rcmap.csv"))

## Logisitic regression (2018 shrub probabilities) ----
# Model likelihood of shrub presence in 2018 as a function of proportion 
# of adjacent cells that are shrub in 2017, stratified by soil type
shrubTibble2018 <- shrubTibble %>% 
  filter(EndTime == 2018) %>% 
  filter(WindowRadius == "90m")

modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType, data = shrubTibble2018, family = binomial)

# Get predicted values
transitionProbability <- tibble(
  TransitionType = "Shrub establishment [Type]",
  ShrubProportionStart = rep(seq(0, 1, 0.1), times = 6),
  SoilType = c(as.factor(rep(c("Gypsic", "Bedrock and Colluvium", "Sandy and Deep Sand",
                               "Gravelly and Calcic", "Bottomland", "Loamy-Clayey"), each = 11)))) 

transitionProbability <- transitionProbability %>% 
  mutate(TransitionProbability = predict(modelShrub, transitionProbability, type = "response")) %>% 
  rename("Primary Stratum" = SoilType,
         "Transition Type/Group" = TransitionType,
         "Neighbor Value" = ShrubProportionStart,
         Multiplier = TransitionProbability) %>% 
  select(`Primary Stratum`, `Transition Type/Group`, `Neighbor Value`, Multiplier)

# Write transition probabilities to disk
write_csv(transitionProbability, file.path(tabularDataDir, "transition-adjacency-multipliers-2018-90m.csv"))

## Plot model prediction ----
# zzz: adjust y-axes to vary for each plot
transitionProbabilityPlot <- shrubTibble %>% 
  #filter(EndTime != 2020) %>% 
  #filter(WindowRadius == "90m") %>% 
  ggplot(aes(x = ShrubProportionStart, y = NewShrubEnd)) + 
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_grid(SoilType ~ EndTime, scales = "free_y") +
  xlab("Proportion of adjacent shrub in previous year") +
  ylab("Probability of new shrub") + 
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Adjacency Analysis", "RCMAP", "grass-to-shrub-probability-full-extent.png"),
  plot = transitionProbabilityPlot,
  device = "png", 
  width = 7,
  height = 10,
  dpi = 300)
  
## Calculate pseudo R squared ----
modelFitTibble <- shrubTibble %>% 
  select(-StartTime, -ShrubPresenceStart) %>% 
  group_by(EndTime, WindowRadius) %>% 
  group_nest() %>% 
  mutate(pseudoR = map_dbl(data,
    function(x) {
      # Fit the logistic regression model
      modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType, data = x, family = binomial)
      
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
write_csv(modelFitTibble, file.path(tabularDataDir, "pseudo-r-adjacency-analysis-rcmap-full-extent.csv"))
  
# Summarize p-values ----
pValuesTibble <- shrubTibble %>% 
  select(-StartTime, -ShrubPresenceStart) %>% 
  group_by(EndTime, WindowRadius) %>% 
  group_nest() %>% 
  mutate(pvalue = map(data,
                      function(x) {
                        # Fit the logistic regression model
                        modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType, data = x, family = binomial)
                        
                        # Extract the p-value for the ShrubProportionStart coefficient
                        pValue <- coef(summary(modelShrub))[,4]
                        return(pValue)
                      }
  )) %>% 
  select(-data) %>% 
  pivot_wider(names_from = WindowRadius,
              values_from = pvalue) %>% 
  unnest(cols = c(`90m`, `180m`, `360m`, `720m`)) %>% 
  mutate(Coefficient = rep(c("Intercept", "Bedrock and Colluvium", "Bottomland", "Gravelly and Calcic", 
                         "Gypsic", "Loamy-Clayey", "Sandy or Deep Sand"), times = 4)) %>% 
  select(EndTime, Coefficient, `90m`, `180m`, `360m`, `720m`)

# Save tabular output to disk
write_csv(pValuesTibble, file.path(tabularDataDir, "p-values-adjacency-analysis-rcmap-full-extent.csv"))
