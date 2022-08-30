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

# State class rasters
# 1990
stateClass2016Raster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_2016.tif"))
# 1995
stateClass2017Raster <- rast(file.path(spatialDataDir, "RCMAP", "Processed", "rcmap_states_2017.tif"))

# Soil type raster
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# Parameters

# Coordinate reference system
targetCrs <- "epsg:3083"

# RAP Extent tested
# targetExtent <- c(896966.2851019165245816,
#                   915889.6527247427729890,
#                   7645343.2786588277667761,
#                   7662889.3997297743335366)

# RCMAP extents tested
# Extent 1
#targetExtent <- c(893990, 974990, 7555546, 7632946)

# Extent 2
#targetExtent <- c(781745, 799745, 7606590, 7630590)

# Full Extent
targetExtent <- c(658270, 1039720, 7497000, 7820100)

targetResolution <- 30

# Radius of circular moving window in units of the crs (meters)
windowRadii <- c(90, 180, 360, 720)

## Crop state class rasters to target extent ----
# Create a template raster with the target extent
templateRaster <- rast()
ext(templateRaster) <- targetExtent
crs(templateRaster) <- targetCrs
res(templateRaster) <- targetResolution

# Set the extent, crs, and res of input rasters
stateClass2016CropRaster <- stateClass2016Raster %>% 
                            project(y = templateRaster, 
                                    method = "near")
                           
stateClass2017CropRaster <- stateClass2017Raster %>% 
                            project(y = templateRaster, 
                                    method = "near")

soilTypeCropRaster <- soilTypeRaster %>% 
                      project(y = templateRaster, 
                              method = "near")
# About 5 minutes to run at full extent

# Reclassify stateClass2016Raster 
# 1 -> 0 : grassland 
# 2 -> 1 : shrub encroachment
# 3 -> 1 : shrubland 
shrubReclassMatrix <- matrix(data = c(1,2,3,0,1,1), nrow = 3, ncol = 2)
shrub2016Raster <- stateClass2016CropRaster %>% 
  classify(rcl = shrubReclassMatrix)

# Reclassify stateClass2017Raster 
shrub2017Raster <- stateClass2017CropRaster %>% 
  classify(rcl = shrubReclassMatrix)

# Reclassify soil type raster
# Combine soil classes 1 (sandy) and 2 (deep sand)
# Reassign to new soil class 8 (sandy and deep sand)
soilTypeCropRaster[soilTypeCropRaster == 1] <- 8
soilTypeCropRaster[soilTypeCropRaster == 2] <- 8

## Create the shrub focal window raster ----
startTime <- Sys.time()
shrubTibble <- map_dfr(windowRadii, 
~{

  # Define the window size and shape
  movingWindow <- focalWeight(shrub2016Raster, .x, type = "circle")
  
  # Generate focal window raster
  shrub2016FocalWindowRaster <- raster::focal(shrub2016Raster, movingWindow, pad = TRUE, padValue = 0.0)
  
  # Get cells that had no shrub in 1990 and have it in 1995
  newshrub2017Raster <- shrub2017Raster - shrub2016Raster
  newshrub2017Raster[newshrub2017Raster == -1] <- 0
  
  # Create a stack of relevant rasters
  names(soilTypeCropRaster) <- "SoilType"
  names(shrub2016Raster) <- "ShrubPresence2016"
  names(shrub2016FocalWindowRaster) <- "ShrubFocalWindow2016"
  names(newshrub2017Raster) <- "newShrub2017"
  
  rasterStack <- c(soilTypeCropRaster, shrub2016Raster, shrub2016FocalWindowRaster, newshrub2017Raster)
  
  # Generate data frame
  output <- as.data.frame(rasterStack) %>% 
    tibble %>% 
    filter(ShrubPresence2016 == 0) %>% 
    mutate(WindowRadius = .x %>% str_c("m"),
           SoilType = case_when(SoilType == 3 ~ "Loamy-Clayey",
                                SoilType == 4 ~ "Gravelly and Calcic",
                                SoilType == 5 ~ "Bedrock and Colluvium",
                                SoilType == 6 ~ "Gypsic",
                                SoilType == 7 ~ "Bottomland",
                                SoilType == 8 ~ "Sandy or Deep Sand") %>% as.factor)
})
# 23.66 minutes to run at full (cropped) extent
endTime <- Sys.time()
endTime - startTime
# Logisitic regression ----
# Model likelihood of shrub presence in 2017 as a function of proportion 
# of adjacent cells that are shrub in 2016, stratified by soil type

# Fit the logistic regression model
shrubModel <- glm(newShrub2017 ~ ShrubFocalWindow2016, data = shrubTibble, family = binomial)

## Get predicted values ----
transitionProbability <- tibble(
  TransitionType = "Shrub Establishment",
  ShrubFocalWindow2016 = c(seq(0, 1, 0.1), seq(0, 1, 0.1)),
  SoilType = c(as.factor(rep(6, times = 11)), as.factor(rep(7, times = 11)))) 

transitionProbability <- transitionProbability %>% 
  mutate(TransitionProbability = predict(shrubModel, transitionProbability, type = "response")) %>% 
  rename(ProportionShrubCover = ShrubFocalWindow2016)

# Write transition probabilities to disk
write_csv(transitionProbability, file.path(tabularDataDir, str_c("transition-multipliers-", windowDiameter, "m-window.csv")))

# Plot model prediction ----
transitionProbabilityPlot <- shrubTibble %>% 
  ggplot(aes(x = ShrubFocalWindow2016, y = newShrub2017)) + 
  stat_smooth(method = "glm", method.args = list(family=binomial), se = FALSE) +
  facet_grid(SoilType ~ WindowRadius, scales = "free_y") +
  # scale_y_continuous(limits = c(0,1),
  #                    breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0)) +
  xlab("Proportion of adjacent shrub in previous year") +
  ylab("Probability of new shrub") + 
  theme_bw() 
  # theme(axis.title = element_blank(),
  #       axis.text.x = element_blank())

ggsave(
  filename = file.path(plotDir, "Adjacency Analysis", "RCMAP", "grassland2016-to-shrubland2017-probability-full-extent-noSE.png"),
  plot = transitionProbabilityPlot,
  device = "png", 
  width = 10,
  height = 10,
  dpi = 300
)

## Calculate pseudo R squared ----
modelFitTibble <- shrubTibble %>% 
  select(-ShrubPresence2016) %>% 
  group_by(WindowRadius, SoilType) %>% 
  group_nest() %>% 
  mutate(pseudoR = map_dbl(data,
                           function(x) {
                             # Fit the logistic regression model
                             modelShrub <- glm(newShrub2017 ~ ShrubFocalWindow2016, data = x, family = binomial)
                             
                             # Fit the null model
                             modelNull <- glm(newShrub2017 ~ 1, data = x, family = binomial)
                             
                             # Calculate McFadden's Pseudo R squared
                             pseudoR <- 1-logLik(modelShrub)/logLik(modelNull)
                             return(pseudoR)
                           }
  )) %>% 
  select(-data) %>% 
  pivot_wider(names_from = WindowRadius,
              values_from = pseudoR)

# Save tabular output to disk
write_csv(modelFitTibble, file.path(tabularDataDir, "RCMAP", "pseudo-r-adjacency-analysis-extent2.csv"))

# Summarize p-values ----
pValuesTibble <- shrubTibble %>% 
  select(-ShrubPresence2016) %>% 
  group_by(WindowRadius, SoilType) %>% 
  group_nest() %>% 
  #filter(!(WindowRadius == "720m" & SoilType == "Gravelly and Calcic")) %>% 
  mutate(pvalue = map_dbl(data,
                          function(x) {
                            # Fit the logistic regression model
                            modelShrub <- glm(newShrub2017 ~ ShrubFocalWindow2016, data = x, family = binomial)
                            
                            # Extract the p-value for the ShrubProportionStart coefficient
                            pvalue <- coef(summary(modelShrub))[,4][[2]]
                            return(pvalue)
                          }
  )) %>% 
  select(-data) %>% 
  pivot_wider(names_from = WindowRadius,
              values_from = pvalue)

# Save tabular output to disk
write_csv(pValuesTibble, file.path(tabularDataDir, "RCMAP", "p-values-adjacency-analysis-extent2.csv"))
