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
tabularModelInputsDir <- file.path("Model Inputs", "Tabular")
plotDir <- file.path("Plots")

# Parameters
# Radius of circular moving window in units of the crs (meters)
windowRadius <- 90

# Timestep pairs
timeStepPairs <- list(list(x = 1985,y = 1986),
                      list(x = 1986,y = 1987),
                      list(x = 1987,y = 1988),
                      list(x = 1988,y = 1989),
                      list(x = 1989,y = 1990),
                      list(x = 1990,y = 1991),
                      list(x = 1991,y = 1992),
                      list(x = 1992,y = 1993),
                      list(x = 1993,y = 1994),
                      list(x = 1994,y = 1995),
                      list(x = 1995,y = 1996),
                      list(x = 1996,y = 1997),
                      list(x = 1997,y = 1998),
                      list(x = 1998,y = 1999),
                      list(x = 1999,y = 2000),
                      list(x = 2000,y = 2001),
                      list(x = 2001,y = 2002),
                      list(x = 2002,y = 2003),
                      list(x = 2003,y = 2004),
                      list(x = 2004,y = 2005),
                      list(x = 2005,y = 2006),
                      list(x = 2006,y = 2007),
                      list(x = 2007,y = 2008),
                      list(x = 2008,y = 2009),
                      list(x = 2009,y = 2010),
                      list(x = 2010,y = 2011),
                      list(x = 2011,y = 2013),
                      list(x = 2013,y = 2014),
                      list(x = 2014,y = 2015),
                      list(x = 2015,y = 2016),
                      list(x = 2016,y = 2017),
                      list(x = 2017,y = 2018),
                      list(x = 2018,y = 2019),
                      list(x = 2019,y = 2020))

# timeStepPairs <- list(startTime = seq(1985,2019), endTime = seq(1986,2020))
# # zzz: remove pairs 2011, 2012 and 2012, 2013 for now
# timeStepPairs$startTime <- timeStepPairs$startTime[-c(27, 28)]
# timeStepPairs$endTime <- timeStepPairs$endTime[-c(27, 28)]

# Load spatial data
# Study area mask
studyAreaMaskRaster <- rast(file.path(spatialDataDir, "study-area-mask-small.tif"))

# Soil type
soilTypeRaster <- rast(file.path(spatialDataDir, "soil-type.tif"))

# Load tabular data
# PDSI drought index
droughtYears <- read_csv(file.path(tabularDataDir, "PDSI_by_year.csv"))

## Prep drought data ----
droughtYearTypes <- droughtYears %>% 
  mutate(yearType = case_when(avg <= -2 ~ "Dry",
                              avg > -2 & avg < 2 ~ "Normal",
                              avg >= 2 ~ "Wet"),
         yearTypeId = case_when(yearType == "Dry" ~ 1,
                                yearType == "Normal" ~ 2,
                                yearType == "Wet" ~ 3)) %>%
  select(year, yearType, yearTypeId)

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
start_time <- Sys.time()
shrubTibble <- imap_dfr(timeStepPairs,
~{
  #.x <- 1
  # start_time <- Sys.time()
  ## Crop and mask the state class rasters to the template ----
  stateClassXRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", .x$x, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = studyAreaMaskRaster, method = "near") %>%
                       mask(mask = studyAreaMaskRaster)
  
  stateClassYRaster <- rast(list.files(file.path(spatialDataDir, "RCMAP", "Processed"),
                                       pattern = str_c("states_", .x$y, ".tif$"),
                                       full.names = TRUE)) %>% 
                       project(y = studyAreaMaskRaster, method = "near")%>%
                       mask(mask = studyAreaMaskRaster) 
  
  ## Reclassify state class rasters to make binary shrub presence layers ----
  # 1 (grassland) -> 0
  # 2 (low shrub) -> 1
  # 3 (shrubland) -> 1
  shrubReclassMatrix <- matrix(data = c(1,2,3,0,1,1), nrow = 3, ncol = 2)

  # Reclassify stateClassXRaster
  shrubXRaster <- stateClassXRaster %>%
    classify(rcl = shrubReclassMatrix)

  # Reclassify stateClassYRaster
  shrubYRaster <- stateClassYRaster %>%
    classify(rcl = shrubReclassMatrix)

  # Get cells that had no shrub in shrubXRaster and have it in shrubYRaster
  newShrubYRaster <- shrubYRaster - shrubXRaster
  newShrubYRaster[newShrubYRaster == -1] <- 0

  # Rename rasters
  names(shrubXRaster) <- "shrubX"
  names(newShrubYRaster) <- "newShrubY"
  
  # ## Reclassify state class rasters to make binary shrub loss layers ----
  # # 1 (grassland) -> 1  
  # # 2 (low shrub) -> 0
  # # 3 (shrubland) -> 0 
  # noShrubReclassMatrix <- matrix(data = c(1,2,3,1,0,0), nrow = 3, ncol = 2)
  # 
  # # Reclassify stateClassXRaster 
  # noShrubXRaster <- stateClassXRaster %>% 
  #   classify(rcl = noShrubReclassMatrix)
  # 
  # # Reclassify stateClassYRaster 
  # noShrubYRaster <- stateClassYRaster %>% 
  #   classify(rcl = noShrubReclassMatrix)
  # 
  # # Get cells that experienced shrub loss (shrubland/low shrub -> grassland)
  # newNoShrubYRaster <- noShrubYRaster - noShrubXRaster
  # newNoShrubYRaster[newNoShrubYRaster == -1] <- 0
  # 
  # # Rename rasters
  # names(noShrubXRaster) <- "noShrubX" 
  # names(newNoShrubYRaster) <- "newNoShrubY" 
  
  # # Create a multiband raster of relevant layers
  # rasterStack <- c(soilTypeReclassRaster, shrubXRaster, newShrubYRaster, noShrubXRaster, newNoShrubYRaster)
  # To run for shrub loss only
  rasterStack <- c(soilTypeReclassRaster, shrubXRaster, newShrubYRaster)
  
  # Memory management
  # rm(studyAreaMaskRaster, soilTypeRaster, soilTypeReclassRaster, stateClassXRaster, stateClassYRaster, 
  #    soilTypeReclassMatrix, shrubReclassMatrix, noShrubReclassMatrix,
  #    shrubYRaster, noShrubYRaster, newShrubYRaster, newNoShrubYRaster)
  rm(stateClassXRaster, stateClassYRaster, 
    shrubReclassMatrix,
     shrubYRaster, newShrubYRaster)
  
    ## Create focal window rasters ----
  # Define the window size and shape
  shrubMovingWindow <- focalWeight(shrubXRaster, windowRadius, type = "circle")
  #lowShrubMovingWindow <- focalWeight(noShrubXRaster, windowRadius, type = "circle")
  
  # Generate focal window raster
  shrubXFocalWindowRaster <- raster::focal(shrubXRaster, shrubMovingWindow, pad = TRUE, padValue = 0.0)
  #noShrubXFocalWindowRaster <- raster::focal(noShrubXRaster, lowShrubMovingWindow, pad = TRUE, padValue = 0.0)
  
  # Rename focal window raster
  # zzz: root of error downstream?
  #names(shrubXFocalWindowRaster) <- "ShrubProportion" %>% str_c(paste0(.x$x, "_", windowRadius, "m"))
  #names(noShrubXFocalWindowRaster) <- "NoShrubProportion" %>% str_c(paste0(.x$x, "_", windowRadius, "m"))
  
  # Shrub proportion raster to stack
  add(rasterStack) <- shrubXFocalWindowRaster
  #add(rasterStack) <- noShrubXFocalWindowRaster
  
  # Memory management
  # rm(shrubXRaster, noShrubXRaster, shrubMovingWindow, lowShrubMovingWindow, 
  #    shrubXFocalWindowRaster, noShrubXFocalWindowRaster)
  rm(shrubXRaster, shrubMovingWindow, 
     shrubXFocalWindowRaster)
  
  ## Generate data frame ----
  output <- as.data.frame(rasterStack) %>% 
    tibble %>% 
    mutate(StartTime = .x$x %>% as.numeric,
           EndTime = .x$y %>%  as.numeric,
           WindowRadius = windowRadius %>% as.factor,
           ShrubProportionStart = focal_sum, 
           ShrubPresenceStart = shrubX,
           NewShrubEnd = newShrubY,
           #NoShrubProportionStart = "NoShrubProportion" %>% str_c(paste0(.x$x, "_", windowRadius, "m")),
           #NoShrubProportionStart = focal_sum,
           #NoShrubPresenceStart = noShrubX,
           #NewNoShrubEnd = newNoShrubY,
           SoilType = case_when(soilType == 3 ~ "Loamy-Clayey",
                                soilType == 4 ~ "Gravelly and Calcic",
                                soilType == 5 ~ "Bedrock and Colluvium",
                                soilType == 6 ~ "Gypsic",
                                soilType == 7 ~ "Bottomland",
                                soilType == 8 ~ "Sandy and Deep Sand") %>% as.factor) %>% 
    # Match the type of drought year base on the EndTime of the adjacency analysis
    # (i.e. is the probability of transition in present year influenced by drought conditions of present year)
    left_join(droughtYearTypes, by = c("EndTime" = "year")) %>% 
    select(StartTime, EndTime, WindowRadius, SoilType, yearType, yearTypeId,
           ShrubPresenceStart, ShrubProportionStart, NewShrubEnd) %>% 
    filter(ShrubPresenceStart == 0)
  
  # shrubEstablishmentAdjacency <- output %>% 
  #   select(StartTime, EndTime, WindowRadius, SoilType, yearType, yearTypeId,
  #          ShrubPresenceStart, ShrubProportionStart, NewShrubEnd) %>% 
  #   filter(ShrubPresenceStart == 0)
  
  # shrubLossAdjacency <- output %>% 
  #   select(StartTime, EndTime, WindowRadius, SoilType, yearType, yearTypeId,
  #          NoShrubPresenceStart, NoShrubProportionStart, NewNoShrubEnd) %>% 
  #   filter(NoShrubPresenceStart == 0)
})
rm(studyAreaMaskRaster, soilTypeRaster, soilTypeReclassRaster)
end_time <- Sys.time()
end_time - start_time
  
# Save tabular summary to disk
write_csv(shrubTibble, file.path(tabularDataDir, "establishment-adjacency-analysis-raw-data-rcmap-27-08-small-extent.csv"))
# write_csv(shrubEstablishmentAdjacency, file.path(tabularDataDir, "shrub-establishment-adjacency-analysis-raw-data-rcmap.csv"))
# write_csv(shrubLossAdjacency, file.path(tabularDataDir, "shrub-loss-adjacency-analysis-raw-data-rcmap.csv"))

## Logisitic regression ----
## Shrub Encroachment ----
# Model likelihood of shrub presence in 2018 as a function of proportion 
# of adjacent cells that are shrub in 2017, stratified by soil type
shrubAdjacency <- read_csv(file.path(tabularDataDir, "establishment-adjacency-analysis-raw-data-rcmap-27-08-small-extent.csv"))

# Test effect of additional variables
m1Var <- glm(NewShrubEnd ~ ShrubProportionStart, data = shrubAdjacency, family = binomial)
m2Var <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType, data = shrubAdjacency, family = binomial)
m3Var <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType + yearType, data = shrubAdjacency, family = binomial)
m3IntVar <- glm(NewShrubEnd ~ ShrubProportionStart*yearType + SoilType, data = shrubAdjacency, family = binomial)

mNull <- glm(NewShrubEnd ~ 1, data = shrubAdjacency, family = binomial)
pseudoR <- 1-logLik(m3IntVar)/logLik(mNull)


modelShrub <- glm(NewShrubEnd ~ ShrubProportionStart + SoilType, data = shrubEstablishmentAdjacency, family = binomial)

# Get predicted values
transitionProbabilityShrub <- tibble(
  TransitionType = "Shrub establishment [Type]",
  ShrubProportionStart = rep(seq(0, 1, 0.1), times = 6), # x values for each unique soil type
  SoilType = c(as.factor(rep(c("Gypsic", "Bedrock and Colluvium", "Sandy and Deep Sand",
                               "Gravelly and Calcic", "Bottomland", "Loamy-Clayey"), each = 11)))) 

transitionProbabilityShrub <- transitionProbabilityShrub %>% 
  mutate(TransitionProbability = predict(modelShrub, transitionProbabilityShrub, type = "response")) %>% 
  rename("Primary Stratum" = SoilType,
         "Transition Type/Group" = TransitionType,
         "Neighbor Value" = ShrubProportionStart,
         Multiplier = TransitionProbability) %>% 
  select(`Primary Stratum`, `Transition Type/Group`, `Neighbor Value`, Multiplier)

# Write transition probabilities to disk
write_csv(transitionProbabilityShrub, file.path(tabularModelInputsDir, "Transition Adjacency Multipliers - Shrub Establishment.csv"))

## Shrub Loss ----
# Model likelihood of grassland presence in 2018 as a function of proportion 
# of adjacent cells that are grassland in 2017, stratified by soil type
shrubLossAdjacency <- read_csv(file.path(tabularDataDir, "loss-adjacency-analysis-raw-data-rcmap-27-08-small-extent.csv"))

m1Var <- glm(NewNoShrubEnd ~ NoShrubProportionStart, data = shrubLossAdjacency, family = binomial)
m2Var <- glm(NewNoShrubEnd ~ NoShrubProportionStart + SoilType, data = shrubLossAdjacency, family = binomial)
m3CatVar <- glm(NewNoShrubEnd ~ NoShrubProportionStart + SoilType + yearType, data = shrubLossAdjacency, family = binomial)
m3ConVar <- glm(NewNoShrubEnd ~ NoShrubProportionStart + SoilType + yearTypeId, data = shrubLossAdjacency, family = binomial)

m3CatIntVar <- glm(NewNoShrubEnd ~ NoShrubProportionStart*yearType + SoilType, data = shrubLossAdjacency, family = binomial)

mNull <- glm(NewNoShrubEnd ~ 1, data = shrubLossAdjacency, family = binomial)
pseudoR <- 1-logLik(m3ConVar)/logLik(mNull)


# Get predicted values
transitionProbabilityNoShrub <- tibble(
  TransitionType = "Shrub loss [Type]",
  NoShrubProportionStart = rep(seq(0, 1, 0.1), times = 18), # x values for each unique soil type
  SoilType = c(as.factor(rep(c("Gypsic", "Bedrock and Colluvium", "Sandy and Deep Sand",
                               "Gravelly and Calcic", "Bottomland", "Loamy-Clayey"), each = 33))),
  yearType = as.factor(rep(c("Dry", "Normal", "Wet"), each = 66))) 

transitionProbabilityNoShrub <- transitionProbabilityNoShrub %>% 
  mutate(TransitionProbability = predict(m3Var, transitionProbabilityNoShrub, type = "response")) %>% 
  rename("Primary Stratum" = SoilType,
         "Transition Type/Group" = TransitionType,
         "Neighbor Value" = NoShrubProportionStart,
         Multiplier = TransitionProbability) %>% 
  select(`Primary Stratum`, `Transition Type/Group`, `Neighbor Value`, Multiplier)

# Write transition probabilities to disk
write_csv(transitionProbabilityNoShrub, file.path(tabularModelInputsDir, "Transition Adjacency Multipliers - Shrub Loss.csv"))

# Memory management
rm(modelShrub, modelNoShrub, shrubEstablishmentAdjacency, shrubLossAdjacency)

## Plot model prediction ----
shrubEstablishmentAdjacency <- read_csv(file.path(tabularDataDir, "establishment-adjacency-analysis-raw-data-rcmap-27-08-small-extent.csv"))
shrubLossAdjacency <- read_csv(file.path(tabularDataDir, "loss-adjacency-analysis-raw-data-rcmap-27-08-small-extent.csv"))

# Shrub Establishment
shrubEstablishmentPlot <- shrubEstablishmentAdjacency %>% 
  ggplot(aes(x = ShrubProportionStart, y = NewShrubEnd)) + 
  stat_smooth(method = "glm", method.args = list(family=binomial), se = F) +
  facet_grid(yearType ~ SoilType) +
  xlab("Proportion of adjacent Shrubland:All and Low Shrub:All in previous year") +
  ylab("Probability of shrub establishment") + 
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Adjacency Analysis", "RCMAP", "shrub-establishment-probability-soil-drought-small-extent-no-se.png"),
  plot = shrubEstablishmentPlot,
  device = "png", 
  width = 10,
  height = 7,
  dpi = 300)

# Shrub Loss
shrubLossPlot <- shrubLossAdjacency %>% 
  ggplot(aes(x = NoShrubProportionStart, y = NewNoShrubEnd)) + 
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) +
  facet_grid(yearType ~ SoilType) +
  xlab("Proportion of adjacent No Shrub:All in previous year") +
  ylab("Probability of shrub loss") + 
  theme_bw()

ggsave(
  filename = file.path(plotDir, "Adjacency Analysis", "RCMAP", "shrub-loss-probability-soil-drought-small-extent.png"),
  plot = shrubLossPlot,
  device = "png", 
  width = 10,
  height = 7,
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
