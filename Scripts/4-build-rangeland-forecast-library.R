## rangelandForecast
## Sarah Chisholm, ApexRMS
##
## Build rangeland forecast st-sim library

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(rsyncrosim)
library(tidyverse)
library(terra)

# Define directories
spatialDataDir <- file.path(getwd(), "Data", "Spatial")
tabularDataDir <- file.path(getwd(), "Data", "Tabular")
tabularModelInputsDir <- file.path(getwd(), "Model-Inputs", "Tabular")
spatialModelInputsDir <- file.path(getwd(), "Model-Inputs", "Spatial")
libraryDir <- file.path(getwd(), "Libraries")

# Load tabular data
# Drought year types
droughtYearTypes <- read_csv(file.path(tabularDataDir, "PDSI_by_year.csv")) %>% 
  filter(year >= 1985 & year <= 2020) %>% 
  mutate(yearType = case_when(avg <= -2 ~ 1,
                              avg > -2 & avg < 2 ~ 2,
                              avg >= 2 ~ 3)) %>% 
  select(year, yearType)

# Parameters
# Forecast Scenario minimum timestep
minimumTimestep <- 2020

# Forecast Scenario maximum timestep
maximumTimestep <- 2040

# Monte carlo minimum iteration
minimumIteration <- 1

# Monte carlo maximum iteration
maximumIteration <- 10

# List transition types
transitionTypes <- c("Shrub in-filling", 
                     "Shrub establishment", 
                     "Shrub loss",
                     "Shrub decline")

# List transition adjacency multiplier files
transitionAdjacencyMultiplierFiles <- list.files(
  path = tabularModelInputsDir,
  pattern = "Transition Adjacency Multiplier", 
  full.names = FALSE)

# List transition size distribution files
transitionSizeDistributionFiles <- list.files(
  path = tabularModelInputsDir,
  pattern = "Transition Size Distribution", 
  full.names = FALSE)

## Create new Library ----
# Create a new SyncroSim library file
# NB: Running this chunk will overwrite any pre-existing library with the same file path
myLibrary <- ssimLibrary(name = file.path(libraryDir, "Rangeland Forecast"),
                         package = "stsim",
                         overwrite = TRUE)

# Open current library
# NB: Running this chunk will connect this R session to an existsing library with the same file path
# myLibrary <- ssimLibrary(name = file.path(libraryDir, "Rangeland Forecast"))

# Open the default project
myProject <- rsyncrosim::project(ssimObject = myLibrary, 
                                 project = "Definitions")

## Project-scope Datasheets ----
### Strata ----
## Primary Stratum
primaryStratumValues <- data.frame(
                          Name = c("Loamy-Clayey", "Gravelly and Calcic", 
                                   "Bedrock and Colluvium", "Gypsic", 
                                   "Bottomland", "Sandy and Deep Sand"),
                          ID = c(3, 4, 5, 6, 7, 8),
                          Color = c("255,128,128,128", "255,231,179,101",
                                    "255,64,128,128", "255,255,128,128",
                                    "255,128,128,64", "255,255,255,0"))

primaryStratum <- datasheet(ssimObject = myProject, 
                            name = "stsim_Stratum",
                            empty = TRUE,
                            optional = TRUE) %>% 
                  addRow(value = primaryStratumValues) 

# Save datasheet to library
saveDatasheet(ssimObject = myProject,
              data = primaryStratum,
              name = "stsim_Stratum",)

### States ----
# State Label X
stateLabelX <- datasheet(ssimObject = myProject,
                         name = "stsim_StateLabelX") %>% 
               addRow(value = "No Shrub") %>% 
               addRow(value = "Low Shrub") %>% 
               addRow(value = "Shrubland")

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = stateLabelX,
              name = "stsim_StateLabelX")

# State Label Y
stateLabelY <- datasheet(ssimObject = myProject,
                         name = "stsim_StateLabelY") %>% 
               addRow(value = "All") 

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = stateLabelY,
              name = "stsim_StateLabelY")

# State Class
stateClassValues <- data.frame(
                      Name = c("No Shrub:All", "Low Shrub:All", "Shrubland:All"),
                      StateLabelXID = c("No Shrub", "Low Shrub", "Shrubland"),
                      StateLabelYID = c("All", "All", "All"),
                      ID = c(1, 2, 3),
                      Color = c("255,0,128,0", "255,255,255,0", "255,128,64,64"))

stateClass <- datasheet(ssimObject = myProject,
                        name = "stsim_StateClass",
                        optional = TRUE,
                        empty = TRUE) %>% 
              addRow(value = stateClassValues) 

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = stateClass,
              name = "stsim_StateClass")

### Transitions ----
# Transition Type
transitionTypeValues <- data.frame(
                          Name = transitionTypes,
                          ID = c(1, 2, 3, 5),
                          Color = c("255,255,255,0", 
                                    "255,255,165,0",
                                    "255,0,0,0",
                                    "255,128,0,255"))

transitionType <- datasheet(ssimObject = myProject,
                            name = "stsim_TransitionType",
                            optional = TRUE, 
                            empty = TRUE) %>% 
                  addRow(value = transitionTypeValues)

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = transitionType,
              name = "stsim_TransitionType")

### Advanced ----
#### State Attribute Type ----
stateAttributeType <- datasheet(
  ssimObject = myProject,
  name = "stsim_StateAttributeType") %>% 
  addRow(value = "Shrub") %>% 
  addRow(value = "No Shrub") %>% 
  addRow(value = "Low Shrub or No Shrub") %>% 
  addRow(value = "Low Shrub or Shrub")

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = stateAttributeType,
              name = "stsim_StateAttributeType", append = FALSE)

#### Distributions ----
# Create distribution names and descriptions
distributionNames <- c()
distributionDescriptions <- c()
for(i in transitionTypes){
  for(j in seq(0, 1, 0.1)) {
    distributionName <- str_c(i, " neighbor value ", j)
    distributionNames <- c(distributionNames, distributionName)
    
    distributionDescription <- str_c(
      i, " adjacency transition multipliers across soil and drought year types for a neighbor value of ", j)
    distributionDescriptions <- c(distributionDescriptions, distributionDescription)
  }
}

# Add to datasheet
distributions <- datasheet(
  ssimObject = myProject,
  name = "corestime_DistributionType",
  optional = TRUE, 
  empty = TRUE) %>% 
  addRow(value = data.frame(Name = distributionNames, 
                            Description = distributionDescriptions))

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = distributions,
              name = "corestime_DistributionType")

#### External Variables ----
externalVariables <- datasheet(
  ssimObject = myProject,
  name = "corestime_ExternalVariableType",
  optional = TRUE) %>% 
  addRow(value = data.frame(
    Name = "Year Type", 
    Description = "Drought year type, either dry (1), normal (2), or wet (3)"))

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = externalVariables,
              name = "corestime_ExternalVariableType")

### Terminology ----
terminology <- datasheet(ssimObject = myProject,
                         name = "stsim_Terminology",
                         optional = TRUE) %>% 
               mutate(AmountUnits = str_replace(AmountUnits, 
                                                pattern = "Acres", 
                                                replacement = "Hectares"),
                      PrimaryStratumLabel = "Soil Type")

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = terminology,
              name = "stsim_Terminology")

# Memory management
rm(primaryStratumValues, primaryStratum, stateLabelX, stateLabelY, 
   stateClassValues, stateClass, transitionTypeValues, transitionType,
   stateAttributeType, distributionNames, distributionDescriptions, 
   externalVariables, terminology)

## Scenario-scope Datasheets ----
## Create Sub-Scenarios (dependencies) for each scenario-scoped datasheet
### Run Control ----
## Run Control - Baseline
scenarioName <- str_c("Run Control - 1985 to 2020, ", maximumIteration, " Iteration")

runControlBaselineSubScenario <- scenario(
  ssimObject = myProject,
  scenario = scenarioName)

runControlBaselineDataSheet <- datasheet(
  ssimObject = runControlBaselineSubScenario,
  name = "stsim_RunControl") %>% 
  addRow(value = list(MinimumIteration = minimumIteration, 
                      MaximumIteration = maximumIteration, 
                      MinimumTimestep = 1985,
                      MaximumTimestep = 2020, 
                      IsSpatial = TRUE))

# Save datasheet to library
saveDatasheet(ssimObject = runControlBaselineSubScenario, 
              data = runControlBaselineDataSheet,
              name = "stsim_RunControl")

## Run Control - Forecast
scenarioName <- str_c("Run Control - ", minimumTimestep, " to ", maximumTimestep,
                      ", ", maximumIteration, " Iteration")

runControlForecastSubScenario <- scenario(
  ssimObject = myProject,
  scenario = scenarioName)

runControlForecastDataSheet <- datasheet(
  ssimObject = runControlForecastSubScenario,
  name = "stsim_RunControl") %>% 
  addRow(value = list(MinimumIteration = minimumIteration, 
                      MaximumIteration = maximumIteration, 
                      MinimumTimestep = minimumTimestep,
                      MaximumTimestep = maximumTimestep, 
                      IsSpatial = TRUE))

# Save datasheet to library
saveDatasheet(ssimObject = runControlForecastSubScenario, 
              data = runControlForecastDataSheet,
              name = "stsim_RunControl")

## Run Control - Test
runControlTestSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Run Control - 1 timestep, 1 Iteration")

runControlTestDataSheet <- datasheet(
  ssimObject = runControlTestSubScenario,
  name = "stsim_RunControl") %>% 
  addRow(value = list(MinimumIteration = 1, 
                      MaximumIteration = 1, 
                      MinimumTimestep = 1985,
                      MaximumTimestep = 1986, 
                      IsSpatial = TRUE))

# Save datasheet to library
saveDatasheet(ssimObject = runControlTestSubScenario, 
              data = runControlTestDataSheet,
              name = "stsim_RunControl")

# Memory management
rm(runControlBaselineSubScenario, runControlBaselineDataSheet, scenarioName, 
   runControlForecastSubScenario, runControlForecastDataSheet, 
   runControlTestSubScenario, runControlTestDataSheet)

### Transition Pathways ----
# Define deterministic transitions
deterministicTransitionPathwayValues <- data.frame(
  StateClassIDSource = c("Low Shrub:All", "No Shrub:All","Shrubland:All"),
  Location = c("B1", "A1", "C1"))

# Define probabilistic transitions
probabilisticTransitionPathwayValues <- data.frame(
  StateClassIDSource = c("Low Shrub:All",
                         "Low Shrub:All",
                         "No Shrub:All",
                         "Shrubland:All"),
  StateClassIDDest = c("No Shrub:All",
                       "Shrubland:All",
                       "Low Shrub:All",
                       "Low Shrub:All"),
  TransitionTypeID = c("Shrub loss",
                       "Shrub in-filling",
                       "Shrub establishment",
                       "Shrub decline"),
  Probability = c(1,1,1,1))

transitionPathwaySubScenario <- scenario(ssimObject = myProject,
                                         scenario = "Transition Pathways")

# Create deterministic transitions datasheet
deterministicTransitionPathwaysDataSheet <- datasheet(
  ssimObject = transitionPathwaySubScenario,
  name = "stsim_DeterministicTransition",
  optional = TRUE) %>% 
addRow(value = deterministicTransitionPathwayValues)

# Create probabilistic transitions datasheet
probabilisticTransitionPathwaysDataSheet <- datasheet(
  ssimObject = transitionPathwaySubScenario,
  name = "stsim_Transition",
  optional = TRUE) %>% 
addRow(value = probabilisticTransitionPathwayValues)

# Save datasheet to library
saveDatasheet(ssimObject = transitionPathwaySubScenario, 
              data = deterministicTransitionPathwaysDataSheet,
              name = "stsim_DeterministicTransition")

saveDatasheet(ssimObject = transitionPathwaySubScenario, 
              data = probabilisticTransitionPathwaysDataSheet,
              name = "stsim_Transition")

# Memory management
rm(deterministicTransitionPathwayValues, probabilisticTransitionPathwayValues, 
   transitionPathwaySubScenario, deterministicTransitionPathwaysDataSheet,
   probabilisticTransitionPathwaysDataSheet)

### Initial Conditions ----
## Initial Conditions - MLRA 42 / New Mexico - 1985
# Create a list of the input tif files
initialConditionsSpatialBaselineValues <- list(
  StratumFileName = file.path(spatialModelInputsDir, "soil-type.tif"), 
  StateClassFileName = file.path(spatialModelInputsDir, "state-class-1985.tif")) 

initialConditionsBaselineSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - MLRA42/New Mexico - Baseline") 

initialConditionsSpatialBaselineDataSheet <- datasheet(
  ssimObject = initialConditionsBaselineSubScenario,
  name = "stsim_InitialConditionsSpatial") %>% 
  addRow(value = initialConditionsSpatialBaselineValues)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsBaselineSubScenario, 
              data = initialConditionsSpatialBaselineDataSheet,
              name = "stsim_InitialConditionsSpatial")

## Initial Conditions - MLRA 42 / New Mexico - 2020
# Create a list of the input tif files
initialConditionsSpatialForecastValues <- list(
  StratumFileName = file.path(spatialModelInputsDir, "soil-type.tif"), 
  StateClassFileName = file.path(spatialModelInputsDir, "state-class-2020.tif")) 

initialConditionsForecastSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - MLRA42/New Mexico - Forecast") 

initialConditionsSpatialForecastDataSheet <- datasheet(
  ssimObject = initialConditionsForecastSubScenario,
  name = "stsim_InitialConditionsSpatial") %>% 
  addRow(value = initialConditionsSpatialForecastValues)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsForecastSubScenario, 
              data = initialConditionsSpatialForecastDataSheet,
              name = "stsim_InitialConditionsSpatial")

## Initial Conditions - MLRA 42 / New Mexico - 1985 - Test extent
# Create a list of the input tif files
initialConditionsSpatialTestValues <- list(
  StratumFileName = file.path(spatialModelInputsDir, "soil-type-test-extent.tif"), 
  StateClassFileName = file.path(spatialModelInputsDir, "state-class-test-extent-1985.tif")) 

initialConditionsTestSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - Test Extent - Baseline") 

initialConditionsSpatialTestDataSheet <- datasheet(
  ssimObject = initialConditionsTestSubScenario,
  name = "stsim_InitialConditionsSpatial") %>% 
  addRow(value = initialConditionsSpatialTestValues)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsTestSubScenario, 
              data = initialConditionsSpatialTestDataSheet,
              name = "stsim_InitialConditionsSpatial")

## Initial Conditions - MLRA 42 / New Mexico - 2020 - Test extent
# Create a list of the input tif files
initialConditionsSpatialTestValues <- list(
  StratumFileName = file.path(spatialModelInputsDir, "soil-type-test-extent.tif"), 
  StateClassFileName = file.path(spatialModelInputsDir, "state-class-test-extent-2020.tif")) 

initialConditionsTestSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - Test Extent - Forecast") 

initialConditionsSpatialTestDataSheet <- datasheet(
  ssimObject = initialConditionsTestSubScenario,
  name = "stsim_InitialConditionsSpatial") %>% 
  addRow(value = initialConditionsSpatialTestValues)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsTestSubScenario, 
              data = initialConditionsSpatialTestDataSheet,
              name = "stsim_InitialConditionsSpatial")

# Memory management
rm(initialConditionsSpatialForecastValues, initialConditionsSpatialBaselineValues,
   initialConditionsSpatialTestValues, initialConditionsForecastSubScenario, 
   initialConditionsBaselineSubScenario, initialConditionsTestSubScenario,
   initialConditionsSpatialForecastDataSheet, initialConditionsSpatialBaselineDataSheet,
   initialConditionsSpatialTestDataSheet)

### Output Options ----
# Define tabular output options
tabularOutputOptionValues <- data.frame(SummaryOutputSC = "TRUE",
                                        SummaryOutputSCTimesteps = 1,
                                        SummaryOutputSCAges = "TRUE",
                                        SummaryOutputTR = "TRUE",
                                        SummaryOutputTRTimesteps = 1,
                                        SummaryOutputTRAges = "TRUE")

# Define spatial output options
spatialOutputOptionValues <- data.frame(RasterOutputSC = "TRUE",
                                        RasterOutputSCTimesteps = 1,
                                        RasterOutputST = "TRUE",
                                        RasterOutputSTTimesteps = 35,
                                        RasterOutputTR = "TRUE",
                                        RasterOutputTRTimesteps = 1)

# Create output options subscenario
outputOptionsSubScenario <- scenario(ssimObject = myProject,
                                     scenario = "Output Options")

# Create tabular output opitons datasheet
tabularOutputOptionsDatasheet <- datasheet(ssimObject = outputOptionsSubScenario,
                                           name = "stsim_OutputOptions") %>% 
                                 addRow(tabularOutputOptionValues)

# Create spatial output opitons datasheet
spatialOutputOptionsDatasheet <- datasheet(ssimObject = outputOptionsSubScenario,
                                           name = "stsim_OutputOptionsSpatial") %>% 
                                 addRow(spatialOutputOptionValues)

# Save datasheets to library
saveDatasheet(ssimObject = outputOptionsSubScenario, 
              data = tabularOutputOptionsDatasheet,
              name = "stsim_OutputOptions")

saveDatasheet(ssimObject = outputOptionsSubScenario, 
              data = spatialOutputOptionsDatasheet,
              name = "stsim_OutputOptionsSpatial")

# Memory management
rm(tabularOutputOptionValues, spatialOutputOptionValues,
   outputOptionsSubScenario, tabularOutputOptionsDatasheet, 
   spatialOutputOptionsDatasheet)

### Advanced ----
#### Transition Size Distribution ----
# Loop over list of transition size distribution datasheets to create subscenarios
for(transitionSizeDistributionFile in transitionSizeDistributionFiles) {

  # Load transition multiplier values
  transitionSizeDistributionValues <- read_csv(
    file.path(tabularModelInputsDir, transitionSizeDistributionFile)) %>%
    rename(TransitionGroupID = TransitionType) %>% 
    as.data.frame()

  # Define subscenario name
  subScenarioName <- str_remove(string = transitionSizeDistributionFile,
                                pattern = ".csv")

  # Create transition multiplier subscenario
  transitionSizeDistributionSubScenario <- scenario(
    ssimObject = myProject,
    scenario = subScenarioName)

  # Create transition multiplier datasheet
  transitionSizeDistributionDataSheet <- datasheet(
    ssimObject = transitionSizeDistributionSubScenario,
    name = "stsim_TransitionSizeDistribution",
    optional = TRUE) %>%
  addRow(value = transitionSizeDistributionValues)

  # Save datasheet to library
  saveDatasheet(ssimObject = transitionSizeDistributionSubScenario,
                data = transitionSizeDistributionDataSheet,
                name = "stsim_TransitionSizeDistribution")
}

# Memory management
rm(transitionSizeDistributionValues, subScenarioName, transitionSizeDistributionSubScenario,
   transitionSizeDistributionDataSheet)

#### Transition Adjacency Multiplier ----
# Define transition adjacency settings values
transitionAdjacencySettingsValues <-
  data.frame(TransitionGroupID = c("Shrub in-filling [Type]", 
                                   "Shrub establishment [Type]",
                                   "Shrub loss [Type]",
                                   "Shrub decline [Type]"),
             StateAttributeTypeID = c("Shrub", 
                                      "Low Shrub or Shrub", 
                                      "No Shrub",
                                      "Low Shrub or No Shrub"),
            NeighborhoodRadius = 90)

# Create distribution types
distributionTypes <- c()
for(i in transitionTypes){
  for(j in seq(0, 1, 0.1)) {
    distributionType <- str_c(i, " neighbor value ", j)
    distributionTypes <- c(distributionTypes, distributionType)
  }
}

# Create dataframe
transitionAdjacencyMultiplierValues <- data.frame(
  TransitionGroupID = rep(transitionTypes, each = 11) %>% str_c(" [Type]"),
  AttributeValue = rep(seq(0, 1, 0.1), times = 4),
  DistributionType = distributionTypes,
  DistributionFrequencyID = "Iteration and Timestep")

# Create transition adjacency multiplier sub-scneario
transitionAdjacencyMultiplierSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "Transition Adjacency Multiplier")

# Create settings datasheet
transitionAdjacencySettingsDatasheet <- datasheet(
  ssimObject = transitionAdjacencyMultiplierSubScenario, 
  name = "stsim_TransitionAdjacencySetting",
  optional = TRUE) %>%
  addRow(value = transitionAdjacencySettingsValues)

# Create values datasheet
transitionAdjacencyMultiplierDatasheet <- datasheet(
  ssimObject = transitionAdjacencyMultiplierSubScenario,
  name = "stsim_TransitionAdjacencyMultiplier",
  optional = TRUE) %>% 
  addRow(value = transitionAdjacencyMultiplierValues)

# Save datasheets to library
saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario, 
              data = transitionAdjacencySettingsDatasheet,
              name = "stsim_TransitionAdjacencySetting")

saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario, 
              data = transitionAdjacencyMultiplierDatasheet,
              name = "stsim_TransitionAdjacencyMultiplier")

#### State Attribute Values ----
# Define state attribute values
stateAttributeValues <- data.frame(
  StateClassID = c("Low Shrub:All", "Low Shrub:All",
                   "Shrubland:All", "Shrubland:All",
                   "No Shrub:All",  "No Shrub:All"),
  StateAttributeTypeID = c("Low Shrub or No Shrub", "Low Shrub or Shrub",
                           "Shrub", "Low Shrub or Shrub",
                           "No Shrub", "Low Shrub or No Shrub"),
  Value = c(1, 1, 1, 1, 1, 1))

# Create state attribute values subscenario
stateAttributeValuesSubScenario <- scenario(ssimObject = myProject,
                                            scenario = "State Attribute Values")

# Create state attribute values datasheet
stateAttributeValuesDatasheet <- 
  datasheet(ssimObject = stateAttributeValuesSubScenario,
            name = "stsim_StateAttributeValue",
            optional = TRUE) %>% 
  addRow(stateAttributeValues)

# Save datasheet to library
saveDatasheet(ssimObject = stateAttributeValuesSubScenario,
              data = stateAttributeValuesDatasheet,
              name = "stsim_StateAttributeValue", append = FALSE)

# Memory management
rm(stateAttributeValues, stateAttributeValuesSubScenario, 
   stateAttributeValuesDatasheet)

#### Distributions ----
for(transitionAdjacencyMultiplierFile in transitionAdjacencyMultiplierFiles){
  
  # Get transition type
  transitionType <- transitionAdjacencyMultiplierFile %>% 
    str_remove(pattern = "Transition Adjacency Multipliers - Shrub") %>% 
    str_remove(patter = ".csv") %>% 
    str_to_lower()
  
  transitionType <- str_c("Shrub", transitionType)
    
  # Prep transition values
  distributionValues <- read_csv(
    file.path(tabularModelInputsDir, transitionAdjacencyMultiplierFile)) %>% 
    rename(StratumID = SoilType,
           Value = pred) %>% 
    mutate(DistributionTypeID = str_c(transitionType, " neighbor value ", ProportionStart),
           ExternalVariableTypeID = "Year Type",
           ExternalVariableMin = case_when(yearType == "Dry" ~ 1,
                                           yearType == "Normal" ~ 2,
                                           yearType == "Wet" ~ 3),
           ExternalVariableMax = ExternalVariableMin) %>% 
    select(StratumID, DistributionTypeID, ExternalVariableTypeID, 
           ExternalVariableMin, ExternalVariableMax, Value) %>% 
    as.data.frame
  
  # Create Sub-scenario
  subScenario <- scenario(ssimObject = myProject,
                          scenario = str_c("Distributions - ", 
                                           transitionType %>% str_to_title()))
  
  # Add data to datasheet
  subScenarioDatasheet <- datasheet(ssimObject = subScenario,
                                    name = "stsim_DistributionValue",
                                    optional = TRUE) %>% 
    addRow(distributionValues)
  
  # Save datasheet to library
  saveDatasheet(ssimObject = subScenario,
                data = subScenarioDatasheet, 
                name = "stsim_DistributionValue")
}

#### External Variables ----
## Baseline Drought Year
# Define external variable values
externalVariableValues <- droughtYearTypes %>% 
  rename(Timestep = year,
         ExternalVariableValue = yearType) %>% 
  mutate(ExternalVariableTypeID = "Year Type") %>% 
  select(Timestep, ExternalVariableTypeID, ExternalVariableValue) %>% 
  as.data.frame()

# Create external variable subscenario
externalVariablesSubScenario <- scenario(ssimObject = myProject,
                                         scenario = "External Variables - Drought Year Types - Baseline")

# Create external variable datasheet
externalVariablesDatasheet <- 
  datasheet(ssimObject = externalVariablesSubScenario,
            name = "corestime_ExternalVariableValue",
            optional = TRUE) %>% 
  addRow(externalVariableValues)

# Save datasheet to library
saveDatasheet(ssimObject = externalVariablesSubScenario,
              data = externalVariablesDatasheet,
              name = "corestime_ExternalVariableValue")

## Forecast Drought Year
# Sequence of historic timesteps
historicTimesteps <- seq(1985, 2020)

# Sequence of iterations 
Iterations <- seq(minimumIteration, maximumIteration)

# Create external variable forecast datasheet
externalVariablesForecastDatasheet <- map_dfr(Iterations, 
  ~{
    # Pick a start position in historic sequence
    startPoint <- sample(x = seq_along(historicTimesteps), size = 1)
    
    # Extract historic sequence
    historicSequence <- seq(startPoint, startPoint + (maximumTimestep - minimumTimestep))
    
    # Wrap around historic years 
    historicSequence <- (historicSequence - 1) %% length(historicTimesteps) + 1
    
    # Extract drought year type
    externalVariablesDatasheet %>% 
      slice(historicSequence) %>% 
      mutate(Iteration = .x,
             Timestep = seq(minimumTimestep, maximumTimestep))
})

# Create external variable forecast subscenario
externalVariablesForecastSubScenario <- scenario(
  ssimObject = myProject,
  scenario = "External Variables - Drought Year Types - Forecast")

# Save datasheet to library
saveDatasheet(ssimObject = externalVariablesForecastSubScenario,
              data = externalVariablesForecastDatasheet,
              name = "corestime_ExternalVariableValue")

# Memory management
rm(externalVariableValues, externalVariablesSubScenario, 
   externalVariablesDatasheet, historicTimesteps, Iterations,
   externalVariablesForecastDatasheet, externalVariablesForecastSubScenario)

## Full Scenarios ----
### Baseline (with transition size distributions) ----
baselineTestScenario <- scenario(
  ssimObject = myProject,
  scenario = "Baseline (with transition size distributions) - Test Extent")

# Merge dependencies for the baseline Scenario
mergeDependencies(baselineTestScenario) <- TRUE

# Add sub-scenarios as dependencies to the full baseline scenario
# Note: sub-scenarios are added in reverse order so that they appear in order in the UI
dependency(baselineTestScenario, "External Variables - Drought Year Types - Baseline")
dependency(baselineTestScenario, "Distributions - Shrub Loss")
dependency(baselineTestScenario, "Distributions - Shrub In-Filling")
dependency(baselineTestScenario, "Distributions - Shrub Establishment")
dependency(baselineTestScenario, "Distributions - Shrub Decline")
dependency(baselineTestScenario, "State Attribute Values")
dependency(baselineTestScenario, "Transition Adjacency Multiplier")
dependency(baselineTestScenario, "Transition Size Distribution - Shrub Loss")
dependency(baselineTestScenario, "Transition Size Distribution - Shrub In-Filling")
dependency(baselineTestScenario, "Transition Size Distribution - Shrub Establishment")
dependency(baselineTestScenario, "Transition Size Distribution - Shrub Decline")
dependency(baselineTestScenario, "Output Options")
dependency(baselineTestScenario, "Initial Conditions Spatial - Test Extent - Baseline")
dependency(baselineTestScenario, "Transition Pathways")
dependency(baselineTestScenario, "Run Control - 1 timestep, 1 iteration")

### Baseline (without transition size distributions) ----
baselineScenario <- scenario(ssimObject = myProject,
                             scenario = "Baseline (without transition size distributions) - Test Extent")

# Merge dependencies for the baseline Scenario
mergeDependencies(baselineScenario) <- TRUE

# Add sub-scenarios as dependencies to the full baseline scenario
# Note: sub-scenarios are added in reverse order so that they appear in order in the UI
dependency(baselineScenario, "External Variables - Drought Year Types - Baseline")
dependency(baselineScenario, "Distributions - Shrub Loss")
dependency(baselineScenario, "Distributions - Shrub In-Filling")
dependency(baselineScenario, "Distributions - Shrub Establishment")
dependency(baselineScenario, "Distributions - Shrub Decline")
dependency(baselineScenario, "State Attribute Values")
dependency(baselineScenario, "Transition Adjacency Multiplier")
dependency(baselineScenario, "Output Options")
dependency(baselineScenario, "Initial Conditions Spatial - Test Extent - Baseline")
dependency(baselineScenario, "Transition Pathways")
dependency(baselineScenario, "Run Control - 1985 to 2020, 10 Iteration")

### Baseline (without transition size distributions) full spatial extent ----
# Create a copy of the baseline scenario and replace name
baselineScenario_full <- scenario(ssimObject = myProject, 
                             scenario = "Baseline (without transition size distributions) - Full Extent", 
                             sourceScenario = baselineScenario)

# Remove some dependencies
dependency(baselineScenario_full, "Initial Conditions Spatial - Test Extent - Baseline", 
           remove = TRUE, force = TRUE)

# Add replacement dependencies
dependency(baselineScenario_full, "Initial Conditions Spatial - MLRA42/New Mexico - Baseline")



### Forecast (without transition size distributions) ----
# Create a copy of the baseline scenario and replace name
forecastScenario <- scenario(ssimObject = myProject, 
                             scenario = "Forecast (without transition size distributions) - Test Extent", 
                             sourceScenario = baselineScenario)

# Remove some dependencies
dependency(forecastScenario, "Run Control - 1985 to 2020, 10 Iteration", 
           remove = TRUE, force = TRUE)
dependency(forecastScenario, "Initial Conditions Spatial - Test Extent - Baseline", 
           remove = TRUE, force = TRUE)
dependency(forecastScenario, "External Variables - Drought Year Types - Baseline", 
           remove = TRUE, force = TRUE)

# Add replacement dependencies
runControlForecast <- str_c("Run Control - ", minimumTimestep, " to ", maximumTimestep,
                            ", ", maximumIteration, " Iteration")

dependency(forecastScenario, "External Variables - Drought Year Types - Forecast")
dependency(forecastScenario, "Initial Conditions Spatial - Test Extent - Forecast")
dependency(forecastScenario, runControlForecast)
