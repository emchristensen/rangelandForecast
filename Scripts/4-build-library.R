## stsimMLRA42
## Sarah Chisholm (ApexRMS)
##
## Build shrub spread st-sim library

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(rsyncrosim)
library(tidyverse)
library(terra)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")
tabularModelInputsDir <- file.path("Model-Inputs", "Tabular")
spatialrModelInputsDir <- file.path("Model-Inputs", "Spatial")
libraryDir <- "Libraries"

# Parameters
# List transition multiplier files
transitionMultiplierFiles <- list.files(
  path = tabularModelInputsDir,
  pattern = "Transition Multiplier", 
  full.names = FALSE)

# Connect R to SyncroSim
mySession <- session()

## Create new Library ----
myLibrary <- ssimLibrary(name = file.path(libraryDir, "MLRA42 Shrub Encroachment"),
                         package = "stsim",
                         session = mySession,
                         overwrite = TRUE)

# Open current library
# myLibrary <- ssimLibrary(name = file.path(libraryDir, "MLRA42 Shrub Encroachment"),
#                          session = mySession)

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
                          Name = c("Shrub in-filling", 
                                   "Shrub establishment", 
                                   "Shrub loss",
                                   "Shrub decline"),
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
# State Attribute Type
stateAttributeType <- datasheet(ssimObject = myProject,
                                name = "stsim_StateAttributeType") %>% 
                      addRow(value = "Shrub") %>% 
                      addRow(value = "No Shrub")

# Save datasheet to library
saveDatasheet(ssimObject = myProject, 
              data = stateAttributeType,
              name = "stsim_StateAttributeType")

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
   stateAttributeType, terminology)

## Scenario-scope Datasheets ----
## Create Sub-Scenarios (dependencies) for each scenario-scoped datasheet
### Run Control ----
## Run Control - 2016 to 2020, 1 Iteration
runControlSubScenario1 <- scenario(
  ssimObject = myProject,
  scenario = "Run Control - 2016 to 2020, 1 Iteration")
  
runControlDataSheet1 <- datasheet(
  ssimObject = runControlSubScenario1,
  name = "stsim_RunControl") %>% 
  addRow(value = list(MinimumIteration = 1,
                      MaximumIteration = 1, 
                      MinimumTimestep = 2016, 
                      MaximumTimestep = 2020, 
                      IsSpatial = TRUE))

# Save datasheet to library
saveDatasheet(ssimObject = runControlSubScenario1, 
              data = runControlDataSheet1,
              name = "stsim_RunControl")

## Run Control - 1985 to 2020, 1 Iteration
runControlSubScenario2 <- scenario(
                            ssimObject = myProject,
                            scenario = "Run Control - 1985 to 2020, 1 Iteration")

runControlDataSheet2 <- datasheet(ssimObject = runControlSubScenario2,
                                 name = "stsim_RunControl") %>% 
                       addRow(value = list(MinimumIteration = 1, 
                                           MaximumIteration = 1, 
                                           MinimumTimestep = 1985,
                                           MaximumTimestep = 2020, 
                                           IsSpatial = TRUE))

# Save datasheet to library
saveDatasheet(ssimObject = runControlSubScenario2, 
              data = runControlDataSheet2,
              name = "stsim_RunControl")

# Memory management
rm(runControlSubScenario1, runControlDataSheet1, 
   runControlSubScenario2, runControlDataSheet2)

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
## Initial Conditions - Test extent
# Create a list of the input tif files
initialConditionsSpatialValues1 <- list(
  StratumFileName = file.path(
                      getwd(), 
                      spatialrModelInputsDir, 
                      "soil-type-test-extent.tif"), 
  StateClassFileName = file.path(
                        getwd(), 
                        spatialrModelInputsDir, 
                        "state-class-test-extent.tif")) 

initialConditionsSubScenario1 <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - Test Extent") 

initialConditionsSpatialDataSheet1 <- datasheet(
  ssimObject = initialConditionsSubScenario1,
  name = "stsim_InitialConditionsSpatial") %>% 
addRow(value = initialConditionsSpatialValues1)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsSubScenario1, 
  data = initialConditionsSpatialDataSheet1,
  name = "stsim_InitialConditionsSpatial")

## Initial Conditions - MLRA 42 / New Mexico
# Create a list of the input tif files
initialConditionsSpatialValues2 <- list(
  StratumFileName = file.path(
                    getwd(), 
                    spatialrModelInputsDir, 
                    "soil-type.tif"), 
  StateClassFileName = file.path(
                       getwd(), 
                       spatialrModelInputsDir, 
                       "state-class.tif")) 

initialConditionsSubScenario2 <- scenario(
  ssimObject = myProject,
  scenario = "Initial Conditions Spatial - MLRA42/New Mexico") 

initialConditionsSpatialDataSheet2 <- datasheet(
  ssimObject = initialConditionsSubScenario2,
  name = "stsim_InitialConditionsSpatial") %>% 
  addRow(value = initialConditionsSpatialValues2)

# Save datasheet to library
saveDatasheet(ssimObject = initialConditionsSubScenario2, 
              data = initialConditionsSpatialDataSheet2,
              name = "stsim_InitialConditionsSpatial")

# Memory management
rm(initialConditionsSpatialValues1, initialConditionsSpatialValues2,
   initialConditionsSubScenario1, initialConditionsSubScenario2, 
   initialConditionsSpatialDataSheet1, initialConditionsSpatialDataSheet2)

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
#### State Attribute Values ----
# Define state attribute values
stateAttributeValues <- data.frame(StateClassID = c("Low Shrub:All", "Shrubland:All", "No Shrub:All"),
                                   StateAttributeTypeID = c("Shrub", "Shrub", "No Shrub"),
                                   Value = c(1,1,1))

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
              name = "stsim_StateAttributeValue")

# Memory management
rm(stateAttributeValues, stateAttributeValuesSubScenario, 
   stateAttributeValuesDatasheet)

#### Transition Multiplier ----
# Loop over list of transition multiplier datasheets to create subscenarios
for(transitionMultiplierFile in transitionMultiplierFiles) {
  
  # Load transition multiplier values
  transitionMultiplierValues <- read_csv(
    file.path(tabularModelInputsDir, transitionMultiplierFile)) %>% 
    as.data.frame()
  
  # Define subscenario name
  subScenarioName <- str_remove(string = transitionMultiplierFile,
                                pattern = ".csv")
  
  # Create transition multiplier subscenario
  transitionMultiplierSubScenario <- scenario(
    ssimObject = myProject,
    scenario = subScenarioName)
  
  # Create transition multiplier datasheet
  transitionMultiplierDataSheet <- datasheet(
    ssimObject = transitionMultiplierSubScenario,
    name = "stsim_TransitionMultiplierValue",
    optional = TRUE) %>% 
  addRow(value = transitionMultiplierValues)
  
  # Save datasheet to library
  saveDatasheet(ssimObject = transitionMultiplierSubScenario, 
                data = transitionMultiplierDataSheet,
                name = "stsim_TransitionMultiplierValue")
}

# Memory management
rm(transitionMultiplierValues, subScenarioName, transitionMultiplierSubScenario,
   transitionMultiplierDataSheet)

#### Transition Adjacency Multiplier ----
## Transition Adjacency Multiplier - Shrub Establishment
# Define transition adjacency settings values
transitionAdjacencySettingsValues1 <- 
  data.frame(TransitionGroupID = "Shrub establishment [Type]",
             StateAttributeTypeID = "Shrub",
            NeighborhoodRadius = 90)

# Load transition adjacency multiplier values
transitionAdjacencyMultiplierValues1 <- 
  read_csv(file.path(tabularModelInputsDir, 
                     "Transition Adjacency Multipliers - Shrub Establishment.csv")) %>% 
  rename(StratumID = `Primary Stratum`,
         TransitionGroupID = `Transition Type/Group`,
         AttributeValue = `Neighbor Value`,
         Amount = Multiplier) %>% 
  as.data.frame()

# Create Transition Adjacency Multiplier - Shrub Establishment subscenario
transitionAdjacencyMultiplierSubScenario1 <- 
  scenario(ssimObject = myProject,
           scenario = "Transition Adjacency Multiplier - Shrub Establishment")

# Define transition adjacency settings datasheet
transitionAdjacencySettingsDatasheet1 <- datasheet(ssimObject = transitionAdjacencyMultiplierSubScenario1, 
                                                   name = "stsim_TransitionAdjacencySetting",
                                                   optional = TRUE) %>% 
  addRow(value = transitionAdjacencySettingsValues1)

# Create transition adjacency datasheet
transitionAdjacencyMultiplierDatasheet1 <- datasheet(ssimObject = transitionAdjacencyMultiplierSubScenario1, 
                                                    name = "stsim_TransitionAdjacencyMultiplier",
                                                    optional = TRUE) %>% 
  addRow(value = transitionAdjacencyMultiplierValues1)

# Save datasheets to library
saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario1,
              data = transitionAdjacencySettingsDatasheet1,
              name = "stsim_TransitionAdjacencySetting")

saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario1,
              data = transitionAdjacencyMultiplierDatasheet1,
              name = "stsim_TransitionAdjacencyMultiplier")

## Transition Adjacency Multiplier - Shrub Loss
# Define transition adjacency settings values
transitionAdjacencySettingsValues2 <- data.frame(
  TransitionGroupID = "Shrub loss [Type]",
  StateAttributeTypeID = "No Shrub",
  NeighborhoodRadius = 90)

# Load transition adjacency multiplier values
transitionAdjacencyMultiplierValues2 <- 
  read_csv(file.path(tabularModelInputsDir, 
                     "Transition Adjacency Multipliers - Shrub Loss.csv")) %>% 
  rename(StratumID = `Primary Stratum`,
         TransitionGroupID = `Transition Type/Group`,
         AttributeValue = `Neighbor Value`,
         Amount = Multiplier) %>% 
  as.data.frame()

# Create Transition Adjacency Multiplier - Shrub Loss subscenario
transitionAdjacencyMultiplierSubScenario2 <- 
  scenario(ssimObject = myProject,
           scenario = "Transition Adjacency Multiplier - Shrub Loss")

# Define transition adjacency settings datasheet
transitionAdjacencySettingsDatasheet2 <- datasheet(
  ssimObject = transitionAdjacencyMultiplierSubScenario2, 
  name = "stsim_TransitionAdjacencySetting",
  optional = TRUE) %>% 
addRow(value = transitionAdjacencySettingsValues2)


transitionAdjacencyMultiplierDatasheet2 <- datasheet(
  ssimObject = transitionAdjacencyMultiplierSubScenario2, 
  name = "stsim_TransitionAdjacencyMultiplier",
  optional = TRUE) %>% 
addRow(value = transitionAdjacencyMultiplierValues2)

# Save datasheet to library
saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario2,
              data = transitionAdjacencySettingsDatasheet2,
              name = "stsim_TransitionAdjacencySetting")

saveDatasheet(ssimObject = transitionAdjacencyMultiplierSubScenario2,
              data = transitionAdjacencyMultiplierDatasheet2,
              name = "stsim_TransitionAdjacencyMultiplier")

# Memory management
rm(transitionAdjacencySettingsValues1, transitionAdjacencySettingsValues2,
   transitionAdjacencyMultiplierValues1, 
   transitionAdjacencyMultiplierValues2, 
   transitionAdjacencyMultiplierSubScenario1, 
   transitionAdjacencyMultiplierSubScenario2,
   transitionAdjacencySettingsDatasheet1, 
   transitionAdjacencySettingsDatasheet2,
   transitionAdjacencyMultiplierDatasheet1, 
   transitionAdjacencyMultiplierDatasheet2)

## Full Scenarios ----
### Baseline ----
baselineScenario <- scenario(ssimObject = myProject,
                             scenario = "Baseline Scenario")

# Merge dependencies for the baseline Scenario
mergeDependencies(baselineScenario) <- TRUE

# Add sub-scenarios as dependencies to the full baseline scenario
# Note: sub-scenarios are added in reverse order so that they appear in order in the UI
dependency(baselineScenario, "Transition Adjacency Multiplier - Shrub Loss")
dependency(baselineScenario, "Transition Adjacency Multiplier - Shrub Establishment")
dependency(baselineScenario, "Transition Multiplier - Annual")
dependency(baselineScenario, "State Attribute Values")
dependency(baselineScenario, "Output Options")
dependency(baselineScenario, "Initial Conditions Spatial - MLRA42/New Mexico")
dependency(baselineScenario, "Transition Pathways")
dependency(baselineScenario, "Run Control - 1985 to 2020, 1 Iteration")

### Transitions Sensitivity Analysis ----
## This section executes the following steps:
## 1 - Create a copy of the baseline scenario
## 2 - Remove the annual transition multipliers
## 3 - Add a new set of transition multiplier probabilities

# Get list of transition multiplier files excluding the baseline (annual) sheet
transitionMultiplierFiles <-
  transitionMultiplierFiles[transitionMultiplierFiles != "Transition Multiplier - Annual.csv"]

# Loop over transition multiplier files to generate individual full scenarios
for(transitionMultiplierFile in transitionMultiplierFiles) {
  
  # Get subscenario name
  subScenarioName <- transitionMultiplierFile %>% 
    str_remove(pattern = ".csv")
  
  # Define new full scenario name
  fullScenarioName <- transitionMultiplierFile %>% 
    str_remove(pattern = "Transition Multiplier - ") %>% 
    str_remove(pattern = ".csv")
  
  # Make a copy of the baseline scenario
  transitionMultiplierScenario <- scenario(
    ssimObject = myProject, 
    scenario = fullScenarioName, 
    sourceScenario = baselineScenario)
  
  # Remove the baseline transition multiplier sub-scenario
  dependency(scenario = transitionMultiplierScenario,
             dependency = "Transition Multiplier - Annual",
             remove = TRUE,
             force = TRUE)
  
  # Add the corresponding sub-scenario to the new full scenario
  dependency(scenario = transitionMultiplierScenario,
             dependency = subScenarioName)
}
## Organize library ----
mainFolderIDs <- list()
subFolderIDs <- list()
mainFolders <- c("Full Scenarios", "Sub Scenarios")
subFolders <- c("Sensitivity Analysis", "Run Control", "Transition Pathways", 
                "Initial Conditions", "Output Options", "Transition Multiplier", 
                "Transition Adjacency Multiplier")

# Create main folders and save the folder ID numbers to mainFolderIDs
for(mainFolder in mainFolders) {
  
  mainFolderID <-rsyncrosim::command(
    args = list(
      create = NULL,
      folder = NULL,
      lib = filepath(myLibrary),
      name = mainFolder,
      tpid = projectId(myProject)),
    session = mySession) %>%
    # "\\d+" is a regular expression to match numbers
    str_extract("\\d+") %>%
    as.integer
  
  mainFolderIDs <- append(mainFolderIDs, list(c(mainFolder, mainFolderID)))
}

# Create sub folders and save the folder ID numbers to subFolderIDs
for(subFolder in subFolders) {
  
  subFolderID <-rsyncrosim::command(
    args = list(
      create = NULL,
      folder = NULL,
      lib = filepath(myLibrary),
      name = subFolder,
      tpid = projectId(myProject)),
    session = mySession) %>%
    # "\\d+" is a regular expression to match numbers
    str_extract("\\d+") %>%
    as.integer
  
  subFolderIDs <- append(subFolderIDs, list(c(subFolder, subFolderID)))
}

# Move the Sub Scenario into the folder
rsyncrosim::command(
  args = list(
    move = NULL,
    scenario = NULL,
    lib = filepath(myLibrary),
    name = "Sub Scenarios",
    sid = scenarioId(myscenario),
    tfid = subScenarioFolderID),
  session = ssimSession) %>%
  invisible()