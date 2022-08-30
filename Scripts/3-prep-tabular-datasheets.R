# stsimMLRA42
# Sarah Chisholm (ApexRMS)
#
# Prep transition multiplier tabular datasheets

# Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(tidyverse)
library(readxl)

# Define Directories
tabularDataDir <- file.path("Data", "Tabular")
tabularModelInputsDir <- file.path("Model Inputs", "Tabular")

# Parameters
soilTypes <- list(list(x = 2, y = "Sandy and Deep Sand"),
                  list(x = 3, y = "Loamy-Clayey"),
                  list(x = 4, y = "Gravelly and Calcic"),
                  list(x = 5, y = "Bedrock and Colluvium"),
                  list(x = 6, y = "Gypsic"),
                  list(x = 7, y = "Bottomland"))

# List transition types
transitionTypes <- c("Shrub decline [Type]", "Shrub in-filling [Type]", "Shrub loss [Type]")

# Transition multipliers by timestep and soil type ----
transitionMultiplierValues <- imap_dfr(soilTypes, 
~{
  sheet <- read_excel(file.path(tabularDataDir, "transition_probabilities_all.xlsx"),
                                sheet = .x$x) 
  output<- sheet %>% 
    rename(Timestep = timestep,
           noShrubToLowShrub = prob_1_2,
           noShrubToShrubland = prob_1_3,
           lowShrubToNoShrub = prob_2_1,
           lowShrubToShrubland = prob_2_3,
           shrublandToNoShrub = prob_3_1,
           shrublandToLowShrub = prob_3_2) %>% 
    mutate(StratumID = .x$y) %>% 
    filter(Timestep != "NA") %>% 
    mutate(Timestep = Timestep %>% str_remove(".*-") %>% as.numeric()) %>% 
   # Uncomment the following line to get transition probability values for timesteps 2017-2020 only  
   # filter(Timestep %in% c(2017,2018,2019,2020)) %>% 
    pivot_longer(cols = noShrubToLowShrub:shrublandToLowShrub,
                 names_to = "TransitionGroupID",
                 values_to = "Amount") %>% 
    # filter(TransitionGroupID %in% c("lowShrubToNoShrub", "lowShrubToShrubland", "shrublandToLowShrub")) %>% 
    filter(TransitionGroupID %in% c("lowShrubToShrubland", "shrublandToLowShrub")) %>%
    mutate(StateClassID = case_when(
                                    # TransitionGroupID == "lowShrubToNoShrub" ~ "Low Shrub:All",
                                    TransitionGroupID == "lowShrubToShrubland" ~ "Low Shrub:All",
                                    TransitionGroupID == "shrublandToLowShrub" ~ "Shrubland:All")) %>% 
    mutate(TransitionGroupID = case_when(
                                         # TransitionGroupID == "lowShrubToNoShrub" ~ "Shrub loss [Type]",
                                         TransitionGroupID == "lowShrubToShrubland" ~ "Shrub in-filling [Type]",
                                         TransitionGroupID == "shrublandToLowShrub" ~ "Shrub decline [Type]")) %>% 
    select(Timestep, StratumID, StateClassID, TransitionGroupID, Amount)
})

write_csv(transitionMultiplierValues, file.path(tabularModelInputsDir, "Transition Multiplier - Annual.csv"))

# Transition multipliers sensitivity analysis ----

# Get the mean and SD transition probability values stratified by soil type (averaged across timesteps)
summariseProbabilities <- transitionMultiplierValues %>% 
  group_by(StratumID, TransitionGroupID, StateClassID) %>% 
  summarise(meanProbability = mean(Amount),
            sdProbability = sd(Amount))

# High and low values per transition type
for(transitionType in transitionTypes) {
  
  HighProbability <- summariseProbabilities %>% 
    filter(TransitionGroupID == transitionType) %>% 
    mutate(Amount = meanProbability + sdProbability) %>% 
    select(StratumID, TransitionGroupID, StateClassID, Amount) %>% 
    bind_rows(summariseProbabilities %>% 
                filter(TransitionGroupID != transitionType) %>% 
                mutate(Amount = meanProbability) %>% 
                select(StratumID, TransitionGroupID, StateClassID, Amount))
  
  LowProbability <- summariseProbabilities %>% 
    filter(TransitionGroupID == transitionType) %>% 
    mutate(Amount = meanProbability - sdProbability) %>% 
    select(StratumID, TransitionGroupID, StateClassID, Amount) %>%
    bind_rows(summariseProbabilities %>% 
                filter(TransitionGroupID != transitionType) %>% 
                mutate(Amount = meanProbability) %>% 
                select(StratumID, TransitionGroupID, StateClassID, Amount))
  
  transitionTypeName <- str_remove(transitionType, pattern = " \\[.*")
  
  write_csv(HighProbability, file.path(tabularModelInputsDir, str_c("Transition Multiplier - High ", transitionTypeName, ".csv")))
  write_csv(LowProbability, file.path(tabularModelInputsDir, str_c("Transition Multiplier - Low", transitionTypeName, ".csv")))
}

# Save mean values for all transition types
meanProbability <- summariseProbabilities %>% 
  mutate(Amount = meanProbability) %>% 
  select(StratumID, TransitionGroupID, StateClassID, Amount)

write_csv(meanProbability, file.path(tabularModelInputsDir, "Transition Multiplier - Mean Transition Probabilities.csv"))