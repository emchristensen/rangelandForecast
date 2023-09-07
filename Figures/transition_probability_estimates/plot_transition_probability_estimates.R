#' Plot Transition probability estimates
#' 
#' EMC 9/7/23
library(dplyr)
library(ggplot2)

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# probability of shrub establishment
establishment = read.csv("Model-Inputs/Tabular/Transition Adjacency Multipliers - Shrub Establishment.csv") %>%
  mutate(SoilName=factor(SoilType,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')))

estab = ggplot(subset(establishment, SoilName != 'Gypsic')) +
  geom_line(aes(x=ProportionStart, y=pred, color=yearType)) +
  facet_wrap(~SoilName, nrow=1) +
  scale_color_manual(values = colorBlindBlack8[5:7]) +
  ylim(0,1) +
  ylab('Probability of Shrub Establishment')+
  xlab('Proportion of adjacent shrubland/low shrub in previous year')+
  theme_bw()

estab

ggsave(estab, filename='Figures/transition_probability_estimates/establishment.png', height=3, width=9)


# probability of shrub infilling
infilling = read.csv("Model-Inputs/Tabular/Transition Adjacency Multipliers - Shrub In-Filling.csv") %>%
  mutate(SoilName=factor(SoilType,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')))

infil = ggplot(subset(infilling, SoilName != 'Gypsic')) +
  geom_line(aes(x=ProportionStart, y=pred, color=yearType)) +
  facet_wrap(~SoilName, nrow=1) +
  scale_color_manual(values = colorBlindBlack8[5:7]) +
  ylim(0,1) +
  ylab('Probability of Shrub In-Filling')+
  xlab('Proportion of adjacent shrubland in previous year')+
  theme_bw()

infil

ggsave(infil, filename='Figures/transition_probability_estimates/infilling.png', height=3, width=9)



# probability of shrub decline
decline = read.csv("Model-Inputs/Tabular/Transition Adjacency Multipliers - Shrub Decline.csv") %>%
  mutate(SoilName=factor(SoilType,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')))

decl = ggplot(subset(decline, SoilName != 'Gypsic')) +
  geom_line(aes(x=ProportionStart, y=pred, color=yearType)) +
  facet_wrap(~SoilName, nrow=1) +
  scale_color_manual(values = colorBlindBlack8[5:7]) +
  ylim(0,1) +
  ylab('Probability of Shrub Decline')+
  xlab('Proportion of adjacent low/no shrub in previous year')+
  theme_bw()

decl

ggsave(decl, filename='Figures/transition_probability_estimates/decline.png', height=3, width=9)



# probability of shrub loss
shrubloss = read.csv("Model-Inputs/Tabular/Transition Adjacency Multipliers - Shrub Loss.csv") %>%
  mutate(SoilName=factor(SoilType,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')))

loss = ggplot(subset(shrubloss, SoilName != 'Gypsic')) +
  geom_line(aes(x=ProportionStart, y=pred, color=yearType)) +
  facet_wrap(~SoilName, nrow=1) +
  scale_color_manual(values = colorBlindBlack8[5:7]) +
  ylim(0,1) +
  ylab('Probability of Shrub Loss')+
  xlab('Proportion of adjacent no shrub in previous year')+
  theme_bw()

loss

ggsave(loss, filename='Figures/transition_probability_estimates/shrubloss.png', height=3, width=9)
