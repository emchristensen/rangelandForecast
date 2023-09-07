#' State transition figures
#' 
#' EMC 8/15/23

library(dplyr)
library(ggplot2)
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# =======================
# Baseline: 1985-2020
# Test extent; with TSD
out = readxl::read_excel(path='Outputs/Baseline_withTSD_testextent/State Classes.xlsx', sheet = 'Area by State Class') %>%
  mutate(`Soil Type`=factor(`Soil Type`,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')),
         Area_sqkm = `Area (Hectares)`/100)



# calculate mean over iterations
out_mean = out %>%
  group_by(`Scenario ID`, Timestep, `Soil Type`, `State Class`) %>%
  summarize(Area = mean(Area_sqkm),
            Area_sd = sd(Area_sqkm))

state_by_soil_base = ggplot(out_mean, aes(x=Timestep, y=Area)) +
  geom_line(aes(group=interaction( `State Class`), color=`State Class`)) +
  scale_color_manual(values = colorBlindBlack8, labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  facet_wrap(~`Soil Type`) +
  ylab('Area (sq km)') +
  theme_bw()
state_by_soil_base

ggsave(state_by_soil_base, filename= 'Figures/Baseline_withTSD_testextent/state_by_soil_1985_2020.png', width=6, height=4)

# sum of all soils
out_allsoil = out %>%
  group_by(Timestep, Iteration, `State Class`) %>%
  summarize(Area = sum(Area_sqkm))

# average over iterations
out_allsoil_mean = out_allsoil %>%
  group_by(Timestep, `State Class`) %>%
  summarize(Area_mean = mean(Area),
            Area_sd = sd(Area))

state_ts_base = ggplot(out_allsoil, aes(x=Timestep, y=Area, group=interaction(Iteration, `State Class`))) +
  geom_line(aes(color=`State Class`)) +
  scale_color_manual(values = colorBlindBlack8, labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  ylab('Area (sq km)') +
  theme_bw()
state_ts_base

ggsave(state_ts_base, filename= 'Figures/Baseline_withTSD_testextent/state_timeseries_1985_2020.png', width=4, height=3)


# percent of area in each class
totalarea = out %>%
  dplyr::filter(Timestep == 1985, Iteration==1) %>%
  dplyr::select(`Area (Hectares)`) %>%
  sum()

pctcover_soil = out %>%
  dplyr::select(Iteration, Timestep, `Soil Type`, `State Class`,Area=`Area (Hectares)`) %>%
  dplyr::mutate(pct_area = Area/totalarea)

pctcover_total = out_allsoil %>%
  dplyr::mutate(pct_area = Area/totalarea) %>%
  group_by(Timestep, `State Class`) %>%
  summarize(pct_area_mean = mean(pct_area),
            pct_area_sd = sd(pct_area))

write.csv(pctcover_total, 'Figures/Baseline_withTSD_testextent/Table_percent_cover_states.csv', row.names = F)


# =========================================
# Future: 2020-2055
# with TSD, test extent
out_f = readxl::read_excel(path='Outputs/Forecast_withTSD_testextent/State Classes.xlsx', sheet = 'Area by State Class') %>%
  dplyr::filter(`Scenario ID`==31) %>%
  mutate(`Soil Type`=factor(`Soil Type`,      # reorder soils
                            levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')),
         Area_sqkm = `Area (Hectares)`/100)

# sum of all soils
out_allsoil = out_f %>%
  group_by(Timestep, Iteration, `State Class`) %>%
  summarize(Area = sum(Area_sqkm))

# average over iterations
out_allsoil_mean = out_allsoil %>%
  group_by(Timestep, `State Class`) %>%
  summarize(Area_mean = mean(Area),
            Area_sd = sd(Area))

state_ts_fore = ggplot(out_allsoil_mean, aes(x=Timestep, y=Area_mean)) +
  geom_line(aes(color=`State Class`)) +
  scale_color_manual(values = colorBlindBlack8, labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  ylab('Area (sq km)') +
  theme_bw()
state_ts_fore
ggsave(state_ts_fore, filename= 'Figures/Forecast_withTSD_testextent/state_timeseries_2020_2055.png', width=4, height=3)

# percent of area in each class
totalarea = out_f %>%
  dplyr::filter(Timestep == 2020, Iteration==1) %>%
  dplyr::select(Area_sqkm) %>%
  sum()

pctcover_soil = out_f %>%
  dplyr::select(Iteration, Timestep, `Soil Type`, `State Class`,Area=`Area (Hectares)`) %>%
  dplyr::mutate(pct_area = Area/totalarea)

pctcover_total = out_allsoil %>%
  dplyr::mutate(pct_area = Area/totalarea) %>%
  group_by(Timestep, `State Class`) %>%
  summarize(pct_area_mean = mean(pct_area),
            pct_area_sd = sd(pct_area))

write.csv(pctcover_total, 'Figures/Forecast_withTSD_testextent/Table_percent_cover_states.csv', row.names = F)
