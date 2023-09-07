#' plot RCMAP states timeseries
#' 
#' Soil types:
#'     3 = Loamy-clayey
#'     4 = gravelly and calcic
#'     5 = bedrock and colluvium
#'     6 = gypsic
#'     7 = bottomland
#'     8 = sandy and deep sand
#' 
#' EMC 9/6/23
library(dplyr)
library(terra)
library(ggplot2)
library(tidyterra)



# read in test extent raster
testext = rast('Model-Inputs/Spatial/soil-type-test-extent.tif')


# folder where RCMAP data are stored
rcfolder = 'Data/Spatial/RCMAP/Processed/'

rcfiles = list.files(rcfolder)

# soil raster
soil = terra::rast('Model-Inputs/Spatial/soil-type.tif')



# ===================================================
# number of cells of each soil type
freq(soil)


# read in one rcmap
rcmap = rast(paste0(rcfolder, rcfiles[1]))

# reproject soil rasters to RCMAP crs
testext_p = project(testext, rcmap)
soil_p = project(soil, rcmap)

# read in each raster and tally veg type -- test extent
states_test = c()
for (f in rcfiles) {
  y = readr::parse_number(f)
  r = terra::rast(paste0(rcfolder, f)) %>% as.factor()
  r_c = crop(r, testext_p)  # crop to test extent
  
  tally_test = crosstab(c(r_c, testext_p)) # number of pixels by state and soil: test extent
  states_test = rbind(states_test, as.data.frame.matrix(tally_test) %>% dplyr::mutate(year=y, State=1:3))
}

# convert to long and calculate total area from number of pixels: 30m x 30m
states_test_long = tidyr::pivot_longer(states_test, cols=1:6, names_to='soil', values_to='pixels') %>%
  dplyr::mutate(Area_sqkm=pixels*900/1000000)

# get names of soils
states_test_long$SoilName = factor(states_test_long$soil, levels=3:8, 
                                   labels=c('Loamy-Clayey','Gravelly and Calcic','Bedrock and Colluvium','Gypsic','Bottomland','Sandy and Deep Sand'))

write.csv(states_test_long, 'Figures/rcmap_states_1985_2020_testextent.csv', row.names=F)

# read in each raster and tally veg type -- full extent
states = c()
for (f in rcfiles) {
  y = readr::parse_number(f)
  r = terra::rast(paste0(rcfolder, f)) %>% as.factor()
  
  tally_full = crosstab(c(r, soil_p))    # number of pixels by state and soil
  states = rbind(states, as.data.frame.matrix(tally_full) %>% dplyr::mutate(year=y, State=1:3))
}

# convert to long and calculate total area from number of pixels: 30m x 30m
states_long = tidyr::pivot_longer(states, cols=1:6, names_to='soil', values_to='pixels') %>%
  dplyr::mutate(Area_sqkm=pixels*900/1000000)

write.csv(states_long, 'Figures/rcmap_states_1985_2020_fullextent.csv', row.names=F)


# ============================================================================
# plot timeseries: full extent

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

states_long = read.csv('Figures/rcmap_states_1985_2020_fullextent.csv')

# all soils grouped
rcmap_allsoil = states_long %>%
  group_by(year, State) %>%
  summarize(Area_sqkm = sum(Area_sqkm)) %>%
  ggplot() +
  geom_line(aes(x=year, y=Area_sqkm, color=State)) +
  scale_color_manual(values = colorBlindBlack8, breaks = c(2,1,3), labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  theme_bw() +
  xlab('') +
  ylab('Area (sq km)')

rcmap_allsoil

ggsave(rcmap_allsoil, filename='Figures/RCMAP_timeseries_1985_2020.png', height=4, width=5)


# ===============================
# plot Timeseries: test extent

states_test_long = read.csv('Figures/rcmap_states_1985_2020_testextent.csv') %>%
  dplyr::mutate(State = as.factor(State)) %>%
  mutate(SoilName=factor(SoilName,      # reorder soils
                         levels=c('Gypsic','Bedrock and Colluvium','Gravelly and Calcic','Sandy and Deep Sand','Loamy-Clayey','Bottomland')))


# all soils grouped
rcmap_allsoil_test = states_test_long %>%
  group_by(year, State) %>%
  summarize(Area_sqkm = sum(Area_sqkm)) %>%
  ggplot() +
  geom_line(aes(x=year, y=Area_sqkm, color=State)) +
  scale_color_manual(values = colorBlindBlack8, breaks = c(2,1,3), labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  theme_bw() +
  xlab('') +
  ylab('Area (sq km)')

rcmap_allsoil_test

ggsave(rcmap_allsoil_test, filename='Figures/RCMAP_timeseries_1985_2020_testextent.png', height=3, width=4)

# by soil
rcmap_test = states_test_long %>%
  ggplot() +
  geom_line(aes(x=year, y=Area_sqkm, color=State)) +
  facet_wrap(~SoilName) +
  scale_color_manual(values = colorBlindBlack8, breaks = c(2,1,3), labels=c('Low shrub','No shrub','Shrubland'), name='State Class') +
  theme_bw() +
  xlab('') +
  ylab('Area (sq km)')

rcmap_test

ggsave(rcmap_test, filename='Figures/RCMAP_timeseries_1985_2020_testextent_bysoil.png', height=4, width=6)
