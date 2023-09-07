#' Plot raster data
#' 
#' EMC 8/15/23

library(dplyr)
library(terra)
library(ggplot2)
library(tidyterra)

testextent = rast('Model-Inputs/Spatial/state-class-test-extent-1985.tif')
plot(testextent)
# create polygon of outline
p_testextent = as.polygons(ext(testextent))

# read in study area raster
studyarea = terra::rast('Data/Spatial/study-area-mask.tif')
plot(studyarea)
lines(p_testextent)

# polygon of outline
test = as.polygons(studyarea> -Inf)
plot(test)

ggplot() +
  geom_spatraster(data=testextent)

test2020 = rast('Model-Inputs/Spatial/state-class-test-extent-2020.tif')
plot(test2020)

# get % area in each class in 2020
table2020 = freq(test2020) %>%
  mutate(percent = count/sum(count))

