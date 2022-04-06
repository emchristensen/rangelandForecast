#' calculate 5-year means of RAP shrub cover rasters
#' 
#' Reads in annual shrub cover rasters
#' Calculates 5-year means and re-saves rasters
#' 
#' I couldn't get the loop to work; did each individually
#' EMC 12/27/21

library(dplyr)
library(raster)


# list 5-year groups of years
yearset_list = list(1986:1990, 1991:1995, 1996:2000, 2001:2005, 2006:2010, 2011:2015, 2016:2020)

yearset = yearset_list[1]

#for (yearset in yearset_list[2:7]){

# list shrub raster files to read
files = paste0('RAP/shrub rasters MLRA42/RAP_shrubtree_', yearset[[1]], '.tif')  

# read in rasters
allrasters = stack(files)

# calculate mean
meanshrub = calc(allrasters, fun=mean, na.rm=T)
sdshrub = calc(allrasters, fun=sd, na.rm=T)
#plot(meanshrub)

# save to file
fname = paste0('RAP/shrub rasters 5yr/RAP_shrubtree_', yearset[[1]][1], '_', yearset[[1]][5], '.tif')
writeRaster(meanshrub, filename=fname, overwrite=T)
#}