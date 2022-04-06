#' Construct shrub cover rasters
#' 
#' Takes RAP rasters for years 1986-2020
#'   - sum shrub and tree layer for each year
#' 
#' RAP raw data bands contain % cover for the following categories:
#'   band 1 = annual forb and grass
#'   band 2 = bare ground
#'   band 3 = litter
#'   band 4 = perennial forb and grass
#'   band 5 = shrub
#'   band 6 = tree
#'   band 7-12 = uncertainty for bands 1-6
#' no data value = 65535
#' see "data_README.txt" for more info
#' EMC 12/6/21
#' 
#' last update: 12/13/21

library(raster)
library(dplyr)

file_list = list.files('RAP/raw files MLRA42', pattern = '*.tif', full.names=T)



for (raw_file in file_list) {
  # get year from file name
  year = unlist(strsplit(tools::file_path_sans_ext(basename(raw_file)),'_'))[4]
  
  # read in raster stack
  s = stack(raw_file)
  
  # create objects for important raster layers
  shrubcover = s@layers[[5]]
  treecover = s@layers[[6]]
  
  # convert missing values to NA
  shrubcover[shrubcover==65535] <- NA
  treecover[treecover==65535] <- NA
  
  # combine shrub and tree
  shrubtree = shrubcover + treecover
  
  writeRaster(shrubtree, filename=paste0('RAP/shrub rasters MLRA42/RAP_shrubtree_', year, '.tif'), overwrite=T)
}

