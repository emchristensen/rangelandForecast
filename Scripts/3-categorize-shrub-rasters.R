#' Categorize 5-year shrub cover rasters
#' 
#'   1 = no shrub (0-1% cover)
#'   2 = low shrub (1-15% cover)
#'   3 = high shrub (>15% cover)
#'   
#' EMC 1/19/22

library(dplyr)
library(raster)

filelist = list.files('RAP/shrub rasters 5yr', pattern = '*[0-9].tif', full.names=T)

for (filename in filelist[2:7]) {
  
  shrubcover = raster(filename)
  
  # inequalities to assign states
  # 1 = no shrub (0-1); 2 = low shrub; 3 = shrub
  m <- c(0,1,1,
         1,14,2,
         14,Inf,3)
  rclmat = matrix(m, ncol=3, byrow=T)
  stateraster = raster::reclassify(shrubcover, rclmat, include.lowest=T)
  
  # save to file
  fname = paste0('RAP/shrub rasters 5yr/', tools::file_path_sans_ext(basename(filename)), '_categories.tif')
  writeRaster(stateraster, filename=fname, overwrite=T)
}

freq(stateraster)