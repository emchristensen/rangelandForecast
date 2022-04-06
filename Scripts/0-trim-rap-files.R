#' trim RAP data in large area to MLRA42
#' EMC 12/7/21

library(raster)
library(sf)

# read in MLRA shapefile
mlras = st_read('shapefile/nrcs142p2_052440/mlra_v42.shp')
mlra42 = subset(mlras, MLRARSYM==42)

mlra42_simplified = st_simplify(mlra42, dTolerance=1000)

file_list = list.files('RAP/raw files MLRA42/raw downloads', pattern = '*.tif', full.names=T)


for (filename in file_list) {
  # read in RAP
  rap = stack(filename)
  
  # mask
  MLRA42_rap = mask(rap, mlra42)
  #plot(MLRA42_rap)
  
  filepath = tools::file_path_sans_ext(filename)
  
  # write to file
  writeRaster(MLRA42_rap, filename=paste0(filepath, '_MLRA42.tif'))
  
}