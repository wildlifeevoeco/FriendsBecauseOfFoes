### Raster Processing MB ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: 
# Inputs: Elk relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'sp', 'magrittr', 'raster')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

# Covariates
lsCovers <- data.table(nm = dir('input/covariates/RMNP', '.tif$'))[, nm := gsub(".tif|100m", "", nm)]$nm
lsPaths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)

### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(lsPaths, FUN = function(r){
  crop(raster(r), bounds)
})

# Log transform
namesTransform <- c('LinFeat_Dist', 'Water_Dist')
rasterTransform <- cropRasters[lapply(cropRasters, names) %in% namesTransform]

transformed <- lapply(seq_along(namesTransform), FUN = function(x){
   r <- log(rasterTransform[[x]] + 1)
   names(r) <- namesTransform[[x]]
   r
})

transformed[[1]] <- resample(transformed[[1]], transformed[[2]])


### Output ----
outRaster <- c(transformed, cropRasters[!(lapply(cropRasters, names) %in% namesTransform)])
outNames <- lapply(outRaster, names)

lapply(seq_along(outRaster), FUN = function(r){
  writeRaster(outRaster[[r]], paste0('output/data-prep/cropped-rasters/RMNP/', outNames[[r]]), 
              format = 'GTiff', 
              overwrite = T)
})

