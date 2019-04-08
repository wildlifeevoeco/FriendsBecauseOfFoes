### Raster Processing MB ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: 
# Inputs: Elk relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 'sp', 'raster')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Covariates
lsCovers <- data.table(nm = dir('input/covariates/RMNP', '.tif$'))[, nm := gsub(".tif|100m", "", nm)]$nm
lsPaths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers

### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(lsPaths, FUN = function(r){
  crop(raster(r), mbBounds)
})

# Log transform
namesTransform <- c('LinFeat_Dist', 'Water_Dist')
rasterTransform <- cropRasters[lapply(cropRasters, names) %in% namesTransform]

transformed <- lapply(seq_along(namesTransform), FUN = function(x){
   r <- log(rasterTransform[[x]] + 1)
   names(r) <- namesTransform[[x]]
   r
})
names(transformed) <- namesTransform

transformed[['LinFeat_Dist']] <-
  resample(transformed[['LinFeat_Dist']], transformed[['Water_Dist']])

cropRasters[['Ruggedness_test']] <-
  resample(cropRasters[['Ruggedness']], cropRasters[['Water_Dist']])

# TODO: need MB elevation data
cropRasters[['Elevation']] <-
  resample(cropRasters[['Elevation']], cropRasters[['Water_Dist']])

### Output ----
outRaster <- c(transformed, cropRasters[!(lapply(cropRasters, names) %in% namesTransform)])
outNames <- lapply(outRaster, names)

lapply(seq_along(outRaster), FUN = function(r){
  writeRaster(outRaster[[r]], paste0('output/1-data-prep/covariates/RMNP/', outNames[[r]]), 
              format = 'GTiff', 
              overwrite = T)
})

