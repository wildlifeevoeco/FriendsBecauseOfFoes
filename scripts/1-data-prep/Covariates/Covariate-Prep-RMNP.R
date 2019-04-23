### Covariate (Raster) Processing ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi

### Packages ----
pkgs <- c('data.table', 'ggplot2', 'sp', 'raster')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Set variables ----
source('scripts/0-variables/variables.R')


### List rasters ----
# Covariates
covers <- gsub(".tif|100m", "", dir('input/covariates/RMNP', '.tif$'))
paths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)
names(paths) <- covers

rmList <- which(covers %in% c('Agriculture', 'Deciduous', 'Grassland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]


### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(
  lsPaths,
  FUN = function(r) {
    crop(raster(r), mbBounds)
  }
)

# Log transform
namesTransform <- c('LinFeat_Dist', 'Water_Dist')
rasterTransform <-
  cropRasters[lapply(cropRasters, names) %in% namesTransform]

transformed <- lapply(
  seq_along(namesTransform),
  FUN = function(x) {
    r <- log(rasterTransform[[x]] + 1)
    names(r) <- namesTransform[[x]]
    r
  }
)
names(transformed) <- namesTransform

transformed[['LinFeat_Dist']] <-
  resample(transformed[['LinFeat_Dist']], transformed[['Water_Dist']])

cropRasters[['Ruggedness_test']] <-
  resample(cropRasters[['Ruggedness']], cropRasters[['Water_Dist']])


### Output ----
outRaster <- c(transformed,
               cropRasters[!(lapply(cropRasters, names) %in% namesTransform)])
outNames <- lapply(outRaster, names)

lapply(
  seq_along(outRaster),
  FUN = function(r) {
    writeRaster(
      outRaster[[r]],
      paste0('output/1-data-prep/covariates/RMNP/', outNames[[r]]),
      format = 'GTiff',
      overwrite = T
    )
  }
)