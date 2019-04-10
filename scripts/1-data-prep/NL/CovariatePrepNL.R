### Covariate (Raster) Processing ----
# Authors: Michel Laforge, Alec Robitaille

### Packages ----
libs <- c('data.table', 'ggplot2', 'sp', 'raster')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Transform elev to ruggedness ----
# elev <- raster('input/covariates/NL/NLElev.tif')

# ruggedness <- terrain(elev, opt = "roughness")

# writeRaster(
#   ruggedness,
#   'input/covariates/NL/Ruggedness',
#   format = 'GTiff',
#   overwrite = TRUE
# )

### List rasters ----
# Covariates
lsCovers <- gsub(".tif|100m", "", dir('input/covariates/NL', '.tif$'))
lsPaths <- dir('input/covariates/NL', '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers


### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(
  lsPaths,
  FUN = function(r) {
    crop(raster(r), nlBounds)
  }
)

# Log transform
namesTransform <- c('LinearDist', 'WaterDist')
rasterTransform <- cropRasters[lapply(cropRasters, names) %in% namesTransform]

transformed <- lapply(
  seq_along(namesTransform),
  FUN = function(x) {
    r <- log(rasterTransform[[x]] + 1)
    names(r) <- namesTransform[[x]]
    r
  }
)
names(transformed) <- namesTransform

transformed[['LinearDist']] <-
  resample(transformed[['LinearDist']], transformed[['Water_Dist']])

transformed[['WaterDist']] <-
  resample(transformed[['WaterDist']], transformed[['Water_Dist']])


### Output ----
outRaster <- c(transformed,
               cropRasters[!(lapply(cropRasters, names) %in% namesTransform)])
outNames <- lapply(outRaster, names)

lapply(
  seq_along(outRaster),
  FUN = function(r) {
    writeRaster(
      outRaster[[r]],
      paste0('output/1-data-prep/covariates/NL/prep', outNames[[r]]),
      format = 'GTiff',
      overwrite = TRUE
    )
  }
)
