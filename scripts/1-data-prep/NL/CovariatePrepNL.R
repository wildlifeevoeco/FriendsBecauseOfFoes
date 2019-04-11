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
rasterOptions(tmpdir = "output/2-rsf/temp")

# Covariates
lsCovers <- gsub(".tif|100", "", dir('input/covariates/NL', '.tif$'))
lsPaths <- dir('input/covariates/NL', '.tif$', full.names = TRUE)


lsRasters <- lapply(lsPaths, raster)
names(lsRasters) <- lsCovers

### Processing ----
# Log transform
namesTransform <- c('LinearDist', 'WaterDist')
whichTransform <- which(names(lsRasters) %in% namesTransform)
rasterTransform <- lsRasters[whichTransform]

transformed <- lapply(
  rasterTransform,
  FUN = function(x) {
    log(x + 1)
  }
)

lsRasters[whichTransform] <- transformed


# Crop the rasters
cropRasters <- lapply(
  lsRasters,
  FUN = function(r) {
    crop(r, nlBounds)
  }
)



reLin <- 
  resample(cropRasters[['LinearDist']], cropRasters[['Anthro']], method = 'bilinear')

transformed[['WaterDist']] <-
  resample(transformed[['WaterDist']], cropRasters[['Anthro']], method = 'bilinear')






### Output ----
outRaster <- c(transformed,
               cropRasters[!(names(cropRasters) %in% namesTransform)])
outNames <- lapply(outRaster, names)
lapply(outRaster, origin)
#TODO: why non matching origin
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
