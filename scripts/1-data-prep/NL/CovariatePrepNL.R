### Covariate (Raster) Processing ----
# Authors: Michel Laforge, Alec Robitaille

### Packages ----
libs <- c('data.table', 'sp', 'raster')
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
covers <- gsub(".tif|100", "", dir('input/covariates/NL', '.tif$'))
paths <- dir('input/covariates/NL', '.tif$', full.names = TRUE)


rmList <- which(covers %in% c('Water', 'NLElev', 'Wetland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]

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


# Crop (and mask) the rasters
cropRasters <- lapply(
  lsRasters,
  FUN = function(r) {
    mask(crop(r, extent(nlBounds)), nlBounds)
  }
)

beginCluster()
cropRasters[['LinearDist']] <- 
  projectRaster(cropRasters[['LinearDist']], cropRasters[['Anthro']], method = 'bilinear')

cropRasters[['WaterDist']] <-
  projectRaster(transformed[['WaterDist']], cropRasters[['Anthro']], method = 'bilinear')
endCluster()


### Output ----
outNames <- lapply(cropRasters, names)

#TODO: why non matching origin

lapply(
  seq_along(cropRasters),
  FUN = function(r) {
    writeRaster(
      cropRasters[[r]],
      paste0('output/1-data-prep/covariates/NL/prep', outNames[[r]]),
      format = 'GTiff',
      overwrite = TRUE
    )
  }
)
