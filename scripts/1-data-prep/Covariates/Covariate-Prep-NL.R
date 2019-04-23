### Covariate (Raster) Processing ----
# Authors: Michel Laforge, Alec Robitaille

### Packages ----
pkgs <- c('data.table', 'sp', 'raster')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


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
rasterOptions(tmpdir = "output/1-data-prep/covariates/temp")

# Covariates
covers <- gsub(".tif|100", "", dir('input/covariates/NL', '.tif$'))
paths <- dir('input/covariates/NL', '.tif$', full.names = TRUE)


rmList <- which(covers %in% c('Water', 'NLElev', 'Wetland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]

lsRasters <- lapply(lsPaths, raster)
names(lsRasters) <- lsCovers


### Processing ----
# Resample non matching rasters
beginCluster()
linDist <- resample(lsRasters[['LinearDist']], lsRasters[['Anthro']])
rugged <- resample(lsRasters[['Ruggedness']], lsRasters[['Anthro']])
endCluster()

lsRasters[['Ruggedness']] <- rugged


# Log transform
namesTransform <- c('LinearDist', 'WaterDist')
whichTransform <- which(names(lsRasters) %in% namesTransform)
rasterTransform <- c(linDist, lsRasters[['WaterDist']])

transformed <- lapply(
  rasterTransform,
  FUN = function(x) {
    log(x + 1)
  }
)

lsRasters[whichTransform] <- transformed


# Find communal smallest extent
lsExtents <- lapply(c(nlBounds = nlBounds, lsRasters), extent)
minExtent <- Reduce(intersect, lsExtents)


# Mask and crop the rasters
cropRasters <- lapply(lsRasters, crop, minExtent)

### Output ----
lapply(
  seq_along(cropRasters),
  FUN = function(r) {
    writeRaster(
      cropRasters[[r]],
      paste0('output/1-data-prep/covariates/NL/prep', names(cropRasters)[r]),
      format = 'GTiff',
      overwrite = TRUE
    )
  }
)
