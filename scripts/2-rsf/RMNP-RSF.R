### RMNP RSF ----
# Elk, wolf
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi


### Packages ----
pkgs <- c('data.table', 'ewc',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4')
lapply(pkgs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')

rasterOptions(tmpdir = 'output/2-rsf/temp')


### Input data ----
# Which species?
# Flexible for Makefile: if running script manually, edit species in else block
if (length(commandArgs(trailingOnly = TRUE) > 1)) {
  species <- tolower(commandArgs(trailingOnly = TRUE)[2])
  print(paste0('using species: ', species))
} else {
  species <- 'elk'
}

DT <- readRDS(paste0('output/1-data-prep/', species, '.Rds'))

if (truelength(DT) == 0) alloc.col(DT)


# Covariates
rpath <- 'output/1-data-prep/covariates/RMNP' 
covers <- gsub(".tif|100m", "", dir(rpath, '.tif$'))
paths <- dir(rpath, '.tif$', full.names = TRUE)
names(paths) <- covers

rmList <- which(covers %in% c('Agriculture', 'Deciduous', 'Grassland'))

if (length(rmList) == 0) {
  lsCovers <- covers
  lsPaths <- paths
} else {
  lsCovers <- covers[-rmList]
  lsPaths <- paths[-rmList]
}

### Processing ----
# MCPs
points <- SpatialPoints(DT[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmMB))

mcps <- mcp(points, 100)

# Create Regular Grid
# TODO: number of regular points?
regPts <- generate_grid(pol = mcps, spacing = 90, crs = utmMB)
setnames(regPts, c('EASTING', 'NORTHING'))


# Combine observed and regular grid points
regPts[, observed := 0]
DT[, observed := 1]

regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, DT), 
                       use.names = TRUE, fill = TRUE)

### Sampling ----
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING', 'season', 'observed')
samplePts <- samplePts[, ..cols]

# Add row ID
samplePts[, rowID := .I]

# Sample rasters
samplePts[, (lsCovers) := lapply(
  lsPaths,
  FUN = function(r) {
    extract(raster(r), matrix(c(EASTING, NORTHING), ncol = 2))
  }
)]

### RSF ----
lsRasters <- lapply(lsPaths, function(r) crop(raster(r), mcps))

## Winter RSF
winterPts <- samplePts[season == "winter" | season == 'grid']
winterPts[season == 'grid', season := "winter"]

winterRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                    family = 'binomial',data = winterPts)

# Pull out the coefficients, dropping the intercept
winterCoefs <- coef(winterRSF)[-1]

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterRSF)[1]

if (all(names(winterCoefs) == names(lsRasters))) {
  winterRaster <-
    exp(intercept + Reduce('+', Map('*', winterCoefs, lsRasters)))
} else {
  stop('names dont match, check coefs and rasters')
}


## Spring RSF
springPts <- samplePts[season == "spring" | season == 'grid']
springPts[observed == 0, season := "spring"]

springRSF <- glm(reformulate(lsCovers, response = 'observed'), 
       family = 'binomial',data = springPts)

# Pull out the coefficients, dropping the intercept
springCoefs <- coef(springRSF)[-1] 

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(springRSF)[1]

if (all(names(springCoefs) == names(lsRasters))) {
  springRaster <-
    exp(intercept + Reduce('+', Map('*', springCoefs, lsRasters)))
} else {
  stop('names dont match, check coef and rasters')
}


### Standardize RSFs ----
# Using feature scaling
winterScaled <-
  (winterRaster - (cellStats(winterRaster, min))) / (cellStats(winterRaster, max) - (cellStats(winterRaster, min)))

springScaled <-
  (springRaster - (cellStats(springRaster, min))) / (cellStats(springRaster, max) - (cellStats(springRaster, min)))


### Output ----
# Save the RSFs 
rsfs <- list('Winter' = winterScaled,
               'Spring' = springScaled)

path <- paste0('output/2-rsf/')

lapply(
  seq_along(rsfs),
  FUN = function(r) {
    writeRaster(
      rsfs[[r]],
      paste0(path, 'rasters/', species, 'rsf', names(rsfs[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)

# Regular points
saveRDS(regPts, paste0(path, 'points/', species, 'RegularPoints.Rds'))

# Sample pts
saveRDS(samplePts, paste0(path, 'points/', species, 'SamplePoints.Rds'))

# RSF
saveRDS(springRSF, paste0(path, 'models/', species, 'SpringModel.Rds'))
saveRDS(winterRSF, paste0(path, 'models/', species, 'WinterModel.Rds'))
