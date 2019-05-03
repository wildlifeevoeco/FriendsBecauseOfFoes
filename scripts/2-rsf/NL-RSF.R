message('=== NL RSF ===')
# Bear, caribou, coyote
# Authors: Michel Laforge, Alec Robitaille


### Packages ----
pkgs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc', 'rgeos', 'lme4', 'car','piecewiseSEM')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Set variables ----
source('scripts/0-variables/variables.R')

rasterOptions(tmpdir = 'output/2-rsf/temp')

### Input data ----
# Which species?
# Flexible for Makefile: if running script manually, edit species in else block
if (length(commandArgs(trailingOnly = TRUE) > 1)) {
  species <- commandArgs(trailingOnly = TRUE)[2]
  print(paste0('using species: ', species))
} else {
  species <- 'bear'
}

DT <- readRDS(paste0('output/1-data-prep/', species, '.Rds'))
caribou <- readRDS(paste0('output/1-data-prep/caribou.Rds'))

if (truelength(DT) == 0) alloc.col(DT)

# Covariates
rpath <- 'output/1-data-prep/covariates/NL'
covers <- gsub(".tif|100|prep", "", dir(rpath, '.tif$'))
paths <- dir(rpath, '.tif$', full.names = TRUE)

dropCovers <- which(covers %in% c('Anthro', 'Broadleaf', 'MixedWood', 'Conifer'))

lsCovers <- covers[-dropCovers]
lsPaths <- paths[-dropCovers]
names(lsPaths) <- lsCovers

### Processing ----
points <- SpatialPoints(DT[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmNL))
mcps <- mcp(points, percent = 100)

# Create Regular Grid
# TODO: size of grid?
gridsize <- data.table(
  sp = c('bear', 'coyote', 'caribou'),
  dist = c(210, 700, 75)
)[sp == species]$dist

regPts <- generate_grid(
  pol = mcps, 
  spacing = gridsize, 
  crs = utmNL
)
setnames(regPts, c('EASTING', 'NORTHING'))

nrow(regPts) / nrow(DT)

# Combine observed and regular grid points
regPts[, observed := 0]
DT[, observed := 1]

# Add fake season to regular grid 'grid'
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
  lsRasters,
  FUN = function(r) {
    extract(r, matrix(c(EASTING, NORTHING), ncol = 2))
  }
)]


### RSF ----
# Remove all points with <50% landcover sampled
not <- which(lsCovers %in% c('Ruggedness' ,'WaterDist', 'LinearDist'))
samplePts[, lcNA := rowSums(.SD), .SDcols = lsCovers[-not]]

samplePts <- samplePts[lcNA > 0.5]

# Spring RSF
springPts <- samplePts[season == "spring" | season == 'grid']
springPts[observed == 0, season := "spring"]

springRSF <- glm(reformulate(lsCovers, response = 'observed'),
                 family = 'binomial',
                 data = springPts)

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

# Winter RSF
winterPts <- samplePts[season == "winter" | season == 'grid']
winterPts[season == 'grid', season := "winter"]

#TODO: warning glm.fit: fitted probabilities numerically 0 or 1 occurred  (bear winter, caribou winter)
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
  stop('names dont match, check coef and rasters')
}


### Rescale RSFs ----
q <- 0.999

winterQ <- quantile(winterRaster, q)
winterRaster[winterRaster > winterQ] <- winterQ

winterScaled <-
  (winterRaster - (cellStats(winterRaster, min))) / (quantile(winterRaster, q) - (cellStats(winterRaster, min)))

springQ <- quantile(springRaster, q)
springRaster[springRaster > springQ] <- springQ

springScaled <-
  (springRaster - (cellStats(springRaster, min))) / (cellStats(springRaster, max) - (cellStats(springRaster, min)))


### Crop ----
outCrop <- mcp(caribou, percent = 100)

winterCrop <- crop(winterRaster, outCrop)
springCrop <- crop(springRaster, outCrop)

### Output ----
# Save the RSFs
rsfs <- list('Winter' = winterCrop, 'Spring' = springCrop)
path <- 'output/2-rsf/'

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

# Remove temporary files
file.remove(dir('output/2-rsf/temp', full.names = TRUE))

message('=== ', toupper(species), ' RSF COMPLETE ===')
