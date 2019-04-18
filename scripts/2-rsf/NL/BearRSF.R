### NL RSF ----
# Bear, caribou, coyote
# Authors: Michel Laforge, Alec Robitaille


### Packages ----
pkgs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc', 'rgeos', 'lme4', 'car','piecewiseSEM')
lapply(pkgs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Which species?
species <- 'bear'
DT <- readRDS(paste0('output/1-data-prep/', species, '.Rds'))

if (truelength(DT) == 0) alloc.col(DT)

# Covariates
lsCovers <- gsub(
  ".tif|100|prep",
  "",
  dir('output/1-data-prep/covariates/NL', '.tif$')
)
lsPaths <- dir('output/1-data-prep/covariates/NL',
               '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers

### Processing ----
points <- SpatialPoints(DT[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmNL))
mcps <- mcp(points, percent = 100)

# Create Regular Grid
regPts <- generate_grid(mcps, 210, crs = utmNL)
setnames(regPts, c('EASTING', 'NORTHING'))

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
lsRasters <- lapply(lsPaths, function(r) crop(raster(r), mcps))

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

# Winter RSF
winterPts <- samplePts[season == "winter" | season == 'grid']
winterPts[season == 'grid', season := "winter"]

#TODO: warning glm.fit: fitted probabilities numerically 0 or 1 occurred 
winterRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                 family = 'binomial',data = winterPts)

summary(winterRSF)
vif(winterRSF)
rsquared(winterRSF)

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

# Spring RSF
springPts <- samplePts[season == "spring" | season == 'grid']
springPts[observed == 0, season := "spring"]

springRSF <- glm(reformulate(lsCovers, response = 'observed'),
                 family = 'binomial',
                 data = springPts)

summary(springRSF)
vif(springRSF)
rsquared(springRSF)

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
rsfs <- list('Winter' = winterScaled, 'Spring' = springScaled)
path <- paste0('output/2-rsf/', species, '/')

lapply(
  seq_along(rsfs),
  FUN = function(r) {
    writeRaster(
      rsfs[[r]],
      paste0(path, species, 'rsf', names(rsfs[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)

# Regular points
saveRDS(regPts, paste0(path, 'RegularPoints.Rds'))

# Sample pts
saveRDS(samplePts, paste0(path, 'SamplePoints.Rds'))
