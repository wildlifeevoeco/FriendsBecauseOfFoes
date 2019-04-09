### Wolf RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: Determine habitat domain for wolves using second order RSFs
# Inputs: Wolf relocation data, habitat layers as rasters
# Outputs: Wolf RSF results for global model used to create winter and spring rasters as TIFF



### Packages ----
libs <- c('data.table', 'ewc',          
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4', 'car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)

### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
wolf <- readRDS('output/1-data-prep/wolf.Rds')

# Covariates
covers <- gsub(".tif|100m", "", 
               dir('output/1-data-prep/covariates/RMNP', '.tif$'))
paths <- dir('output/1-data-prep/covariates/RMNP', 
             '.tif$', full.names = TRUE)
names(paths) <- covers

rmList <- which(covers %in% c('Agriculture', 'Deciduous', 'Grassland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]

### Processing ----
# MCPs
points <- SpatialPoints(wolf[, .(EASTING, NORTHING)],
                       proj4string = CRS(utmMB))

mcps <- mcp(points, 100)

# Create Regular Grid
regPts <- generate_grid(mcps, 90, crs = utm)

setnames(regPts, c('EASTING', 'NORTHING'))

# Combine observed and regular grid points
regPts[, observed := 0]
wolf[, observed := 1]

# Add fake season to regular grid 'grid'
regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, wolf), 
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
lsRasters <- lapply(lsPaths, raster)

# Winter RSF
winterPts <- samplePts[season == "winter" | season == 'grid']
winterPts[season == 'grid', season := "winter"]

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
springwolf <- samplePts[season == "spring" | season == 'grid']
springwolf[observed == 0, season := "spring"]

springwolfRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                    family = 'binomial',data = springwolf)

summary(springwolfRSF)
vif(springwolfRSF)
rsquared(springwolfRSF)

# Pull out the coefficients, dropping the intercept
springCoefs <- coef(springwolfRSF)[-1]

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterwolfRSF)[1]

if (all(names(winterCoefs) == names(lsRasters))) {
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

lapply(
  seq_along(rsfs),
  FUN = function(r) {
    writeRaster(
      rsfs[[r]],
      paste0('output/2-rsf/wolf/wolfrsf', names(rsfs[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)

# Regular points
saveRDS(regPts, 'output/2-rsf/wolf/wolfRegularPoints.Rds')

# Sample pts
saveRDS(samplePts, 'output/2-rsf/wolf/wolfSamplePoints.Rds')
