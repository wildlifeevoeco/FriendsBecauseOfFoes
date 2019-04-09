### Caribou RSF ----
# Authors: Michel Laforge, Alec Robitaille

### Packages ----
libs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc', 'rgeos', 'lme4', 'car','piecewiseSEM')
# libs <- c('adehabitatHR', 'rgeos', 'spatstat', 'raster',
#           'ewc', 
#           'data.table', 'piecewiseSEM', 'rgdal')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
caribou <- readRDS('output/1-data-prep/caribou.Rds')

# Covariates
covers <- gsub(".tif|100|prep", "", 
               dir('output/1-data-prep/covariates/NL', '.tif$'))
paths <- dir('output/1-data-prep/covariates/NL', 
             '.tif$', full.names = TRUE)
names(paths) <- covers

rmList <- which(covers %in% c('Water', 'NLElev', 'Wetland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]


### Processing ----
points <- SpatialPoints(caribou[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmNL))
width <- caribou[, mean(stepLength)]
buffers <- buffer(points, width = width)
intersect <- gIntersection(nlBounds, gUnaryUnion(buffers))

buffered <- buffer(points, width = caribou[, mean(stepLength)])

# Create Regular Grid
regPts <- generate_grid(intersect, 90, crs = utmNL)
setnames(regPts, c('EASTING', 'NORTHING'))

# Combine observed and regular grid points
regPts[, observed := 0]
caribou[, observed := 1]

# Add fake season to regular grid 'grid'
regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, caribou), 
                       use.names = TRUE, fill = TRUE)


### Sampling ----
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING', 'season', 'observed')
samplePts <- samplePts[, ..cols]

# Add row ID
samplePts[, rowID := .I]

# Sample rasters
lsRasters <- lapply(lsPaths, raster)

samplePts[, (lsCovers) := lapply(
  lsRasters,
  FUN = function(r) {
    extract(r, matrix(c(EASTING, NORTHING), ncol = 2))
  }
)]


### RSF ----
# TODO: Remove all points with 50% NA data


# Winter RSF
# TODO: need ruggedness
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
# TODO: need ruggedness
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
      paste0('output/2-rsf/caribou/cariboursf', names(rsfs[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)

# Regular points
saveRDS(regPts, 'output/2-rsf/caribou/caribouRegularPoints.Rds')

# Sample pts
saveRDS(samplePts, 'output/2-rsf/caribou/caribouSamplePoints.Rds')
