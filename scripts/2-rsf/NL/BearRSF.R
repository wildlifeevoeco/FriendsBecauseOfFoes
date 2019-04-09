### Bear RSF ----
# Authors: Michel Laforge, Alec Robitaille


### Packages ----
libs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc', 'rgeos')
# libs <- c('adehabitatHR', 'rgeos', 'spatstat', 'raster',
#           'ewc', 
#           'data.table', 'piecewiseSEM', 'rgdal')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
bear <- readRDS('output/1-data-prep/bear.Rds')

# Covariates
lsCovers <- gsub(".tif|100m|prep", "", 
               dir('output/1-data-prep/covariates/NL', '.tif$'))

lsPaths <- dir('output/1-data-prep/covariates/NL', 
             '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers


### Processing ----
points <- SpatialPoints(bear[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmNL))

CarAvailBuf <- gBuffer(points, width = bear[, mean(stepLength)])

Carclipped<-gIntersection(nlBounds,CarAvailBuf)
Carclipped2<-as.owin.SpatialPolygons(Carclipped)



mcps <- mcp(points, percent = 90)

mapview::mapview(mcps) + mapview::mapview(points)
# Create Regular Grid
regPts <- generate_grid(mcps, 90, crs = utmNL)
setnames(regPts, c('EASTING', 'NORTHING'))

# Combine observed and regular grid points
regPts[, observed := 0]
bear[, observed := 1]

# Add fake season to regular grid 'grid'
regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, bear), 
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
## Remove all points with 50% NA data


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
# RSFbearSum<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=bearSummer, family='binomial')
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
