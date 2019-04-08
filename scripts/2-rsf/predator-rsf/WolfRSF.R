### Wolf RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: Determine habitat domain for wolves using second order RSFs
# Inputs: Wolf relocation data, habitat layers as rasters
# Outputs: Wolf RSF results for global model used to create winter and spring rasters as TIFF
# Project: Easter Week Challenge 2018


### Packages ----
libs <- c('data.table', 'ewc',          
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4', 'car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Animal locations
wolf <- readRDS('output/1-data-prep/wolf.Rds')

# Covariates
lsCovers <- gsub(".tif|100m", "", 
                 dir('output/1-data-prep/covariates/RMNP', '.tif$'))
lsPaths <- dir('output/1-data-prep/covariates/RMNP', '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers

rmList <- which(lsCovers %in% c('Agriculture', 'Deciduous', 'Grassland'))

lsCovers <- lsCovers[-rmList]
lsPaths <- lsPaths[-rmList]

### Processing ----
# MCPs
wolfSP <- SpatialPoints(wolf[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

wolfMCP <- mcp(wolfSP, 100)

# Create Regular Grid
regPts <- generate_grid(wolfMCP, 90, crs = utm)

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
winterwolf <- samplePts[season == "winter" | season == 'grid']
winterwolf[season == 'grid', season := "winter"]

# TODO: Dist not logged yet - should it be?
winterwolfRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                    family = 'binomial',data = winterwolf)

summary(winterwolfRSF)
vif(winterwolfRSF)
rsquared(winterwolfRSF)

# Pull out the coefficients, dropping the intercept
winwolf.b <- coef(winterwolfRSF)[-1]


# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterwolfRSF)[1]

if (all(names(winwolf.b) == names(lsRasters))) {
  winterwolfRSF.rstr <-
    exp(intercept + Reduce('+', Map('*', winwolf.b, lsRasters)))
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
sprwolf.b <- coef(springwolfRSF)[-1]

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterwolfRSF)[1]

if (all(names(winwolf.b) == names(lsRasters))) {
  springwolfRSF.rstr <-
    exp(intercept + Reduce('+', Map('*', sprwolf.b, lsRasters)))
} else {
  stop('names dont match, check coef and rasters')
}

### Standardize RSFs ----
# Using feature scaling
winterwolfRSF.s <-
  (winterwolfRSF.rstr - (cellStats(winterwolfRSF.rstr, min))) / (cellStats(winterwolfRSF.rstr, max) - (cellStats(winterwolfRSF.rstr, min)))

springwolfRSF.s <-
  (springwolfRSF.rstr - (cellStats(springwolfRSF.rstr, min))) / (cellStats(springwolfRSF.rstr, max) - (cellStats(springwolfRSF.rstr, min)))


### Output ----
# Save the RSFs
ls.rsf <- list('Winter' = winterwolfRSF.s,
               'Spring' = springwolfRSF.s)
lapply(
  seq_along(ls.rsf),
  FUN = function(r) {
    writeRaster(
      ls.rsf[[r]],
      paste0('output/2-rsf/wolf/wolfrsf', names(ls.rsf[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)

# Regular points
saveRDS(regPts, 'output/2-rsf/wolf/wolfRegularPoints.Rds')

# Sample pts
saveRDS(samplePts, 'output/2-rsf/wolf/wolfSamplePoints.Rds')
