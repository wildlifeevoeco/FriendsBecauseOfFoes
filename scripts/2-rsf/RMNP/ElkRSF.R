### Elk RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: Determine habitat domain for elk using second order RSFs
# Inputs: Elk relocation data, habitat layers as rasters
# Outputs: Elk RSF results for global model used to create winter and spring rasters as TIFF
# Project: Easter Week Challenge 2018


### Packages ----
libs <- c('data.table', 'ewc',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4', 'car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
elk <- readRDS('output/1-data-prep/elk.Rds')


# Covariates
lsCovers <- gsub(".tif|100m", "", 
                 dir('output/1-data-prep/covariates/RMNP', '.tif$'))
lsPaths <- dir('output/1-data-prep/covariates/RMNP', 
               '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers

rmList <- which(lsCovers %in% c('Agriculture', 'Deciduous', 'Grassland'))

lsCovers <- lsCovers[-rmList]
lsPaths <- lsPaths[-rmList]

### Processing ----
# MCPs
elkSP <- SpatialPoints(elk[, .(EASTING, NORTHING)],
                       proj4string = CRS(utmMB))

elkMCP <- mcp(elkSP, 100)

# Create Regular Grid
regPts <- generate_grid(pol = elkMCP, spacing = 90, crs = utmMB)
setnames(regPts, c('EASTING', 'NORTHING'))

# Combine observed and regular grid points
regPts[, observed := 0]
elk[, observed := 1]

regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, elk), 
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

## Winter RSF
winterElk <- samplePts[season == "winter" | season == 'grid']
winterElk[season == 'grid', season := "winter"]

winterElkRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                    family = 'binomial',data = winterElk)

summary(winterElkRSF)
vif(winterElkRSF)
rsquared(winterElkRSF)

# Pull out the coefficients, dropping the intercept
winElk.b <- coef(winterElkRSF)[-1]

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterElkRSF)[1]

if (all(names(winElk.b) == names(lsRasters))) {
  winterElkRSF.rstr <-
    exp(intercept + Reduce('+', Map('*', winElk.b, lsRasters)))
} else {
  stop('names dont match, check coef and rasters')
}


mapview::mapview(winterElkRSF.rstr)


## Spring RSF
# TODO: why is.na season?
springElk <- samplePts[season == "spring" | is.na(season)]
springElk[observed == 0, season := "spring"]

springElkRSF <- glm(reformulate(lsCovers, response = 'observed'), 
       family = 'binomial',data = springElk)

summary(springElkRSF)
rsquared(springElkRSF)

# Pull out the coefficients, dropping the intercept
sprElk.b <- coef(springElkRSF)[-1] 

# Create the raster matching the first raster layer with the first fixed effect
# TODO: with intercept of -2.053773?
springElkRSF.rstr <-
  exp(
    -2.053773 + lsRasters[[1]] * sprElk.b[1] + lsRasters[[2]] * sprElk.b[2] +
      lsRasters[[3]] * sprElk.b[3] + lsRasters[[4]] * sprElk.b[4] +
      lsRasters[[5]] * sprElk.b[5] + lsRasters[[6]] * sprElk.b[6] +
      lsRasters[[7]] * sprElk.b[7] + lsRasters[[8]] * sprElk.b[8] +
      lsRasters[[9]] * sprElk.b[9]
  )

plot(springElkRSF.rstr)

### Standardize RSFs ----
# Using feature scaling
winterElkRSF.s <-
  (winterElkRSF.rstr - (cellStats(winterElkRSF.rstr, min))) / (cellStats(winterElkRSF.rstr, max) - (cellStats(winterElkRSF.rstr, min)))

springElkRSF.s <-
  (springElkRSF.rstr - (cellStats(springElkRSF.rstr, min))) / (cellStats(springElkRSF.rstr, max) - (cellStats(springElkRSF.rstr, min)))


### Output ----
saveRDS(regPts, 'output/2-rsf/elk/elkRegularPoints.Rds')

saveRDS(samplePts, 'output/2-rsf/elk/elkSamplePoints.Rds')

# Save the RSFs 
ls.rsf <- list('WINTER' = winterElkRSF.s,
               'SPRING' = springElkRSF.s)

lapply(
  seq_along(ls.rsf),
  FUN = function(r) {
    writeRaster(
      ls.rsf[[r]],
      paste0('output/prey-rsf/elkrsf', names(ls.rsf[r])),
      format = 'GTiff',
      overwrite = T
    )
  }
)