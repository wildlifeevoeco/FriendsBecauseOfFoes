### Caribou data RSF values extraction ----
# Authors: Alec Robitaille, Michel Laforge


### Packages ----
pkgs <- c('data.table', 'raster')
lapply(pkgs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
DT <- readRDS('output/1-data-prep/caribou.Rds')

# RSFs
rsfPath <- 'output/2-rsf/'

bearSpring <- raster(paste0('bear/bearSummer.tif'))
# bearWinter <- raster(paste0('bear/bearWinter.tif'))
caribouSpring <- raster(paste0('caribou/caribouSummer.tif'))
caribouWinter <- raster(paste0('caribou/caribouWinter.tif'))
coyoteSpring <- raster(paste0('coyote/coyoteSpring.tif'))
coyoteWinter <- raster(paste0('coyote/coyoteWinter.tif'))


rasters <-
  list(bearSpring,
       # bearWinter,
       caribouSpring,
       caribouWinter,
       coyoteSpring,
       coyoteWinter)

names <-
  c(
    'bearspring',
    # 'bearWinter',
    'caribouspring',
    'caribouwinter',
    'coyotespring',
    'coyotewinter'
  )

### Sampling ----
# Sample rasters
DT[, (names) := lapply(rasters, FUN = function(r){
  extract(r, matrix(c(EASTING, NORTHING), ncol = 2))})]

# Bears
DT[season == 'spring', bearRSF := bearspring]
# DT[season == 'winter', bearRSF := bearwinter]

# Caribou
DT[season == 'spring', caribouRSF := caribouspring]
DT[season == 'winter', caribouRSF := caribouwinter]

# Coyote
DT[season == 'spring', coyoteRSF := coyotespring]
DT[season == 'winter', coyoteRSF := coyotewinter]

# Predators
DT[season == 'spring', predatorRSF := coyotespring + bearspring]
DT[season == 'winter', predatorRSF := coyotewinter]

# Prey
DT[season == 'spring', preyRSF := caribouspring]
DT[season == 'winter', preyRSF := caribouwinter]

DT[, (names) := NULL]

### Save output ----
saveRDS(DT, 'output/3-extraction/caribouRsfValues.Rds')