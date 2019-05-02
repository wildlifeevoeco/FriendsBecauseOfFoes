message('=== Caribou data RSF values extraction ===')
# Authors: Alec Robitaille, Michel Laforge


### Packages ----
pkgs <- c('data.table', 'raster')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
DT <- readRDS('output/1-data-prep/caribou.Rds')

# RSFs
bearSpring <- raster('output/2-rsf/rasters/bearrsfSpring.tif')

caribouWinter <- raster('output/2-rsf/rasters/cariboursfWinter.tif')
caribouSpring <- raster('output/2-rsf/rasters/cariboursfSpring.tif')
coyoteWinter <- raster('output/2-rsf/rasters/coyotersfWinter.tif')
coyoteSpring <- raster('output/2-rsf/rasters/coyotersfSpring.tif')


rasters <-
  list(bearSpring,
       caribouSpring,
       caribouWinter,
       coyoteSpring,
       coyoteWinter)

names <-
  c(
    'bearspring',
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

# Drop caribouRSF
DT[, 'caribouRSF' := NULL]


### Save output ----
saveRDS(DT, 'output/3-extraction/caribouExtract.Rds')
message('=== CARIBOU EXTRACTION COMPLETE ===')