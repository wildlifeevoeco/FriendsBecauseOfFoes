### Caribou data RSF values extraction ----
# Authors: Alec Robitaille, Michel Laforge


# TODO: rename script/folder to domain? domain extraction? etc
### Packages ----
pkgs <- c('data.table', 'raster')
lapply(pkgs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
caribou <- readRDS('output/1-data-prep/caribou.Rds')

# RSFs
bearSpring <- raster("output/2-rsf/bear/bearSummer.tif")
caribouSpring <- raster("output/2-rsf/caribou/CaribouSummer.tif")
caribouWinter <- raster("output/2-rsf/caribou/CaribouWinter.tif")
coyoteSpring <- raster("output/2-rsf/coyote/CoyoteSummer.tif")
coyoteWinter <- raster("output/2-rsf/coyote/CoyoteWinter.tif")


rasters <- list(bearSpring, caribouSpring, caribouWinter, coyoteSpring, coyoteWinter)
names <- c('bearspring', 'caribouspring', 'caribouwinter', 'coyotespring', 'coyotewinter')

### Sampling ----
# Sample rasters
elk[, (names) := lapply(rasters, FUN = function(r){
  extract(r, matrix(c(EASTING, NORTHING), ncol = 2))})]

elk[season == 'spring', bearRSF := bearspring]
elk[season == 'spring', caribouRSF := caribouspring]
elk[season == 'winter', caribouRSF := caribouwinter]
elk[season == 'spring', coyoteRSF := coyotespring]
elk[season == 'winter', coyoteRSF := coyotewinter]

elk[, (names) := NULL]

### Save output ----
saveRDS(elk, 'output/3-extraction/caribouRsfValues.Rds')