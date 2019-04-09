### Elk data RSF values extraction ----
# Authors: Alec Robitaille, Sana Zabihi, Christina M Prokopenko


# TODO: rename script/folder to domain? domain extraction? etc
### Packages ----
libs <- c('data.table', 'raster')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
elk <- readRDS('output/1-data-prep/elk.Rds')

# RSFs
wolfwinter.rsf <- raster('output/2-rsf/wolf/wolfrsfWinter.tif')
wolfspring.rsf <- raster('output/2-rsf/wolf/wolfrsfSpring.tif')
elkwinter.rsf <- raster('output/2-rsf/elk/elkrsfWinter.tif')
elkspring.rsf <- raster('output/2-rsf/elk/elkrsfSpring.tif')

lsrasters <- list(wolfwinter.rsf, wolfspring.rsf, elkwinter.rsf, elkspring.rsf)
lsnames <- c('wolfwinter', 'wolfspring', 'elkwinter', 'elkspring')

### Sampling ----
# Sample rasters
elk[, (lsnames) := lapply(lsrasters, FUN = function(r){
  extract((r), matrix(c(EASTING, NORTHING), ncol = 2))})]

elk[season == 'winter', predatorRSF := wolfwinter]
elk[season == 'winter', preyRSF := elkwinter]
elk[season == 'spring', predatorRSF := wolfspring]
elk[season == 'spring', preyRSF := elkspring]

elk[, (lsnames) := NULL]

### Save output ----
saveRDS(elk, 'output/3-extraction/elkRsfValues.Rds')
