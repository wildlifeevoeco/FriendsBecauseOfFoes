### Elk data RSF values extraction ----
# Authors: Alec Robitaille, Sana Zabihi, Christina M Prokopenko
# Purpose: Extract elk and wolf RSF values to elk GPS data
# Inputs: Elk relocation data, Elk RSF raster, Wolf RSF raster
# Outputs: Elk relocation data with corresponding RSF values at the points (csv)



# TODO: rename script/folder to domain? domain extraction? etc
### Packages ----
libs <- c('data.table',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4',
          'ggplot2','car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
elk <- readRDS('output/1-data-prep/elk.Rds')

#RSFs

# RSFs
wolfwinter.rsf <- raster('output/predator-rsf/wolfrsfWinter.tif')
wolfspring.rsf <- raster('output/predator-rsf/wolfrsfSpring.tif')
elkwinter.rsf <- raster('output/prey-rsf/elkrsfWinter.tif')
elkspring.rsf <- raster('output/prey-rsf/elkrsfSpring.tif')

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
saveRDS(elk, 'output/rsfvalues/elkRsfValues.Rds')
