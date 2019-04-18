### Elk data RSF values extraction ----
# Authors: Alec Robitaille, Sana Zabihi, Christina M Prokopenko


### Packages ----
pkgs <- c('data.table', 'raster')
lapply(pkgs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Animal locations
DT <- readRDS('output/1-data-prep/elk.Rds')

# RSFs
wolfWinter <- raster('output/2-rsf/wolf/wolfrsfWinter.tif')
wolfSpring <- raster('output/2-rsf/wolf/wolfrsfSpring.tif')
elkWinter <- raster('output/2-rsf/elk/elkrsfWinter.tif')
elkSpring <- raster('output/2-rsf/elk/elkrsfSpring.tif')

rasters <- list(wolfWinter, wolfSpring, elkWinter, elkSpring)
names <- c('wolfwinter', 'wolfspring', 'elkwinter', 'elkspring')

### Sampling ----
DT[, (names) := lapply(rasters, FUN = function(r){
  extract(r, matrix(c(EASTING, NORTHING), ncol = 2))})]

DT[season == 'winter', predatorRSF := wolfwinter]
DT[season == 'winter', preyRSF := elkwinter]
DT[season == 'spring', predatorRSF := wolfspring]
DT[season == 'spring', preyRSF := elkspring]

DT[, (names) := NULL]

### Save output ----
saveRDS(DT, 'output/3-extraction/elkRsfValues.Rds')
