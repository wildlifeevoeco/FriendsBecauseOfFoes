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
wolfWinter <- raster('output/2-rsf/wolf/wolfrsfWinter.tif')
wolfSpring <- raster('output/2-rsf/wolf/wolfrsfSpring.tif')
elkWinter <- raster('output/2-rsf/elk/elkrsfWinter.tif')
elkSpring <- raster('output/2-rsf/elk/elkrsfSpring.tif')

rasters <- list(wolfWinter, wolfSpring, elkWinter, elkSpring)
names <- c('wolfwinter', 'wolfspring', 'elkwinter', 'elkspring')

### Sampling ----
elk[, (names) := lapply(rasters, FUN = function(r){
  extract(r, matrix(c(EASTING, NORTHING), ncol = 2))})]

elk[season == 'winter', predatorRSF := wolfwinter]
elk[season == 'winter', preyRSF := elkwinter]
elk[season == 'spring', predatorRSF := wolfspring]
elk[season == 'spring', preyRSF := elkspring]

elk[, (names) := NULL]

### Save output ----
# TODO: rename output "rsfvalues"
saveRDS(elk, 'output/3-extraction/elkRsfValues.Rds')
