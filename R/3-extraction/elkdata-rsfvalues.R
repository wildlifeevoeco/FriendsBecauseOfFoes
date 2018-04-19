### Elk data RSF values extraction ----
# Authors: Alec Robitaille, Sana Zabihi, Christina M Prokopenko
# Purpose: Extract elk and wolf RSF values to elk GPS data
# Inputs: Elk relocation data, Elk RSF raster, Wolf RSF raster
# Outputs: Elk relocation data with corresponding RSF values at the points (csv)
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

### Packages ----
libs <- c('data.table', 'magrittr',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4',
          'ggplot2','car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Animal locations
elk <- readRDS('output/data-prep/elk.Rds')

#RSFs

# RSFs
wolfwinter.rsf <- raster('output/predator-rsf/wolfrsfWINTER.tif')
wolfspring.rsf <- raster('output/predator-rsf/wolfrsfSPRING.tif')
elkwinter.rsf <- raster('output/prey-rsf/elkrsfWINTER.tif')
elkspring.rsf <- raster('output/prey-rsf/elkrsfSPRING.tif')

lsrasters <- list(wolfwinter.rsf, wolfspring.rsf, elkwinter.rsf, elkspring.rsf)
lsnames <- c('wolfwinter', 'wolfspring', 'elkwinter', 'elkspring')

### Sampling ----
# Sample rasters
elk[, (lsnames) := lapply(lsrasters, FUN = function(r){
  extract((r), matrix(c(EASTING, NORTHING), ncol = 2))})]

### Save output ----
saveRDS(elk, 'output/rsfvalues/elkRsfValues.Rds')
