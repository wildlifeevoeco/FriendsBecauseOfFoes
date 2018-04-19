### Calculate Turn Angles ----
# Authors: Alec Robitaille
# Started: March 15 2018
# Purpose: Calculate absolute and relative turn angle 
#          between locs of an individual
# Inputs: Elk, caribou relocation data
# Outputs: Abs TA data
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Theory ====
# Math:
# arctan(dif-y, dif-x) * 180 / pi

### Packages ----
libs <- c('data.table', 'ggplot2',
          'SearchTrees',
          'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'elk'
DT <- readRDS(paste0('output/rsf-values/', species, 'RsfValues.Rds'))

# List relevant column names
coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'

### Variables ----
DT[, rowID := rleid(EASTING)]

### Calculate Absolute Angle ----
source('R/0-functions/AbsoluteAngle.R')
AbsoluteAngle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

source('R/0-functions/RelativeAngle.R')
RelativeAngle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

### Output ----
saveRDS(DT, paste0('output/angles/', species, 'Angle.Rds'))

### Figures ----
qplot(absAngle, data = DT)