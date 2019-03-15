### Calculate Turn Angles ----
# Authors: Alec Robitaille
# Started: March 15 2018
# Purpose: Calculate absolute and relative turn angle 
#          between locs of an individual
# Inputs: Elk, caribou relocation data
# Outputs: Abs TA data
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

#TODO: Grab Hance's updated social metrics from modeling script

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
DT[, rowID := rleid(EASTING), by = c("id", "yr")]

### Calculate Absolute Angle ----

calc_abs_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

#TODO: check that yrcol is well handled
calc_rel_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

### Output ----
saveRDS(DT, paste0('output/angles/', species, 'Angle.Rds'))

### Figures ----
qplot(absAngle, data = DT)






### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'Caribou'
DT <- readRDS(paste0('output/rsf-values/', species, 'RSFVals.Rds'))

# List relevant column names
coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'

### Variables ----
DT[, rowID := rleid(EASTING), by = c(idCol, yrCol)]

### Calculate Absolute Angle ----

calc_abs_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)


calc_rel_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

### Output ----
saveRDS(DT, paste0('output/angles/', species, 'AngleNEW.Rds')) ### New coyote RSF

### Figures ----
qplot(absAngle, data = DT)
