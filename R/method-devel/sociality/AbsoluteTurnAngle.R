### Absolute Turn Angle ----
# Authors: Alec Robitaille
# Started: March 15 2018
# Purpose: Calculate absolute turn angle between locs of an individual
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
elk <- readRDS('output/data-prep/elk.Rds')

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'


### Variables ----
elk[, rowID := rleid(EASTING)]


### Calculate Absolute Angle ----
source('R/functions/AbsoluteAngle.R')
AbsoluteAngle(elk, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)


### Figures ----
qplot(absAngle, data = elk)