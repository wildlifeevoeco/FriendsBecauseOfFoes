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


### Variables ----
elk[, rowID := rleid(EASTING)]

qplot(EASTING, NORTHING, color = absAngle, data = elk)

source('R/functions/StepLength.R')
StepLength(elk, 'id', datetimeCol = 'datetime', yrCol = 'yr',
           xCol = coordCols[1], yCol = coordCols[2],
           returnIntermediate = TRUE)

elk[, absAngle := atan2(difY, difX) * 180 / pi]
elk[absAngle < 0, absAngle := absAngle + 360]

ggplot(elk, aes(EASTING, NORTHING)) + 
  geom_path(aes(color = absAngle)) #+ 
  # geom_path()
elk[, hist(absAngle)]

# Pseudo code

# Shift points 1 row

# atan2

# Ensure it is by ID


atan2(dist(x), diff_x(x)) * 180 / pi
locs[1:4, .(EASTING, amt:::diff_rcpp(EASTING),
            NORTHING, amt:::diff_rcpp(NORTHING),
            atan2(amt:::diff_rcpp(NORTHING), amt:::diff_rcpp(EASTING)) * 180 / pi)]
qplot(EASTING, NORTHING, color = V1, data = locs[1:4])

atan2(amt:::diff_rcpp(locs$NORTHING), amt:::diff_rcpp(locs$EASTING)) * 180 / pi

x <- amt::mk_track(locs[COLLAR_ID == 1810], X_COORD, Y_COORD, timeGroup)
amt::as_sp()

library(amt)
xx <- sp::coordinates(as_sp(x))
c((450 + ((360 - geosphere::bearing(xx[-nrow(xx), ], xx[-1, ]))) %% 360) %% 360, NA)

locs[1:4, 
     {print(qplot(EASTING, NORTHING, color = V1))
       atan2(amt:::diff_rcpp(NORTHING), amt:::diff_rcpp(EASTING)) * 180 / pi}
     ]
