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

### Variables ----
elk[, rowID := rleid(EASTING)]

# qplot(EASTING, NORTHING, color = absAngle, data = elk)

# source('R/functions/StepLength.R')
# StepLength(elk, 'id', datetimeCol = 'datetime', yrCol = 'yr',
#            xCol = coordCols[1], yCol = coordCols[2],
#            returnIntermediate = TRUE)

# Create lag and dif column names
lagCols <- paste('lag', coordCols, sep = '')
difCols <- c('difX', 'difY')

lagDateTimeCol <- paste0('lag', datetimeCol)
difDateTimeCol <- paste0('dif', datetimeCol)

elk[order(get(datetimeCol)), 
   (lagCols) := shift(.SD, 1, NA, 'lag'),
   by = c(idCol, 'yr'), .SDcols = coordCols]

# Find the difference between subsequent points in x,y
elk[, (difCols) := .((get(coordCols[1]) - get(lagCols[1])),
                     (get(coordCols[2]) - get(lagCols[2])))]



elk[, absAngle := atan2(difY, difX) * 180 / pi]
elk[absAngle < 0, absAngle := absAngle + 360]

lt<- adehabitatLT::as.ltraj(elk[id == 93, ..coordCols], 
                       elk[id == 93, datetime],
                       elk[id == 93, id])

# elk[, jdAbsAngle := 180 - abs(abs(dif))]

ltc <- cbind(elk[id == 93], lt[[1]])
ltc[, absa := abs.angle*(180/pi)]
ltc[absa < 0, absaPl := absa + 360][absa > 0, absaPl := absa]
ltc[, rela := rel.angle*(180/pi)]
ltc[rela < 0, relaPl := rela + 360][rela > 0, relaPl := rela]

ltc[, shift(absAngle, 1, type = 'lag') - absAngle]
ltc[, .(absAngle, absa, absaPl, rela, relaPl)]
ltc[1:10, .(absAngle, absaPl, rela, relaPl,
        shift(absAngle, 1, type = 'lag') - absAngle,
        # abs(shift(absAngle, 1, type = 'lag') - absAngle),
        # ifelse(shift(absAngle, 1, type = 'lag') - absAngle < 0,
        #        shift(absAngle, 1, type = 'lag') - absAngle,
        #        (shift(absAngle, 1, type = 'lag') - absAngle) + 360),
        (180 - abs(abs(shift(absAngle, 1, type = 'lag') - absAngle) - 180)))]

ltc[, shift(absAngle, 1, type = "lag")]


ggplot(elk, aes(EASTING, NORTHING)) + 
  geom_path(aes(color = absAngle)) #+ 
  # geom_path()
elk[, hist(absAngle)]

# Pseudo code

# Shift points 1 row

# atan2

# Ensure it is by ID

