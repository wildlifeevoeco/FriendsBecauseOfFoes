### Nearest Neighbour Analysis ----
# Authors: Alec Robitaille
# Purpose: 
# Inputs: Elk, caribou relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'SearchTrees',
          'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
elk <- readRDS('output/data-prep/elk.Rds')

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'


### Checks ----
# Do any timegroups have the same individual twice?
all.equal(elk[, .(N = uniqueN(id)), by = timegroup],
          elk[, .N, by = timegroup])

# Which timegroups have more than one individual?
elk[, NbyTime := .N, by = timegroup]
elk[, qplot(NbyTime)]

### Quadtree - Find Nearest Neighbour ----
# How many neighbours 
neighbours <- 1
neighbourCols <- paste0('neighbour', seq(1, neighbours))

# Read in function
source('R/functions/NumbQuadTreeNeighbours.R')
# Only running on where there are at least 2 in a timegroup, else the bomb!
elk[NbyTime > neighbours, 
    (neighbourCols) := NumbQuadTreeNeighbours(.SD, coordCols,
                                              neighbours, idCol),
    by = timegroup]


# NA in neighbour means that there were less than the NbyTime in the timegroup
elkNN <- merge(elk[, .(id, EASTING, NORTHING, neighbour1, timegroup, stepLength)],
               elk[, .(neighbour1 = id, rEASTING = EASTING, rNORTHING = NORTHING, timegroup, 
                       rstepLength = stepLength)],
               all.x = TRUE)

neighbourCols <- c('rEASTING', 'rNORTHING', 'rstepLength')
elkNN[id == neighbour1, (neighbourCols) := NA]


### Calculate dyadic distance ----
source('R/functions/DyadicDistance.R')

# TODO flex for multiple neighbours
# elk[, (dyadDistCols) := DyadicDistance(.SD, coordCols = coordCols,
#                                        neighbourCoordCols = paste0(coordCols, "Right"))]

DyadicDistance(elkNN, coordCols = coordCols,
               neighbourCoordCols = paste0('r', coordCols),
               returnIntermediate = TRUE)

### Difference in Step length ----
elkNN[, dSI := abs(stepLength - rstepLength)]


### Find Minimum Distance Neighbour ----
# Calc Nearest Neighbour
source('R/functions/FindMinimumDistance.R')
nn <- elk[NbyTime > 1, FindMinimumDistance(.SD, coordCols, idCol),
          by = timegroup]

# Merge the nearest neighbours back on
withNeighbours <- merge(elk, nn, 
                        by.x = c('id', 'timegroup'),
                        by.y = c('left', 'timegroup'))

# And row bind those in a timegroup alone
elk <- rbindlist(list(withNeighbours, elk[NbyTime == 1]), fill = TRUE)


### Figures ----
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk[timegroup == 4]) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()
