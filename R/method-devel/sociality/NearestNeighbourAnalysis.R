### Nearest Neighbor Analysis ----
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

### Find Minimum Distance Neighbor ----
# Do any timegroups have the same individual twice?
all.equal(elk[, .(N = uniqueN(id)), by = timegroup],
          elk[, .N, by = timegroup])

# Which timegroups have more than one individual?
elk[, NbyTime := .N, by = timegroup]
elk[, qplot(NbyTime)]

# Calc Nearest Neighbour
source('R/functions/FindMinimumDistance.R')
nn <- elk[NbyTime > 1, FindMinimumDistance(.SD, coordCols, idCol),
    by = timegroup]

# Merge the nearest neighbors back on
withNeighbours <- merge(elk, nn, 
                        by.x = c('id', 'timegroup'),
                        by.y = c('left', 'timegroup'))

# And row bind those in a timegroup alone
elk <- rbindlist(list(withNeighbours, elk[NbyTime == 1]), fill = TRUE)


### Quadtree ----
# How many neighbors 
neighbours <- 3
neighbourCols <- paste0('neighbour', seq(1, neighbours))

source('R/functions/NumbQuadTreeNeighbours.R')

# Only running on where there are at least 2 in a timegroup, else ! the bomb !
elk[NbyTime > neighbours, 
    (neighbourCols) := NumbQuadTreeNeighbours(.SD, coordCols,
                                              neighbours, idCol),
    by = timegroup]

elk <- merge(elk, elk,
                   by.x = c('neighbour1', 'timegroup'),
                   by.y = c('id', 'timegroup'),
                   suffixes = c("", "Right"))

### Calculate dyadic distance ----
source('R/functions/DyadicDistance.R')
# dyadDistCols <- paste0('dyadDist', seq(1, neighbours))

# TODO flex for multiple neighbors
# elk[, (dyadDistCols) := DyadicDistance(.SD, coordCols = coordCols,
#                                        neighbourCoordCols = paste0(coordCols, "Right"))]

DyadicDistance(withRight, coordCols = coordCols,
               neighbourCoordCols = paste0(coordCols, "Right"))

### Compare methods ----
elk[!is.na(right)]
elk[!is.na(neighbour1)]
elk[neighbour1 == right]
elk[neighbour1 != right]


### Figures ----
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk[timegroup == 4]) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()
