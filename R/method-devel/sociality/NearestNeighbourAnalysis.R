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

elks <- elk[timegroup < 4]

### Find Minimum Distance Neighbor ----
# Do any timegroups have the same individual twice?
all.equal(elk[, .(N = uniqueN(id)), by = timegroup],
          elk[, .N, by = timegroup])

# Which timegroups have more than one individual?
elk[, NbyTime := .N, by = timegroup]
elk[, qplot(NbyTime)]

# Calc Nearest Neighbour
source('R/functions/FindMinimumDistance.R')
nn <- elk[NbyTime > 10, FindMinimumDistance(.SD, coordCols, idCol),
    by = timegroup]


### Quadtree ----
# How many neighbors 
neighbours <- 1
neighbourCols <- paste('neighbour', seq(1, neighbours))

source('R/functions/NumbQuadTreeNeighbours.R')

# Only running on where there are at least 2 in a timegroup, else ! the bomb !
elk[NbyTime > 1, 
    (neighbourCols) := NumbQuadTreeNeighbours(.SD, coordCols,
                                              neighbours, 'id'),
    by = timegroup]


### Figures ----
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk[timegroup == 4]) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()

