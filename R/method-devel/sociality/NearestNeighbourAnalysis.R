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
# Which timegroups have more than one individual?
all.equal(elk[, .(N = uniqueN(id)), by = timegroup],
          elk[, .N, by = timegroup])

elk[, NbyTime := .N, by = timegroup]

source('R/functions/FindMinimumDistance.R')
nn <- elk[NbyTime > 10, FindMinimumDistance(.SD, coordCols, idCol),
    by = timegroup]#[, left := as.integer(left)]
nn[, .(left, as.numeric(as.character(left)))]

nn


merge(nn, elk, by.x = c('left', 'roundtime'), )
### Figures ----
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()

### Quadtree ----
# elk[, rowID := .I]
neighbours <- 1

NumbQuadtreeNeighbours <- function(DT, coordCols, numbNeighbors, idCol) {
  tree <- createTree(DT[, ..coordCols])
  DT[, get(idCol)[knnLookup(tree, newdat = DT[, ..coordCols], k = numbNeighbors + 1)[, 2]]]
}

# Careful - the bomb
elks[NbyTime > 1, nearestRowID := NumbQuadtreeNeighbours(.SD, coordCols,
                                                         neighbours, 'id'),
    by = timegroup]

tree <- createTree(elk[timegroup == 3, ..coordCols])
elk[timegroup == 3, get("id")[knnLookup(tree, newdat = elk[timegroup == 3, ..coordCols], k = 2)[, 2]]]
