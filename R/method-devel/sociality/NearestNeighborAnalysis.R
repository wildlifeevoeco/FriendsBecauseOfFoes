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

### Dist ----
# Apply dist by time step
MeanPairwiseDists <- function(DT, coordCols, idCol){
  names <- DT[, get(idCol)]
  dists <- data.table(dist(DT[, ..coordCols],
                           method = "euclidean",
                           diag = FALSE,
                           upper = TRUE), 
                      keep.rownames = TRUE)
  dists#[, lapply(.SD, min), by = rn]
}

elk[, MeanPairwiseDists(.SD, coordCols, 'id'), by = julday]

data.matrix(dist(elk[julday == 200, ..coordCols],
     method = "euclidean",
     diag = FALSE,
     upper = TRUE), keep.rownames = TRUE)

### Quadtree ----
elk[, rowID := .I]
coordCols <- c('EASTING', 'NORTHING')
NumberQuadtreeNeighbors <- function(DT, coordCols, numbNeighbors) {
  tree <- createTree(DT)
  knnLookup(tree, newdat = DT, k = numbNeighbors)[, 2]
}

# Careful - the bomb
elk[, nearestRowID := NumberQuadtreeNeighbors(.SD, numbNeighbors = 2),
    by = .(julday),
    .SDcols = coordCols]

