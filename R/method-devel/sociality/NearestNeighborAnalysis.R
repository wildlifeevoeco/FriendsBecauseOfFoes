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

elk <- elk[julday == 25 & id < 300][, nbyid := .N, by = id][nbyid == 1]

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

ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()


# elk[, MeanPairwiseDists(.SD, coordCols, 'id'), by = julday]


apply(data.table(a), 2, which.min)



data.table(a)[, apply(.SD, 1, which.min)]

data.table(a.)[, [which.min(.SD)]]

Nfunction = function(DT){
  a <- data.matrix(dist(DT[, ..coordCols],
                        method = "euclidean",
                        diag = FALSE,
                        upper = TRUE))
  diag(a) <- NA
  
  lsids <- DT$id
  
  dtA <- data.table(a, id = lsids)
  setnames(dtA, c(lsids, 'id'))
  # dtA[, id[which.min(.SD)]]
  b <- dtA[, lapply(.SD, FUN = function(x) id[which.min(x)]),
      .SDcols = as.character(lsids)]
  suppressWarnings(melt(b, variable.name = 'left', value.name = 'right'))

}


Nfunction(elk)


system.time(
  data.table(a, keep.rownames = TRUE)[, rn[which.min(`2`)]]
)


### Quadtree ----
elk[, rowID := .I]
NumberQuadtreeNeighbors <- function(DT, coordCols, numbNeighbors) {
  tree <- createTree(DT)
  knnLookup(tree, newdat = DT, k = numbNeighbors)[, 2]
}

# Careful - the bomb
elk[, nearestRowID := NumberQuadtreeNeighbors(.SD, numbNeighbors = 2),
    by = .(julday),
    .SDcols = coordCols]

