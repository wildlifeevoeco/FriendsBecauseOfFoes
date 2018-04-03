NumbQuadtreeNeighbours <- function(DT, coordCols, numbNeighbors, idCol) {
  tree <- createTree(DT[, ..coordCols])
  DT[, get(idCol)[knnLookup(tree, newdat = DT[, ..coordCols], 
                            k = numbNeighbors + 1)[, 2]]]
}