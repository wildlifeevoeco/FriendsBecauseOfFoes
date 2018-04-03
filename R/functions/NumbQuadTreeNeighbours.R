NumbQuadTreeNeighbours <- function(DT, coordCols, numbNeighbours, idCol) {
  tree <- createTree(DT[, ..coordCols])
  knn <- DT[, get(idCol)[knnLookup(tree, newdat = DT[, ..coordCols], 
                                   k = numbNeighbours + 1)[, -1]]]
  dim(knn) <- c(length(DT[, get(idCol)]), numbNeighbours)
  as.data.table(knn)
}
