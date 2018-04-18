NumbQuadTreeNeighbours <- function(DT, coords, numbNeighbours, idCol) {
  tree <- createTree(DT[, ..coords])
  knn <- DT[, get(idCol)[knnLookup(tree, newdat = DT[, ..coords], 
                                   k = numbNeighbours + 1)[, -1]]]
  dim(knn) <- c(length(DT[, get(idCol)]), numbNeighbours)
  as.data.table(knn)
}
