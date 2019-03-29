NumbQuadTreeNeighbours <- function(DT, coords, numbNeighbours, idCol) {
  # NSE
  ..coords <- NULL
  
  tree <- SearchTrees::createTree(DT[, ..coords])
  knn <- DT[, get(idCol)[SearchTrees::knnLookup(tree, newdat = DT[, ..coords], 
                                   k = numbNeighbours + 1)[, -1]]]
  dim(knn) <- c(length(DT[, get(idCol)]), numbNeighbours)
  as.data.table(knn)
}
