FindNumbWithinDist = function(DT, distThreshold, coordCols, idCol){
  a <- as.matrix(dist(DT[, ..coordCols],
                      method = 'euclidean'))
  diag(a) <- NA
  
  lsID <- DT[, get(idCol)]
  
  dtA <- data.table(a, id = lsID)
  
  setnames(dtA, c(lsID, 'id'))

  findWithin <- dtA[, lapply(.SD, FUN = function(x) sum(x < distThreshold, 
                                                        na.rm = TRUE)),
                 .SDcols = as.character(lsID)]
  
  melt(findWithin, measure.vars = colnames(findWithin),
       variable.name = 'id', value.name = 'nWithinDist',
       variable.factor = FALSE)$nWithinDist
}
