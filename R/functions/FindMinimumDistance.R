FindMinimumDistance = function(DT, coordCols, idCol){
  a <- as.matrix(dist(DT[, ..coordCols],
                      method = 'euclidean'))
  diag(a) <- NA
  
  lsID <- DT[, get(idCol)]
  
  dtA <- data.table(a, id = lsID)
  
  setnames(dtA, c(lsID, 'id'))
  
  findMin <- dtA[, lapply(.SD, FUN = function(x) id[which.min(x)]),
                 .SDcols = as.character(lsID)]
  
  suppressWarnings(melt(findMin, variable.factor = FALSE, 
                   variable.name = 'left', 
                   value.name = 'right')[, left := as.integer(left)])
}
