AbsoluteAngle <- function(DT, coords, datetime, id, yr,
                          allCW = FALSE, returnIntermediate = FALSE) {
  # Create lag and dif column names
  lagCols <- paste('lag', coords, sep = '')
  difCols <- c('difX', 'difY')
  
  DT[order(datetime), 
      (lagCols) := data.table::shift(.SD, 1, NA, 'lead'),
      by = c(id, yr), .SDcols = coords]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(lagCols[1]) - get(coords[1])),
                       (get(lagCols[2]) - get(coords[2])))]
  
  DT[, absAngle := atan2(difY, difX) * 180 / pi]
  
  if(!returnIntermediate) {
    DT[, c(lagCols, difCols) := NULL]
  }
  
  if(!allCW){
    DT
  } else {
    DT[absAngle < 0, absAngle := absAngle + 360]  
  }
  
}