# Function to calculate step length between nearest neighbours

# Alec Robitaille
# March 2018
DyadicDistance <- function(DT, coordCols, neighbourCoordCols = NULL, 
                       returnIntermediate = FALSE) {
  
  # Create and dif column names
  difCols <- c('difX', 'difY')
  
  # Use shift  to create lagged cols
  DT[, (neighbourCoordCols) := shift(.SD, 1, NA, 'lag'), .SDcols = coordCols]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(coordCols[1]) - get(neighbourCoordCols[1])),
                      (get(coordCols[2]) - get(neighbourCoordCols[2])))]
  
  difSqCols <- paste0('sq', difCols)
  
  # Square the difference 
  DT[, (difSqCols) := .(get(difCols[1]) ^ 2, get(difCols[2]) ^ 2)]
  
  # Square root the summed difference for a simple step length
  DT[, dyadDist := sqrt(rowSums(.SD)), .SDcols = difSqCols]
  
  if(returnIntermediate) {
    DT
  } else {
    DT[, c(neighbourCoordCols, difCols, difSqCols) := NULL]
  }
}


