# Function to calculate step length

# Alec Robitaille
# March 2018
StepLength <- function(DT, idCol, dateCol, timeCol, yrCol, xCol, yCol, returnIntermediate = FALSE) {
  
  coordCols <- c(xCol, yCol)
  
  # Create lag and dif column names
  lagCols <- paste('lag', coordCols, sep = '')
  difCols <- c('difX', 'difY')
  
  lagTimeCol <- paste0('lag', timeCol)
  difTimeCol <- paste0('dif', timeCol)
  
  lagDateCol <- paste0('lag', dateCol)
  difDateCol <- paste0('dif', dateCol)
  
  # Use shift  to create lagged cols
  DT[order(get(dateCol), get(timeCol)), 
     (lagCols) := shift(.SD, 1, NA, 'lag'),
     by = c(idCol, yrCol),
     .SDcols = coordCols]
  
  # Find the difference squared between all points in each x,y separately
  DT[, (difCols) := .((get(coordCols[1]) - get(lagCols[1])) ^ 2,
                      (get(coordCols[2]) - get(lagCols[2])) ^ 2)]

  # Square root the summed difference for a simple step length
  DT[, stepLength := sqrt(rowSums(.SD)), .SDcols = difCols]

  ## Delta Time
  DT[order(get(dateCol), get(timeCol)),
     c(lagTimeCol, lagDateCol) := shift(.SD, 1, NA, 'lag'),
     by = c(idCol, yrCol), .SDcols = c(timeCol, dateCol)]

  # difference in time in hours
  DT[, (difTimeCol) := (as.numeric(difftime(get(timeCol), get(lagTimeCol), units = 'hours')))]
  
  # for comparison across days
  DT[get(difTimeCol) < 0, (difTimeCol) := get(difTimeCol) + 24]
  
  if(DT[get(difTimeCol) > 24, .N] != 0) stop("check difitime, shouldn't be greater than 24")
  
  
  # difference in days 
  DT[, (difDateCol) := (as.numeric(difftime(get(dateCol), get(lagDateCol), units = 'days')))]
  
  # Step length divided by time difference
  DT[, moveRate := stepLength / (get(difTimeCol))]
  
  if(returnIntermediate) {
    DT
  } else {
    DT[, c(lagCols, difCols, lagTimeCol, difTimeCol) := NULL]
  }
}


