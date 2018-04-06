# Function to calculate step length

# Alec Robitaille
# March 2018
StepLength <- function(DT, idCol, datetimeCol, yrCol, xCol, yCol, 
                       returnIntermediate = FALSE) {
  
  coordCols <- c(xCol, yCol)
  
  # Create lag and dif column names
  lagCols <- paste('lag', coordCols, sep = '')
  difCols <- c('difX', 'difY')
  
  lagDateTimeCol <- paste0('lag', datetimeCol)
  difDateTimeCol <- paste0('dif', datetimeCol)
  
  # Use shift  to create lagged cols
  DT[order(get(datetimeCol)), 
     (lagCols) := shift(.SD, 1, NA, 'lag'),
     by = c(idCol, yrCol), .SDcols = coordCols]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(coordCols[1]) - get(lagCols[1])),
                      (get(coordCols[2]) - get(lagCols[2])))]

  difSqCols <- paste0('sq', difCols)
  
  # Square the difference 
  DT[, (difSqCols) := .(get(difCols[1]) ^ 2, get(difCols[2]) ^ 2)]
  
  # Square root the summed difference for a simple step length
  DT[, stepLength := sqrt(rowSums(.SD)), .SDcols = difSqCols]

  ## Delta Time
  DT[order(get(datetimeCol)),
     (lagDateTimeCol) := shift(.SD, 1, NA, 'lag'),
     by = c(idCol, yrCol), .SDcols = datetimeCol]

  # difference in time in hours
  DT[, (difDateTimeCol) := as.numeric(difftime(get(datetimeCol), 
                                               get(lagDateTimeCol), 
                                               units = 'hours'))]
  
  # Step length divided by time difference
  DT[, moveRate := stepLength / (get(difDateTimeCol))]
  
  
  # if(DT[(get(datetimeCol) == get(lagDateTimeCol)) |
  #       (get(xCol) == get(lagCols[1])) |
  #       (get(yCol) == get(lagCols[2])) |
  #       (get(datetimeCol) == get(lagDateTimeCol)) |])
  # 
  
  if(returnIntermediate) {
    DT
  } else {
    DT[, c(lagCols, difCols, lagDateTimeCol, difSqCols) := NULL]
  }
}


