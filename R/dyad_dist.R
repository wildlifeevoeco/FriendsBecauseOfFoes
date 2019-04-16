#' Calculate dyad distance
#'
#' For each dyad, calculate the euclidean distance between locations.
#'
#' @param DT
#' @param coords
#' @param suffix
#' @param returnIntermediate
#'
#' @return
#' @export
#'
#' @examples
dyad_dist <-
  function(DT,
           coords,
           suffix = NULL,
           returnIntermediate = FALSE) {
    # NSE
    . <- dyadDist <- NULL
    
    # Create and dif column names
    difCols <- c('difX', 'difY')
    
    # Find the difference between subsequent points in x,y
    DT[, (difCols) := .((get(coords[1]) - get(suffix[1])),
                        (get(coords[2]) - get(suffix[2])))]
    
    difSqCols <- paste0('sq', difCols)
    
    # Square the difference
    DT[, (difSqCols) := .(get(difCols[1]) ^ 2, get(difCols[2]) ^ 2)]
    
    # Square root the summed difference for a simple step length
    DT[, dyadDist := sqrt(rowSums(.SD)), .SDcols = difSqCols]
    
    if (returnIntermediate) {
      DT
    } else {
      DT[, c(difCols, difSqCols) := NULL]
    }
  }
