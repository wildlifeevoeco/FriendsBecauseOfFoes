#' Calculate dyad distance
#'
#' For each dyad, calculate the euclidean distance between locations.
#'
#' @inheritParams abs_angle 
#' @param suffix in column names associated with neighbours (e.g.: '.nn')
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
    
    # Difference column names
    difCols <- c('difX', 'difY')
    
    # Neighbour column names
    neighs <- paste0(coords, suffix)
    
    
    # Find the difference between subsequent points in x,y
    DT[, (difCols) := .(.SD[[1]] - .SD[[3]], .SD[[2]] - .SD[[4]]),
       .SDcols = c(coords, neighs)]
    
    sqCols <- paste0('sq', difCols)
    
    # Square the difference
    DT[, (sqCols) := .(.SD[[1]] ^ 2, .SD[[2]] ^ 2),
       .SDcols = difCols]
    
    # Square root the summed difference for a simple step length
    DT[, dyadDist := sqrt(rowSums(.SD)), .SDcols = sqCols]
    
    if (returnIntermediate) {
      return(DT)
    } else {
      DT[, c(difCols, sqCols) := NULL]
      return(DT)
    }
  }
