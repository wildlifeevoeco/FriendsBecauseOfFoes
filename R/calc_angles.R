#' Calculate Absolute Angles
#'
#' @param DT data.table
#' @param coords character vector; coordinate columns
#' @param datetime character; datetime column
#' @param by character vector; columns identifying within which groups (e.g. each individual and year) to calculate angles
#' @param returnIntermediate boolean; return intermediate columns
#'
#' @return input DT appended with absolute angle column
#'
#' @export
#'
#' @examples
calc_abs_angle <- function(DT,
                           coords,
                           datetime,
                           by,
                           returnIntermediate = FALSE) {
  # NSE
  difY <- difX <- . <- absAngle <- NULL
  
  # Create lead and dif column names
  lagCols <- paste('lead', coords, sep = '')
  difCols <- c('difX', 'difY')
  
  data.table::setorderv(DT, datetime)
  DT[, (lagCols) := data.table::shift(.SD, 1, NA, 'lead'),
     by = by, .SDcols = coords]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(lagCols[1]) - get(coords[1])),
                      (get(lagCols[2]) - get(coords[2])))]
  
  # Calculate absolute angle
  DT[, absAngle := atan2(difY, difX) * (180 / pi)]
  
  # Rescale between 0-360
  DT[absAngle < 0, absAngle := absAngle + 360]
  
  if (returnIntermediate) {
    return(DT)
  } else {
    DT[, c(lagCols, difCols) := NULL]
    return(DT)
  }
}


#' Calculate Relative Angles
#'
#' @inheritParams calc_abs_angle
#'
#' @return
#' @export
#'
#' @examples
calc_rel_angle <-
  function(DT,
           coords = NULL,
           datetime = NULL,
           by = NULL) {
    # NSE
    absAngle <- relAngle <- NULL
    
    if (missing(DT)) {
      stop('must provide an input DT')
    }
    
    data.table::setorderv(DT, datetime)
    
    if ('absAngle' %in% colnames(DT)) {
      DT[, relAngle := absAngle -
           data.table::shift(absAngle, 1, type = 'lag'),
         by = by]
      
    } else {
      calc_abs_angle(DT, coords, datetime, by = by)
      
      DT[, relAngle := absAngle -
           data.table::shift(absAngle, 1, type = 'lag'),
         by = by]
    }
    
    return(DT)
  }
