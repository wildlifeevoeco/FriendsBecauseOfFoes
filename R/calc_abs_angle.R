#' Absolute angle
#'
#' @param DT data.table
#' @param coords character vector; coordinate columns
#' @param datetime character; datetime column
#' @param id character; id column
#' @param yr character; year column
#' @param allCW boolean; all clockwise
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
  difY <- difX <- . <- absAngle <- relAngle <- NULL
  
  # Create lead and dif column names
  lagCols <- paste('lead', coords, sep = '')
  difCols <- c('difX', 'difY')
  
  setorderv(DT, datetime)
  DT[, (lagCols) := data.table::shift(.SD, 1, NA, 'lead'),
     by = by, .SDcols = coords]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(lagCols[1]) - get(coords[1])),
                      (get(lagCols[2]) - get(coords[2])))]
  
  DT[, absAngle := atan2(difY, difX) * 180 / pi]
  DT[absAngle < 0, absAngle := absAngle + 360]
  
  if (!returnIntermediate) {
    DT[, c(lagCols, difCols) := NULL]
  }
  
  # TODO: what did this mean? .. my poor code commenting
  if (!allCW) {
    DT
  } else {
    DT[absAngle < 0, absAngle := absAngle + 360]
  }
  
}