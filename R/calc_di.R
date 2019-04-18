#' Calculate DI
#' 
#' Dynamic interaction as described by Long and Nelson (2013). 
#'
#' @param DT 
#' @inheritParams diff_azimuth
#' @return
#' @export
#' 
#' @references Long, Jed A., and Trisalyn A. Nelson. "Measuring dynamic interaction in movement data." Transactions in GIS 17.1 (2013): 62-77.
#' 
#' @seealso abs_angle diff_azimuth
#'
#' @examples
calc_di <- function(DT, suffix, angle, dist) {
  
  
  diff_azimuth(DT, suffix = suffix, angle = angle)
  
  
  
  
}



#' Difference in azimuth
#'
#' Difference in azimuth or absolute angle, calculated with \code{\link{abs_angle}}. 
#'
#' @inheritParams abs_angle
#' @param suffix character to append directly to angle column name, indicating the angle of the neighbour. 
#' @param angles character of column name with  azimuth angle.
#'
#' @return
#' @seealso abs_angle
#' 
#' @references Long, Jed A., and Trisalyn A. Nelson. "Measuring dynamic interaction in movement data." Transactions in GIS 17.1 (2013): 62-77.
#' 
#' @export
#'
#' @examples
diff_azimuth <- function(DT, suffix, angle) {
  # NSE
  didist <- NULL
  
  
  
  
  angle1 <- angle
  angle2 <- paste0(angle, suffix)
  
  if (any(!(c(angle1, angle2) %in% colnames(DT)))) {
    stop('angle (and angle+suffix) columns not found in DT')
  }
  
  # If one undefined
  DT[is.na(get(angle1)) | is.na(get(angle2)), 
      diAngle := 0]
  
  # If both undefined
  DT[is.na(get(angle1)) & is.na(get(angle2)), 
      diAngle := 1]
  
  # Otherwise
  DT[is.na(diAngle),
     diAngle := cos(.SD[[1]] - .SD[[2]]),
     .SDcols = c(angle1, angle2)]
  
  return(DT)
}



#' Difference in distance
#'
#' @inheritParams diff_azimuth
#' @param dist 
#' @param alpha default is 1.
#'
#' @return
#' 
#' @references Long, Jed A., and Trisalyn A. Nelson. "Measuring dynamic interaction in movement data." Transactions in GIS 17.1 (2013): 62-77.
#' 
#' @export
#'
#' @examples
diff_dist <- function(DT, suffix, dist, alpha = 1) {
  # NSE
  didist <- NULL
  
  dist1 <- dist
  dist2 <- paste0(dist, suffix)
  
  if (any(!(c(dist1, dist2) %in% colnames(DT)))) {
    stop('dist (and dist+suffix) columns not found in DT')
  }
  
  DT[get(dist1) + get(dist2) == 0, diDist := 1]
  
  DT[is.na(diDist), 
     diDist := 1 - ((abs(.SD[[1]] - .SD[[2]]) / .SD[[1]] + .SD[[2]]) ^ alpha),
     .SDcols = c(dist1, dist2)]
  
  return(DT)
}