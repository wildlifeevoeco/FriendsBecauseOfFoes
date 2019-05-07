#' Calculate DI
#' 
#' Dynamic interaction as described by Long and Nelson (2013). 
#'
#' @param DT 
#' @inheritParams diff_azimuth
#' @inheritParams diff_dist
#' @return
#' @export
#' 
#' @references Long, Jed A., and Trisalyn A. Nelson. "Measuring dynamic interaction in movement data." Transactions in GIS 17.1 (2013): 62-77.
#' 
#'
#' @examples
calc_di <- function(DT, suffix, angle, dist, alpha = 1, radians) {
  # NSE
  diAngle <- diDist <- di <- NULL
  
  diff_azimuth(DT, suffix = suffix, angle = angle, radians = radians)
  
  diff_dist(DT, suffix = suffix, dist = dist, alpha = alpha)
  
  
  if ('di' %in% colnames(DT)) {
    message('overwriting di, found in colnames(DT).')
    data.table::set(DT, j = 'di', value = NULL)
  }
  
  DT[, di := diAngle * diDist]
  
  return(DT)
}



#' Difference in azimuth
#'
#' Difference in azimuth or absolute angle, calculated with \code{\link{abs_angle}}. 
#'
#' @inheritParams abs_angle
#' @param suffix character to append directly to angle column name, indicating the angle of the neighbour. 
#' @param angles character of column name with  azimuth angle.
#' @param radians TRUE/FALSE if angles provided are in radians (TRUE) or in degrees (FALSE)
#'
#' @return
#' 
#' @references Long, Jed A., and Trisalyn A. Nelson. "Measuring dynamic interaction in movement data." Transactions in GIS 17.1 (2013): 62-77.
#' 
#' @export
#'
#' @examples
diff_azimuth <- function(DT, suffix, angle, radians) {
  # NSE
  didist <- NULL
  
  
  angle1 <- angle
  angle2 <- paste0(angle, suffix)
  
  if (any(!(c(angle1, angle2) %in% colnames(DT)))) {
    stop('angle (and angle+suffix) columns not found in DT')
  }
  
  if ('diAngle' %in% colnames(DT)) {
    message('overwriting diAngle, found in colnames(DT).')
    data.table::set(DT, j = 'diAngle', value = NULL)
  }
  
  
  adjustDegrees <- ifelse(radians, 1, (pi/180))
  
  
  # If one undefined
  DT[is.na(get(angle1)) | is.na(get(angle2)), 
      diAngle := 0]
  
  # If both undefined
  DT[is.na(get(angle1)) & is.na(get(angle2)), 
      diAngle := 1]
  
  # Otherwise
  
  DT[is.na(diAngle),
     diAngle := cos((.SD[[1]] * adjustDegrees) - (.SD[[2]] * adjustDegrees)),
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
  
  if ('diDist' %in% colnames(DT)) {
    message('overwriting diDist, found in colnames(DT).')
    data.table::set(DT, j = 'diDist', value = NULL)
  }
  
  DT[get(dist1) + get(dist2) == 0, diDist := 1]
  
  DT[is.na(diDist), 
     diDist := 1 - (
       (abs(.SD[[1]] - .SD[[2]]) / 
          (.SD[[1]] + .SD[[2]])
        ) ^ alpha),
     .SDcols = c(dist1, dist2)]
  
  return(DT)
}