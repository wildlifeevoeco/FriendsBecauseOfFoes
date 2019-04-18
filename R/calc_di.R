#' Calculate DI
#' 
#' Dynamic interaction. 
#'
#' @param DT 
#' @inheritParams diff_azimuth
#' @return
#' @export
#' @seealso abs_angle diff_azimuth
#'
#' @examples
calc_di <- function(DT, suffix, angle, dist) {
  # TODO: add reference to long and nelson?
  
  angle1 <- angle
  angle2 <- paste0(angle, suffix)
  
  diff_azimuth(DT, angle1 = angle1, angle2 = angle2)
  
  
  
  
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
#' @export
#'
#' @examples
diff_azimuth <- function(DT, suffix, angle) {
  # TODO: add reference to long and nelson?
  
  if (any(!(c(angle1, angle2) %in% colnames(DT)))) {
    stop('columns provided for angle1 and angle2 not found in DT')
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
#'
#' @return
#' @export
#'
#' @examples
diff_dist <- function(DT, suffix, dist) {
  
}