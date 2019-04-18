#' Title
#'
#' @param DT 
#'
#' @return
#' @export
#'
#' @examples
calc_di <- function(DT) {
  # TODO: add reference to long and nelson?
  
  
  
  
}



#' Difference in azimuth
#'
#' @inheritParams abs_angle
#' @param angle1 azimuth angle for left individual
#' @param angle2 azimuth angle for right individual
#'
#' @return
#' @export
#'
#' @examples
diff_azimuth <- function(DT, angle1, angle2) {
  # TODO: add reference to long and nelson?
  
  if (any(!(c(angle1, angle2) %in% colnames(DT)))) {
    stop('columns provided for angle1 and angle2 not found in DT')
  }
  
  
  DT[is.na(get(angle1)) | is.na(get(angle2)), 
      diAngle := 0]
  
  DT[is.na(get(angle1)) & is.na(get(angle2)), 
      diAngle := 1]
  
  
  DT[is.na(diAngle), 
     diAngle := cos(.SD[[1]] - .SD[[2]]), 
     .SDcols = c(angle1, angle2)]
  
  return(DT)
}