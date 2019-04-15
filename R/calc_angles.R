#' Title
#'
#' @param DT 
#' @param coords 
#' @param time 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
calc_angles <- function(DT, coords, time, by) {
  
  
  DT[, relative := {
    traj <- TrajFromCoords(
      .SD,
      xCol = coords[[1]],
      yCol = coords[[2]],
      timeCol = time
    )
    
    c(TrajAngles(traj), NA, NA)
  }, by = by]
}