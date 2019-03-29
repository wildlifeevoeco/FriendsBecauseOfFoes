#' Relative Angles
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
           id = NULL,
           yr = NULL,
           allCW = FALSE,
           returnIntermediate = FALSE) {
    # NSE
    absAngle <- relAngle <- NULL
    
    if ('absAngle' %in% colnames(DT)) {
    
      DT[, relAngle := 
           data.table::shift(absAngle, 1, type = 'lead') - absAngle]
      
    } else {
      
      calc_abs_angle(DT, coords, datetime, id, yr, allCW, returnIntermediate)
      
      DT[, relAngle := 
           data.table::shift(absAngle, 1, type = 'lead') - absAngle]
    }
  }
