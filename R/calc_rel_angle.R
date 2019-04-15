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
