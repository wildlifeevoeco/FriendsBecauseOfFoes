#' Relative Angles
#'
#' @inheritParams calc_abs_angle
#' @param abs
#' @param coords
#' @param datetime
#' @param id
#' @param allCW
#' @param returnIntermediate
#'
#' @return
#' @export
#'
#' @examples
calc_rel_angle <-
  function(DT,
           abs = NULL,
           coords = NULL,
           datetime = NULL,
           id = NULL,
           allCW = FALSE,
           returnIntermediate = FALSE) {
    if ('absAngle' %in% colnames(DT)) {
      DT[, relAngle := data.table::shift(absAngle, 1, type = 'lead') - absAngle]
    } else {
      calc_abs_angle(DT, coord, datetime, id, yr, allCW, returnIntermediate)
      DT[, relAngle := data.table::shift(absAngle, 1, type = 'lead') - absAngle]
    }
  }
