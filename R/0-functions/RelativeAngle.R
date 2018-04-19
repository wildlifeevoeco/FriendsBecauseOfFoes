RelativeAngle <- function(DT, abs = NULL, coord = NULL, datetime = NULL, 
                          id = NULL, allCW = FALSE, returnIntermediate = FALSE) {
  if('absAngle' %in% colnames(DT)){
    DT[, relAngle := absAngle - shift(absAngle, 1, type = 'lag')]
  } else {
    AbsoluteAngle(DT, coord, datetime, id, yr, allCW, returnIntermediate)
    DT[, relAngle := absAngle - shift(absAngle, 1, type = 'lag')]
  }
}
