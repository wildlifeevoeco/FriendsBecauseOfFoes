#' Date prep
#'
#' @inheritParams calc_abs_angle
#' @param dateCol 
#' @param timeCol 
#' @param dateFormat 
#' @param timeFormat 
#'
#' @import data.table
#'
#' @return
#' @export
#'
#' @examples
prep_date <- function(DT, dateCol, timeCol, dateFormat = NULL, timeFormat = NULL) {
  if(is.null(dateFormat)) {
    DT[, idate := data.table::as.IDate(get(dateCol))]
  } else {
    DT[, idate := data.table::as.IDate(get(dateCol), format = dateFormat)]
  }
  
  if(is.null(timeFormat)) {
    DT[, itime := data.table::as.ITime(get(timeCol))]
  } else {
    DT[, itime := data.table::as.ITime(get(timeCol), format = timeFormat)]
  }
  
  DT[, datetime := as.POSIXct(paste(idate, itime), format = '%F %T')]
  
  DT[, julday := data.table::yday(idate)]
  DT[, yr := data.table::year(idate)]
  DT[, mnth := data.table::month(idate)]
  
  DT[, hr := data.table::hour(itime)]
  
  if(all(c('idate', 'itime', 'datetime', 'julday', 'yr', 'mnth') %in% colnames(DT))){ 
    message('date and time fields added successfully')
  } else {
    stop('date and time fields NOT added successfully')
  }

}

