#' Date prep
#'
#' @inheritParams abs_angle
#' @param dateCol character of date column name
#' @param timeCol character of time column name
#' @param dateFormat character of date format as described in `strptime`
#' @param timeFormat character of time format as described in `strptime`
#'
#' @import data.table
#'
#' @return
#' @export
#'
#' @examples
prep_date <- function(DT, dateCol, timeCol, dateFormat = NULL, timeFormat = NULL) {
  # TODO: add a tz argument
  
  # NSE
  idate <- itime <- datetime <- yr <- mnth <- hr <- julday <- NULL
  
  if (is.null(dateFormat)) {
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

