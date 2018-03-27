DatePrep <- function(DT, dateCol, timeCol, dateFormat = NULL, timeFormat = NULL) {
  if(is.null(dateFormat)) {
    DT[, idate := as.IDate(get(dateCol))]
  } else {
    DT[, idate := as.IDate(get(dateCol), format = dateFormat)]
  }
  
  if(is.null(timeFormat)) {
    DT[, itime := as.ITime(get(timeCol))]
  } else {
    DT[, itime := as.ITime(get(timeCol), format = timeFormat)]
  }
  
  DT[, datetime := as.POSIXct(paste(idate, itime), format = '%F %T')]
  
  DT[, julday := yday(idate)]
  DT[, yr := year(idate)]
  DT[, mnth := month(idate)]
  
  if(all(c('idate', 'itime', 'datetime', 'julday', 'yr', 'mnth') %in% colnames(DT))){ 
    message('date and time fields added successfully')
  } else {
    stop('date and time fields NOT added successfully')
  }

}

