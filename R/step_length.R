#' Step Length
#'
#' Calculate basic step length with data.table
#'
#' @param DT data.table
#' @param coords character vector; length 2, coordinate column names. UTM required.
#' @param time character; time column name.
#' @param splitBy character; vector of column names to split step length calculation by. default is id and yr (individual identifier and year as numeric).
#' @param type character; default: lag. alternative: lead.
#' @param moverate boolean; calculate movement rate? stepLength / dif time, unit hours.
#' @param preserve boolean; preserve intermediate cols? default: no.
#' @import data.table
#'
#' @export
#'
#' @examples
#'
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "toast"))
#'
#' DT[, datetime := as.POSIXct(datetime)]
#'
#' DT[, yr := year(datetime)]
#'
#' step_length(DT, coords = c('X', 'Y'), splitBy = c('ID', 'yr'))
step_length <-
  function(DT,
           coords = c('EASTING', 'NORTHING'),
           time = 'datetime',
           splitBy = c('id', 'yr'),
           moverate = FALSE,
           type = 'lag',
           preserve = FALSE) {
    # NSE
    .SD <- . <- stepLength <- moveRate <- NULL
    
    if (type != 'lag' & type != 'lead') {
      stop('must provide one of lag or lead for argument type')
    }
    
    check_col(DT, coords[1], 'X coords')
    check_col(DT, coords[2], 'Y coords')
    check_col(DT, time, 'time')
    lapply(splitBy, function(col) check_col(DT, col))
    
    shiftXY <- paste0('lag', coords)
    difXY <- paste0('dif', coords)
    
    data.table::setorderv(DT, time)
    
    DT[, (shiftXY) := data.table::shift(.SD, n = 1, fill = NA, type),
       by = splitBy, .SDcols = coords]
    
    DT[, (difXY) := .((.SD[[1]] - .SD[[3]]) ^ 2, (.SD[[2]] - .SD[[4]]) ^ 2),
       .SDcols = c(coords, shiftXY)]
    
    DT[, stepLength := sqrt(rowSums(.SD, na.rm = TRUE)),
       .SDcols = difXY]
    
    DT[is.na(get(difXY[1])) | is.na(get(difXY[2])), stepLength := NA]
    
    dropem <- c(shiftXY, difXY)
    
    if (moverate) {
      shiftT <- paste0('lag', time)
      difT <- paste0('dif', time)
      
      dropem <- c(dropem, shiftT)
      
      data.table::setorderv(DT, time)
      DT[, (shiftT) := data.table::shift(.SD, 1, NA, 'lag'),
         by = splitBy,
         .SDcols = time]
      
      DT[, (difT) := as.numeric(.SD[[1]] - .SD[[2]], units = 'hours'),
         .SDcols = c(time, shiftT)]
      
      DT[, moveRate := .SD[[1]] / .SD[[2]],
         .SDcols = c('stepLength', difT)]
    }
    
    if (!preserve) {
      data.table::set(DT, j = dropem, value = NULL)
    }
    
    DT[]
  }


.datatable.aware = TRUE 