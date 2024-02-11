message('=== Calculate Turn Angles ===')
# Authors: Alec Robitaille



library(data.table)

### Set variables ----
source('scripts/0-variables/variables.R')

#' Calculate Absolute Angles
#'
#' @param DT data.table
#' @param coords character vector; coordinate columns
#' @param datetime character; datetime column
#' @param by character vector; columns identifying within which groups (e.g. each individual and year) to calculate angles
#' @param returnIntermediate boolean; return intermediate columns
#'
#' @return input DT appended with absolute angle column
#'
#' @export
#'
#' @examples
abs_angle <- function(DT,
                      coords,
                      datetime,
                      by,
                      returnIntermediate = FALSE) {
  
  # NSE
  difY <- difX <- . <- absAngle <- NULL
  
  # Create lead and dif column names
  lagCols <- paste('lead', coords, sep = '')
  difCols <- c('difX', 'difY')
  
  data.table::setorderv(DT, datetime)
  DT[, (lagCols) := data.table::shift(.SD, 1, NA, 'lead'),
     by = by, .SDcols = coords]
  
  # Find the difference between subsequent points in x,y
  DT[, (difCols) := .((get(lagCols[1]) - get(coords[1])),
                      (get(lagCols[2]) - get(coords[2])))]
  
  # Calculate absolute angle
  DT[, absAngle := atan2(difY, difX) * (180 / pi)]
  
  # Rescale between 0-360
  DT[absAngle < 0, absAngle := absAngle + 360]
  
  if (returnIntermediate) {
    return(DT)
  } else {
    DT[, c(lagCols, difCols) := NULL]
    return(DT)
  }
}


#' Calculate Relative Angles
#'
#' @inheritParams abs_angle
#'
#' @return
#' @export
#'
#' @examples
rel_angle <-
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
      stop('absAngle column not found - please run abs_angle')
    }
    
    return(DT)
  }


### Input data ----
# Which species would you like to calculate abs and rel TA for?
# Flexible for Makefile: if running script manually, edit species in else block
if (length(commandArgs(trailingOnly = TRUE) > 1)) {
  species <- commandArgs(trailingOnly = TRUE)[2]
  print(paste0('using species: ', species))
} else {
  species <- 'elk'
}

# DT <- readRDS(paste0('output/3-extraction/', species, 'Extract.Rds'))
load('OUTPUT_NEW/EXTRACT_DOMAINS/for_step4_ewc.Rdata')
if (species == 'caribou') {
  DT <- caribouExtract
} else if (species == 'elk') {
  DT <- elkExtract
}

if (truelength(DT) == 0) alloc.col(DT) 

# List relevant column names
coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'

### Calculate Angles ----
# Absolute angles
abs_angle(
  DT = DT,
  coords = coordCols,
  datetime = datetimeCol,
  by = c('season', idCol, yrCol),
  returnIntermediate = FALSE
)

# Relative angles
rel_angle(
  DT = DT,
  coords = coordCols,
  datetime = datetimeCol,
  by = c('season', idCol, yrCol)
)

### Output ----
# Check results
if (any(!(DT[is.na(relAngle), .N, by = .(id, yr, season)][, range(N)] == 
          c(2, 2)))) {
  stop('check NAs in relAngle')
}

dir.create("OUTPUT_NEW/SOCIAL")

saveRDS(DT, paste0('OUTPUT_NEW/SOCIAL/', species, 'Angle_log2.Rds'))



### Input data ----
# Which species would you like to calculate abs and rel TA for?
# Flexible for Makefile: if running script manually, edit species in else block
if (length(commandArgs(trailingOnly = TRUE) > 1)) {
  species <- commandArgs(trailingOnly = TRUE)[2]
  print(paste0('using species: ', species))
} else {
  species <- 'caribou'
}


if (species == 'caribou') {
  DT <- caribouExtract
} else if (species == 'elk') {
  DT <- elkExtract
}

if (truelength(DT) == 0) alloc.col(DT) 

# List relevant column names
coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'

### Calculate Angles ----
# Absolute angles
abs_angle(
  DT = DT,
  coords = coordCols,
  datetime = datetimeCol,
  by = c('season', idCol, yrCol),
  returnIntermediate = FALSE
)

# Relative angles
rel_angle(
  DT = DT,
  coords = coordCols,
  datetime = datetimeCol,
  by = c('season', idCol, yrCol)
)

### Output ----
# Check results
if (any(!(DT[is.na(relAngle), .N, by = .(id, yr, season)][, range(N)] == 
          c(2, 2)))) {
  stop('check NAs in relAngle')
}



saveRDS(DT, paste0('OUTPUT_NEW/SOCIAL/', species, 'Angle_log2.Rds'))

message('=== TURN ANGLES COMPLETE ===')
