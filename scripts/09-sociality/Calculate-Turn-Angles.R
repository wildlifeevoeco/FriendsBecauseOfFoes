# Calculate Turn Angles


library(data.table)


### Input data ----
load('for_step4_ewc_new.Rdata')
DT <- elkExtract
species <- 'Elk'
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

# dir.create("OUTPUT_NEW/SOCIAL")

saveRDS(DT, paste0(species, 'Angle_log2.Rds'))



### Input data ----
DT <- caribouExtract
species <- 'caribou'
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



saveRDS(DT, paste0(species, 'Angle_log2.Rds'))