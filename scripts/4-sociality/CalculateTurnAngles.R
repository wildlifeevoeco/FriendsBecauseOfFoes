### Calculate Turn Angles ----
# Authors: Alec Robitaille


### Packages ----
libs <- c('data.table', 'ggplot2', 'ewc', 'SearchTrees')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'elk'
DT <- readRDS(paste0('output/3-extraction/', species, 'RsfValues.Rds'))

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

saveRDS(DT, paste0('output/4-sociality/', species, 'Angle.Rds'))