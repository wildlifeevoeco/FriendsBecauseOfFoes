### Calculate Turn Angles ----
# Authors: Alec Robitaille


#TODO: Grab Hance's updated social metrics from modeling script

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


## DECISIONS
# turning angle is related to the t = 1 step if turning angle is 
#   angle t = 1 and angle t = 2 


###########
one <- DT[season == 'winter' & id == 862]
setorder(one, datetime)

calc_angles(DT, coordCols, datetimeCol, c('season', 'id'))
DT



library(trajr)

traj <- TrajFromCoords(
  one,
  xCol = 'EASTING',
  yCol = 'NORTHING',
  timeCol = 'datetime',
  spatialUnits = 'm'
)
TrajStepLengths(traj)
traj$angle <- c(NA, NA, TrajAngles(traj))

calc_abs_angle(one, coordCols, datetimeCol, idCol, yrCol, TRUE, FALSE)
calc_rel_angle(one, coordCols, datetimeCol, idCol, yrCol, TRUE, FALSE)


# List relevant column names
coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'
datetimeCol <- 'datetime' 
yrCol <- 'yr'

### Variables ----
# TODO: check rleid/ http://stackoverflow.com/q/21421047/559784
DT[, rowID := rleid(EASTING), by = c("id", "yr")]

### Calculate Absolute Angle ----
# TODO: (Ask Hance) would it be better if we had range from 0-360 instead? for dif abs angle
calc_abs_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

#TODO: check that yrcol is well handled
calc_rel_angle(DT, coordCols, datetimeCol, idCol, yrCol, FALSE, FALSE)

### Output ----
saveRDS(DT, paste0('output/4-angles/', species, 'Angle.Rds'))

### Figures ----
qplot(absAngle, data = DT)