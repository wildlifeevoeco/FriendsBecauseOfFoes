### Nearest Neighbour Analysis ----
# Authors: Alec Robitaille

#TODO: Grab Hance's updated social metrics from modeling script

### Packages ----
libs <- c('data.table', 'ewc', 'spatsoc', 'igraph')
lapply(libs, require, character.only = TRUE)


### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'elk'
DT <- readRDS(paste0('output/4-sociality/', species, 'Angle.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

### Checks ----
# Do any timegroups have the same individual twice?
all.equal(DT[, .(N = uniqueN(id)), by = timegroup],
          DT[, .N, by = timegroup])

# Number of individuals in each timegroup
DT[, nByTimegroup := .N, timegroup]


### Find nearest neighbour with spatsoc ----
edges <- edge_nn(DT, id = idCol, coords = coordCols, 
                 timegroup = 'timegroup')
# Check
edges[, .N, timegroup][, sum(N)] == nrow(edges)


### Combine ID + NN columns
cols <- c('EASTING',
          'NORTHING',
          'stepLength',
          'absAngle',
          'relAngle')

if (species == 'elk') {
  cols <- c(cols, 'predatorRSF', 'preyRSF')
} else if (species == 'caribou') {
  cols <- c(cols, 'caribouRSF', 'coyoteRSF', 'bearRSF')
}

both <- merge(
  x = edges,
  y = DT,
  by.x = c('id', 'timegroup'),
  by.y = c('NN', 'timegroup'),
  all.x = TRUE,
  suffixes = c('', '.nn')
)
both

# NA in neighbour means that there were less than the NbyTime in the timegroup
# Careful with the columns selected in the second argument and 
#  subsequent neighbourValCols
DT <- merge(DT, 
            DT[, .(neighbour1 = id, timegroup,
                   rEASTING = EASTING, rNORTHING = NORTHING,
                   rstepLength = stepLength,
                   rpredatorRSF = predatorRSF, rpreyRSF = preyRSF,
                   rabsAngle = absAngle, rrelAngle = relAngle)],
            all.x = TRUE,
            suffixes = c('', 'r'))


neighbourValCols <- c('rEASTING', 'rNORTHING', 'rstepLength',
                      'rpredatorRSF', 'rpreyRSF',
                      'rabsAngle', 'rrelAngle')
message(paste(DT[id == neighbour1, .N], 
"row(s) where id is equal to the NN
... replaced with NA"))
DT[id == neighbour1, (neighbourValCols) := NA]


### Create Dyadic ID ----
source('R/0-functions/DyadicID.R')
# Since the merge reorders, we have to reassign
DT <- DyadId(DT, idCol, neighbourCols)

### Calculate dyadic distance ----
source('R/0-functions/DyadicDistance.R')
DyadicDistance(DT, coordCols = coordCols,
               neighbourCoordCols = paste0('r', coordCols),
               returnIntermediate = FALSE)

### Differences within dyads ----
# Dif in step length
DT[, dSI := abs(stepLength - rstepLength)]

# Dif in abs Angle
DT[, dAbsAng := abs(absAngle - rabsAngle)]

# Dif in RSF
DT[, dPredRSF := abs(predatorRSF - rpredatorRSF)]
DT[, dPreyRSF := abs(preyRSF - rpreyRSF)]

# Avg RSF
DT[, avgPredRSF := rowMeans(.SD), .SDcols = c('predatorRSF', 'rpredatorRSF')]
DT[, avgPreyRSF := rowMeans(.SD), .SDcols = c('preyRSF', 'rpreyRSF')]

### End RSF values ----
DT[, endPredRSF := shift(predatorRSF, 1, NA, 'lead')]
DT[, endPreyRSF := shift(preyRSF, 1, NA, 'lead')]

### Number of neighbours within distance ----
# Find the number of neighbours within specific distance threshold
distanceThreshold <- 500
withinCol <- paste0('nWithin', distanceThreshold)

source('R/0-functions/FindNumbWithinDistance.R')
DT[NbyTime > 1, 
    (withinCol) := FindNumbWithinDist(.SD, distanceThreshold,
                                      coordCols, idCol),
    by = timegroup]

### Output ----
saveRDS(DT, paste0('output/nna/', species, 'NNA.Rds'))
