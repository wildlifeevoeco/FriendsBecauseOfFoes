### Nearest Neighbour Analysis ----
# Purpose: Determine number of NN within dist threshold and dist to NN
# Inputs: Elk, caribou relocation data
# Outputs: NN data



#TODO: Grab Hance's updated social metrics from modeling script

### Packages ----
libs <- c('data.table', 
          'ewc',
          'SearchTrees', 'igraph')
lapply(libs, require, character.only = TRUE)


### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'elk'
DT <- readRDS(paste0('output/angles/', species, 'Angle.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

### Checks ----
# Do any timegroups have the same individual twice?
all.equal(DT[, .(N = uniqueN(id)), by = timegroup],
          DT[, .N, by = timegroup])

# Which timegroups have more than one individual?
DT[, NbyTime := .N, by = timegroup]
DT[, qplot(NbyTime)]

### Quadtree - Find Nearest Neighbour ----
# How many neighbours 
neighbours <- 1
neighbourCols <- paste0('neighbour', seq(1, neighbours))

# Read in function
source('R/0-functions/NumbQuadTreeNeighbours.R')
# Only running on where there are at least 2 in a timegroup, else the bomb!
DT[NbyTime > neighbours, 
    (neighbourCols) := NumbQuadTreeNeighbours(.SD, coords = coordCols,
                                              neighbours, idCol),
    by = timegroup]
# TODO: investigate the both coords and ..coords exist in calling scope data.table error

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

### Input data ----
# Which species would you like to calculate abs and rel TA for?
species <- 'Caribou'
DT <- readRDS(paste0('output/angles/', species, 'AngleNEW.Rds')) ### new coyote RSF data

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

### Checks ----
# Do any timegroups have the same individual twice?
all.equal(DT[, .(N = uniqueN(id)), by = timegroup],
          DT[, .N, by = timegroup])

# Which timegroups have more than one individual?
DT[, NbyTime := .N, by = timegroup]
DT[, qplot(NbyTime)]

### Quadtree - Find Nearest Neighbour ----
# How many neighbours 
neighbours <- 1
neighbourCols <- paste0('neighbour', seq(1, neighbours))

# Read in function
source('R/0-functions/NumbQuadTreeNeighbours.R')
# Only running on where there are at least 2 in a timegroup, else the bomb!
DT[NbyTime > neighbours, 
   (neighbourCols) := NumbQuadTreeNeighbours(.SD, coords = coordCols,
                                             neighbours, idCol),
   by = timegroup]
# TODO: investigate the both coords and ..coords exist in calling scope data.table error

# NA in neighbour means that there were less than the NbyTime in the timegroup
# Careful with the columns selected in the second argument and 
#  subsequent neighbourValCols
DT <- merge(DT, 
            DT[, .(neighbour1 = id, timegroup,
                   rEASTING = EASTING, rNORTHING = NORTHING,
                   rstepLength = stepLength,
                   rCoyRSF = CoyRSF, rBearRSF = BearRSF, rCarRSF = CarRSF,
                   rabsAngle = absAngle, rrelAngle = relAngle)],
            all.x = TRUE,
            suffixes = c('', 'r'))


neighbourValCols <- c('rEASTING', 'rNORTHING', 'rstepLength',
                      'rCoyRSF', 'rBearRSF', 'rCarRSF',
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
DT[, dCoyRSF := abs(CoyRSF - rCoyRSF)]
DT[, dBearRSF := abs(BearRSF - rBearRSF)]
DT[, dCarRSF := abs(CarRSF - rCarRSF)]

# Avg RSF
DT[, avgCoyRSF := rowMeans(.SD), .SDcols = c('CoyRSF', 'rCoyRSF')]
DT[, avgBearRSF := rowMeans(.SD), .SDcols = c('BearRSF', 'rBearRSF')]
DT[, avgCarRSF := rowMeans(.SD), .SDcols = c('CarRSF', 'rCarRSF')]

### End RSF values ----
DT[, endCoyRSF := shift(CoyRSF, 1, NA, 'lead')]
DT[, endBearRSF := shift(BearRSF, 1, NA, 'lead')]
DT[, endCarRSF := shift(CarRSF, 1, NA, 'lead')]


### ALEC THIS PORTION CAUSED AN ERROR, NOT RUNNING IT, IT WILL NEED TO BE ADDRESSED

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
saveRDS(DT, paste0('output/nna/', species, 'NNANEW.Rds')) ### New coyote RSF values

### Figures ----
qplot(get(withinCol), data = DT, 
      main = paste('Number of neighbours within', distanceThreshold),
      xlab = 'Number of Neighbours')

# Check NN at a timegroup
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = DT[timegroup == 4]) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()


