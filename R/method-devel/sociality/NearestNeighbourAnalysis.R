### Nearest Neighbour Analysis ----
# Authors: Alec Robitaille
# Purpose: Determine number of NN within dist threshold and dist to NN
# Inputs: Elk, caribou relocation data
# Outputs: NN data
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'SearchTrees', 'igraph',
          'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
elk <- readRDS('output/data-prep/elk.Rds')

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

### Checks ----
# Do any timegroups have the same individual twice?
all.equal(elk[, .(N = uniqueN(id)), by = timegroup],
          elk[, .N, by = timegroup])

# Which timegroups have more than one individual?
elk[, NbyTime := .N, by = timegroup]
elk[, qplot(NbyTime)]

### Quadtree - Find Nearest Neighbour ----
# How many neighbours 
neighbours <- 1
neighbourCols <- paste0('neighbour', seq(1, neighbours))

# Read in function
source('R/functions/NumbQuadTreeNeighbours.R')
# Only running on where there are at least 2 in a timegroup, else the bomb!
elk[NbyTime > neighbours, 
    (neighbourCols) := NumbQuadTreeNeighbours(.SD, coords = coordCols,
                                              neighbours, idCol),
    by = timegroup]
# TODO: investigate the both coords and ..coords exist in calling scope data.table error

# NA in neighbour means that there were less than the NbyTime in the timegroup
elk <- merge(elk,
             elk[, .(neighbour1 = id, rEASTING = EASTING, rNORTHING = NORTHING, 
                     timegroup, rstepLength = stepLength)],
             all.x = TRUE)


neighbourCols <- c('rEASTING', 'rNORTHING', 'rstepLength')
message(paste(elk[id == neighbour1, .N], 
"row(s) where id is equal to the NN
... replaced with NA"))
elk[id == neighbour1, (neighbourCols) := NA]


### Create Dyadic ID ----
source('R/functions/DyadicID.R')
DyadId(elk, idCol, neighbourCols)


### Calculate dyadic distance ----
source('R/functions/DyadicDistance.R')
DyadicDistance(elk, coordCols = coordCols,
               neighbourCoordCols = paste0('r', coordCols),
               returnIntermediate = FALSE)

### Difference in Step length ----
elk[, dSI := abs(stepLength - rstepLength)]

### Number of neighbours within distance ----
# Find the number of neighbours within specific distance threshold
distanceThreshold <- 5000
withinCol <- paste0('nWithin', distanceThreshold)

source('R/functions/FindNumbWithinDistance.R')
elk[NbyTime > 1, 
    (withinCol) := FindNumbWithinDist(.SD, distanceThreshold,
                                      coordCols, idCol),
    by = timegroup]


### Figures ----
qplot(get(withinCol), data = elk, 
      main = paste('Number of neighbours within', distanceThreshold),
      xlab = 'Number of Neighbours')

# Check NN at a timegroup
ggplot(aes(EASTING, NORTHING, color = factor(id)), 
       data = elk[timegroup == 4]) +
  geom_point() + ggthemes::scale_colour_pander() +
  coord_fixed()
