message('=== Nearest Neighbour Analysis ===')
# Authors: Alec Robitaille, Hance Ellington

### Packages ----
pkgs <- c('data.table', 'ewc', 'spatsoc', 'igraph')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Input data ----
# Which species would you like to calculate abs and rel TA for?
# Flexible for Makefile: if running script manually, edit species in else block
if (length(commandArgs(trailingOnly = TRUE) > 1)) {
  species <- tolower(commandArgs(trailingOnly = TRUE)[2])
  print(paste0('using species: ', species))
} else {
  species <- 'elk'
}
DT <- readRDS(paste0('output/4-sociality/', species, 'Angle.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

if (truelength(DT) == 0) alloc.col(DT)

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

### Create Dyadic ID ----
dyads <- dyad_id(edges, id = 'ID', nn = 'NN')

edges[dyads, dyadID := dyadID, on = .(ID = ID, NN = NN)]

# Check
edges[is.na(dyadID), .N] == 0


### Combine ID + NN columns
cols <- c('EASTING',
          'NORTHING',
          'stepLength',
          'absAngle',
          'relAngle',
          'nByTimegroup')

if (species == 'elk') {
  rsfCols <- c('predatorRSF', 'preyRSF')
  cols <- c(cols, rsfCols)
} else if (species == 'caribou') {
  rsfCols <-
    c('predatorRSF',
      'preyRSF',
      'caribouRSF',
      'coyoteRSF',
      'bearRSF')
  cols <- c(cols, rsfCols)
}

slim <- DT[, .SD, .SDcols = c('id', 'timegroup', cols)]

m <- merge(
  x = edges,
  y = slim,
  by.x = c('ID', 'timegroup'),
  by.y = c('id', 'timegroup'),
  all.x = TRUE
)

suff <- '.nn'
out <- merge(
  x = m,
  y = slim,
  by.x = c('NN', 'timegroup'),
  by.y = c('id', 'timegroup'),
  all.x = TRUE,
  suffixes = c('', suff)
)

### Calculate dyadic distance ----
dyad_dist(
  DT = out,
  coords = coordCols,
  suffix = suff,
  returnIntermediate = FALSE
)

### Differences within dyads ----
# Dif in step length
out[, dSI := abs(stepLength - stepLength.nn)]

# Dif in abs Angle
out[, dAbsAng := abs(absAngle - absAngle.nn)]


## RSF
#TODO: where NL + spring, this:     (nl.dyad2$predatorRSF + nl.dyad2$rpredatorRSF) / 2, ?

lapply(rsfCols, function(col) {
  difnm <- paste0('d', col)
  avgnm <- paste0('avg', col)
  endnm <- paste0('end', col)
  sufnm <- paste0(col, suff)
  
  # Difference within dyad
  out[, (difnm) := abs(.SD[[1]] - .SD[[2]]), .SDcols = c(col, sufnm)]
  
  # Average within dyad 
  out[, (avgnm) := rowMeans(.SD), .SDcols = c(col, sufnm)]
  
  # End RSF 
  out[, (endnm) := shift(.SD, 1, NA, 'lead'), .SDcols = col]
})


### Find neighbours within distance with spatsoc ----
threshold <- 500

# Which individuals are within threshold distance of each other in each timegroup?
wiDist <- edge_dist(
  DT = DT,
  threshold = threshold,
  id = idCol,
  coords = coordCols,
  timegroup = 'timegroup',
  fillNA = FALSE
)

# Check
unique(wiDist)[, .N] == wiDist[, .N]

# Count number of edges for each individual
wiDist[, nWithin := .N, by = .(timegroup, ID1)]


out[wiDist, nWithin := nWithin, 
    on = .(ID = ID1, timegroup = timegroup)]

### Calculate DI
calc_di(
  DT = out,
  suffix = '.nn',
  angle = 'absAngle',
  dist = 'stepLength'
)


### Output ----
# DyadID + timegroup
out[, dyadTime := paste(dyadID, timegroup, sep = '-')]


saveRDS(out, paste0('output/4-sociality/', species, 'NNA.Rds'))


message('=== NNA COMPLETE ===')
