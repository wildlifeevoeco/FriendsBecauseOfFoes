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
  species <- 'caribou'
}
DT <- readRDS(paste0('output/4-sociality/', species, 'Angle.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

if (truelength(DT) == 0) invisible(alloc.col(DT))

### Checks ----
# Do any timegroups have the same individual twice?
all.equal(DT[, .(N = uniqueN(id)), by = timegroup],
          DT[, .N, by = timegroup])

# Number of individuals in each timegroup
DT[, nByTimegroup := .N, timegroup]


### Find nearest neighbour with spatsoc ----
edges <- edge_nn(DT, id = idCol, coords = coordCols, 
                 timegroup = 'timegroup',
                 returnDist = TRUE)
# Check
edges[, .N, timegroup][, sum(N)] == nrow(edges)

### Create Dyadic ID ----
dyad_id(edges, id1 = 'ID', id2 = 'NN')

# Check
edges[is.na(dyadID), .N] == 0

# NOTE: dyadN is not always equal to 2 - some cases the NN for A is not B, while B-A, A-C and C-A for example
edges[, dyadN := .N, .(dyadID, timegroup)]

edges[dyadN > 2, .N] == 0


### Combine ID + NN columns
cols <- c('EASTING',
          'NORTHING',
          'stepLength',
          'absAngle',
          'relAngle',
          'nByTimegroup',
          'season')

if (species == 'elk') {
  rsfCols <- c('predatorRSF', 'preyRSF')
  cols <- c(cols, rsfCols)
} else if (species == 'caribou') {
  rsfCols <-
    c('predatorRSF',
      'preyRSF',
      'coyoteRSF',
      'bearRSF')
  cols <- c(cols, rsfCols)
}

slim <- DT[, .SD, .SDcols = c('id', 'timegroup', cols)]

setnames(slim, 'id', 'ID')

m <- edges[slim, on = c('ID', 'timegroup')]


suff <- '.nn'
out <- merge(
  x = m,
  y = slim,
  by.x = c('NN', 'timegroup'),
  by.y = c('ID', 'timegroup'),
  all.x = TRUE,
  suffixes = c('', suff)
)


### Differences within dyads ----
# Dif in step length
out[, dSI := abs(stepLength - stepLength.nn)]

# Dif in abs Angle
out[, dAbsAng := 180 - abs(abs(absAngle - absAngle.nn) - 180)]


## RSF
for (col in rsfCols) {
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
}


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
  dist = 'stepLength',
  radians = FALSE
)


### Output ----
# DyadID + timegroup
out[, dyadTime := paste(dyadID, timegroup, sep = '-')]


saveRDS(out, paste0('output/4-sociality/', species, 'NNA.Rds'))


message('=== NNA COMPLETE ===')
