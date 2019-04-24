message('=== Wolf data preparation ===')
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi


### Packages ----
pkgs <- c('data.table', 'ggplot2', 'magrittr',
          'spatsoc', 'ewc', 
          'sp', 'rgdal', 'adehabitatLT')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
# List individual wolf sheets
paths <- dir('input/locs/RMNP_WolfLocations', '*.csv',
             full.names = TRUE)

# Check how many times each column appears in the sheets
cols <-
  rbindlist(lapply(
    paths,
    FUN = function(x)
      fread(x) %>% colnames(.) %>%
      data.table(col = ., path = x, val = 1)
  )) %>% dcast(path ~ col, value.var = 'val')
cols[, path := rowid(path)][, lapply(.SD, sum, na.rm = TRUE), .SDcol = colnames(cols)]

# Read in each and rbindlist
#   To make sure LATITUDE and Latitude are considered as the same, 
#   change all colnames to lower case, and rbindlist!
wolf <- rbindlist(lapply(
  paths,
  FUN = function(p) {
    fread(p) %>% setnames(colnames(.), tolower(colnames(.)))
  }
),
fill = TRUE,
use.names = TRUE)

# List drop and keep columns
keepCols <-
  c(
    'wolfid',
    'packid',
    'longitude',
    'latitude',
    'collar',
    'gmtdate',
    'gmttime',
    'time',
    'date',
    '2d3d',
    'fixstatus',
    'info'
  )
dropCols <- colnames(wolf)[!(colnames(wolf) %in% keepCols)]

# Drop columns above and rename 2d3d to Fix2d3d
wolf[, (dropCols) := NULL][, c('Fix2d3d', '2d3d') := .(`2d3d`, NULL)]

### Variables ----
xCol <- 'longitude'
yCol <- 'latitude'
timeCol <- 'gmttime'
dateCol <- 'gmtdate'
idCol <- 'wolfid'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

tz <- 'GMT'

### Add fields ----
# Date time fields
prep_date(
  DT = wolf, 
  dateCol = dateCol, 
  timeCol = timeCol,
  tz = tz
)

# Season
wolf[julday %between% winter, season := 'winter']
wolf[julday %between% spring, season := 'spring']

# Group Time - from spatsoc
group_times(wolf, 'datetime', '15 minutes')

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
wolf <- na.omit(wolf, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
wolf <- wolf[get(xCol) != 0 & get(xCol) < 0]

# Drop duplicate fixes
wolf[wolf[, .N, by = c('datetime', idCol)][N > 1],
     on = c('datetime', idCol), drop := TRUE]
wolf[!is.na(drop), drop := c(FALSE, rep(TRUE, .N-1)), by = c('datetime', idCol)]
wolf <- wolf[is.na(drop) | !(drop)]


### Project + Step Length ----
# Project coordinates to UTM
wolf[, c(projXCol, projYCol) := 
       as.data.table(project(cbind(get(xCol), get(yCol)), utmMB))]

# Step Length
step_length(
  wolf,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr', 'season'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Rarify locs ----
fixrate <- data.table(
  season = c('spring', 'winter'),
  rate = c(2, 2)
)

wolf[, idtime := paste(get(idCol), datetime, sep = " ")]

collect <- rbindlist(lapply(
  1:nrow(fixrate),
  FUN = function(x) {
    subSeason <- wolf[season == fixrate[x, season]]
    
    traj <- as.ltraj(
      subSeason[, .SD, .SDcols = c(projXCol, projYCol)],
      date = subSeason$datetime,
      id = subSeason[[idCol]],
      infolocs = subSeason[, c('idtime')]
    )
    
    ref <- round(min(subSeason$datetime), "hour")
    
    trajNA <-
      setNA(traj, ref, fixrate[x, rate], units = "hour")
    
    traj0 <- sett0(trajNA, ref, fixrate[x, rate], units = "hour")
    
    ld(traj0)
  }
))

out <- merge(na.omit(collect), 
             wolf, by = 'idtime')

step_length(
  out,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr', 'season'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Summary information ----
# N by fix quality
# wolf[, .N, by = info]
# wolf[, .N, by = Fix2d3d]
# wolf[, .N, by = fixstatus]

# How many unique animals?
# wolf[, uniqueN(get(idCol))]

# How many unique animals per year?
# wolf[, .('N Unique Wolves' = uniqueN(get(idCol))), by = yr]

# Temporal distribution of locs
# wolf[order(mnth), .N, by = mnth]
# wolf[order(yr), .N, by = yr]

### Subset ----
# Thresholds
moveRateThreshold <- 20000

wolf <- wolf[moveRate < moveRateThreshold]


# Spatially constrain to RMNP bounds
wolfSP <- SpatialPointsDataFrame(wolf[, .(get(projXCol), get(projYCol))],
                                 wolf,
                                 proj4string = CRS(utmMB))

wolf <- data.table(over(mbBounds, wolfSP, returnList = TRUE)[[1]])

# Remove points at the office (le::crop)
minOfficeX <- 432969
maxOfficeX <- 433579
minOfficeY <- 5611525
maxOfficeY <- 5612110
wolf <- wolf[!(inrange(EASTING, minOfficeX, maxOfficeX) &
                 inrange(NORTHING, minOfficeY, maxOfficeY))]


### Output ----
# Match variables to output variables = consistent variables across species
source('scripts/0-variables/variables.R')
wolf[, SPECIES := 'WOLF']

outputVariables <- c(outputVariables, 'packid')

setnames(wolf, c('wolfid', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime',
                 'packid'),
         outputVariables)

saveRDS(wolf[, ..outputVariables], 'output/1-data-prep/wolf.Rds')

message('=== WOLF PREP COMPLETE ===')
