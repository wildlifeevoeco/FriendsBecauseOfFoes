### Wolf data preparation ----
# Authors: Alec Robitaille
# Purpose: To prepare wolf data for EWC
# Inputs: Wolf relocation data
# Outputs: Prepared wolf data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',  'spatsoc',
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)

### Input data ----
# List individual wolf sheets
paths <- dir('input/locs/RMNP_WolfLocations', '*.csv',
             full.names = TRUE)

# Check how many times each column appears in the sheets
cols <- rbindlist(lapply(paths, FUN = function(x) fread(x) %>% colnames(.) %>% 
                           data.table(col = ., path = x, val = 1))) %>% dcast(path~col, value.var = 'val')
cols[, path := rowid(path)][, lapply(.SD, sum, na.rm = TRUE), .SDcol = colnames(cols)]

# Read in each and rbindlist
#   To make sure LATITUDE and Latitude are considered as the same, 
#   change all colnames to lower case, and rbindlist!
wolf <- rbindlist(lapply(paths, FUN = function(p){
  fread(p) %>% setnames(colnames(.), tolower(colnames(.)))
}), fill = TRUE, use.names = TRUE)

# List drop and keep columns
keepCols <- c('wolfid', 'packid', 'longitude', 'latitude', 'collar', 'gmtdate', 'gmttime', 
              'time', 'date', '2d3d', 'fixstatus', 'info')
dropCols <- colnames(wolf)[!(colnames(wolf) %in% keepCols)]

# Drop columns above and rename 2d3d to Fix2d3d
wolf[, (dropCols) := NULL][, c('Fix2d3d', '2d3d') := .(`2d3d`, NULL)]

# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

### Variables ----
xCol <- 'longitude'
yCol <- 'latitude'
# Time zone of this data is CST (at least some seasons)
timeCol <- 'time'
dateCol <- 'date'
idCol <- 'wolfid'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

### Add fields ----
# Date time fields
source('R/0-functions/DatePrep.R')
DatePrep(wolf, dateCol, timeCol)

# Season
source('R/0-variables/CutOffThresholds.R')
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

### Project + Step Length ----
# Project coordinates to UTM
wolf[, c(projXCol, projYCol) := as.data.table(project(cbind(get(xCol), get(yCol)), 
                                                      utm))]

# Step Length
source('R/0-functions/StepLength.R')
StepLength(wolf, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

difTimeThreshold <- 2
wolf <- wolf[round(difdatetime) == difTimeThreshold]

StepLength(wolf, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)


### Subset by collar fields ----
# N by fix quality
kable(wolf[, .N, by = info])
kable(wolf[, .N, by = Fix2d3d])
kable(wolf[, .N, by = fixstatus])

### Summary information ----
# How many unique animals?
wolf[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(wolf[, .('N Unique Wolves' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(wolf[order(mnth), .N, by = mnth])
kable(wolf[order(yr), .N, by = yr])

### Subset ----
# Thresholds
moveRateThreshold <- 20000

# Map_Quality, NAV

wolf <- wolf[moveRate < moveRateThreshold]


# Spatially constrain to RMNP bounds
wolfSP <- SpatialPointsDataFrame(wolf[, .(get(projXCol), get(projYCol))],
                                 wolf,
                                 proj4string = CRS(utm))

wolf <- data.table(over(bounds, wolfSP, returnList = TRUE)[[1]])

# Remove points at the office (le::crop)
minOfficeX <-432969
maxOfficeX <-433579
minOfficeY <-5611525
maxOfficeY <-5612110
wolf<-wolf[!(inrange(EASTING, minOfficeX, maxOfficeX) & 
         inrange(NORTHING, minOfficeY, maxOfficeY))]


### Output ----
# Match variables to output variables = consistent variables across species
source('R/0-variabless/PrepDataOutputVariables.R')
wolf[, SPECIES := 'WOLF']

outputVariables <- c(outputVariables, 'packid')

setnames(wolf, c('wolfid', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime',
                 'packid'),
         outputVariables)

saveRDS(wolf[, ..outputVariables], 'output/data-prep/wolf.Rds')


### Plots ----
# Plot locs by year on RMNP bounds 
source('R/0-functions/PlotLocsByFigure.R')

# To PDF 
# pdf('graphics/data-prep/wolf-locs-by-year.pdf')
wolf[order(yr), PlotLocsBy(.SD, bounds, .BY[[1]], 'id'),
     by = yr]
# dev.off()

# Temporal distribution of locs
source('R/0-functions/TemporalDistributionFigure.R')
TempDistFig(wolf)

# ggsave('graphics/data-prep/wolf-temp-dist.png', TempDistFig(wolf), 'png')
