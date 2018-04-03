### Elk data preparation ----
# Authors: Alec Robitaille
# Purpose: To prepare elk data for EWC
# Inputs: Elk relocation data
# Outputs: Prepared elk data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

### Packages ----
libs <- c('data.table', 'ggplot2', 
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)

### Input data ----
# Read in elk data
elk <- fread('input/locs/RMNP_ElkData_clean.csv')

# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

### Variables ----
xCol <- 'X'
yCol <- 'Y'
dateCol <- 'datetime'
hourCol <- 'Hour'
minCol <- 'Minute'
idCol <- 'ElkID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

### Add fields ----
# Missing single time field, we'll combine hour and minute and save as 'time'
# Combine hour, minute to timeCol
elk[, time := paste0(get(hourCol), ':', sprintf('%02d', get(minCol)))]
timeCol <- 'time'

## Date time fields
source('R/functions/DatePrep.R')
DatePrep(elk, dateCol, timeCol, dateFormat = '%d/%m/%Y')

# Check!
elk[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Drop old date time fields
dropCol <- c('Year', 'Month', 'Day', 'Hour', 'Minute', 'time')
elk[, (dropCol) := NULL]

# Season
source('R/variables/CutOffThresholds.R')
elk[julday %between% winter, season := 'winter']
elk[julday %between% spring, season := 'spring']

# Group Time - from spatsoc
source('R/functions/Group-Time-spatsoc.R')
GroupTimes(bear, 'datetime', '15 minutes')

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, dateCol, 'season')
elk <- na.omit(elk, cols = checkCols)


### Project + Step Length ----
# Coordinates already projected, simply rename
elk[, c(projXCol, projYCol) := .(get(xCol), get(yCol))]

# Step Length
source('R/functions/StepLength.R')
StepLength(elk, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

difTimeThreshold <- 2
elk <- elk[round(difdatetime) == difTimeThreshold]

StepLength(elk, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)
  
### Summary information ----
# How many unique animals?
elk[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(elk[order(yr), .('N Unique Elks' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(elk[order(mnth), .N, by = mnth])
kable(elk[order(yr), .N, by = yr])

# Steplength distribution
elk[, qplot(stepLength)]

### Subset ----
# Thresholds
stepLengthThreshold <- 7750000
moveRateThreshold <- 2000
lowJul <- 0
highJul <- 365

# Map_Quality, NAV

elk <- elk[stepLength < stepLengthThreshold & 
             moveRate < moveRateThreshold &
             between(julday, lowJul, highJul)]

# Spatially constrain to RMNP bounds
elkSP <- SpatialPointsDataFrame(elk[, .(get(projXCol), get(projYCol))],
                                elk,
                                proj4string = CRS(utm))

elk <- data.table(over(bounds, elkSP, returnList = TRUE)[[1]])


### Output ----
# Match variables to output variables = consistent variables across species
source('R/variables/PrepDataOutputVariables.R')

elk[, SPECIES := 'ELK']

setnames(elk, c('ElkID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime'),
         outputVariables)

saveRDS(elk[, ..outputVariables], 'output/data-prep/elk.Rds')

### Plots ----
# Plot locs by year on RMNP bounds 
source('R/functions/PlotLocsByFigure.R')

# To PDF 
# pdf('graphics/data-prep/elk-locs-by-year.pdf')
elk[order(yr),
    PlotLocsBy(.SD, bounds, .BY[[1]], 'id'),
    by = yr]
# dev.off()

# Temporal distribution of locs
source('R/functions/TemporalDistributionFigure.R')
TempDistFig(elk)

# ggsave('graphics/data-prep/elk-temp-dist.png', TempDistFig(elk), 'png')