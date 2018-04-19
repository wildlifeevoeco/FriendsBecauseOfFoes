### Bear data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare bear data for EWC
# Inputs: Bear relocation data
# Outputs: Prepared bear data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra', 
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
dropCols <- c('FIX_ID','VENDOR_CL','AGE','COLLAR_FILE_ID','EXCLUDE','DOP','LOCQUAL',
              'VALIDATED','COLLAR_TYPE_CL','COLLAR_ID','Fix_Time_Delta','EPSG_CODE')
              #'Map_Quality','NAV')

# Read in bear data, dropping above columns
bear <- fread('input/locs/Bears.csv',
              drop = dropCols)

# UTM zone 21N
utm <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm)

### Variables ----
xCol <- 'X_COORD'
yCol <- 'Y_COORD'
dateCol <- 'FIX_DATE'
timeCol <- 'FIX_TIME'
idCol <- 'ANIMAL_ID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'


### Add fields ----
## Date time fields
source('R/functions/DatePrep.R')
DatePrep(bear, dateCol, timeCol)

# Check!
bear[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Season
source('R/variables/CutOffThresholds.R')

bear[julday %between% winter, season := 'winter']
bear[julday %between% spring, season := 'spring']

# Group Time - from spatsoc
source('R/functions/Group-Time-spatsoc.R')
GroupTimes(bear, 'datetime', '15 minutes')


### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
bear <- na.omit(bear, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
bear <- bear[get(xCol) != 0 & get(xCol) < 0]

### Project + Step Length ----
# Project coordinates to UTM
bear[, c(projXCol, projYCol) := as.data.table(project(cbind(get(xCol), get(yCol)), 
                                                         utm))]

# Step Length
source('R/functions/StepLength.R')
StepLength(bear, idCol, datetimeCol = 'datetime', yrCol = 'yr',
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

### Summary information ----
# How many unique animals?
bear[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(bear[, .('N Unique Bears' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(bear[order(mnth), .N, by = mnth])
kable(bear[order(yr), .N, by = yr])

### Subset ----
# Thresholds
stepLengthThreshold <- 7750000
moveRateThreshold <- 10000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365
herdList <- 'MIDRIDGE'

# Map_Quality, NAV

bear <- bear[stepLength < stepLengthThreshold & 
               moveRate < moveRateThreshold &
               difdatetime < difTimeThreshold &
               between(julday, lowJul, highJul) & 
               HERD %in% herdList]

### Output ----
# Match variables to output variables = consistent variables across species
source('R/variables/PrepDataOutputVariables.R')

outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(bear, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime',
                 'HERD', 'SEX'),
         outputVariables)

saveRDS(bear[, ..outputVariables], 'output/data-prep/bear.Rds')

### Figures ----
# Plot locs by year on NL bounds 
source('R/functions/PlotLocsByFigure.R')

# To PDF 
# pdf('graphics/data-prep/bear-locs-by-year.pdf')
bear[,
     PlotLocsBy(.SD, nlBounds, .BY[[1]], 'id'),
     by = yr]
# dev.off()

# Temporal distribution of locs
source('R/functions/TemporalDistributionFigure.R')
TempDistFig(bear)

# ggsave('graphics/data-prep/bear-temp-dist.png', TempDistFig(bear), 'png')