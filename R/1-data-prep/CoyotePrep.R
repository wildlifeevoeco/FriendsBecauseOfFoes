### Coyote data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare coyote data for EWC
# Inputs: Coyote relocation data
# Outputs: Prepped coyote data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc',
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)

# Note: if spatsoc is not installed, uncomment and run these lines:
# drat::addRepo('LocalRepo', 'https://spatsoc.gitlab.io')
# install.packages('spatsoc')

### Input data ----
dropCols <- c('FIX_ID','VENDOR_CL','AGE','COLLAR_FILE_ID','EXCLUDE','DOP','LOCQUAL',
              'VALIDATED','COLLAR_TYPE_CL','COLLAR_ID','Fix_Time_Delta','EPSG_CODE')
              #'Map_Quality','NAV')

# Read in coyote data, dropping above columns
coyote <- fread('input/locs/Coyote.csv',
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
# Date time fields
source('R/0-functions/DatePrep.R')
DatePrep(coyote, dateCol, timeCol)

# Check!
coyote[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Season
source('R/0-variables/CutOffThresholds.R')

coyote[julday %between% winter, season := 'winter']
coyote[julday %between% spring, season := 'spring']

# group_times from spatsoc
group_times(coyote, 'datetime', '15 minutes')

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
coyote <- na.omit(coyote, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
coyote <- coyote[get(xCol) != 0 & get(xCol) < 0]

### Project Coords, Step length  ----
# Project coordinates to UTM
coyote[, c(projXCol, projYCol) := as.data.table(project(cbind(get(xCol), get(yCol)), utm))]

# Step Length
source('R/0-functions/StepLength.R')
StepLength(coyote, idCol, datetimeCol = 'datetime', yrCol = 'yr',
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

difTimeThreshold <- 2
# Number of locs by id with rounded fixrate..
fr <- coyote[, .N, by = .(ANIMAL_ID, round(difdatetime), season)]
# subset < 12, which is rounded bin has the most locs by id, histo it
fr[round < 12][, .SD[which.max(N)], by = ANIMAL_ID][, qplot(round, binwidth = 1, xlab = 'fix rate (rounded)', facets = ~season)]


coyote <- coyote[round(difdatetime) == difTimeThreshold]

StepLength(coyote, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

### Summary information ----
# How many unique animals?
coyote[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(coyote[, .('N Unique coyotes' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(coyote[order(mnth), .N, by = mnth])
kable(coyote[order(yr), .N, by = yr])

# Herd distribution of coyotes
kable(coyote[, .N, by = HERD])

### Subset ----
# Thresholds
stepLengthThreshold <- 7750000
moveRateThreshold <- 10000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365
herdList <- 'MIDRIDGE'

# Map_Quality, NAV

coyote <- coyote[stepLength < stepLengthThreshold & 
                 moveRate < moveRateThreshold &
                 between(julday, lowJul, highJul)]
                 #$HERD %in% herdList]

### Output ----
# Match variables to output variables = consistent variables across species
source('R/0-variables/PrepDataOutputVariables.R')

outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(coyote, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime',
                 'HERD', 'SEX'),
         outputVariables)

saveRDS(coyote[, ..outputVariables], 'output/data-prep/coyote.Rds')


### Plots ----
# Plot locs by year on NL bounds 
source('R/0-functions/PlotLocsByFigure.R')

# To PDF 
# pdf('graphics/data-prep/coyote-locs-by-year.pdf')
coyote[, PlotLocsBy(.SD, nlBounds, .BY[[1]], 'id'),
       by = yr]
# dev.off()


# Temporal distribution of locs
source('R/0-functions/TemporalDistributionFigure.R')
TempDistFig(coyote)

# ggsave('graphics/data-prep/coyote-temp-dist.png', TempDistFig(coyote), 'png')
