message('=== Bear data preparation ===')
# Authors: Quinn Webber, Alec Robitaille


### Packages ----
library(data.table)
library(ggplot2)
library(spatsoc)
library(sp)
library(rgdal)

### Variables ----
source('scripts/0-variables/variables.R')
source('R/prep_date.R')

### Input data ----
dropCols <-
  c(
    'FIX_ID',
    'VENDOR_CL',
    'AGE',
    'COLLAR_FILE_ID',
    'EXCLUDE',
    'DOP',
    'LOCQUAL',
    'VALIDATED',
    'COLLAR_TYPE_CL',
    'COLLAR_ID',
    'Fix_Time_Delta',
    'EPSG_CODE'
  )

# Read in bear data, dropping above columns
bear <- fread('input/locs/Bears.csv',
              drop = dropCols)


### Variables ----
xCol <- 'X_COORD'
yCol <- 'Y_COORD'
dateCol <- 'FIX_DATE'
timeCol <- 'FIX_TIME'
idCol <- 'ANIMAL_ID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

tz <- 'America/St_Johns'

### Add fields ----
## Date time fields
prep_date(
  DT = bear, 
  dateCol = dateCol, 
  timeCol = timeCol, 
  tz = tz
)

# Check!
bear[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Season
bear[julday %between% winter, season := 'winter']
bear[julday %between% spring, season := 'spring']

group_times(bear, 'datetime', '15 minutes')

bear[, .N, c('timegroup', idCol)][N > 1, .N]

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
bear <- na.omit(bear, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
bear <- bear[get(xCol) != 0 & get(xCol) < 0]

### Project + Step Length ----
# Project coordinates to UTM
bear[, c(projXCol, projYCol) := 
       as.data.table(project(cbind(get(xCol), get(yCol)), utmNL))]

# Step Length
step_length(
  bear,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Summary information ----
# How many unique animals?
bear[, uniqueN(get(idCol))]

# How many unique animals per year?
bear[, .('N Unique Bears' = uniqueN(get(idCol))), by = yr]

# Temporal distribution of locs
bear[order(mnth), .N, by = mnth]
bear[order(yr), .N, by = yr]

### Subset ----
# Thresholds
moveRateThreshold <- 10000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365
herdList <- 'MIDRIDGE'

bear <- bear[moveRate < moveRateThreshold &
               difdatetime < difTimeThreshold &
               between(julday, lowJul, highJul) & 
               HERD %in% herdList]

### Output ----
# Match variables to output variables = consistent variables across species
outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(bear, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 
                 'difdatetime', 'HERD', 'SEX'),
         outputVariables)

saveRDS(bear[, ..outputVariables], 'output/1-data-prep/bear.Rds')
