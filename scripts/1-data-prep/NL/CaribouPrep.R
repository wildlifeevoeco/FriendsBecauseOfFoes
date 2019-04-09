### Caribou data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare caribou data for EWC
# Inputs: Caribou relocation data
# Outputs: Prepared caribou data as RDS

# Copyright: ./LICENSE.md 

### Packages ----
libs <- c('data.table', 'ggplot2', 
          'spatsoc', 'ewc',
          'sp', 'rgdal')
lapply(libs, require, character.only = TRUE)

### Set variables ----
source('scripts/0-variables/variables.R')

### Input data ----
dropCols <- c('V1','FIX_ID','EPSG_CODE','Fix_Time_Delta',
              'COLLAR_FILE_ID',
              'COLLAR_ID','COLLAR_TYPE_CL',
              'EXCLUDE','VENDOR_CL','AGE','DOP','VALIDATED')

# Read in caribou data, dropping above columns
caribou <- fread('input/locs/AllCaribouDataRaw.csv',
                 drop = dropCols)

### Variables ----
xCol <- 'X_COORD'
yCol <- 'Y_COORD'
dateCol <- 'FIX_DATE'
timeCol <- 'FIX_TIME'
idCol <- 'ANIMAL_ID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

# Subset to Middle Ridge right away, so subsequent steps are faster
caribou <- caribou[HERD == 'MIDRIDGE']

### Add fields ----
# Date time fields
prep_date(caribou, dateCol, timeCol)

# Season
caribou[julday %between% winter, season := 'winter']
caribou[julday %between% spring, season := 'spring']

# Temporal grouping
group_times(caribou, 'datetime', '15 minutes')

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
caribou <- na.omit(caribou, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
caribou <- caribou[get(xCol) != 0 & get(xCol) < 0]
 
### Project + Step Length ----
# Project coordinates to UTM
caribou[, c(projXCol, projYCol) := as.data.table(
  project(cbind(get(xCol), get(yCol)), utmNL))]

# Step Length
step_length(
  caribou,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Summary information ----
# How many unique animals?
caribou[, uniqueN(get(idCol))]

# How many unique animals per year?
caribou[order(yr), .('N Unique Caribou' = uniqueN(get(idCol))), by = yr]

# Temporal distribution of locs
caribou[order(mnth), .N, by = mnth]
caribou[order(yr), .N, by = yr]

### Subset ----
# Thresholds
stepLengthThreshold <- 10000
moveRateThreshold <- 500000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365

caribou <- caribou[stepLength < stepLengthThreshold & 
                   moveRate < moveRateThreshold &
                   difdatetime < difTimeThreshold &
                   between(julday, lowJul, highJul)]

### Output ----
# Match variables to output variables = consistent variables across species
outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(caribou, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 
                 'moveRate', 'difdatetime',
                 'HERD', 'SEX'),
         outputVariables)

saveRDS(caribou[, ..outputVariables], 
        'output/1-data-prep/caribou.Rds')
