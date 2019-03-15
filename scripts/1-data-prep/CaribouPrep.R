### Caribou data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare caribou data for EWC
# Inputs: Caribou relocation data
# Outputs: Prepared caribou data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

### Packages ----
libs <- c('data.table', 'ggplot2', 
          'spatsoc', 'ewc',
          'sp', 'rgdal')
lapply(libs, require, character.only = TRUE)

### Input data ----
dropCols <- c('V1','FIX_ID','EPSG_CODE','Fix_Time_Delta',
              'COLLAR_FILE_ID',
              'COLLAR_ID','COLLAR_TYPE_CL',
              'EXCLUDE','VENDOR_CL','AGE','DOP','VALIDATED')

# Read in caribou data, dropping above columns
caribou <- fread('input/locs/AllCaribouDataRaw.csv',
                 drop = dropCols)

# UTM zone 21N
utm <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- spTransform(readOGR('input/etc/NL-Bounds/NL-Bounds.shp'),
                        CRSobj = utm)

### Variables ----
xCol <- 'X_COORD'
yCol <- 'Y_COORD'
dateCol <- 'FIX_DATE'
timeCol <- 'FIX_TIME'
idCol <- 'ANIMAL_ID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'


# Drop down to Middle Ridge right away, so subsequent steps are faster
caribou <- caribou[HERD == 'MIDRIDGE']

### Add fields ----
# Date time fields
prep_date(caribou, dateCol, timeCol)

# Check!
caribou[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Season
source('scripts/0-variables/CutOffThresholds.R')

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
  project(cbind(get(xCol), get(yCol)), utm))]

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

# Map_Quality, NAV

caribou <- caribou[stepLength < stepLengthThreshold & 
                   moveRate < moveRateThreshold &
                   difdatetime < difTimeThreshold &
                   between(julday, lowJul, highJul)]


### Output ----
# Match variables to output variables = consistent variables across species
source('scripts/0-variables/PrepDataOutputVariables.R')

outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(caribou, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 
                 'moveRate', 'difdatetime',
                 'HERD', 'SEX'),
         outputVariables)

saveRDS(caribou[, ..outputVariables], 
        'output/data-prep/caribou.Rds')


### Plots ----
# Plot locs by year on NL bounds 
source('R/0-functions/PlotLocsByFigure.R')

# To PDF 
pdf('graphics/data-prep/caribou-locs-by-year.pdf')
caribou[, PlotLocsBy(.SD, nlBounds, .BY[[1]], 'id'),
        by = yr]
dev.off()


# Temporal distribution of locs
source('R/0-functions/TemporalDistributionFigure.R')
TempDistFig(caribou)

ggsave('graphics/data-prep/caribou-temp-dist.png', TempDistFig(caribou), 'png')