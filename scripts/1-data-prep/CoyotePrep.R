### Coyote data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare coyote data for EWC
# Inputs: Coyote relocation data
# Outputs: Prepped coyote data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 
          'spatsoc', 'ewc',
          'sp', 'rgdal', 'adehabitatLT')
lapply(libs, require, character.only = TRUE)

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

# Read in coyote data, dropping above columns
coyote <- fread('input/locs/Coyote.csv',
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

### Add fields ----
# Date time fields
prep_date(coyote, dateCol, timeCol)

# What is the difference in hours between fixes
coyote[order(datetime), 
       difdatetime := 
         difftime(datetime, 
                  shift(datetime, 1), 
                  units = 'hours'),
       by = idCol]

# Check!
coyote[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

# Season
source('scripts/0-variables/CutOffThresholds.R')

coyote[julday %between% winter, season := 'winter']
coyote[julday %between% spring, season := 'spring']

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, timeCol, dateCol, 'season')
coyote <- na.omit(coyote, cols = checkCols)

# Subset any 0 in lat/long and where longitude is positive
coyote <- coyote[get(xCol) != 0 & get(xCol) < 0]

# Drop any rows that have identical fix times as previous
coyote <- coyote[difdatetime != 0]

### Project Coords, Step length  ----
# Project coordinates to UTM
coyote[, c(projXCol, projYCol) := 
         as.data.table(
           project(cbind(get(xCol), get(yCol)), utm))]

# Step Length
step_length(
  coyote,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Rarify locs ----
fixrate <- data.table(
  season = c('spring', 'winter'),
  rate = c(4, 8)
) 
coyote[, idtime := paste(coyote$ANIMAL_ID, coyote$datetime, sep=" ")]

collect <- rbindlist(lapply(
  1:nrow(fixrate),
  FUN = function(x) {
    subSeason <- coyote[season == fixrate[x, season]]
    
    traj <- as.ltraj(
      subSeason[, c('EASTING', 'NORTHING')],
      date = subSeason$datetime,
      id = subSeason$ANIMAL_ID,
      infolocs = subSeason[, c('idtime')]
    )

    ref <- round(min(subSeason$datetime), "hour")

    trajNA <-
      setNA(traj, ref, fixrate[x, rate], units = "hour")

    traj0 <- sett0(trajNA, ref, fixrate[x, rate], units = "hour")

    ld(traj0)
  }
))

coyote <- merge(collect, coyote, by = 'idtime')

# Recalculate step length
step_length(
  coyote,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

# group_times from spatsoc
group_times(coyote, 'datetime', '15 minutes')

### Summary information ----
# How many unique animals?
coyote[, uniqueN(get(idCol))]

# How many unique animals per year?
coyote[, .('N Unique coyotes' = uniqueN(get(idCol))), by = yr]

# Temporal distribution of locs
coyote[order(mnth), .N, by = mnth]
coyote[order(yr), .N, by = yr]

# Herd distribution of coyotes
coyote[, .N, by = HERD]

### Subset ----
# Thresholds
stepLengthThreshold <- 7750000
moveRateThreshold <- 10000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365
herdList <- 'MIDRIDGE'

coyote <- coyote[stepLength < stepLengthThreshold & 
                   moveRate < moveRateThreshold &
                   between(julday, lowJul, highJul)]

### Output ----
# Match variables to output variables = consistent variables across species
source('scripts/0-variables/PrepDataOutputVariables.R')

outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(coyote, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                   'idate', 'itime', 'datetime', 
                   'EASTING', 'NORTHING',
                   'julday', 'yr', 'mnth', 'stepLength', 
                   'moveRate', 'difdatetime',
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










### Alec's rounding fix rate code ####

subsample(coyoteSpringTraj,
          4,
          units = 'hours')

length(coyote$datetime)
length(coyote$projXCol)
coyote <- coyote[!is.na(season)]

coyote[, difdatetime := 
         difftime(datetime, shift(datetime, 1), units = 'hours'),
       by = c(idCol)]

# group_times from spatsoc
coyote[, nMinutes := minute(datetime)]
group_times(coyote, 'datetime', paste0(fixrateSpring, ' hours'))
coyote[, .(difdatetime,
           difdatetime %% 0.5)]

coyote[nMinutes > 30, nearestDif := ceiling(difdatetime)]
coyote[nMinutes <= 30, nearestDif := floor(difdatetime)]

coyote[, .N, by = .(yday(datetime),
                    year(datetime),
                    nearestDif,
                    get(idCol),
                    timegroup)]
coyote[yr == '2009' & get(idCol) == 'co_lp0803' & timegroup == 3] 
coyote[season == 'spring' &
         difdatetime == fixrateSpring,
       keep := 1]

coyote[season == 'spring' & 
         is.na(keep)]

coyote[season == 'spring' & 
         keep == 1]

coyote[between(timegroup, 500, 502), .(datetime, difdatetime, hours, timegroup, get(idCol))][order(datetime)]

coyote[order(datetime)][between(timegroup, 500, 502),
       .(datetime, hour(datetime),
         round(hour(datetime)),
         hours,
         timegroup)]

coyote[, .SD[1], by = c(idCol, 'timegroup')]
#####################

# coyote <- coyote[round(difdatetime) == difTimeThreshold]
