# Elk data preparation


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
# Read in elk data
elk <- fread('input/locs/RMNP_ElkData_clean.csv')

### Variables ----
xCol <- 'X'
yCol <- 'Y'
dateCol <- 'datetime'
hourCol <- 'Hour'
minCol <- 'Minute'
idCol <- 'ElkID'
projXCol <- 'EASTING'
projYCol <- 'NORTHING'

tz <- 'GMT'

### Add fields ----
# Missing single time field, we'll combine hour and minute and save as 'time'
# Combine hour, minute to timeCol
elk[, time := paste0(get(hourCol), ':', sprintf('%02d', get(minCol)))]
timeCol <- 'time'

## Date time fields
prep_date(
  DT = elk,
  dateCol = dateCol,
  timeCol = timeCol,
  tz = tz,
  dateFormat = '%d/%m/%Y'
)

# Drop old date time fields
dropCol <- c('Year', 'Month', 'Day', 'Hour', 'Minute', 'time')
elk[, (dropCol) := NULL]

# Season
elk[julday %between% winter, season := 'winter']
elk[julday %between% spring, season := 'spring']

# Temporal grouping
group_times(elk, 'datetime', '15 minutes')


# Drop duplicates
if (elk[, .N, by = c('timegroup', idCol)][N > 1, .N] > 0) {
  elk <- elk[!duplicated(elk[, .SD, .SDcols = !'fixID'])]
}

### Subset ----
# Subset any NAs in defined cols
checkCols <- c(xCol, yCol, dateCol, 'season')
elk <- na.omit(elk, cols = checkCols)

### Project + Step Length ----
# Coordinates already projected, simply rename
elk[, c(projXCol, projYCol) := .(get(xCol), get(yCol))]

# Step Length
step_length(
  elk,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

difTimeThreshold <- 2
elk <- elk[round(difdatetime) == difTimeThreshold]

step_length(
  elk,
  coords = c(projXCol, projYCol),
  time = 'datetime',
  splitBy = c(idCol, 'yr'),
  type = 'lead',
  moverate = TRUE,
  preserve = FALSE
)

### Summary information ----
# How many unique animals?
elk[, uniqueN(get(idCol))]

# How many unique animals per year?
elk[order(yr), .('N Unique Elks' = uniqueN(get(idCol))), by = yr]

# Temporal distribution of locs
elk[order(mnth), .N, by = mnth]
elk[order(yr), .N, by = yr]

### Subset ----
# Thresholds
moveRateThreshold <- 20000
lowJul <- 0
highJul <- 365

elk <- elk[moveRate < moveRateThreshold &
             between(julday, lowJul, highJul)]

# Spatially constrain to RMNP bounds
elkSP <- SpatialPointsDataFrame(elk[, .(get(projXCol), get(projYCol))],
                                elk,
                                proj4string = CRS(utmMB))

elk <- data.table(over(mbBounds, elkSP, returnList = TRUE)[[1]])

### Output ----
# Check 
if (elk[, .N, c('timegroup', idCol)][N > 1, .N] != 0) {
  stop('duplicate fixes in a timegroup * ID')
}

# Match variables to output variables = consistent variables across species
elk[, SPECIES := 'ELK']

setnames(elk, c('ElkID', 'SPECIES', 'season', 'timegroup',
                 'idate', 'itime', 'datetime', 
                 'EASTING', 'NORTHING',
                 'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'difdatetime'),
         outputVariables)

saveRDS(elk[, ..outputVariables], 'output/1-data-prep/elk.Rds')