message('=== Coyote data preparation ===')
# Authors: Quinn Webber, Alec Robitaille


### Packages ----
pkgs <- c('data.table', 'spatsoc', 'ewc',
          'sp', 'rgdal', 'adehabitatLT')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)

### Set variables ----
source('scripts/0-variables/variables.R')


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
# Date time fields
prep_date(
  DT = coyote, 
  dateCol = dateCol, 
  timeCol = timeCol, 
  tz = tz
)

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

## Subset to residents and sub transients
coyote[, status := 'RESIDENT']

unknowns <- c("co_lp1109", "co_mr1003", "co_mr1012", "co_np1022")

subtransients <- c("co_lp0902", "co_lp1001", "co_lp1003", "co_lp1012",
                   "co_lp1107", "co_mr1005", "co_mr1010", "co_mr1312",
                   "co_mr1303", "co_np0915", "co_np1005", "co_np1006", 
                   "co_np1017")

toofews <- c("co_lp1005", "co_lp1106", "co_mr0806", "co_mr0901", 
             "co_mr0907", "co_mr0911", "co_mr0914", "co_mr1014",
             "co_mr1015", "co_mr1102", "co_mr1106", "co_mr1205",
             "co_mr1304", "co_mr1311", "co_mr1313", "co_mr1314", 
             "co_np0801", "co_np0904", "co_np1002", "co_np1015", 
             "co_np1014", "co_lp0908", "co_lp1010", "co_lp1013", 
             "co_lp1101", "co_mr1009", "co_mr1107", "co_mr1108", 
             "co_mr1204", "co_mr1210", "co_mr1211", "co_np0908",
             "co_np1021")

transients <- c("co_lp0904", "co_lp0910", "co_lp1002", "co_lp1006",
                "co_lp1008", "co_lp1014", "co_mr0905", "co_mr0906", 
                "co_mr0908", "co_mr1001", "co_mr1004", "co_mr1006", 
                "co_mr1007", "co_mr1011", "co_mr1013", "co_mr1101", 
                "co_mr1105", "co_mr1209", "co_mr1212", "co_mr1302", 
                "co_mr1306", "co_mr1308", "co_np0903", "co_np1016")

coyote[get(idCol) %chin% unknowns, status := 'UNKNOWN']
coyote[get(idCol) %chin% subtransients, status := 'SUB-TRANSIENT']
coyote[get(idCol) %chin% toofews, status := 'TOOFEW']
coyote[get(idCol) %chin% transients, status := 'TRANSIENT']

coyote <- coyote[status == "RESIDENT" | status == "SUB-TRANSIENT"]


### Project Coords, Step length  ----
# Project coordinates to UTM
coyote[, c(projXCol, projYCol) := 
         as.data.table(
           project(cbind(get(xCol), get(yCol)), utmNL))]

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
coyote[, idtime := paste(coyote$ANIMAL_ID, coyote$datetime, sep = " ")]

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

coyote[, .N, c('timegroup', 'idCol')][N > 1, .N]

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
# TODO: is this really the step length threshold?? 
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
outputVariables <- c(outputVariables, 'herd', 'sex')

setnames(coyote, c('ANIMAL_ID', 'SPECIES', 'season', 'timegroup',
                   'idate', 'itime', 'datetime', 
                   'EASTING', 'NORTHING',
                   'julday', 'yr', 'mnth', 'stepLength', 
                   'moveRate', 'difdatetime',
                   'HERD', 'SEX'),
         outputVariables)

saveRDS(coyote[, ..outputVariables], 'output/1-data-prep/coyote.Rds')


message('=== COYOTE PREP COMPLETE ===')
