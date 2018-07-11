### Coyote data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare coyote data for EWC
# Inputs: Coyote relocation data
# Outputs: Prepped coyote data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 'spatsoc',
          'knitr', 'sp', 'rgdal', 'magrittr',
          'adehabitatLT')
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
source('R/0-variables/CutOffThresholds.R')

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
source('R/0-functions/StepLength.R')
StepLength(
  coyote,
  idCol,
  datetimeCol = 'datetime',
  yrCol = 'yr',
  xCol = projXCol,
  yCol = projYCol,
  returnIntermediate = FALSE
)



### Rarify locs ----
fixrateSpring <- 4
fixrateWinter <- 8

###########################
##### Hance's attempt at rounding fixes to nearest 4 and 8 fix rate #####

coyote$idtime<-paste(coyote$ANIMAL_ID, coyote$datetime, sep=" ")

CoySpr<-coyote[season == 'spring']
CoyWin<-coyote[season == 'winter']

coyoteSpringTraj <- as.ltraj(CoySpr[,c("EASTING","NORTHING")], date=CoySpr$datetime, id=CoySpr$ANIMAL_ID, infolocs=CoySpr[,c("idtime")])
coyoteWinterTraj <- as.ltraj(CoyWin[,c("EASTING","NORTHING")], date=CoyWin$datetime, id=CoyWin$ANIMAL_ID, infolocs=CoyWin[,c("idtime")])

refSprda<-round(min(coyote[season=="spring"]$datetime), "hour")
refWinda<-round(min(coyote[season=="winter"]$datetime), "hour")

CoySpr1<-setNA(coyoteSpringTraj, refSprda, fixrateSpring, units="hour")
CoyWin1<-setNA(coyoteWinterTraj, refWinda, fixrateWinter, units="hour")

CoySpr2<-sett0(CoySpr1, refSprda, fixrateSpring, units="hour")
CoyWin2<-sett0(CoyWin1, refWinda, fixrateWinter, units="hour")

CoySpr3<-ld(CoySpr2)
CoyWin3<-ld(CoyWin2)

coyote2<-rbind(CoySpr3,CoyWin3)

coyote3<-merge(coyote,coyote2,by="idtime")

coyote3$RTIME<-coyote3$date
coyote4<-coyote3[,c(1:24,27)]




#### Back to orginal flow of code #####


StepLength(coyote4, idCol, 
           datetimeCol = 'datetime', yrCol = 'yr', 
           xCol = projXCol, yCol = projYCol,
           returnIntermediate = FALSE)

### Summary information ----
# How many unique animals?
coyote4[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(coyote4[, .('N Unique coyotes' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(coyote4[order(mnth), .N, by = mnth])
kable(coyote4[order(yr), .N, by = yr])

# Herd distribution of coyotes
kable(coyote4[, .N, by = HERD])

### Subset ----
# Thresholds
stepLengthThreshold <- 7750000
moveRateThreshold <- 10000
difTimeThreshold <- 24
lowJul <- 0
highJul <- 365
herdList <- 'MIDRIDGE'

# Map_Quality, NAV

coyote5 <- coyote4[stepLength < stepLengthThreshold & 
                   moveRate < moveRateThreshold &
                   between(julday, lowJul, highJul)]


#### STOPPED HERE #####





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
coyote5[, PlotLocsBy(.SD, nlBounds, .BY[[1]], 'id'),
       by = yr]
# dev.off()


# Temporal distribution of locs
source('R/0-functions/TemporalDistributionFigure.R')
TempDistFig(coyote5)

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
