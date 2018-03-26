### Wolf data preparation ----
# Authors: Alec Robitaille
# Purpose: To prepare wolf data for EWC
# Inputs: Wolf relocation data
# Outputs: Prepared wolf data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)

### Input data ----
# List individual wolf sheets
paths <- dir('input/locs/RMNP_WolfLocations', '*.csv',
             full.names = TRUE)

# Check how many times each column appears in the sheets
cols <- rbindlist(lapply(paths, FUN = function(x) fread(x) %>% colnames(.) %>% 
                           data.table(col = ., path = x, val = 1))) %>% dcast(path~col, value.var = 'val')
cols[, path := rowid(path)][, lapply(.SD, sum, na.rm = TRUE), .SDcol = colnames(cols)]

# Read in each and rbindlist
#   To make sure LATITUDE and Latitude are considered as the same, 
#   change all colnames to lower case, and rbindlist!
wolf <- rbindlist(lapply(paths, FUN = function(p){
  fread(p) %>% setnames(colnames(.), tolower(colnames(.)))
}), fill = TRUE, use.names = TRUE)

# List drop and keep columns
keepCols <- c('wolfid', 'packid', 'longitude', 'latitude', 'collar', 'gmtdate', 'gmttime', 
              'time', 'date', '2d3d', 'fixstatus', 'info')
dropCols <- colnames(wolf)[!(colnames(wolf) %in% keepCols)]

# Drop columns above and rename 2d3d to Fix2d3d
wolf[, (dropCols) := NULL][, c('Fix2d3d', '2d3d') := .(`2d3d`, NULL)]

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

### Add fields ----
## Date time fields
source('R/functions/DatePrep.R')
DatePrep(wolf, dateCol, timeCol, dateFormat = '%d/%m/%Y')

## Project coordinates to UTM
wolf[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), 
                                                         utm))]

### Summary information ----
# How many unique animals?
wolf[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(wolf[, .('N Unique Wolfs' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(wolf[order(mnth), .N, by = mnth])
kable(wolf[order(yr), .N, by = yr])

### Plots ----
# Plot locs by year on RMNP bounds 
source('R/functions/PlotLocsByFigure.R')

# To PDF 
pdf('graphics/data-prep/wolf-locs-by-year.pdf')
wolf[, PlotLocsBy(.SD, bounds, .BY[[1]], idCol),
     by = yr]
dev.off()


# Temporal distribution of locs
source('R/functions/TemporalDistributionFigure.R')
TempDistFig(wolf)

ggsave('graphics/data-prep/wolf-temp-dist.png', TempDistFig(wolf), 'png')
