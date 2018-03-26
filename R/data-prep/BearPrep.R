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

### Add fields ----
## Date time fields
source('R/functions/DatePrep.R')
DatePrep(bear, dateCol, timeCol)

# Check!
bear[sample(.N, 5), .(idate, itime, yr, mnth, julday)]

## Project coordinates to UTM
bear[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(get(xCol), get(yCol)), 
                                                         utm))]

### Summary information ----
# How many unique animals?
bear[, uniqueN(get(idCol))]

# How many unique animals per year?
kable(bear[, .('N Unique Bears' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(bear[order(mnth), .N, by = mnth])
kable(bear[order(yr), .N, by = yr])

### Plots ----
# Plot locs by year on NL bounds 
source('R/functions/PlotLocsByFigure.R')

# To PDF 
pdf('graphics/data-prep/bear-locs-by-year.pdf')
bear[NAV == '3D',
     PlotLocsBy(.SD, nlBounds, .BY[[1]], idCol),
     by = yr]
dev.off()


# Temporal distribution of locs
source('R/functions/TemporalDistributionFigure.R')
TempDistFig(bear)

ggsave('graphics/data-prep/bear-temp-dist.png', TempDistFig(bear), 'png')