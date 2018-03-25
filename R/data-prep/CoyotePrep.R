### Coyote data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare coyote data for EWC
# Inputs: Coyote relocation data
# Outputs: Prepped coyote data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2', 
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)

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

### Add fields ----
## Date time fields
source('R/functions/DatePrep.R')
DatePrep(coyote, dateCol, timeCol)

## Project coordinates to UTM
coyote[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(get(xCol), get(yCol)), utm))]

### Summary information ----
# How many unique animals?
coyote[, uniqueN(get(idCol))]

# How many unique animals per year?
coyote[, .('N Unique coyotes' = uniqueN(get(idCol))), by = yr]
# kable(coyote[, .('N Unique coyotes' = uniqueN(get(idCol))), by = yr])

# Temporal distribution of locs
kable(coyote[order(mnth), .N, by = mnth])
kable(coyote[order(yr), .N, by = yr])

### Plots ----
# Plot locs by year on NL bounds 
PlotLocsBy <- function(DT, bounds, by){
  print(
    ggplot(nlBounds) +
      geom_polygon(aes(long, lat, group = group), 
                   color = 'black', fill = 'grey', alpha = 0.25) + 
      geom_point(aes(EASTING, NORTHING, color = factor(get(idCol))), 
                 data = DT) + 
      guides(color = FALSE) + 
      labs(title = paste('year: ', by)))
  return(1)
}

# To PDF 
pdf('graphics/data-prep/coyote-locs-by-year.pdf')
coyote[NAV == '3D',
     PlotLocsBy(.SD, nlBounds, .BY[[1]]),
     by = yr]
dev.off()


# Temporal distribution of locs
source('R/functions/TemporalDistributionFigure.R')
TempDistFig(coyote)

ggsave('graphics/data-prep/coyote-temp-dist.png', TempDistFig(coyote), 'png')
