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
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm21N)


### Add fields ----
## Date time fields
bear[, idate := as.IDate(FIX_DATE)]
bear[, itime := as.ITime(FIX_TIME)]

bear[, datetime := as.POSIXct(paste(idate, itime))]

bear[, julday := yday(idate)]
bear[, year := year(idate)]
bear[, month := month(idate)]

## Project coordinates to UTM
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
bear[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

### Summary information ----
# How many unique animals?
bear[, uniqueN(ANIMAL_ID)]

# How many unique animals per year?
bear[, .('N Unique Bears' = uniqueN(ANIMAL_ID)), by = year]
# kable(bear[, .('N Unique Bears' = uniqueN(ANIMAL_ID)), by = year])

# Temporal distribution of locs
kable(bear[order(month), .N, by = month])
kable(bear[order(year), .N, by = year])

### Plots ----
# Plot locs by year on NL bounds 
PlotLocsBy <- function(DT, bounds, by){
  print(
    ggplot(nlBounds) +
    geom_polygon(aes(long, lat, group = group), 
                 color = 'black', fill = 'grey', alpha = 0.25) + 
    geom_point(aes(EASTING, NORTHING, color = factor(ANIMAL_ID)), 
               data = DT) + 
    guides(color = FALSE) + 
    labs(title = paste('year: ', by)))
  return(1)
}

# To PDF 
pdf('graphics/data-prep/bear-locs-by-year.pdf')
bear[NAV == '3D',
     PlotLocsBy(.SD, nlBounds, .BY[[1]]),
     by = year]
dev.off()


# Temporal distribution of locs
ggplot(bear[order(month), .N, by = .(month, year)]) + 
  geom_tile(aes(month, year, fill = N)) + 
  scale_x_discrete(breaks = seq(1:12)) +  
  scale_fill_distiller(type = "div", palette = 6, direction = -1) + 
  coord_equal()