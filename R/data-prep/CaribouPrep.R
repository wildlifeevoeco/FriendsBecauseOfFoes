### Caribou data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare caribou data for EWC
# Inputs: Caribou relocation data
# Outputs: Prepared caribou data as RDS
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 



# NOTE: skeleton simply carried over from bear, coyote. this code is not functional



### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra', 
          'knitr', 'sp', 'rgdal', 'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
# dropCols <- c('FIX_ID','VENDOR_CL','AGE','COLLAR_FILE_ID','EXCLUDE','DOP','LOCQUAL',
#               'VALIDATED','COLLAR_TYPE_CL','COLLAR_ID','Fix_Time_Delta','EPSG_CODE')
#'Map_Quality','NAV')

# Read in caribou data, dropping above columns
caribou <- fread('input/locs/Caribous.csv',
             drop = dropCols)

# UTM zone 21N
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm21N)


### Add fields ----
## Date time fields
caribou[, idate := as.IDate(FIX_DATE)]
caribou[, itime := as.ITime(FIX_TIME)]

caribou[, datetime := as.POSIXct(paste(idate, itime))]

caribou[, julday := yday(idate)]
caribou[, year := year(idate)]
caribou[, month := month(idate)]

## Project coordinates to UTM
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
caribou[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

### Summary information ----
# How many unique animals?
caribou[, uniqueN(ANIMAL_ID)]

# How many unique animals per year?
caribou[, .('N Unique Caribous' = uniqueN(ANIMAL_ID)), by = year]
# kable(caribou[, .('N Unique Caribous' = uniqueN(ANIMAL_ID)), by = year])

# Temporal distribution of locs
kable(caribou[order(month), .N, by = month])
kable(caribou[order(year), .N, by = year])

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
pdf('graphics/data-prep/caribou-locs-by-year.pdf')
caribou[NAV == '3D',
    PlotLocsBy(.SD, nlBounds, .BY[[1]]),
    by = year]
dev.off()


# Temporal distribution of locs
ggplot(caribou[order(month), .N, by = .(month, year)]) + 
  geom_tile(aes(month, year, fill = N)) + 
  scale_x_discrete(breaks = seq(1:12)) +  
  scale_fill_distiller(type = "div", palette = 6, direction = -1) + 
  coord_equal()