### Wolf data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare wolf data for EWC
# Inputs: Wolf relocation data
# Outputs: Prepared wolf data as RDS
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

# Read in wolf data, dropping above columns
wolf <- fread('input/locs/Wolfs.csv',
             drop = dropCols)

# UTM zone 21N
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm21N)


### Add fields ----
## Date time fields
wolf[, idate := as.IDate(FIX_DATE)]
wolf[, itime := as.ITime(FIX_TIME)]

wolf[, datetime := as.POSIXct(paste(idate, itime))]

wolf[, julday := yday(idate)]
wolf[, year := year(idate)]
wolf[, month := month(idate)]

## Project coordinates to UTM
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
wolf[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

### Summary information ----
# How many unique animals?
wolf[, uniqueN(ANIMAL_ID)]

# How many unique animals per year?
wolf[, .('N Unique Wolfs' = uniqueN(ANIMAL_ID)), by = year]
# kable(wolf[, .('N Unique Wolfs' = uniqueN(ANIMAL_ID)), by = year])

# Temporal distribution of locs
kable(wolf[order(month), .N, by = month])
kable(wolf[order(year), .N, by = year])

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
pdf('graphics/data-prep/wolf-locs-by-year.pdf')
wolf[NAV == '3D',
    PlotLocsBy(.SD, nlBounds, .BY[[1]]),
    by = year]
dev.off()


# Temporal distribution of locs
ggplot(wolf[order(month), .N, by = .(month, year)]) + 
  geom_tile(aes(month, year, fill = N)) + 
  scale_x_discrete(breaks = seq(1:12)) +  
  scale_fill_distiller(type = "div", palette = 6, direction = -1) + 
  coord_equal()