### Elk data preparation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare elk data for EWC
# Inputs: Elk relocation data
# Outputs: Prepared elk data as RDS
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

# Read in elk data, dropping above columns
elk <- fread('input/locs/Elks.csv',
              drop = dropCols)

# UTM zone 21N
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm21N)


### Add fields ----
## Date time fields
elk[, idate := as.IDate(FIX_DATE)]
elk[, itime := as.ITime(FIX_TIME)]

elk[, datetime := as.POSIXct(paste(idate, itime))]

elk[, julday := yday(idate)]
elk[, year := year(idate)]
elk[, month := month(idate)]

## Project coordinates to UTM
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
elk[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

### Summary information ----
# How many unique animals?
elk[, uniqueN(ANIMAL_ID)]

# How many unique animals per year?
elk[, .('N Unique Elks' = uniqueN(ANIMAL_ID)), by = year]
# kable(elk[, .('N Unique Elks' = uniqueN(ANIMAL_ID)), by = year])

# Temporal distribution of locs
kable(elk[order(month), .N, by = month])
kable(elk[order(year), .N, by = year])

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
pdf('graphics/data-prep/elk-locs-by-year.pdf')
elk[NAV == '3D',
     PlotLocsBy(.SD, nlBounds, .BY[[1]]),
     by = year]
dev.off()


# Temporal distribution of locs
ggplot(elk[order(month), .N, by = .(month, year)]) + 
  geom_tile(aes(month, year, fill = N)) + 
  scale_x_discrete(breaks = seq(1:12)) +  
  scale_fill_distiller(type = "div", palette = 6, direction = -1) + 
  coord_equal()