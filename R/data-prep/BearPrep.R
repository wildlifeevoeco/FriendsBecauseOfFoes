### Wolf Step Length Calculation ----
# Authors: Quinn Webber, Alec Robitaille
# Purpose: To prepare bear data for EWC
# Inputs: Bear relocation data
# Outputs: 
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
# knitr::kable(bear[, .('N Unique Bears' = uniqueN(ANIMAL_ID)), by = year])


bear[Map_Quality == 'Y',
     {print(ggplot() + 
        geom_point(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID))) + 
        guides(color = FALSE) + 
        labs(title = paste('year: ', .BY[[1]])))
      1},
     by = year]


ggplot(bear) +
  geom_point(aes(Y_COORD, X_COORD, color = factor(ANIMAL_ID)),
             alpha = 0.5) + 
  guides(color = FALSE)

bear2 <- subset(bear, Y_COORD > 40 & Y_COORD < 60 & X_COORD < -30 & X_COORD > -70)

bear2008 = subset(bear2, Year == '2008')
bear2009 = subset(bear2, Year == '2009')
bear2010 = subset(bear2, Year == '2010')
bear2011 = subset(bear2, Year == '2011')
bear2012 = subset(bear2, Year == '2012')
bear2013 = subset(bear2, Year == '2013')



## quick look at where the bears are located:
aa = ggplot(bear2008[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
bb = ggplot(bear2009[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
cc = ggplot(bear2010[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
dd = ggplot(bear2011[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ee = ggplot(bear2012[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ff = ggplot(bear2013[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
grid.arrange(aa,bb,cc,dd,ee,ff,ncol = 3, nrow = 2)




