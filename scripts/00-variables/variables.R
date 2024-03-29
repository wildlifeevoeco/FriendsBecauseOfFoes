### Set variables ----
# Season thresholds
winter <- c(1, 72)
spring <- c(141, 212)

# UTM zone 21N
utmNL <- '+proj=utm +zone=21 ellps=WGS84'

# NL Bounds shapefile
nlBounds <- sp::spTransform(rgdal::readOGR('input/extent/NL/NL-Bounds.shp',
                                verbose = FALSE),
                        CRSobj = utmNL)


# UTM zone 14N
utmMB <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# MB Bounds shapefile
mbBounds <- sp::spTransform(rgdal::readOGR('input/extent/RMNP/RMNPextent.shp', verbose = FALSE),
                        CRSobj = utmMB)


### Set output columns (for prep) ----
outputVariables <- c('id', 'species', 'season', 'timegroup',
                     'idate', 'itime', 'datetime', 
                     'EASTING', 'NORTHING',
                     'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'fixRate')
