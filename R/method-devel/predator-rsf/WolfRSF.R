### Wolf RSF ----
# Authors: 
# Purpose: 
# Inputs: Wolf relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'sp', 'adehabitatHR', 'raster',
          'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
wolf <- readRDS('output/data-prep/wolf.Rds')

# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'


# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

### MCPs ----
wolfSP <- SpatialPoints(wolf[, .(EASTING, NORTHING)], proj4string = CRS(utm))

wolfMCP <- mcp(wolfSP, 95)

### Create Regular Grid of Points ----
source('R/functions/GenerateGrid.R')
regGrid <- GenerateGrid(3000, mcpExtent = wolfMCP, crs = utm)

regPts <- data.table(regGrid@coords)[over(bounds, regGrid, returnList = TRUE)[[1]]]
setnames(regPts, c('EASTING', 'NORTHING'))
# ggplot(wolfMCP) + 
#   geom_polygon(aes(long, lat, group = group)) + 
#   geom_point(aes(x, y), data = regPts)


### Combine Observed and Random ----
regPts[, observed := 0]
wolf[, observed := 1]

samplePts <- rbindlist(list(regPts, wolf), 
                       use.names = TRUE, fill = TRUE)

