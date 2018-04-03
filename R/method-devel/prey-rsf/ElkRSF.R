### Elk RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: 
# Inputs: Elk relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'magrittr',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4',
          'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Animal locations
elk <- readRDS('output/data-prep/elk.Rds')

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

# Covariates
lsCovers <- data.table(nm = dir('input/covariates/RMNP', '.tif$'))[, 
  nm := gsub(".tif|100m", "", nm)]$nm
lsPaths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)

### Processing ----
# MCPs
elkSP <- SpatialPoints(elk[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

elkMCP <- mcp(elkSP, 100)

# Create Regular Grid
source('R/functions/GenerateGrid.R')
regPts <- GenerateGrid(300, mcpExtent = elkMCP, crs = utm)

setnames(regPts, c('EASTING', 'NORTHING'))

# saveRDS(regPts, 'output/prey-rsf/elkRegularPoints.Rds')
# regPts <- readRDS('output/prey-rsf/elkRegularPoints.Rds')

# Check that points are within MCP
ggplot(elkMCP) +
  geom_polygon(aes(long, lat, group = group), alpha = 0.25) +
  geom_point(aes(EASTING, NORTHING), size = 0.1, data = regPts)

# Combine observed and regular grid points
regPts[, observed := 0]
elk[, observed := 1]

samplePts <- rbindlist(list(regPts, elk), 
                       use.names = TRUE, fill = TRUE)

### Sampling ----
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING', 'season', 'observed')
samplePts <- samplePts[, ..cols]

# Add row ID
samplePts[, rowID := .I]

# Sample rasters
samplePts[, (lsCovers) := lapply(lsPaths, FUN = function(r){
  extract(raster(r), matrix(c(EASTING, NORTHING), ncol = 2))})]

# saveRDS(samplePts, 'output/prey-rsf/elkSamplePoints.Rds')
# samplePts <- readRDS('output/prey-rsf/elkSamplePoints.Rds')

### RSF ====
# Winter RSF
winterElk <- samplePts[season == "winter"]

winterElkRSF <- glm(reformulate(lsCovers, response = 'observed'),
                    family = binomial,
                    data = winterElk)

# Spring RSF
springElk <- samplePts[season == "spring"]

springElkRSF <- glm(observed ~ agprop + bgprop + cnprop + dcprop + grprop + 
                      hudist + mrprop + mwprop + odprop + rgdns  + wtdist, 
                    family = binomial,
                    data = springElk)