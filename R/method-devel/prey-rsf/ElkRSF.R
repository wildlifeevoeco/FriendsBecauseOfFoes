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
lsCovers <- data.table(nm = dir('input/covariates/RMNP', '.tif$'))[, nm := gsub(".tif|100m", "", nm)]
lsPaths <- data.table(nm = dir('input/covariates/RMNP', '.tif$', full.names = TRUE))

### Processing ----
# MCPs
elkSP <- SpatialPoints(elk[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

elkMCP <- mcp(elkSP, 100)

# Create Regular Grid
source('R/functions/GenerateGrid.R')
regGrid <- GenerateGrid(3000, mcpExtent = elkMCP, crs = utm)

regPts <- data.table(regGrid@coords)[over(elkMCP, regGrid, returnList = TRUE)[[1]]]
setnames(regPts, c('EASTING', 'NORTHING'))

# Check that points are within MCP
ggplot(elkMCP) +
  geom_polygon(aes(long, lat, group = group)) +
  geom_point(aes(EASTING, NORTHING), data = regPts)

# Combine observed and regular grid points
regPts[, observed := 0]
elk[, observed := 1]

samplePts <- rbindlist(list(regPts, elk), 
                       use.names = TRUE, fill = TRUE)

### RSF ----
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING','season')

samplePts <- samplePts[, ..cols]


# These should also match (they do)
nrow(available.elk)
nrow(observed.elk)

# Combine the observed and random elk and assign a rowID
sample.elk <- rbindlist(list(available.elk, observed.elk))

# Add row ID
sample.elk[, rowID := .I]

saveRDS(sample.elk, 'output/sample-elk')
sample.elk <- readRDS('output/sample-elk')



####extract covariates for used and available

### Extract land cover, DEM and focal rasters ----
## Functions
# Extract raster values at points
ExtractPoints <- function(pt.matrix, raster.layer){
  # Extract in raster layer values at points
  extract(raster.layer, pt.matrix)
}
  
##Covariate sampling
# Sample landcover at each point
sample.elk[, agprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), agriculture)]
sample.elk[, bgprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), bog)]
sample.elk[, cnprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), coniferous)]
sample.elk[, dcprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), deciduous)]
sample.elk[, grprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), grassland)]
sample.elk[, hudist := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), human)]
sample.elk[, mrprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), marsh)]
sample.elk[, mwprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), mixedwood)]
sample.elk[, odprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), opendec)]
sample.elk[, rgdns := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), ruggedness)]
sample.elk[, wtdist := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), water)]


###winter RSF
winter.elk <- sample.elk[season=="winter"]

winterelkrsf <- glm(observed~agprop+bgprop+cnprop+dcprop+grprop+hudist+mrprop+mwprop+odprop+rgdns+wtdist, family = binomial,data=winter.elk)


###spring RSF
spring.elk <- sample.elk[season=="spring"]

springelkrsf <- glm(observed~agprop+bgprop+cnprop+dcprop+grprop+hudist+mrprop+mwprop+odprop+rgdns+wtdist, family = binomial,data=spring.elk)

