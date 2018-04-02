### Elk RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko
# Purpose: 
# Inputs: Elk relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4',
          'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

## Animal locations
elk.locs <- readRDS('output/data-prep/elk.Rds')

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

### MCPs ----
elkSP <- SpatialPoints(elk.locs[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

elkMCP <- mcp(elkSP, 100)

##covariates
#Distance
human <- raster('input/covariates/RMNP/LinFeat_Dist.tif')
water <- raster('input/covariates/RMNP/Water_Dist.tif')

#Habitat
agriculture<-raster('input/covariates/RMNP/Agriculture100m.tif')
bog<-raster('input/covariates/RMNP/Bog100m.tif')
coniferous<-raster('input/covariates/RMNP/Coniferous100m.tif')
deciduous<-raster('input/covariates/RMNP/Deciduous100m.tif')
grassland<-raster('input/covariates/RMNP/Grassland100m.tif')
marsh<-raster('input/covariates/RMNP/Marsh100m.tif')
mixedwood<-raster('input/covariates/RMNP/Mixedwood100m.tif')
opendec<-raster('input/covariates/RMNP/Opendeciduous100m.tif')

#Topography
ruggedness<-raster('input/covariates/RMNP/Ruggedness_test.tif')

# Create Regular Grid
source('R/functions/GenerateGrid.R')
regGrid <- GenerateGrid(3000, mcpExtent = elkMCP, crs = utm)

available.elk.locs <- data.table(regGrid@coords)[over(bounds, regGrid, returnList = TRUE)[[1]]]
##################################################3
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING','season')

# and set all elk as observed
observed.elk <- elk.locs[, ..cols][, observed := 1]

# Create identical for random with observed == 0
available.elk <- available.elk.locs[, ..cols][, observed := 0]

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

