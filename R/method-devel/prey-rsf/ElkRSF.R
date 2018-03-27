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
##animal locations
elk <- readRDS('output/data-prep/elk.Rds')

### MCPs ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

elkSP <- SpatialPoints(elk[, .(EASTING, NORTHING)], proj4string = CRS(utm))

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

###generate regular available 
##alec


# and set all locs as observed
observed.locs <- locs[, ..cols][, observed := 1]

# Create identical for random with observed == 0
available.locs <- locs[, ..cols][, observed := 0]

# These should also match (they do)
nrow(available.locs)
nrow(observed.locs)

# Combine the observed and random locs and assign a rowID
sample.locs <- rbindlist(list(available.locs, observed.locs))

# Add row ID
sample.locs[, rowID := .I]

saveRDS(sample.locs, 'output/sample-locs')
sample.locs <- readRDS('output/sample-locs')



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
sample.locs[, agprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), agriculture)]
sample.locs[, bgprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), bog)]
sample.locs[, cnprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), coniferous)]
sample.locs[, dcprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), deciduous)]
sample.locs[, grprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), grassland)]
sample.locs[, hudist := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), human)]
sample.locs[, mrprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), marsh)]
sample.locs[, mwprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), mixedwood)]
sample.locs[, odprop := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), opendec)]
sample.locs[, rgdns := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), ruggedness)]
sample.locs[, wtdist := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), water)]


###winter RSF
winter.locs <- sample.locs[season=="winter"]

winterelkrsf <- glm(observed~agprop+bgprop+cnprop+dcprop+grprop+hudist+mrprop+mwprop+odprop+rgdns+wtdist, family = binomial,data=winter.locs)


###spring RSF
spring.locs <- sample.locs[season=="spring"]

springelkrsf <- glm(observed~agprop+bgprop+cnprop+dcprop+grprop+hudist+mrprop+mwprop+odprop+rgdns+wtdist, family = binomial,data=spring.locs)

