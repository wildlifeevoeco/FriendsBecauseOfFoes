### Wolf RSF ----
# Authors: Alec Robitaille
# Purpose: 
# Inputs: Wolf relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'sp', 'adehabitatHR', 'raster',
          'magrittr','piecewiseSEM','car')
lapply(libs, require, character.only = TRUE)


### Input data ----
wolf <- readRDS('output/data-prep/wolf.Rds')

# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

# Covariates
lsCovers <- data.table(nm = dir('input/covariates/RMNP', '.tif$'))[, 
                                                                   nm := gsub(".tif|100m", "", nm)]$nm
lsPaths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)

### MCPs ----
wolfSP <- SpatialPoints(wolf[, .(EASTING, NORTHING)], proj4string = CRS(utm))

wolfMCP <- mcp(wolfSP, 95)

# Create Regular Grid
source('R/functions/GenerateGrid.R')
regPts <- GenerateGrid(90, mcpExtent = wolfMCP, crs = utm)

setnames(regPts, c('EASTING', 'NORTHING'))

# saveRDS(regPts, 'output/predator-rsf/wolfRegularPoints.Rds)
# regPts <- readRDS('output/predator-rsf/wolfRegularPoints.Rds')

# Check that points are within MCP
ggplot(wolfMCP) +
  geom_polygon(aes(long, lat, group = group)) +
  geom_point(aes(EASTING, NORTHING), data = regPts)

# Combine observed and regular grid points
regPts[, observed := 0]
wolf[, observed := 1]

samplePts <- rbindlist(list(regPts, wolf), 
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

# saveRDS(samplePts, 'output/predator-rsf/wolfSamplePoints.Rds')
#samplePts <- readRDS('output/predator-rsf/wolfSamplePoints.Rds'')

### RSF ====
# Winter RSF
winterWolf <- samplePts[season == "winter" | is.na(season)]
winterWolf[observed == 0, season := "winter"]

winterWolfRSF <- glm(observed ~ Bog + Coniferous + Grassland + log(LinFeat_Dist+1) + 
                      Marsh + Mixedwood + Opendeciduous + Ruggedness_test  + log(Water_Dist+1),
                    family = binomial,
                    data = winterWolf)

summary(winterWolfRSF)
vif(winterWolfRSF)
rsquared(winterWolfRSF)

# Spring RSF
springWolf <- samplePts[season == "spring" | is.na(season)]
springWolf[observed == 0, season := "spring"]

springWolfRSF <- glm(observed ~ Bog + Coniferous + Grassland + log(LinFeat_Dist+1) + 
                      Marsh + Mixedwood + Opendeciduous + Ruggedness_test  + log(Water_Dist+1), 
                    family = binomial,
                    data = springWolf)

summary(springWolfRSF)
rsquared(springWolfRSF)


