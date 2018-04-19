### Wolf RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: Determine habitat domain for wolves using second order RSFs
# Inputs: Wolf relocation data, habitat layers as rasters
# Outputs: Wolf RSF results for global model used to create winter and spring rasters as TIFF
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'magrittr',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4',
          'ggplot2','car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Animal locations
wolf <- readRDS('output/data-prep/wolf.Rds')

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

# Covariates
lsCovers <- data.table(nm = dir('output/data-prep/cropped-rasters/RMNP', '.tif$'))[, 
                                                                                   nm := gsub(".tif|100m", "", nm)]$nm[-c(1, 4, 5)]
lsPaths <- dir('output/data-prep/cropped-rasters/RMNP', '.tif$', full.names = TRUE)[-c(1, 4, 5)]

### Processing ----
# MCPs
wolfSP <- SpatialPoints(wolf[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

wolfMCP <- mcp(wolfSP, 100)

# Create Regular Grid
source('R/0-functions/GenerateGrid.R')
regPts <- GenerateGrid(90, mcpExtent = wolfMCP, crs = utm)

setnames(regPts, c('EASTING', 'NORTHING'))

# saveRDS(regPts, 'output/predator-rsf/wolfRegularPoints.Rds')
#regPts <- readRDS('output/predator-rsf/wolfRegularPoints.Rds')

# Check that points are within MCP
# ggplot(wolfMCP) +
#   geom_polygon(aes(long, lat, group = group), alpha = 0.25) +
#   geom_point(aes(EASTING, NORTHING), size = 0.1, data = regPts)

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
#samplePts <- readRDS('output/predator-rsf/wolfSamplePoints.Rds')

### RSF ====
lsRasters <- lapply(lsPaths, raster)

# Winter RSF
winterwolf <- samplePts[season == "winter" | is.na(season)]
winterwolf[observed == 0, season := "winter"]

winterwolfRSF <- glm(reformulate(lsCovers, response = 'observed'), #### Dist not logged yet
                    family = 'binomial',data = winterwolf)

summary(winterwolfRSF)
vif(winterwolfRSF)
rsquared(winterwolfRSF)

# Pull out the coefficients, dropping the intercept
winwolf.b <- coef(winterwolfRSF)[-1]


# Create the raster matching the first raster layer with the first fixed effect
###### with intercept of -3.044340?
winterwolfRSF.rstr <- exp(-3.044340 +lsRasters[[1]] * winwolf.b[1] + lsRasters[[2]] * winwolf.b[2] +
                           lsRasters[[3]] * winwolf.b[3] + lsRasters[[4]] * winwolf.b[4] + 
                           lsRasters[[5]] * winwolf.b[5]+ lsRasters[[6]] * winwolf.b[6]+ 
                           lsRasters[[7]] * winwolf.b[7]+ lsRasters[[8]] * winwolf.b[8]+ 
                           lsRasters[[9]] * winwolf.b[9])

plot(winterwolfRSF.rstr)


# Spring RSF
springwolf <- samplePts[season == "spring" | is.na(season)]
springwolf[observed == 0, season := "spring"]

springwolfRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                    family = 'binomial',data = springwolf)

summary(springwolfRSF)
vif(springwolfRSF)
rsquared(springwolfRSF)

# Pull out the coefficients, dropping the intercept
sprwolf.b <- coef(springwolfRSF)[-1]


# Create the raster matching the first raster layer with the first fixed effect
###with intercept of -2.711875?
springwolfRSF.rstr <- exp(-2.711875 + lsRasters[[1]] * sprwolf.b[1] + lsRasters[[2]] * sprwolf.b[2] +
                           lsRasters[[3]] * sprwolf.b[3] + lsRasters[[4]] * sprwolf.b[4] + 
                           lsRasters[[5]] * sprwolf.b[5]+ lsRasters[[6]] * sprwolf.b[6]+ 
                           lsRasters[[7]] * sprwolf.b[7]+ lsRasters[[8]] * sprwolf.b[8]+ 
                           lsRasters[[9]] * sprwolf.b[9])

plot(springwolfRSF.rstr)

####standardize RSFs 
###using feature scaling
winterwolfRSF.s <- (winterwolfRSF.rstr - (cellStats(winterwolfRSF.rstr,min)))/(cellStats(winterwolfRSF.rstr,max) - (cellStats(winterwolfRSF.rstr,min)))
plot(winterwolfRSF.s)
springwolfRSF.s <- (springwolfRSF.rstr - (cellStats(springwolfRSF.rstr,min)))/(cellStats(springwolfRSF.rstr,max) - (cellStats(springwolfRSF.rstr,min)))
plot(springwolfRSF.s)

####using z score
#winterwolfRSF.z <- (winterwolfRSF.rstr - cellStats(winterwolfRSF.rstr,stat=mean))/cellStats(winterwolfRSF.rstr,stat=sd)
#plot(winterwolfRSF.z)
#springwolfRSF.z <- (springwolfRSF.rstr - cellStats(springwolfRSF.rstr,stat=mean))/cellStats(springwolfRSF.rstr,stat=sd)
#plot(springwolfRSF.z)


### Save the RSFs ----
ls.rsf <- list('WINTER' = winterwolfRSF.s, 
               'SPRING' = springwolfRSF.s)

lapply(seq_along(ls.rsf), FUN = function(r){
  writeRaster(ls.rsf[[r]], paste0('output/predator-rsf/wolfrsf', names(ls.rsf[r])), 
              format = 'GTiff',
              overwrite = T)
})


