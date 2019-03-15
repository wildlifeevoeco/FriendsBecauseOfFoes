### Elk RSF ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi
# Purpose: Determine habitat domain for elk using second order RSFs
# Inputs: Elk relocation data, habitat layers as rasters
# Outputs: Elk RSF results for global model used to create winter and spring rasters as TIFF
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'ewc',
          'adehabitatHR', 'sp', 'rgdal', 'raster', 
          'lme4', 'car','piecewiseSEM')
lapply(libs, require, character.only = TRUE)

### Input data ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# Animal locations
elk <- readRDS('output/data-prep/elk.Rds')

# MB Bounds shapefile
bounds <- spTransform(readOGR('input/etc/RMNP-extent/RMNPextent.shp'), 
                      CRSobj = utm)

# Covariates
lsCovers <- data.table(nm = dir('output/data-prep/cropped-rasters/RMNP', '.tif$'))[, 
  nm := gsub(".tif|100m", "", nm)]$nm[-c(1, 4, 5)]
lsPaths <- dir('output/data-prep/cropped-rasters/RMNP', '.tif$', full.names = TRUE)[-c(1, 4, 5)]

### Processing ----
# MCPs
elkSP <- SpatialPoints(elk[, .(EASTING, NORTHING)],
                       proj4string = CRS(utm))

elkMCP <- mcp(elkSP, 100)

# Create Regular Grid
regPts <- generate_grid(pol = elkMCP, spacing = 90, crs = utm)
setnames(regPts, c('EASTING', 'NORTHING'))

saveRDS(regPts, 'output/prey-rsf/elkRegularPoints.Rds')

# Check that points are within MCP
# ggplot(elkMCP) +
#   geom_polygon(aes(long, lat, group = group), alpha = 0.25) +
#   geom_point(aes(EASTING, NORTHING), size = 0.1, data = regPts)

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
#samplePts <- readRDS('output/prey-rsf/elkSamplePoints.Rds')

### RSF ====
lsRasters <- lapply(lsPaths, raster)

# Winter RSF
winterElk <- samplePts[season == "winter" | is.na(season)]
winterElk[observed == 0, season := "winter"]

winterElkRSF <- glm(reformulate(lsCovers, response = 'observed'), #### Dist not logged yet
        family = 'binomial',data = winterElk)

summary(winterElkRSF)
vif(winterElkRSF)
rsquared(winterElkRSF)

# Pull out the coefficients, dropping the intercept
winElk.b <- coef(winterElkRSF)[-1]


# Create the raster matching the first raster layer with the first fixed effect
###with intercept of -3.242852 ?
winterElkRSF.rstr <- exp(-3.242852 + lsRasters[[1]] * winElk.b[1] + lsRasters[[2]] * winElk.b[2] +
                        lsRasters[[3]] * winElk.b[3] + lsRasters[[4]] * winElk.b[4] + 
                        lsRasters[[5]] * winElk.b[5]+ lsRasters[[6]] * winElk.b[6]+ 
                        lsRasters[[7]] * winElk.b[7]+ lsRasters[[8]] * winElk.b[8]+ 
                        lsRasters[[9]] * winElk.b[9])

plot(winterElkRSF.rstr)


# Spring RSF
springElk <- samplePts[season == "spring" | is.na(season)]
springElk[observed == 0, season := "spring"]


springElkRSF <- glm(reformulate(lsCovers, response = 'observed'), 
       family = 'binomial',data = springElk)

summary(springElkRSF)
rsquared(springElkRSF)

# Pull out the coefficients, dropping the intercept
sprElk.b <- coef(springElkRSF)[-1] 


# Create the raster matching the first raster layer with the first fixed effect
###with intercept of -2.053773?
springElkRSF.rstr <- exp(-2.053773 + lsRasters[[1]] * sprElk.b[1] + lsRasters[[2]] * sprElk.b[2] +
                        lsRasters[[3]] * sprElk.b[3] + lsRasters[[4]] * sprElk.b[4] + 
                        lsRasters[[5]] * sprElk.b[5]+ lsRasters[[6]] * sprElk.b[6]+ 
                        lsRasters[[7]] * sprElk.b[7]+ lsRasters[[8]] * sprElk.b[8]+ 
                        lsRasters[[9]] * sprElk.b[9])

plot(springElkRSF.rstr)

####standardize RSFs 
###using feature scaling
winterElkRSF.s <- (winterElkRSF.rstr - (cellStats(winterElkRSF.rstr,min)))/(cellStats(winterElkRSF.rstr,max) - (cellStats(winterElkRSF.rstr,min)))
plot(winterElkRSF.s)
springElkRSF.s <- (springElkRSF.rstr - (cellStats(springElkRSF.rstr,min)))/(cellStats(springElkRSF.rstr,max) - (cellStats(springElkRSF.rstr,min)))
plot(springElkRSF.s)

####using z score
#winterElkRSF.z <- (winterElkRSF.rstr - cellStats(winterElkRSF.rstr,stat=mean))/cellStats(winterElkRSF.rstr,stat=sd)
#plot(winterElkRSF.z)
#springElkRSF.z <- (springElkRSF.rstr - cellStats(springElkRSF.rstr,stat=mean))/cellStats(springElkRSF.rstr,stat=sd)
#plot(springElkRSF.z)


### Save the RSFs ----
ls.rsf <- list('WINTER' = winterElkRSF.s, 
               'SPRING' = springElkRSF.s)

lapply(seq_along(ls.rsf), FUN = function(r){
  writeRaster(ls.rsf[[r]], paste0('output/prey-rsf/elkrsf', names(ls.rsf[r])), 
              format = 'GTiff',
              overwrite = T)
})


                       