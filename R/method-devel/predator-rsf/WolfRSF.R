### Wolf RSF ----
# Authors: 
# Purpose: 
# Inputs: Wolf relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('data.table', 'ggplot2',
          'sp', 'adehabitatHR',
          'magrittr')
lapply(libs, require, character.only = TRUE)


### Input data ----
wolf <- readRDS('output/data-prep/wolf.Rds')

# MB Bounds shapefile
bounds <- rgdal::readOGR('input/etc/RMNP-extent/RMNPextent.shp') %>%
  spTransform(CRSobj = utm)

### MCPs ----
# UTM zone 14N
utm <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

wolfSP <- SpatialPoints(wolf[, .(EASTING, NORTHING)], proj4string = CRS(utm))

wolfMCP <- mcp(wolfSP, 95)

# Create Regular Grid
source('R/functions/GenerateGrid.R')
regGrid <- GenerateGrid(30, mcpExtent = wolfMCP@bbox)
rasterToPoints(r, spatial = TRUE)
plot(regGrid)
spGrid <- SpatialPointsDataFrame(regGrid, data = regGrid, proj4string = CRS(utm))

withinBounds <- data.table(over(bounds, spGrid, returnList = TRUE)[[1]])

bDT <- data.table(wolfMCP@bbox, keep.rownames = TRUE)
ext <- extent(bDT[rn == 'x', min], bDT[rn == 'x', min], 
              y_ori, y_ori + (y_cell * cell_size)) 

r <- raster::raster(wolfMCP@bbox, crs = CRS(utm))
raster::res(r) <- c(30, 30)
rp <- raster::rasterToPoints(r, spatial = TRUE) 
gridded(rp) <- TRUE

plot()

ggplot(wolfMCP) + 
  geom_polygon(aes(long, lat, group=group), fill = NA, color = 'black') + 
  geom_point(aes(x, y), data = withinBounds)


### Generate Random Points ----
# Drop columns leaving only needed
cols <- c('EASTING', 'NORTHING', 'blockByIDYear', 'ANIMAL_ID', 'block', 'HERD')

# and set all locs as observed
observed.locs <- locs[, ..cols][, observed := 1]

# Create identical for random with observed == 0
random.locs <- locs[, ..cols][, observed := 0]

# Generate an equivalent number of random points in vertices as observed (with encamped state)
# (updating the observed locs EASTING, NORTHING columns)
random.locs[, c('EASTING', 'NORTHING') := as.data.table(spsample(vertices.95[vertices.95@data$id == .BY[[1]],],
                                                                 .N, iter = 100, type = "random")@coords),
            by = blockByIDYear]
