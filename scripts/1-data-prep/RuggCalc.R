elevation <- raster('input/covariates/RMNP/Elevation.tif')

#####calculate ruggedness from elevation

ruggedness<- terrain(elevation,opt="roughness")
writeRaster(ruggedness,'input/covariates/RMNP/Ruggedness', 
              format = 'GTiff', 
              overwrite = T)

plot(ruggedness)
 