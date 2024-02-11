### Socality Maps ====

### Packages ----
pkgs <- c('data.table', 'raster')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


bearSpring <- raster('output/2-rsf/rasters/bearrsfSpring.tif')
caribouWinter <- raster('output/2-rsf/rasters/cariboursfWinter.tif')
caribouSpring <- raster('output/2-rsf/rasters/cariboursfSpring.tif')
coyoteWinter <- raster('output/2-rsf/rasters/coyotersfWinter.tif')
coyoteSpring <- raster('output/2-rsf/rasters/coyotersfSpring.tif')

wolfWinter <- raster('output/2-rsf/rasters/wolfrsfWinter.tif')
elkWinter <- raster('output/2-rsf/rasters/elkrsfWinter.tif')

z.elkWinter<-(elkWinter-0.217327)/0.1051894
z.wolfWinter<-(wolfWinter-0.3760138)/0.1210041
z.bearSpring<-(bearSpring-0.08631733)/0.03585594
z.coyoteSpring<-(coyoteSpring-0.2582346)/0.1240057
z.caribouSpring<-(caribouSpring-0.1435675)/0.06448588
z.coyoteWinter<-(coyoteWinter-0.2582346)/0.1240057
z.caribouWinter<-(caribouWinter-0.1435675)/0.06448588


#Winter RMNP DI MAP

RM_W_DI<-0.33041+(0.02017*z.elkWinter)+(0*z.wolfWinter)+(0*z.elkWinter*z.wolfWinter)
MR_W_DI<-0.24586+(-0.02488*z.caribouWinter)+(-0.03907*z.coyoteWinter)+(0*z.caribouWinter*z.coyoteWinter)
MR_S_DI<-0.15116+(0*z.caribouSpring)+(-0.04079*z.bearSpring)+(0.04142*z.coyoteSpring)+(-0.02676*z.caribouSpring*z.bearSpring)+(0*z.caribouSpring*z.coyoteSpring)

library(rgdal)

writeRaster(RM_W_DI,"output/5-model/RM_W_DI.tif", format="GTiff")
writeRaster(MR_W_DI,"output/5-model/MR_W_DI.tif", format="GTiff")
writeRaster(MR_S_DI,"output/5-model/MR_S_DI.tif", format="GTiff")



