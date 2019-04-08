### Covariate (Raster) Processing ----
# Authors: Alec Robitaille, Christina M Prokopenko, Sana Zabihi

### Packages ----
libs <- c('data.table', 'ggplot2', 'sp', 'raster')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### List rasters ----
# Covariates
covers <- gsub(".tif|100m", "", dir('input/covariates/RMNP', '.tif$'))
paths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)
names(paths) <- covers

rmList <- which(covers %in% c('Agriculture', 'Deciduous', 'Grassland'))

lsCovers <- covers[-rmList]
lsPaths <- paths[-rmList]


### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(
  lsPaths,
  FUN = function(r) {
    crop(raster(r), mbBounds)
  }
)

# Log transform
namesTransform <- c('LinFeat_Dist', 'Water_Dist')
rasterTransform <-
  cropRasters[lapply(cropRasters, names) %in% namesTransform]

transformed <- lapply(
  seq_along(namesTransform),
  FUN = function(x) {
    r <- log(rasterTransform[[x]] + 1)
    names(r) <- namesTransform[[x]]
    r
  }
)
names(transformed) <- namesTransform

transformed[['LinFeat_Dist']] <-
  resample(transformed[['LinFeat_Dist']], transformed[['Water_Dist']])

cropRasters[['Ruggedness_test']] <-
  resample(cropRasters[['Ruggedness']], cropRasters[['Water_Dist']])


### Output ----
outRaster <- c(transformed,
               cropRasters[!(lapply(cropRasters, names) %in% namesTransform)])
outNames <- lapply(outRaster, names)

lapply(
  seq_along(outRaster),
  FUN = function(r) {
    writeRaster(
      outRaster[[r]],
      paste0('output/1-data-prep/covariates/RMNP/', outNames[[r]]),
      format = 'GTiff',
      overwrite = T
    )
  }
)


#### NL ==========================================================
### List rasters ----
# Covariates
lsCovers <-
  data.table(nm = dir('input/covariates/RMNP', '.tif$'))
lsCovers <- gsub(".tif|100m", "", lsCovers$nm)
lsPaths <- dir('input/covariates/RMNP', '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers

### Processing ----
# Crop the rasters, holding as temp files in a list
cropRasters <- lapply(
  lsPaths,
  FUN = function(r) {
    crop(raster(r), mbBounds)
  }
)
Ant <- raster('input/Landcover/Reproj/Anthro100.tif')
Bro <- raster('input/Landcover/Reproj/Broadleaf100.tif')
Con <- raster('input/Landcover/Reproj/Conifer100.tif')
Lic <- raster('input/Landcover/Reproj/Lichen100.tif')
Mix <- raster('input/Landcover/Reproj/MixedWood100.tif')
Roc <- raster('input/Landcover/Reproj/Rocky100.tif')
Scr <- raster('input/Landcover/Reproj/Scrub100.tif')
Wat <- raster('input/Landcover/Reproj/Water100.tif')
Wet <- raster('input/Landcover/Reproj/Wetland100.tif')
WaD1 <- raster('input/Landcover/Reproj/WaterDist.tif')
Lin1 <- raster('input/Landcover/Reproj/LinearDist.tif')

WaD <- log(WaD1 + 1)
Lin <- log(Lin1 + 1)

Elev <- raster('input/Landcover/Reproj/NLElev.tif')
Rug <- terrain(Elev, opt = "roughness")

AntCrop <- crop(Ant, Carclipped)
BroCrop <- crop(Bro, Carclipped)
ConCrop <- crop(Con, Carclipped)
LicCrop <- crop(Lic, Carclipped)
MixCrop <- crop(Mix, Carclipped)
RocCrop <- crop(Roc, Carclipped)
ScrCrop <- crop(Scr, Carclipped)
WatCrop <- crop(Wat, Carclipped)
WetCrop <- crop(Wet, Carclipped)
RugCrop <- crop(Rug, Carclipped)
WaDCrop <- crop(WaD, Carclipped)
LinCrop <- crop(Lin, Carclipped)

AntR <- AntCrop
BroR <- resample(BroCrop, AntCrop)
ConR <- resample(ConCrop, AntCrop)
LicR <- resample(LicCrop, AntCrop)
MixR <- resample(MixCrop, AntCrop)
RocR <- resample(RocCrop, AntCrop)
ScrR <- resample(ScrCrop, AntCrop)
WaDR <- resample(WaDCrop, AntCrop)
LinR <- resample(LinCrop, AntCrop)
RugR <- resample(RugCrop, AntCrop)
