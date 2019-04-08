### Bear RSF ----
# Inputs: Bear relocation data
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md


### Packages ----
libs <- c('adehabitatHR',
          'rgeos',
          'spatstat',
          'raster',
          'data.table',
          'piecewiseSEM',
          'rgdal')
lapply(libs, require, character.only = TRUE)

### Input data ----
MRcar <- readRDS('output/1-data-prep/caribou.Rds')


plot(nlBounds)
points(MRcar$EASTING, MRcar$NORTHING)

CarPoints <-
  SpatialPoints(data.frame(MRcar$EASTING, MRcar$NORTHING), proj4string = CRS(utm))

CarAvail <- mcp(CarPoints, percent = 100)
shapefile(CarAvail, "Output/caribouMCP,shp")

plot(nlBounds)
plot(CarAvail, add = T)
points(CarPoints)

### Buffer by mean step length
CarAvailBuf <- gBuffer(CarAvail, width = mean(MRcar$stepLength))

Carclipped <- gIntersection(nlBounds, CarAvailBuf)
Carclipped2 <- as.owin.SpatialPolygons(Carclipped)

#### Load in habitat layers

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


bears <- readRDS('output/1-data-prep/bear.Rds')
nrow(bears)
plot(bears$EASTING, bears$NORTHING)

head(bears)


plot(nlBounds)
points(bears$EASTING, bears$NORTHING)


BearPoints <-
  SpatialPoints(data.frame(bears$EASTING, bears$NORTHING), proj4string = CRS(utm))

BearAvail <- mcp(BearPoints, percent = 100)
shapefile(BearAvail, "output/bearsMCP.shp")

### Buffer by mean step length
BearAvailBuf <- gBuffer(BearAvail, width = mean(bears$stepLength))

Bearclipped <- gIntersection(nlBounds, BearAvailBuf)
#clipped2<-as.owin.SpatialPolygons(clipped)

# Create Regular Grid

regPts <- generate_grid(Bearclipped, 90, crs = utm)



BearAntUsed <- extract(Ant, BearPoints)
BearBroUsed <- extract(Bro, BearPoints)
BearConUsed <- extract(Con, BearPoints)
BearLicUsed <- extract(Lic, BearPoints)
BearMixUsed <- extract(Mix, BearPoints)
BearRocUsed <- extract(Roc, BearPoints)
BearScrUsed <- extract(Scr, BearPoints)
BearWatUsed <- extract(Wat, BearPoints)
BearWetUsed <- extract(Wet, BearPoints)
BearRugUsed <- extract(Rug, BearPoints)
BearWaDUsed <- extract(WaD, BearPoints)
BearLinUsed <- extract(Lin, BearPoints)

BearRandPoints <-
  SpatialPoints(data.frame(regPts[, 1], regPts[, 2]), proj4string = CRS(utm))


BearAntAvail <- extract(Ant, BearRandPoints)
BearBroAvail <- extract(Bro, BearRandPoints)
BearConAvail <- extract(Con, BearRandPoints)
BearLicAvail <- extract(Lic, BearRandPoints)
BearMixAvail <- extract(Mix, BearRandPoints)
BearRocAvail <- extract(Roc, BearRandPoints)
BearScrAvail <- extract(Scr, BearRandPoints)
BearWatAvail <- extract(Wat, BearRandPoints)
BearWetAvail <- extract(Wet, BearRandPoints)
BearRugAvail <- extract(Rug, BearRandPoints)
BearWaDAvail <- extract(WaD, BearRandPoints)
BearLinAvail <- extract(Lin, BearRandPoints)

CNames <-
  c(
    "use",
    "Ant",
    "Bro",
    "Con",
    "Lic",
    "Mix",
    "Roc",
    "Scr",
    "Wat",
    "Wet",
    "Rug",
    "WaD",
    "Lin",
    "x",
    "y",
    "Season"
  )

BearUsedData <-
  data.frame(
    1,
    BearAntUsed,
    BearBroUsed,
    BearConUsed,
    BearLicUsed,
    BearMixUsed,
    BearRocUsed,
    BearScrUsed,
    BearWatUsed,
    BearWetUsed,
    BearRugUsed,
    BearWaDUsed,
    BearLinUsed,
    bears$EASTING,
    bears$NORTHING,
    bears$season
  )

colnames(BearUsedData) <- CNames

BearAvailData <-
  data.frame(
    0,
    BearAntAvail,
    BearBroAvail,
    BearConAvail,
    BearLicAvail,
    BearMixAvail,
    BearRocAvail,
    BearScrAvail,
    BearWatAvail,
    BearWetAvail,
    BearRugAvail,
    BearWaDAvail,
    BearLinAvail,
    regPts[, 1],
    regPts[, 2],
    "Avail"
  )

colnames(BearAvailData) <- CNames

bearsRSF <- rbind(BearUsedData, BearAvailData)


write.csv(bearsRSF, "output/bearsRSFdata.csv")

bearsRSF <- read.csv("output/bearsRSFdata.csv")

bearsRSF$X <- NULL
head(bearsRSF)




## Remove all points with 50% NA data
bearsRSF$rs <- rowSums(bearsRSF[2:10])
coyRSF2 <- subset(bearsRSF, rs > 0.5)
summary(coyRSF2$rs)

coyRSF2[, 2] <- coyRSF2[, 2] / coyRSF2$rs
coyRSF2[, 3] <- coyRSF2[, 3] / coyRSF2$rs
coyRSF2[, 4] <- coyRSF2[, 4] / coyRSF2$rs
coyRSF2[, 5] <- coyRSF2[, 5] / coyRSF2$rs
coyRSF2[, 6] <- coyRSF2[, 6] / coyRSF2$rs
coyRSF2[, 7] <- coyRSF2[, 7] / coyRSF2$rs
coyRSF2[, 8] <- coyRSF2[, 8] / coyRSF2$rs
coyRSF2[, 9] <- coyRSF2[, 9] / coyRSF2$rs
coyRSF2[, 10] <- coyRSF2[, 10] / coyRSF2$rs

## Check to make sure it worked
coyRSF2$rs2 <- rowSums(coyRSF2[2:10])
head(coyRSF2)


### Wetland is the reference
BearSummer <- subset(coyRSF2, Season == "spring" | Season == "Avail")


RSFbearsSum <-
  glm(use ~ Ant + Bro + Con + Lic + Mix + Roc + Scr + WaD + Lin + Rug,
      data = BearSummer,
      family = 'binomial')


summary(RSFbearsSum)
rsquared(RSFbearsSum)

SumRSF <-
  exp(
    coef(RSFbearsSum)[1] + AntR * coef(RSFbearsSum)[2] + BroR * coef(RSFbearsSum)[3] +
      ConR * coef(RSFbearsSum)[4] + LicR * coef(RSFbearsSum)[5] +
      MixR * coef(RSFbearsSum)[6] + RocR * coef(RSFbearsSum)[7] +
      ScrR * coef(RSFbearsSum)[8] + WaDR * coef(RSFbearsSum)[9] +
      LinR * coef(RSFbearsSum)[10] + RugR * coef(RSFbearsSum)[11]
  )

cellStats(SumRSF, max)

SumRSFsc <-
  (SumRSF - (cellStats(SumRSF, min))) / (cellStats(SumRSF, max) - cellStats(SumRSF, min))

plot(SumRSFsc)

writeRaster(SumRSFsc, "output/PredRSFNL/bearSummer.tif", overwrite = T)

saveRDS(RSFbearsSum, "output/PredRSFNL/BearsSummerRSF.RDS")
