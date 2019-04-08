### Caribou RSF ----
# Inputs: Caribou relocation data
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md


### Packages ----
libs <- c(
  'adehabitatHR',
  'rgeos',
  'spatstat',
  'polyCub',
  'raster',
  'data.table',
  'ewc',
  'piecewiseSEM',
  'rgdal'
)
lapply(libs, require, character.only = TRUE)


### Input ----
utm <- '+proj=utm +zone=21 ellps=WGS84'
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>%
  spTransform(CRSobj = utm)

############ caribou


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


# Create Regular Grid

regPts <- generate_grid(Carclipped, 90, crs = utm)

#CarRandSum<-runifpoint(n=nrow(CarSummer)*10,win=Carclipped2)
#CarRandWin<-runifpoint(n=nrow(CarWinter)*10,win=Carclipped2)

#CarSumPoints<-SpatialPoints(data.frame(CarSummer$EASTING,CarSummer$NORTHING),proj4string = CRS(utm))
#CarWinPoints<-SpatialPoints(data.frame(CarWinter$EASTING,CarWinter$NORTHING),proj4string = CRS(utm))

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



CarAntUsed <- extract(Ant, CarPoints)
CarBroUsed <- extract(Bro, CarPoints)
CarConUsed <- extract(Con, CarPoints)
CarLicUsed <- extract(Lic, CarPoints)
CarMixUsed <- extract(Mix, CarPoints)
CarRocUsed <- extract(Roc, CarPoints)
CarScrUsed <- extract(Scr, CarPoints)
CarWatUsed <- extract(Wat, CarPoints)
CarWetUsed <- extract(Wet, CarPoints)
CarRugUsed <- extract(Rug, CarPoints)
CarWaDUsed <- extract(WaD, CarPoints)
CarLinUsed <- extract(Lin, CarPoints)

CarRandPoints <-
  SpatialPoints(data.frame(regPts[, 1], regPts[, 2]), proj4string = CRS(utm))


CarAntAvail <- extract(Ant, CarRandPoints)
CarBroAvail <- extract(Bro, CarRandPoints)
CarConAvail <- extract(Con, CarRandPoints)
CarLicAvail <- extract(Lic, CarRandPoints)
CarMixAvail <- extract(Mix, CarRandPoints)
CarRocAvail <- extract(Roc, CarRandPoints)
CarScrAvail <- extract(Scr, CarRandPoints)
CarWatAvail <- extract(Wat, CarRandPoints)
CarWetAvail <- extract(Wet, CarRandPoints)
CarRugAvail <- extract(Rug, CarRandPoints)
CarWaDAvail <- extract(WaD, CarRandPoints)
CarLinAvail <- extract(Lin, CarRandPoints)

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

CarUsedData <-
  data.frame(
    1,
    CarAntUsed,
    CarBroUsed,
    CarConUsed,
    CarLicUsed,
    CarMixUsed,
    CarRocUsed,
    CarScrUsed,
    CarWatUsed,
    CarWetUsed,
    CarRugUsed,
    CarWaDUsed,
    CarLinUsed,
    MRcar$EASTING,
    MRcar$NORTHING,
    MRcar$season
  )

colnames(CarUsedData) <- CNames

CarAvailData <-
  data.frame(
    0,
    CarAntAvail,
    CarBroAvail,
    CarConAvail,
    CarLicAvail,
    CarMixAvail,
    CarRocAvail,
    CarScrAvail,
    CarWatAvail,
    CarWetAvail,
    CarRugAvail,
    CarWaDAvail,
    CarLinAvail,
    regPts[, 1],
    regPts[, 2],
    "Avail"
  )

colnames(CarAvailData) <- CNames

caribouRSF <- rbind(CarUsedData, CarAvailData)

summary(CaribouRSF)


write.csv(caribouRSF, "output/CaribouRSFdata.csv")

caribouRSF <- read.csv("output/CaribouRSFdata.csv")

caribouRSF$X <- NULL
head(caribouRSF)


## Remove all points with 50% NA data
#!! no data is removed for RMNP !! <- because RMNP cover data does not contain NAs#
caribouRSF$rs <- rowSums(caribouRSF[2:10])
carRSF2 <- subset(caribouRSF, rs > 0.5)
summary(carRSF2$rs)

carRSF2[, 2] <- carRSF2[, 2] / carRSF2$rs
carRSF2[, 3] <- carRSF2[, 3] / carRSF2$rs
carRSF2[, 4] <- carRSF2[, 4] / carRSF2$rs
carRSF2[, 5] <- carRSF2[, 5] / carRSF2$rs
carRSF2[, 6] <- carRSF2[, 6] / carRSF2$rs
carRSF2[, 7] <- carRSF2[, 7] / carRSF2$rs
carRSF2[, 8] <- carRSF2[, 8] / carRSF2$rs
carRSF2[, 9] <- carRSF2[, 9] / carRSF2$rs
carRSF2[, 10] <- carRSF2[, 10] / carRSF2$rs

## Check to make sure it worked
carRSF2$rs2 <- rowSums(carRSF2[2:10])

head(carRSF2)


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


### Wetland is the reference
library(car)
summary(carRSF2)

CarSummerRSF <- subset(carRSF2, Season == "spring" | Season == "Avail")
CarWinterRSF <- subset(carRSF2, Season == "winter" | Season == "Avail")
summary(CarWinterRSF)

RSFCaribouSum <-
  glm(use ~ Ant + Bro + Con + Lic + Mix + Roc + Scr + WaD + Lin + Rug,
      data = CarSummerRSF,
      family = 'binomial')
RSFCaribouWin <-
  glm(use ~ Con + Lic + Mix + Roc + Scr + WaD + Lin + Rug,
      data = CarWinterRSF,
      family = 'binomial')

vif(RSFCaribouSum)
vif(RSFCaribouWin)

summary(RSFCaribouSum)
rsquared(RSFCaribouSum)

summary(RSFCaribouWin)
rsquared(RSFCaribouWin)


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

SumRSF <-
  exp(
    coef(RSFCaribouSum)[1] + AntR * coef(RSFCaribouSum)[2] + BroR * coef(RSFCaribouSum)[3] +
      ConR * coef(RSFCaribouSum)[4] + LicR * coef(RSFCaribouSum)[5] +
      MixR * coef(RSFCaribouSum)[6] + RocR * coef(RSFCaribouSum)[7] +
      ScrR * coef(RSFCaribouSum)[8] + WaDR * coef(RSFCaribouSum)[9] +
      LinR * coef(RSFCaribouSum)[10] + RugR * coef(RSFCaribouSum)[11]
  )

WinRSF <-
  exp(
    coef(RSFCaribouWin)[1] + ConR * coef(RSFCaribouWin)[2] + LicR * coef(RSFCaribouWin)[3] +
      MixR * coef(RSFCaribouWin)[4] + RocR * coef(RSFCaribouWin)[5] +
      ScrR * coef(RSFCaribouWin)[6] + WaDR * coef(RSFCaribouWin)[7] +
      LinR * coef(RSFCaribouWin)[8] + RugR * coef(RSFCaribouWin)[9]
  )


cellStats(SumRSF, max)

SumRSFsc <-
  (SumRSF - (cellStats(SumRSF, min))) / (cellStats(SumRSF, max) - cellStats(SumRSF, min))
WinRSFsc <-
  (WinRSF - (cellStats(WinRSF, min))) / (cellStats(WinRSF, max) - cellStats(WinRSF, min))

cellStats(SumRSFsc, max)

plot(SumRSFsc)
plot(WinRSFsc)

writeRaster(SumRSFsc, "output/PredRSFNL/CaribouSummer.tif", overwrite =
              T)
writeRaster(WinRSFsc, "output/PredRSFNL/CaribouWinter.tif", overwrite =
              T)

saveRDS(RSFCaribouSum, "output/PredRSFNL/CaribouSummerRSF.RDS")
saveRDS(RSFCaribouWin, "output/PredRSFNL/CaribouWinterRSF.RDS")
