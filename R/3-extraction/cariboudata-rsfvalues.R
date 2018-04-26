library(raster)

BeaSum<-raster("output/PredRSFNL/bearSummer.tif")
CarSum<-raster("output/PredRSFNL/CaribouSummer.tif")
CarWin<-raster("output/PredRSFNL/CaribouWinter.tif")
CoySum<-raster("output/PredRSFNL/CoyoteSummer.tif")
CoyWin<-raster("output/PredRSFNL/CoyoteWinter.tif")

utm <- '+proj=utm +zone=21 ellps=WGS84'
caribou<-readRDS('output/data-prep/caribou.Rds')
str(caribou)
Summer<-subset(caribou,season=='spring')
Winter<-subset(caribou,season=='winter')

CarSumPoints<-SpatialPoints(data.frame(Summer$EASTING,Summer$NORTHING),proj4string = CRS(utm))
CarWinPoints<-SpatialPoints(data.frame(Winter$EASTING,Winter$NORTHING),proj4string = CRS(utm))

plot(CarSum)
plot(BeaSum)
points(CarSumPoints)


Summer$CarRSF<-extract(CarSum,CarSumPoints)
Winter$CarRSF<-extract(CarWin,CarWinPoints)

Summer$CoyRSF<-extract(CoySum,CarSumPoints)
Winter$CoyRSF<-extract(CoyWin,CarWinPoints)

Summer$BearRSF<-extract(BeaSum,CarSumPoints)
Winter$BearRSF<-NA

Summer$Season<-"Summer"
Winter$Season<-"Winter"

AllData<-rbind(Summer,Winter)

summary(Summer)

head(AllData)

saveRDS(AllData,"output/PredRSFNL/CaribouRSFVals.RDS")

