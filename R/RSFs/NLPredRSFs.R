################ Coyote RSFs ######################

library(adehabitatHR)
library(rgeos)
library(spatstat)
library(polyCub)
library(raster)
library(data.table)
library(knitr)
library(magrittr)
library(piecewiseSEM)

utm <- '+proj=utm +zone=21 ellps=WGS84'
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm)

coyote<-readRDS('output/data-prep/coyote.Rds')

plot(coyote$EASTING,coyote$NORTHING)

head(coyote)


plot(nlBounds)
points(coyote$EASTING,coyote$NORTHING)

MRcoy<-subset(coyote,herd=="MIDRIDGE")

unique(MRcoy$herd)
str(coyote)

plot(nlBounds)
points(MRcoy$EASTING,MRcoy$NORTHING)


coyote$STATUS<-"RESIDENT"
coyote$STATUS<-ifelse(coyote$id=="co_lp1109", "UNKNOWN", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1003", "UNKNOWN", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1012", "UNKNOWN", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1022", "UNKNOWN", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp0902", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1001", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1003", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1012", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1107", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1005", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1010", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1312", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1303", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np0915", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1005", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1006", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1017", "SUB-TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1005", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1106", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0806", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0901", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0907", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0911", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0914", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1014", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1015", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1102", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1106", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1205", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1304", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1311", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1313", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1314", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np0801", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np0904", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1002", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1015", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1014", "TOO FEW DAYS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp0908", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1010", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1013", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1101", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1009", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1107", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1108", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1204", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1210", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1211", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np0908", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1021", "TOO FEW LOCS", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp0904", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp0910", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1002", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1006", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1008", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_lp1014", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0905", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0906", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr0908", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1001", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1004", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1006", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1007", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1011", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1013", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1101", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1105", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1209", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1212", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1302", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1306", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_mr1308", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np0903", "TRANSIENT", coyote$STATUS)
coyote$STATUS<-ifelse(coyote$id=="co_np1016", "TRANSIENT", coyote$STATUS)

coyMR<-subset(coyote, herd=="MIDRIDGE")
coyMRres<-subset(coyMR, STATUS=="RESIDENT" | STATUS=="SUB-TRANSIENT")

points<-SpatialPoints(data.frame(coyMRres$EASTING,coyMRres$NORTHING),proj4string = CRS(utm))

CoyAvail<-mcp(points, percent = 100)

### Buffer by mean step length
CoyAvailBuf<-gBuffer(CoyAvail,width=mean(coyMRres$stepLength))

clipped<-gIntersection(nlBounds,CoyAvailBuf)
clipped2<-as.owin.SpatialPolygons(clipped)

winter<-subset(coyMRres,julday<74)
summer<-subset(coyMRres,julday>106 &julday<214)

randSum<-runifpoint(n=nrow(summer)*10,win=clipped2)
randWin<-runifpoint(n=nrow(winter)*10,win=clipped2)

sumPoints<-SpatialPoints(data.frame(summer$EASTING,summer$NORTHING),proj4string = CRS(utm))
winPoints<-SpatialPoints(data.frame(winter$EASTING,winter$NORTHING),proj4string = CRS(utm))

#### Load in habitat layers

Ant<-raster('input/Landcover/Reproj/Anthro100.tif')
Bro<-raster('input/Landcover/Reproj/Broadleaf100.tif')
Con<-raster('input/Landcover/Reproj/Conifer100.tif')
Lic<-raster('input/Landcover/Reproj/Lichen100.tif')
Mix<-raster('input/Landcover/Reproj/MixedWood100.tif')
Roc<-raster('input/Landcover/Reproj/Rocky100.tif')
Scr<-raster('input/Landcover/Reproj/Scrub100.tif')
Wat<-raster('input/Landcover/Reproj/Water100.tif')
Wet<-raster('input/Landcover/Reproj/Wetland100.tif')
WaD1<-raster('input/Landcover/Reproj/WaterDist.tif')
Lin1<-raster('input/Landcover/Reproj/LinearDist.tif')

WaD<-log(WaD1+1)
Lin<-log(Lin1+1)

Elev<-raster('input/Landcover/Reproj/NLElev.tif')
Rug<-terrain(Elev, opt="roughness")




## Summer
AntUsedSum<-extract(Ant,sumPoints)
BroUsedSum<-extract(Bro,sumPoints)
ConUsedSum<-extract(Con,sumPoints)
LicUsedSum<-extract(Lic,sumPoints)
MixUsedSum<-extract(Mix,sumPoints)
RocUsedSum<-extract(Roc,sumPoints)
ScrUsedSum<-extract(Scr,sumPoints)
WatUsedSum<-extract(Wat,sumPoints)
WetUsedSum<-extract(Wet,sumPoints)
RugUsedSum<-extract(Rug,sumPoints)
RugUsedSum<-extract(Rug,sumPoints)
WaDUsedSum<-extract(WaD,sumPoints)
LinUsedSum<-extract(Lin,sumPoints)

randCoSum<-coords(randSum)

xyrSum<-cbind(randCoSum$x,randCoSum$y)

randPointsSum<-SpatialPoints(data.frame(xyrSum[,1],xyrSum[,2]),proj4string = CRS(utm))


AntAvailSum<-extract(Ant,randPointsSum)
BroAvailSum<-extract(Bro,randPointsSum)
ConAvailSum<-extract(Con,randPointsSum)
LicAvailSum<-extract(Lic,randPointsSum)
MixAvailSum<-extract(Mix,randPointsSum)
RocAvailSum<-extract(Roc,randPointsSum)
ScrAvailSum<-extract(Scr,randPointsSum)
WatAvailSum<-extract(Wat,randPointsSum)
WetAvailSum<-extract(Wet,randPointsSum)
RugAvailSum<-extract(Rug,randPointsSum)
WaDAvailSum<-extract(WaD,randPointsSum)
LinAvailSum<-extract(Lin,randPointsSum)

CNames<-c("use","Ant","Bro","Con","Lic","Mix","Roc","Scr","Wat","Wet","Rug","WaD","Lin","x","y","Season")

UsedDataSum<-data.frame(1,AntUsedSum,BroUsedSum,ConUsedSum,LicUsedSum,MixUsedSum,RocUsedSum,
                        ScrUsedSum,WatUsedSum,WetUsedSum,RugUsedSum,WaDUsedSum,LinUsedSum,summer$EASTING,summer$NORTHING,"Summer")
colnames(UsedDataSum)<-CNames

AvailDataSum<-data.frame(0,AntAvailSum,BroAvailSum,ConAvailSum,LicAvailSum,MixAvailSum,RocAvailSum,ScrAvailSum,
                      WatAvailSum,WetAvailSum,RugAvailSum,WaDAvailSum,LinAvailSum,xyrSum,"Summer")
colnames(AvailDataSum)<-CNames



## Winter
AntUsedWin<-extract(Ant,winPoints)
BroUsedWin<-extract(Bro,winPoints)
ConUsedWin<-extract(Con,winPoints)
LicUsedWin<-extract(Lic,winPoints)
MixUsedWin<-extract(Mix,winPoints)
RocUsedWin<-extract(Roc,winPoints)
ScrUsedWin<-extract(Scr,winPoints)
WatUsedWin<-extract(Wat,winPoints)
WetUsedWin<-extract(Wet,winPoints)
RugUsedWin<-extract(Rug,winPoints)
WaDUsedWin<-extract(WaD,winPoints)
LinUsedWin<-extract(Lin,winPoints)

randCoWin<-coords(randWin)

xyrWin<-cbind(randCoWin$x,randCoWin$y)

randPointsWin<-SpatialPoints(data.frame(xyrWin[,1],xyrWin[,2]),proj4string = CRS(utm))


AntAvailWin<-extract(Ant,randPointsWin)
BroAvailWin<-extract(Bro,randPointsWin)
ConAvailWin<-extract(Con,randPointsWin)
LicAvailWin<-extract(Lic,randPointsWin)
MixAvailWin<-extract(Mix,randPointsWin)
RocAvailWin<-extract(Roc,randPointsWin)
ScrAvailWin<-extract(Scr,randPointsWin)
WatAvailWin<-extract(Wat,randPointsWin)
WetAvailWin<-extract(Wet,randPointsWin)
RugAvailWin<-extract(Rug,randPointsWin)
WaDAvailWin<-extract(WaD,randPointsWin)
LinAvailWin<-extract(Lin,randPointsWin)

UsedDataWin<-data.frame(1,AntUsedWin,BroUsedWin,ConUsedWin,LicUsedWin,MixUsedWin,RocUsedWin,
                        ScrUsedWin,WatUsedWin,WetUsedWin,RugUsedWin,WaDUsedWin,LinUsedWin,winter$EASTING,winter$NORTHING,"Winter")
colnames(UsedDataWin)<-CNames

AvailDataWin<-data.frame(0,AntAvailWin,BroAvailWin,ConAvailWin,LicAvailWin,MixAvailWin,RocAvailWin,ScrAvailWin,
                         WatAvailWin,WetAvailWin,RugAvailWin,WaDAvailWin,LinAvailWin,xyrWin,"Winter")
colnames(AvailDataWin)<-CNames


coyoteRSF<-rbind(UsedDataSum,AvailDataSum,UsedDataWin,AvailDataWin)


## Remove all points with 50% NA data
coyoteRSF$rs<-rowSums(coyoteRSF[2:10])
coyRSF2<-subset(coyoteRSF,rs>0.5)
summary(coyRSF2$rs)

coyRSF2[,2]<-coyRSF2[,2]/coyRSF2$rs
coyRSF2[,3]<-coyRSF2[,3]/coyRSF2$rs
coyRSF2[,4]<-coyRSF2[,4]/coyRSF2$rs
coyRSF2[,5]<-coyRSF2[,5]/coyRSF2$rs
coyRSF2[,6]<-coyRSF2[,6]/coyRSF2$rs
coyRSF2[,7]<-coyRSF2[,7]/coyRSF2$rs
coyRSF2[,8]<-coyRSF2[,8]/coyRSF2$rs
coyRSF2[,9]<-coyRSF2[,9]/coyRSF2$rs
coyRSF2[,10]<-coyRSF2[,10]/coyRSF2$rs

## Check to make sure it worked
coyRSF2$rs2<-rowSums(coyRSF2[2:10])

head(coyRSF2)
##
mean(coyRSF2$Ant)
mean(coyRSF2$Bro)
mean(coyRSF2$Con)
mean(coyRSF2$Lic)
mean(coyRSF2$Mix)
mean(coyRSF2$Roc)
mean(coyRSF2$Scr)
mean(coyRSF2$Wat)
mean(coyRSF2$Wet)


AntCrop<-crop(Ant,clipped)
BroCrop<-crop(Bro,clipped)
ConCrop<-crop(Con,clipped)
LicCrop<-crop(Lic,clipped)
MixCrop<-crop(Mix,clipped)
RocCrop<-crop(Roc,clipped)
ScrCrop<-crop(Scr,clipped)
WatCrop<-crop(Wat,clipped)
WetCrop<-crop(Wet,clipped)
RugCrop<-crop(Rug,clipped)
WaDCrop<-crop(WaD,clipped)
LinCrop<-crop(Lin,clipped)

str(CoySummer)

### Wetland is the reference
CoySummer<-subset(coyRSF2,Season=="Summer")
CoyWinter<-subset(coyRSF2,Season=="Winter")

RSFCoyoteSum<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=CoySummer, family='binomial')
RSFCoyoteWin<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=CoyWinter, family='binomial')

summary(RSFCoyoteSum)
rsquared(RSFCoyoteSum)

summary(RSFCoyoteWin)
rsquared(RSFCoyoteWin)


AntR<-AntCrop
BroR<-resample(BroCrop,AntCrop)
ConR<-resample(ConCrop,AntCrop)
LicR<-resample(LicCrop,AntCrop)
MixR<-resample(MixCrop,AntCrop)
RocR<-resample(RocCrop,AntCrop)
ScrR<-resample(ScrCrop,AntCrop)
WaDR<-resample(WaDCrop,AntCrop)
LinR<-resample(LinCrop,AntCrop)
RugR<-resample(RugCrop,AntCrop)

SumRSF<-exp(AntR*coef(RSFCoyoteSum)[2]+BroR*coef(RSFCoyoteSum)[3]+ConR*coef(RSFCoyoteSum)[4]+LicR*coef(RSFCoyoteSum)[5]+
              MixR*coef(RSFCoyoteSum)[6]+RocR*coef(RSFCoyoteSum)[7]+ScrR*coef(RSFCoyoteSum)[8]+WaDR*coef(RSFCoyoteSum)[9]+
              LinR*coef(RSFCoyoteSum)[10]+RugR*coef(RSFCoyoteSum)[11])

WinRSF<-exp(AntR*coef(RSFCoyoteWin)[2]+BroR*coef(RSFCoyoteWin)[3]+ConR*coef(RSFCoyoteWin)[4]+LicR*coef(RSFCoyoteWin)[5]+
              MixR*coef(RSFCoyoteWin)[6]+RocR*coef(RSFCoyoteWin)[7]+ScrR*coef(RSFCoyoteWin)[8]+WaDR*coef(RSFCoyoteWin)[9]+
              LinR*coef(RSFCoyoteWin)[10]+RugR*coef(RSFCoyoteWin)[11])

cellStats(SumRSF,min)

SumRSFsc<-(SumRSF-(cellStats(SumRSF,min)))/(cellStats(SumRSF,max)-cellStats(SumRSF,min))
WinRSFsc<-(WinRSF-(cellStats(WinRSF,min)))/(cellStats(WinRSF,max)-cellStats(WinRSF,min))

SumRSFz<-(SumRSF-(cellStats(SumRSF,mean)))/(cellStats(SumRSF,sd))
WinRSFz<-(WinRSF-(cellStats(WinRSF,mean)))/(cellStats(WinRSF,sd))

?cellStats

hist(log(WinRSFsc))

plot(SumRSFz,zlim=c(0,1))
plot(WinRSFz)

cellStats(SumRSFsc,max)

plot(SumRSFsc)
plot(WinRSFsc)

writeRaster(SumRSF,"output/PredRSFNL/CoyoteSummer.tif",overwrite=T)
writeRaster(WinRSF,"output/PredRSFNL/CoyoteWinter.tif",overwrite=T)


################## Black bears ####################

utm <- '+proj=utm +zone=21 ellps=WGS84'
nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm)

bear<-readRDS('output/data-prep/bear.Rds')

head(bear)

plot(nlBounds)
points(bear$EASTING,bear$NORTHING)


points<-SpatialPoints(data.frame(bear$EASTING,bear$NORTHING),proj4string = CRS(utm))

BearAvail<-mcp(points, percent = 100)

### Buffer by mean step length
BearAvailBuf<-gBuffer(BearAvail,width=mean(bear$stepLength))

clipped<-gIntersection(nlBounds,BearAvailBuf)
clipped2<-as.owin.SpatialPolygons(clipped)

winter<-subset(bear,julday<74)
summer<-subset(bear,julday>106 &julday<214)

randSum<-runifpoint(n=nrow(summer)*10,win=clipped2)
randWin<-runifpoint(n=nrow(winter)*10,win=clipped2)

sumPoints<-SpatialPoints(data.frame(summer$EASTING,summer$NORTHING),proj4string = CRS(utm))
winPoints<-SpatialPoints(data.frame(winter$EASTING,winter$NORTHING),proj4string = CRS(utm))

#### Load in habitat layers

Ant<-raster('input/Landcover/Reproj/Anthro100.tif')
Bro<-raster('input/Landcover/Reproj/Broadleaf100.tif')
Con<-raster('input/Landcover/Reproj/Conifer100.tif')
Lic<-raster('input/Landcover/Reproj/Lichen100.tif')
Mix<-raster('input/Landcover/Reproj/MixedWood100.tif')
Roc<-raster('input/Landcover/Reproj/Rocky100.tif')
Scr<-raster('input/Landcover/Reproj/Scrub100.tif')
Wat<-raster('input/Landcover/Reproj/Water100.tif')
Wet<-raster('input/Landcover/Reproj/Wetland100.tif')
WaD1<-raster('input/Landcover/Reproj/WaterDist.tif')
Lin1<-raster('input/Landcover/Reproj/LinearDist.tif')

WaD<-log(WaD1+1)
Lin<-log(Lin1+1)

Elev<-raster('input/Landcover/Reproj/NLElev.tif')
Rug<-terrain(Elev, opt="roughness")




## Summer
AntUsedSum<-extract(Ant,sumPoints)
BroUsedSum<-extract(Bro,sumPoints)
ConUsedSum<-extract(Con,sumPoints)
LicUsedSum<-extract(Lic,sumPoints)
MixUsedSum<-extract(Mix,sumPoints)
RocUsedSum<-extract(Roc,sumPoints)
ScrUsedSum<-extract(Scr,sumPoints)
WatUsedSum<-extract(Wat,sumPoints)
WetUsedSum<-extract(Wet,sumPoints)
RugUsedSum<-extract(Rug,sumPoints)
RugUsedSum<-extract(Rug,sumPoints)
WaDUsedSum<-extract(WaD,sumPoints)
LinUsedSum<-extract(Lin,sumPoints)

randCoSum<-coords(randSum)

xyrSum<-cbind(randCoSum$x,randCoSum$y)

randPointsSum<-SpatialPoints(data.frame(xyrSum[,1],xyrSum[,2]),proj4string = CRS(utm))


AntAvailSum<-extract(Ant,randPointsSum)
BroAvailSum<-extract(Bro,randPointsSum)
ConAvailSum<-extract(Con,randPointsSum)
LicAvailSum<-extract(Lic,randPointsSum)
MixAvailSum<-extract(Mix,randPointsSum)
RocAvailSum<-extract(Roc,randPointsSum)
ScrAvailSum<-extract(Scr,randPointsSum)
WatAvailSum<-extract(Wat,randPointsSum)
WetAvailSum<-extract(Wet,randPointsSum)
RugAvailSum<-extract(Rug,randPointsSum)
WaDAvailSum<-extract(WaD,randPointsSum)
LinAvailSum<-extract(Lin,randPointsSum)

CNames<-c("use","Ant","Bro","Con","Lic","Mix","Roc","Scr","Wat","Wet","Rug","WaD","Lin","x","y","Season")

UsedDataSum<-data.frame(1,AntUsedSum,BroUsedSum,ConUsedSum,LicUsedSum,MixUsedSum,RocUsedSum,
                        ScrUsedSum,WatUsedSum,WetUsedSum,RugUsedSum,WaDUsedSum,LinUsedSum,summer$EASTING,summer$NORTHING,"Summer")
colnames(UsedDataSum)<-CNames

AvailDataSum<-data.frame(0,AntAvailSum,BroAvailSum,ConAvailSum,LicAvailSum,MixAvailSum,RocAvailSum,ScrAvailSum,
                         WatAvailSum,WetAvailSum,RugAvailSum,WaDAvailSum,LinAvailSum,xyrSum,"Summer")
colnames(AvailDataSum)<-CNames



## Winter
AntUsedWin<-extract(Ant,winPoints)
BroUsedWin<-extract(Bro,winPoints)
ConUsedWin<-extract(Con,winPoints)
LicUsedWin<-extract(Lic,winPoints)
MixUsedWin<-extract(Mix,winPoints)
RocUsedWin<-extract(Roc,winPoints)
ScrUsedWin<-extract(Scr,winPoints)
WatUsedWin<-extract(Wat,winPoints)
WetUsedWin<-extract(Wet,winPoints)
RugUsedWin<-extract(Rug,winPoints)
WaDUsedWin<-extract(WaD,winPoints)
LinUsedWin<-extract(Lin,winPoints)

randCoWin<-coords(randWin)

xyrWin<-cbind(randCoWin$x,randCoWin$y)

randPointsWin<-SpatialPoints(data.frame(xyrWin[,1],xyrWin[,2]),proj4string = CRS(utm))


AntAvailWin<-extract(Ant,randPointsWin)
BroAvailWin<-extract(Bro,randPointsWin)
ConAvailWin<-extract(Con,randPointsWin)
LicAvailWin<-extract(Lic,randPointsWin)
MixAvailWin<-extract(Mix,randPointsWin)
RocAvailWin<-extract(Roc,randPointsWin)
ScrAvailWin<-extract(Scr,randPointsWin)
WatAvailWin<-extract(Wat,randPointsWin)
WetAvailWin<-extract(Wet,randPointsWin)
RugAvailWin<-extract(Rug,randPointsWin)
WaDAvailWin<-extract(WaD,randPointsWin)
LinAvailWin<-extract(Lin,randPointsWin)

UsedDataWin<-data.frame(1,AntUsedWin,BroUsedWin,ConUsedWin,LicUsedWin,MixUsedWin,RocUsedWin,
                        ScrUsedWin,WatUsedWin,WetUsedWin,RugUsedWin,WaDUsedWin,LinUsedWin,winter$EASTING,winter$NORTHING,"Winter")
colnames(UsedDataWin)<-CNames

AvailDataWin<-data.frame(0,AntAvailWin,BroAvailWin,ConAvailWin,LicAvailWin,MixAvailWin,RocAvailWin,ScrAvailWin,
                         WatAvailWin,WetAvailWin,RugAvailWin,WaDAvailWin,LinAvailWin,xyrWin,"Winter")
colnames(AvailDataWin)<-CNames


bearRSF<-rbind(UsedDataSum,AvailDataSum,UsedDataWin,AvailDataWin)


## Remove all points with 50% NA data
bearRSF$rs<-rowSums(bearRSF[2:10])
bearRSF2<-subset(bearRSF,rs>0.5)
summary(bearRSF2$rs)

bearRSF2[,2]<-bearRSF2[,2]/bearRSF2$rs
bearRSF2[,3]<-bearRSF2[,3]/bearRSF2$rs
bearRSF2[,4]<-bearRSF2[,4]/bearRSF2$rs
bearRSF2[,5]<-bearRSF2[,5]/bearRSF2$rs
bearRSF2[,6]<-bearRSF2[,6]/bearRSF2$rs
bearRSF2[,7]<-bearRSF2[,7]/bearRSF2$rs
bearRSF2[,8]<-bearRSF2[,8]/bearRSF2$rs
bearRSF2[,9]<-bearRSF2[,9]/bearRSF2$rs
bearRSF2[,10]<-bearRSF2[,10]/bearRSF2$rs

## Check to make sure it worked
bearRSF2$rs2<-rowSums(bearRSF2[2:10])

head(bearRSF2)
##
mean(bearRSF2$Ant)
mean(bearRSF2$Bro)
mean(bearRSF2$Con)
mean(bearRSF2$Lic)
mean(bearRSF2$Mix)
mean(bearRSF2$Roc)
mean(bearRSF2$Scr)
mean(bearRSF2$Wat)
mean(bearRSF2$Wet)


AntCrop<-crop(Ant,clipped)
BroCrop<-crop(Bro,clipped)
ConCrop<-crop(Con,clipped)
LicCrop<-crop(Lic,clipped)
MixCrop<-crop(Mix,clipped)
RocCrop<-crop(Roc,clipped)
ScrCrop<-crop(Scr,clipped)
WatCrop<-crop(Wat,clipped)
WetCrop<-crop(Wet,clipped)
RugCrop<-crop(Rug,clipped)
WaDCrop<-crop(WaD,clipped)
LinCrop<-crop(Lin,clipped)

str(BearSummer)

### Wetland is the reference
bearSummer<-subset(bearRSF2,Season=="Summer")
bearWinter<-subset(bearRSF2,Season=="Winter")

RSFbearSum<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=bearSummer, family='binomial')
RSFbearWin<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=bearWinter, family='binomial')

summary(RSFbearSum)
rsquared(RSFbearSum)

summary(RSFbearWin)
rsquared(RSFbearWin)


AntR<-AntCrop
BroR<-resample(BroCrop,AntCrop)
ConR<-resample(ConCrop,AntCrop)
LicR<-resample(LicCrop,AntCrop)
MixR<-resample(MixCrop,AntCrop)
RocR<-resample(RocCrop,AntCrop)
ScrR<-resample(ScrCrop,AntCrop)
WaDR<-resample(WaDCrop,AntCrop)
LinR<-resample(LinCrop,AntCrop)
RugR<-resample(RugCrop,AntCrop)

SumRSFbear<-exp(AntR*coef(RSFbearSum)[2]+BroR*coef(RSFbearSum)[3]+ConR*coef(RSFbearSum)[4]+LicR*coef(RSFbearSum)[5]+
              MixR*coef(RSFbearSum)[6]+RocR*coef(RSFbearSum)[7]+ScrR*coef(RSFbearSum)[8]+WaDR*coef(RSFbearSum)[9]+
              LinR*coef(RSFbearSum)[10]+RugR*coef(RSFbearSum)[11])

#WinRSFbear<-exp(AntR*coef(RSFbearWin)[2]+BroR*coef(RSFbearWin)[3]+ConR*coef(RSFbearWin)[4]+LicR*coef(RSFbearWin)[5]+
#              MixR*coef(RSFbearWin)[6]+RocR*coef(RSFbearWin)[7]+ScrR*coef(RSFbearWin)[8]+WaDR*coef(RSFbearWin)[9]+
#              LinR*coef(RSFbearWin)[10]+RugR*coef(RSFbearWin)[11])

cellStats(SumRSFbear,min)

SumRSFbearsc<-(SumRSFbear-(cellStats(SumRSFbear,min)))/(cellStats(SumRSFbear,max)-cellStats(SumRSFbear,min))
#WinRSFsc<-(WinRSFbear-(cellStats(WinRSFbear,min)))/(cellStats(WinRSFbear,max)-cellStats(WinRSFbear,min))

hist(SumRSFsc)

plot(SumRSFbear)
plot(WinRSF)

cellStats(SumRSFsc,max)

plot(SumRSFbearsc)
#plot(WinRSFsc)

writeRaster(SumRSFbear,"output/PredRSFNL/bearSummer.tif",overwrite=T)
#writeRaster(WinRSF,"output/PredRSFNL/bearWinter.tif",overwrite=T)








############ caribou


caribou<-readRDS('output/data-prep/caribou.Rds')
MRcar<-subset(caribou,herd=="MIDRIDGE")

plot(nlBounds)
points(MRcar$EASTING,MRcar$NORTHING)

points<-SpatialPoints(data.frame(MRcar$EASTING,MRcar$NORTHING))

carMCP<-mcp(points, percent=99.9)

plot(nlBounds)
plot(carMCP,add=T)
points(points)


