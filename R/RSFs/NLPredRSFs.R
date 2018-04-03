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
WaD<-raster('input/Landcover/Reproj/WaterDist.tif')
Lin<-raster('input/Landcover/Reproj/LinearDist.tif')

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

RSFCoyoteSum<-glm(use~Ant+Lin+Bro+Con+Lic+Mix+Roc+Scr+WaD+Rug,data=CoySummer, family='binomial')
RSFCoyoteWin<-glm(use~Ant+Lin+Bro+Con+Lic+Mix+Roc+Scr+WaD+Rug,data=CoyWinter, family='binomial')

summary(RSFCoyoteSum)
rsquared(RSFCoyoteSum)

summary(RSFCoyoteWin)
rsquared(RSFCoyoteWin)











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


