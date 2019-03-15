### Coyote RSF ----
# Authors: 
# Purpose: 
# Inputs: Coyote relocation data
# Outputs: 
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 


### Packages ----
libs <- c('adehabitatHR', 'rgeos', 'spatstat', 'polyCub', 
          'raster', 'data.table', 'ewc',
          'piecewiseSEM', 'rgdal')
lapply(libs, require, character.only = TRUE)

### Input data ----

utm <- '+proj=utm +zone=21 ellps=WGS84'
nlBounds <- spTransform(readOGR('input/etc/NL-Bounds/NL-Bounds.shp'),
                        CRSobj = utm)

############ caribou


MRcar<-readRDS('output/data-prep/caribou.Rds')


plot(nlBounds)
points(MRcar$EASTING,MRcar$NORTHING)

CarPoints<-SpatialPoints(data.frame(MRcar$EASTING,MRcar$NORTHING),proj4string = CRS(utm))

CarAvail<-mcp(CarPoints, percent = 100)
shapefile(CarAvail,"Output/caribouMCP.shp")

plot(nlBounds)
plot(CarAvail,add=T)
points(CarPoints)

### Buffer by mean step length
CarAvailBuf<-gBuffer(CarAvail,width=mean(MRcar$stepLength))

Carclipped<-gIntersection(nlBounds,CarAvailBuf)
Carclipped2<-as.owin.SpatialPolygons(Carclipped)

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

AntCrop<-crop(Ant,Carclipped)
BroCrop<-crop(Bro,Carclipped)
ConCrop<-crop(Con,Carclipped)
LicCrop<-crop(Lic,Carclipped)
MixCrop<-crop(Mix,Carclipped)
RocCrop<-crop(Roc,Carclipped)
ScrCrop<-crop(Scr,Carclipped)
WatCrop<-crop(Wat,Carclipped)
WetCrop<-crop(Wet,Carclipped)
RugCrop<-crop(Rug,Carclipped)
WaDCrop<-crop(WaD,Carclipped)
LinCrop<-crop(Lin,Carclipped)

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


coyote<-readRDS('output/data-prep/coyote.Rds')
nrow(coyote)
plot(coyote$EASTING,coyote$NORTHING)

head(coyote)


plot(nlBounds)
points(coyote$EASTING,coyote$NORTHING)

unique(MRcoy$herd)
str(coyote)

plot(nlBounds)
#points(MRcoy$EASTING,MRcoy$NORTHING)


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

saveRDS(coyMRres,"output/ResidentCoyotes.RDS")

nrow(coyMRres)
CoyPoints<-SpatialPoints(data.frame(coyMRres$EASTING,coyMRres$NORTHING),proj4string = CRS(utm))

CoyAvail<-mcp(CoyPoints, percent = 100)
shapefile(CoyAvail,"output/coyoteMCP.shp",overwrite=T)

### Buffer by mean step length
CoyAvailBuf<-gBuffer(CoyAvail,width=mean(coyMRres$stepLength))

Coyclipped<-gIntersection(nlBounds,CoyAvailBuf)
#clipped2<-as.owin.SpatialPolygons(clipped)

# Create Regular Grid

regPts <- generate_grid(Coyclipped, 90, crs = utm)



CoyAntUsed<-extract(Ant,CoyPoints)
CoyBroUsed<-extract(Bro,CoyPoints)
CoyConUsed<-extract(Con,CoyPoints)
CoyLicUsed<-extract(Lic,CoyPoints)
CoyMixUsed<-extract(Mix,CoyPoints)
CoyRocUsed<-extract(Roc,CoyPoints)
CoyScrUsed<-extract(Scr,CoyPoints)
CoyWatUsed<-extract(Wat,CoyPoints)
CoyWetUsed<-extract(Wet,CoyPoints)
CoyRugUsed<-extract(Rug,CoyPoints)
CoyWaDUsed<-extract(WaD,CoyPoints)
CoyLinUsed<-extract(Lin,CoyPoints)

CoyRandPoints<-SpatialPoints(data.frame(regPts[,1],regPts[,2]),proj4string = CRS(utm))


CoyAntAvail<-extract(Ant,CoyRandPoints)
CoyBroAvail<-extract(Bro,CoyRandPoints)
CoyConAvail<-extract(Con,CoyRandPoints)
CoyLicAvail<-extract(Lic,CoyRandPoints)
CoyMixAvail<-extract(Mix,CoyRandPoints)
CoyRocAvail<-extract(Roc,CoyRandPoints)
CoyScrAvail<-extract(Scr,CoyRandPoints)
CoyWatAvail<-extract(Wat,CoyRandPoints)
CoyWetAvail<-extract(Wet,CoyRandPoints)
CoyRugAvail<-extract(Rug,CoyRandPoints)
CoyWaDAvail<-extract(WaD,CoyRandPoints)
CoyLinAvail<-extract(Lin,CoyRandPoints)

CNames<-c("use","Ant","Bro","Con","Lic","Mix","Roc","Scr","Wat","Wet","Rug","WaD","Lin","x","y","Season")

CoyUsedData<-data.frame(1,CoyAntUsed,CoyBroUsed,CoyConUsed,CoyLicUsed,CoyMixUsed,CoyRocUsed,
                        CoyScrUsed,CoyWatUsed,CoyWetUsed,CoyRugUsed,CoyWaDUsed,CoyLinUsed,coyMRres$EASTING,coyMRres$NORTHING,coyMRres$season)

colnames(CoyUsedData)<-CNames

CoyAvailData<-data.frame(0,CoyAntAvail,CoyBroAvail,CoyConAvail,CoyLicAvail,CoyMixAvail,CoyRocAvail,CoyScrAvail,
                         CoyWatAvail,CoyWetAvail,CoyRugAvail,CoyWaDAvail,CoyLinAvail,regPts[,1],regPts[,2],"Avail")

colnames(CoyAvailData)<-CNames

coyoteRSF<-rbind(CoyUsedData,CoyAvailData)


write.csv(coyoteRSF,"output/CoyoteRSFdata.csv")

coyoteRSF<-read.csv("output/CoyoteRSFdata.csv")

coyoteRSF$X<-NULL
head(coyoteRSF)




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


### Wetland is the reference
CoySummer<-subset(coyRSF2,Season=="spring"|Season=="Avail")
CoyWinter<-subset(coyRSF2,Season=="winter"|Season=="Avail")

RSFCoyoteSum<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=CoySummer, family='binomial')
RSFCoyoteWin<-glm(use~Ant+Bro+Con+Lic+Mix+Roc+Scr+WaD+Lin+Rug,data=CoyWinter, family='binomial')

summary(RSFCoyoteSum)
rsquared(RSFCoyoteSum)

summary(RSFCoyoteWin)
rsquared(RSFCoyoteWin)

SumRSF<-exp(coef(RSFCoyoteSum)[1]+AntR*coef(RSFCoyoteSum)[2]+BroR*coef(RSFCoyoteSum)[3]+ConR*coef(RSFCoyoteSum)[4]+LicR*coef(RSFCoyoteSum)[5]+
              MixR*coef(RSFCoyoteSum)[6]+RocR*coef(RSFCoyoteSum)[7]+ScrR*coef(RSFCoyoteSum)[8]+WaDR*coef(RSFCoyoteSum)[9]+
              LinR*coef(RSFCoyoteSum)[10]+RugR*coef(RSFCoyoteSum)[11])

WinRSF<-exp(coef(RSFCoyoteWin)[1]+AntR*coef(RSFCoyoteWin)[2]+BroR*coef(RSFCoyoteWin)[3]+ConR*coef(RSFCoyoteWin)[4]+LicR*coef(RSFCoyoteWin)[5]+
              MixR*coef(RSFCoyoteWin)[6]+RocR*coef(RSFCoyoteWin)[7]+ScrR*coef(RSFCoyoteWin)[8]+WaDR*coef(RSFCoyoteWin)[9]+
              LinR*coef(RSFCoyoteWin)[10]+RugR*coef(RSFCoyoteWin)[11])

cellStats(SumRSF,max)

SumRSFsc<-(SumRSF-(cellStats(SumRSF,min)))/(cellStats(SumRSF,max)-cellStats(SumRSF,min))
WinRSFsc<-(WinRSF-(cellStats(WinRSF,min)))/(cellStats(WinRSF,max)-cellStats(WinRSF,min))

plot(SumRSFsc)
plot(WinRSFsc)

writeRaster(SumRSFsc,"output/PredRSFNL/CoyoteSummer.tif",overwrite=T)
writeRaster(WinRSFsc,"output/PredRSFNL/CoyoteWinter.tif",overwrite=T)

saveRDS(RSFCoyoteSum, "output/PredRSFNL/CoyoteSummerRSF.RDS")
saveRDS(RSFCoyoteWin, "output/PredRSFNL/CoyoteWinterRSF.RDS")




