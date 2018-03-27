################ Coyote RSFs ######################

library(adehabitatHR)
library(rgeos)
library(spatstat)
library(polyCub)
library(raster)

coyote<-readRDS('output/data-prep/coyote.Rds')

plot(coyote$EASTING,coyote$NORTHING)

head(coyote)

nlBounds <- rgdal::readOGR('input/etc/NL-Bounds/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm)

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

plot(nlBounds)
points(coyMRres$EASTING,coyMRres$NORTHING)


CoyAvail<-mcp(points, percent = 99.9)

mean(coyMRres$stepLength)

plot(nlBounds)
plot(CoyAvail,add=T)

### Buffer by mean step length
CoyAvailBuf<-gBuffer(CoyAvail,width=mean(coyMRres$stepLength))

clipped<-gIntersection(nlBounds,CoyAvailBuf)

plot(clipped)

plot(nlBounds)
plot(clipped,add=T)

clipped2<-as.owin.SpatialPolygons(clipped)
rand<-runifpoint(n=nrow(coyMRres)*10,win=clipped2)

plot(rand)

#### Load in habitat layers

Ant<-raster('input/Landcover/Anthro100.tif')
Bro<-raster('input/Landcover/Broadleaf100.tif')
Con<-raster('input/Landcover/Conifer100.tif')
Lic<-raster('input/Landcover/Lichen100.tif')
Mix<-raster('input/Landcover/MixedWood100.tif')
Roc<-raster('input/Landcover/Rocky100.tif')
Scr<-raster('input/Landcover/Scrub100.tif')
Wat<-raster('input/Landcover/Water100.tif')
Wet<-raster('input/Landcover/Wetland100.tif')

AntUsed<-extract(Ant,points)
BroUsed<-extract(Bro,points)
ConUsed<-extract(Con,points)
LicUsed<-extract(Lic,points)
MixUsed<-extract(Mix,points)
RocUsed<-extract(Roc,points)
ScrUsed<-extract(Scr,points)
WatUsed<-extract(Wat,points)
WetUsed<-extract(Wet,points)

randCo<-coords(rand)

xyr<-cbind(randCo$x,randCo$y)

randPoints<-SpatialPoints(data.frame(xyr[,1],xyr[,2]))


AntAvail<-extract(Ant,randPoints)
BroAvail<-extract(Bro,randPoints)
ConAvail<-extract(Con,randPoints)
LicAvail<-extract(Lic,randPoints)
MixAvail<-extract(Mix,randPoints)
RocAvail<-extract(Roc,randPoints)
ScrAvail<-extract(Scr,randPoints)
WatAvail<-extract(Wat,randPoints)
WetAvail<-extract(Wet,randPoints)






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


