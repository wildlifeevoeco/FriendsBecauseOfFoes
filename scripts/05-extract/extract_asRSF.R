### extracting lc

load("ready_for_extract2.Rdata")

library(raster)
library(sp)

### RMNP
#### Extracting raster values for ISSF
crs<-CRS("+init=epsg:32614") 

GIS_source<-"C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/input/RMNP/"

### RMNP rasters

ag<-raster(paste(GIS_source,"Agriculture100m.tif",sep=""))
projection(ag)<-crs

bg<-raster(paste(GIS_source,"Bog100m.tif",sep=""))
projection(bg)<-crs

cf<-raster(paste(GIS_source,"Coniferous100m.tif",sep=""))
projection(cf)<-crs

df<-raster(paste(GIS_source,"Deciduous100m.tif",sep=""))
projection(df)<-crs

gl<-raster(paste(GIS_source,"Grassland100m.tif",sep=""))
projection(gl)<-crs

mh<-raster(paste(GIS_source,"Marsh100m.tif",sep=""))
projection(mh)<-crs

mf<-raster(paste(GIS_source,"Mixedwood100M.tif",sep=""))
projection(mf)<-crs

od<-raster(paste(GIS_source,"Opendeciduous100m.tif",sep=""))
projection(od)<-crs

lf1<-raster(paste(GIS_source,"LinFeat_Dist.tif",sep=""))
projection(lf1)<-crs

w1<-raster(paste(GIS_source,"Water_Dist.tif",sep=""))
projection(w1)<-crs

ru1<-raster(paste(GIS_source,"Ruggedness.tif",sep=""))
projection(ru1)<-crs

### extract elk and wolf
elk2h_sp<-SpatialPoints(elk2h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(elk2h_sp)<- crs  

wolf1h_sp<-SpatialPoints(wolf1h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(wolf1h_sp)<- crs  

elk2h_full$ag<-extract(ag, elk2h_sp)
elk2h_full$bg<-extract(bg, elk2h_sp)
elk2h_full$cf<-extract(cf, elk2h_sp)
elk2h_full$df<-extract(df, elk2h_sp)
elk2h_full$gl<-extract(gl, elk2h_sp)
elk2h_full$mh<-extract(mh, elk2h_sp)
elk2h_full$mf<-extract(mf, elk2h_sp)
elk2h_full$od<-extract(od, elk2h_sp)
elk2h_full$lf<-extract(lf1, elk2h_sp)
elk2h_full$wt<-extract(w1, elk2h_sp)
elk2h_full$ru<-extract(ru1, elk2h_sp)

wolf1h_full$ag<-extract(ag, wolf1h_sp)
wolf1h_full$bg<-extract(bg, wolf1h_sp)
wolf1h_full$cf<-extract(cf, wolf1h_sp)
wolf1h_full$df<-extract(df, wolf1h_sp)
wolf1h_full$gl<-extract(gl, wolf1h_sp)
wolf1h_full$mh<-extract(mh, wolf1h_sp)
wolf1h_full$mf<-extract(mf, wolf1h_sp)
wolf1h_full$od<-extract(od, wolf1h_sp)
wolf1h_full$lf<-extract(lf1, wolf1h_sp)
wolf1h_full$wt<-extract(w1, wolf1h_sp)
wolf1h_full$ru<-extract(ru1, wolf1h_sp)

elk2h_rsf<-subset(elk2h_full, !is.na(elk2h_full$ag))
wolf1h_rsf<-subset(wolf1h_full, !is.na(wolf1h_full$ag))

### NL

#### Extracting raster values for ISSF
crs<-CRS("+init=epsg:32620") #### same mistake again using wrong zone, should be zone21 
crs2<-CRS("+init=epsg:32621") ### this is the right zone

GIS_source<-"C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/input/NL/"

### NL rasters

f<-raster(paste(GIS_source,"prepForest.tif",sep=""))
# f1<-f
projection(f)<-crs  ### this is not reprojecting this is assigning a projection, no transformation is occurring, projectRaster should have been function
# f2<-projectRaster(f1, crs=crs)  ### reproject definitively changes raster position
# 
# compareCRS(f1, f)
# compareCRS(f1, f2)
# compareCRS(f, f2)

l<-raster(paste(GIS_source,"prepLichen.tif",sep=""))
projection(l)<-crs

r<-raster(paste(GIS_source,"prepRocky.tif",sep=""))
projection(r)<-crs

s<-raster(paste(GIS_source,"prepScrub.tif",sep=""))
projection(s)<-crs

lf2<-raster(paste(GIS_source,"prepLinearDist.tif",sep=""))
projection(lf2)<-crs

w2<-raster(paste(GIS_source,"prepWaterDist.tif",sep=""))
projection(w2)<-crs

ru2<-raster(paste(GIS_source,"prepRuggedness.tif",sep=""))
projection(ru2)<-crs


### extract bear, caribou, coyote
caribou2h_sp<-SpatialPoints(caribou2h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(caribou2h_sp)<- crs  

bear2h_sp<-SpatialPoints(bear2h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(bear2h_sp)<- crs  


# 
# test<-head(bear2h_sp)
# test$f<-extract(f, test)
# test$f1<-extract(f1, test)
# 
# bear2h_full$f<-extract(f, bear2h_sp)
# bear2h_full$f1<-extract(f1, bear2h_sp)
# 
# bear2h_full$match<-ifelse(bear2h_full$f==bear2h_full$f1,0,1)
# 
# summary(bear2h_full$match)

# bear2h_full$f2<-extract(f2, bear2h_sp) ### reproject definitively changes raster position
# 
# bear2h_full$match2<-ifelse(bear2h_full$f==bear2h_full$f2,0,1) ### reproject definitively changes raster position
# 
# summary(bear2h_full$match2)


#### values match if extracted from f raster if it is crs or crs2, so again this mistake appears to not be hurting anything, just bad practice


bear4h_sp<-SpatialPoints(bear4h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(bear4h_sp)<- crs  

coyote4h_sp<-SpatialPoints(coyote4h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote4h_sp)<- crs  

coyote8h_sp<-SpatialPoints(coyote8h_full[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote8h_sp)<- crs  

caribou2h_full$f<-extract(f, caribou2h_sp)
caribou2h_full$l<-extract(l, caribou2h_sp)
caribou2h_full$r<-extract(r, caribou2h_sp)
caribou2h_full$s<-extract(s, caribou2h_sp)
caribou2h_full$lf<-extract(lf2, caribou2h_sp)
caribou2h_full$w<-extract(w2, caribou2h_sp)
caribou2h_full$ru<-extract(ru2, caribou2h_sp)

bear2h_full$f<-extract(f, bear2h_sp)
bear2h_full$l<-extract(l, bear2h_sp)
bear2h_full$r<-extract(r, bear2h_sp)
bear2h_full$s<-extract(s, bear2h_sp)
bear2h_full$lf<-extract(lf2, bear2h_sp)
bear2h_full$w<-extract(w2, bear2h_sp)
bear2h_full$ru<-extract(ru2, bear2h_sp)

bear4h_full$f<-extract(f, bear4h_sp)
bear4h_full$l<-extract(l, bear4h_sp)
bear4h_full$r<-extract(r, bear4h_sp)
bear4h_full$s<-extract(s, bear4h_sp)
bear4h_full$lf<-extract(lf2, bear4h_sp)
bear4h_full$w<-extract(w2, bear4h_sp)
bear4h_full$ru<-extract(ru2, bear4h_sp)

#### removing locations beyond bounds

caribou2h_rsf<-subset(caribou2h_full, !is.na(caribou2h_full$f))
bear2h_rsf<-subset(bear2h_full, !is.na(bear2h_full$f))
bear4h_rsf<-subset(bear4h_full, !is.na(bear4h_full$f))



coyote4h_full$f<-extract(f, coyote4h_sp)
coyote4h_rsf<-subset(coyote4h_full, !is.na(coyote4h_full$f))
coyote4h_sp<-SpatialPoints(coyote4h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote4h_sp)<- crs  
coyote4h_rsf$lf<-extract(lf2, coyote4h_sp)
coyote4h_rsf<-subset(coyote4h_rsf, !is.na(coyote4h_rsf$lf))
coyote4h_sp<-SpatialPoints(coyote4h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote4h_sp)<- crs  
coyote4h_rsf$ru<-extract(ru2, coyote4h_sp)
coyote4h_rsf<-subset(coyote4h_rsf, !is.na(coyote4h_rsf$ru))
coyote4h_sp<-SpatialPoints(coyote4h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote4h_sp)<- crs  

save.image("WIP_EXTRACT1_200.Rdata")

coyote4h_rsf$l<-extract(l, coyote4h_sp)
coyote4h_rsf$r<-extract(r, coyote4h_sp)
coyote4h_rsf$s<-extract(s, coyote4h_sp)
coyote4h_rsf$w<-extract(w2, coyote4h_sp)

save.image("WIP_EXTRACT2_200.Rdata")

coyote8h_full$f<-extract(f, coyote8h_sp)
coyote8h_rsf<-subset(coyote8h_full, !is.na(coyote8h_full$f))
coyote8h_sp<-SpatialPoints(coyote8h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote8h_sp)<- crs  
coyote8h_rsf$lf<-extract(lf2, coyote8h_sp)
coyote8h_rsf<-subset(coyote8h_rsf, !is.na(coyote8h_rsf$lf))
coyote8h_sp<-SpatialPoints(coyote8h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote8h_sp)<- crs  
coyote8h_rsf$ru<-extract(ru2, coyote8h_sp)
coyote8h_rsf<-subset(coyote8h_rsf, !is.na(coyote8h_rsf$ru))
coyote8h_sp<-SpatialPoints(coyote8h_rsf[,c(1,2)]) # Specify x_end and y_end columns
proj4string(coyote8h_sp)<- crs  

save.image("WIP_EXTRACT3_200.Rdata")

coyote8h_rsf$l<-extract(l, coyote8h_sp)
coyote8h_rsf$r<-extract(r, coyote8h_sp)
coyote8h_rsf$s<-extract(s, coyote8h_sp)
coyote8h_rsf$w<-extract(w2, coyote8h_sp)

save.image("WIP_EXTRACT4_200.Rdata")

rm(list= ls()[!(ls() %in% c('bear2h_rsf','bear4h_rsf','caribou2h_rsf',
                            'coyote4h_rsf','coyote8h_rsf','elk2h_rsf','wolf1h_rsf'))])



save.image("READY_FOR_MODELS2.Rdata")
