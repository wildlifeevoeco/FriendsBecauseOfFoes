
library(raster)
library(sp)
library(adehabitatHR)
library(data.table)
library(dplyr)

load("RSF_RASTERS.Rdata")  ### just rasters.R

b<-brick(c(bg,cf,mh,mf,od,lf1,w1,ru1))
b2<-brick(c(f,l,r,s,lf2,w2,ru2))

#### RMNP
crs1<-CRS("+init=epsg:32614") 

#### NL
crs2<-CRS("+init=epsg:32621") ### this is the correct crs earlier versions of this script had an incorrect crs that does not appear to have impacted results

projection(b2)<-crs2  ### fixing previous mistakes in code scripts

rm(ag,bg,cf,gl,mh,mf,od,lf1,w1,ru1,f,l,r,s,lf2,w2,ru2)

load("OUTPUT_NEW/rsf_results/log_RSF_MODELS_NEW_LOG.Rdata")

rm(list=setdiff(ls(), c("b","b2","bear.spr.st2","caribou.spr.st1","caribou.win.st1","coyote4.spr.st2","coyote.win.st2","elk.win.st1","res.RMNP",
                        "wolf.win.st2","res.NL","crs1","crs2","min.log_RMNP","min.rlog_RMNP","min.log_NL","min.rlog_NL")))


caribou <- readRDS("output/1-data-prep/caribou.Rds")
elk <- readRDS("output/1-data-prep/elk.Rds")


### sps of elk and caribou for extracting domains
elk_sp<-SpatialPoints(elk[,c(8,9)]) # Specify x_end and y_end columns
proj4string(elk_sp)<- crs1  

outCrop1 <- mcp(elk_sp, percent = 100)

caribou_sp<-SpatialPoints(caribou[,c(8,9)]) # Specify x_end and y_end columns
proj4string(caribou_sp)<- crs2  

outCrop2 <- mcp(caribou_sp, percent = 100)

### cropping raster bricks to study areas
b1_1<-crop(b, outCrop1)
b2_1<-crop(b2, outCrop2)

rm(b,b2)

plot(b1_1, nc = 3)
plot(b2_1, nc = 3)

### converting raster bricks to cell values for predicting domains
b1.values <- values(b1_1)
b2.values <- values(b2_1)

b1.df<-b1.values %>% data.frame
b2.df<-b2.values %>% data.frame

###### RMNP

colnames(b1.df)<-c("bg","cf","mh","mf","od","lf","wt","ru")

system.time(elk_w1.pred <- predict(elk.win.st1, newdata = b1.df, allow.new.levels=TRUE, type="response"))

RSF.elk_w1.layer <- b1_1[[1]] %>% setValues(elk_w1.pred)
# plot(elk_w1.layer)
# RSF.elk_w1.layer <- exp(elk_w1.layer)/max(values(exp(elk_w1.layer)), na.rm = TRUE)
plot(RSF.elk_w1.layer)

library(terra)

nx <- minmax(as(RSF.elk_w1.layer, "SpatRaster"))    
RSF.elk_w1.layer_n <- (RSF.elk_w1.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.elk_w1.layer)
plot(RSF.elk_w1.layer_n)

# (breaks <- quantile(elk_w1.pred, seq(0,1,1/10), na.rm = TRUE))
# elk_w1.RSF.discrete <- cut(elk_w1.pred, breaks = breaks, labels = 1:10)
# elk_w1.class <- RSF.elk_w1.layer %>% setValues(elk_w1.RSF.discrete)
# plot(elk_w1.class)

system.time(wolf_w2.pred <- predict(wolf.win.st2, newdata = b1.df, allow.new.levels=TRUE, type="response"))

RSF.wolf_w2.layer <- b1_1[[1]] %>% setValues(wolf_w2.pred)
# plot(wolf_w2.layer)
# RSF.wolf_w2.layer <- exp(wolf_w2.layer)/max(values(exp(wolf_w2.layer)), na.rm = TRUE)
plot(RSF.wolf_w2.layer)



nx <- minmax(as(RSF.wolf_w2.layer, "SpatRaster"))

RSF.wolf_w2.layer_n <- (RSF.wolf_w2.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.wolf_w2.layer)
plot(RSF.wolf_w2.layer_n)


RSF_elk.values<-values(RSF.elk_w1.layer)
RSF_wolf.values<-values(RSF.wolf_w2.layer)

RSF_elk_wolf.values<-RSF_elk.values*RSF_wolf.values

elk_wolf.layer <- b1_1[[1]] %>% setValues(RSF_elk_wolf.values)
plot(elk_wolf.layer)

# (breaks <- quantile(RSF_elk_wolf.values, seq(0,1,1/10), na.rm = TRUE))
# elk_wolf.RSF.discrete <- cut(RSF_elk_wolf.values, breaks = breaks, labels = 1:10)
# elk_wolf.class <- elk_wolf.layer %>% setValues(elk_wolf.RSF.discrete)
# plot(elk_wolf.class)

###### NL

colnames(b2.df)<-c("f","l","r","s","lf","w","ru")

system.time(caribou_w1.pred <- predict(caribou.win.st1, newdata = b2.df, allow.new.levels=TRUE, type="response"))

RSF.caribou_w1.layer <- b2_1[[1]] %>% setValues(caribou_w1.pred)
# plot(caribou_w1.layer)
# RSF.caribou_w1.layer <- exp(caribou_w1.layer)/max(values(exp(caribou_w1.layer)), na.rm = TRUE)
plot(RSF.caribou_w1.layer)

nx <- minmax(as(RSF.caribou_w1.layer, "SpatRaster"))

RSF.caribou_w1.layer_n <- (RSF.caribou_w1.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.caribou_w1.layer)
plot(RSF.caribou_w1.layer_n)


# (breaks <- quantile(caribou_w1.pred, seq(0,1,1/10), na.rm = TRUE))
# caribou_w1.RSF.discrete <- cut(caribou_w1.pred, breaks = breaks, labels = 1:10)
# caribou_w1.class <- RSF.caribou_w1.layer %>% setValues(caribou_w1.RSF.discrete)
# plot(caribou_w1.class)

system.time(caribou_s1.pred <- predict(caribou.spr.st1, newdata = b2.df, allow.new.levels=TRUE, type="response"))

RSF.caribou_s1.layer <- b2_1[[1]] %>% setValues(caribou_s1.pred)
# plot(caribou_s1.layer)
# RSF.caribou_s1.layer <- exp(caribou_s1.layer)/max(values(exp(caribou_s1.layer)), na.rm = TRUE)
plot(RSF.caribou_s1.layer)

nx <- minmax(as(RSF.caribou_s1.layer, "SpatRaster"))

RSF.caribou_s1.layer_n <- (RSF.caribou_s1.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.caribou_s1.layer)
plot(RSF.caribou_s1.layer_n)


system.time(coyote_s2.pred <- predict(coyote4.spr.st2, newdata = b2.df, allow.new.levels=TRUE, type="response"))

RSF.coyote_s2.layer <- b2_1[[1]] %>% setValues(coyote_s2.pred)
# plot(coyote_s2.layer)
# RSF.coyote_s2.layer <- exp(coyote_s2.layer)/max(values(exp(coyote_s2.layer)), na.rm = TRUE)
plot(RSF.coyote_s2.layer)

nx <- minmax(as(RSF.coyote_s2.layer, "SpatRaster"))

RSF.coyote_s2.layer_n <- (RSF.coyote_s2.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.coyote_s2.layer)
plot(RSF.coyote_s2.layer_n)


system.time(coyote_w2.pred <- predict(coyote.win.st2, newdata = b2.df, allow.new.levels=TRUE, type="response"))

RSF.coyote_w2.layer <- b2_1[[1]] %>% setValues(coyote_w2.pred)
# plot(coyote_w2.layer)
# RSF.coyote_w2.layer <- exp(coyote_w2.layer)/max(values(exp(coyote_w2.layer)), na.rm = TRUE)
plot(RSF.coyote_w2.layer)


nx <- minmax(as(RSF.coyote_w2.layer, "SpatRaster"))

RSF.coyote_w2.layer_n <- (RSF.coyote_w2.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.coyote_w2.layer)
plot(RSF.coyote_w2.layer_n)





# (breaks <- quantile(coyote_w2.pred, seq(0,1,1/10), na.rm = TRUE))
# coyote_w2.RSF.discrete <- cut(coyote_w2.pred, breaks = breaks, labels = 1:10)
# coyote_w2.class <- RSF.coyote_w2.layer %>% setValues(coyote_w2.RSF.discrete)
# plot(coyote_w2.class)

system.time(bear_s2.pred <- predict(bear.spr.st2, newdata = b2.df, allow.new.levels=TRUE, type="response"))

RSF.bear_s2.layer <- b2_1[[1]] %>% setValues(bear_s2.pred)
# plot(bear_s2.layer)
# RSF.bear_s2.layer <- exp(bear_s2.layer)/max(values(exp(bear_s2.layer)), na.rm = TRUE)
plot(RSF.bear_s2.layer)

nx <- minmax(as(RSF.bear_s2.layer, "SpatRaster"))

RSF.bear_s2.layer_n <- (RSF.bear_s2.layer - nx[1,]) / (nx[2,] - nx[1,])

plot(RSF.bear_s2.layer)
plot(RSF.bear_s2.layer_n)

nx2 <- minmax(as(RSF.bear_s2.layer_n, "SpatRaster"))



# 
# RSF_caribou.values_w<-values(RSF.caribou_w1.layer)
# RSF_coyote.values_w<-values(RSF.coyote_w2.layer)
# 
# RSF_caribou_coyote.values_w<-RSF_caribou.values_w*RSF_coyote.values_w
# 
# caribou_coyote.layer_w <- b2_1[[1]] %>% setValues(RSF_caribou_coyote.values_w)
# plot(caribou_coyote.layer_w)

# (breaks <- quantile(RSF_caribou_coyote.values_w, seq(0,1,1/10), na.rm = TRUE))
# caribou_coyote.RSF.discrete_w <- cut(RSF_caribou_coyote.values_w, breaks = breaks, labels = 1:10)
# caribou_coyote.class_w <- caribou_coyote.layer_w %>% setValues(caribou_coyote.RSF.discrete_w)
# plot(caribou_coyote.class_w)

# 
# RSF_caribou.values_s<-values(RSF.caribou_s1.layer)
# RSF_coyote.values_s<-values(RSF.coyote_s2.layer)
# 
# RSF_caribou_coyote.values_s<-RSF_caribou.values_s*RSF_coyote.values_s
# 
# caribou_coyote.layer_s <- b2_1[[1]] %>% setValues(RSF_caribou_coyote.values_s)
# plot(caribou_coyote.layer_s)

# (breaks <- quantile(RSF_caribou_coyote.values_s, seq(0,1,1/10), na.rm = TRUE))
# caribou_coyote.RSF.discrete_s <- cut(RSF_caribou_coyote.values_s, breaks = breaks, labels = 1:10)
# caribou_coyote.class_s <- caribou_coyote.layer_s %>% setValues(caribou_coyote.RSF.discrete_s)
# plot(caribou_coyote.class_s)

# 
# 
# RSF_bear.values_s<-values(RSF.bear_s2.layer)
# 
# RSF_caribou_bear.values_s<-RSF_caribou.values_s*RSF_bear.values_s
# 
# caribou_bear.layer_s <- b2_1[[1]] %>% setValues(RSF_caribou_bear.values_s)
# plot(caribou_bear.layer_s)
# 
# # (breaks <- quantile(RSF_caribou_bear.values_s, seq(0,1,1/10), na.rm = TRUE))
# caribou_bear.RSF.discrete_s <- cut(RSF_caribou_bear.values_s, breaks = breaks, labels = 1:10)
# caribou_bear.class_s <- caribou_bear.layer_s %>% setValues(caribou_bear.RSF.discrete_s)
# plot(caribou_bear.class_s)








dir.create("OUTPUT_NEW/DOMAINS")

# writeRaster(elk_w1.layer, "OUTPUT_NEW/DOMAINS/elk_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.elk_w1.layer, "OUTPUT_NEW/DOMAINS/elk_rsf", format = 'GTiff',overwrite = T)
# writeRaster(elk_w1.class, "OUTPUT_NEW/DOMAINS/elk_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(wolf_w2.layer, "OUTPUT_NEW/DOMAINS/wolf_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.wolf_w2.layer, "OUTPUT_NEW/DOMAINS/wolf_rsf", format = 'GTiff',overwrite = T)
# writeRaster(wolf_w2.class, "OUTPUT_NEW/DOMAINS/wolf_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(caribou_w1.layer, "OUTPUT_NEW/DOMAINS/caribouw_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.caribou_w1.layer, "OUTPUT_NEW/DOMAINS/caribouw_rsf", format = 'GTiff',overwrite = T)
# writeRaster(caribou_w1.class, "OUTPUT_NEW/DOMAINS/caribouw_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(caribou_s1.layer, "OUTPUT_NEW/DOMAINS/caribous_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.caribou_s1.layer, "OUTPUT_NEW/DOMAINS/caribous_rsf", format = 'GTiff',overwrite = T)
# writeRaster(caribou_s1.class, "OUTPUT_NEW/DOMAINS/caribous_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(coyote_w2.layer, "OUTPUT_NEW/DOMAINS/coyotew_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.coyote_w2.layer, "OUTPUT_NEW/DOMAINS/coyotew_rsf", format = 'GTiff',overwrite = T)
# writeRaster(coyote_w2.class, "OUTPUT_NEW/DOMAINS/coyotew_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(coyote_s2.layer, "OUTPUT_NEW/DOMAINS/coyotes_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.coyote_s2.layer, "OUTPUT_NEW/DOMAINS/coyotes_rsf", format = 'GTiff',overwrite = T)
# writeRaster(coyote_s2.class, "OUTPUT_NEW/DOMAINS/coyotes_class", format = 'GTiff',overwrite = T)
# 
# writeRaster(bear_s2.layer, "OUTPUT_NEW/DOMAINS/bear_raw", format = 'GTiff',overwrite = T)
# writeRaster(RSF.bear_s2.layer, "OUTPUT_NEW/DOMAINS/bear_rsf", format = 'GTiff',overwrite = T)
# writeRaster(bear_s2.class, "OUTPUT_NEW/DOMAINS/bear_class", format = 'GTiff',overwrite = T)

# 
# writeRaster(RSF.elk_w1.layer, "output/DOMAINS/elk_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.wolf_w2.layer, "output/DOMAINS/wolf_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.caribou_w1.layer, "output/DOMAINS/caribouw_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.caribou_s1.layer, "output/DOMAINS/caribous_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.coyote_w2.layer, "output/DOMAINS/coyotew_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.coyote_s2.layer, "output/DOMAINS/coyotes_rsf", format = 'GTiff',overwrite = T)
# writeRaster(RSF.bear_s2.layer, "output/DOMAINS/bear_rsf", format = 'GTiff',overwrite = T)


writeRaster(RSF.elk_w1.layer, "output/DOMAINS/elk_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.wolf_w2.layer, "output/DOMAINS/wolf_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.caribou_w1.layer, "output/DOMAINS/caribouw_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.caribou_s1.layer, "output/DOMAINS/caribous_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.coyote_w2.layer, "output/DOMAINS/coyotew_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.coyote_s2.layer, "output/DOMAINS/coyotes_rsf_LOG", format = 'GTiff',overwrite = T)
writeRaster(RSF.bear_s2.layer, "output/DOMAINS/bear_rsf_LOG", format = 'GTiff',overwrite = T)


writeRaster(RSF.elk_w1.layer_n, "output/DOMAINS/elk_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.wolf_w2.layer_n, "output/DOMAINS/wolf_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.caribou_w1.layer_n, "output/DOMAINS/caribouw_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.caribou_s1.layer_n, "output/DOMAINS/caribous_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.coyote_w2.layer_n, "output/DOMAINS/coyotew_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.coyote_s2.layer_n, "output/DOMAINS/coyotes_rsf_LOG_n", format = 'GTiff',overwrite = T)
writeRaster(RSF.bear_s2.layer_n, "output/DOMAINS/bear_rsf_LOG_n", format = 'GTiff',overwrite = T)



save.image("DOMAINS_NEW2.Rdata")
