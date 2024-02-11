library(dplyr)
library(raster)
library(sp)
library(tibble)

### stacking rasters together by study area for faster processing and extracting

rastlist <- list.files(path = "OUTPUT_NEW/DOMAINS", 
                       pattern =".tif$", full.names=TRUE)

allrasters.NL <- lapply(rastlist[c(1:15)], raster)
allrasters.RMNP <- lapply(rastlist[c(16:21)], raster)

load("RSF_RASTERS.Rdata")

rsf.RMNP<-list(bg,cf,mh,mf,od,lf1,w1,ru1)
rsf.NL<-list(f,l,r,s,lf2,w2,ru2)

#### RMNP
crs1<-CRS("+init=epsg:32614") 

#### NL
crs2<-CRS("+init=epsg:32621") 

rm(list=setdiff(ls(), c("allrasters.NL","allrasters.RMNP",
                        "rsf.RMNP","rsf.NL","crs1","crs2")))

### scaling by 'q' quantile to remove extreme outliers

Q.allrasters.NL<-list()
Q.allrasters.RMNP<-list()
q <- 0.999

for(i in 1:length(allrasters.RMNP)){

 test1<-allrasters.RMNP[[i]]
 test2<-quantile(test1, q)
 test1[test1 > test2] <- test2

 test3<-
   (test1- (cellStats(test1, min))) / (quantile(test1, q) - (cellStats(test1, min)))

 Q.allrasters.RMNP[[i]]<-test3

}

for(i in 1:length(allrasters.NL)){

  test1<-allrasters.NL[[i]]
  test2<-quantile(test1, q)
  test1[test1 > test2] <- test2

  test3<-
    (test1- (cellStats(test1, min))) / (quantile(test1, q) - (cellStats(test1, min)))

  Q.allrasters.NL[[i]]<-test3

}

### caribou and elk files

caribou <- readRDS("output/1-data-prep/caribou.Rds")
elk <- readRDS("output/1-data-prep/elk.Rds")



### sps of elk and caribou for extracting domains
elk_sp<-SpatialPoints(as.data.frame(elk)[,c(8,9)]) # Specify x_end and y_end columns
proj4string(elk_sp)<- crs1  

caribou_sp<-SpatialPoints(as.data.frame(caribou)[,c(8,9)]) # Specify x_end and y_end columns
proj4string(caribou_sp)<- crs2  


caribou_domain<-caribou
elk_domain<-elk

### elk domain extraction

for(i in 1:length(allrasters.RMNP)){
  
  test1<-allrasters.RMNP[[i]]
  
  elk_domain <- add_column(elk_domain, Emptycol = NA)
  elk_domain[,(ncol(elk_domain))]<-extract(test1, elk_sp)
  colnames(elk_domain)[(ncol(elk_domain))]<-names(allrasters.RMNP[[i]])
  
}
  
for(i in 1:length(Q.allrasters.RMNP)){
  
  test1<-Q.allrasters.RMNP[[i]]
  
  elk_domain <- add_column(elk_domain, Emptycol = NA)
  elk_domain[,(ncol(elk_domain))]<-extract(test1, elk_sp)
  colnames(elk_domain)[(ncol(elk_domain))]<-paste("q_",names(Q.allrasters.RMNP[[i]]), sep="")
  
}

for(i in 1:length(rsf.RMNP)){
  
  test1<-rsf.RMNP[[i]]
  
  elk_domain <- add_column(elk_domain, Emptycol = NA)
  elk_domain[,(ncol(elk_domain))]<-extract(test1, elk_sp)
  colnames(elk_domain)[(ncol(elk_domain))]<-names(rsf.RMNP[[i]])
  
}



### caribou domain extraction

for(i in 1:length(allrasters.NL)){
  
  test1<-allrasters.NL[[i]]
  
  caribou_domain <- add_column(caribou_domain, Emptycol = NA)
  caribou_domain[,(ncol(caribou_domain))]<-extract(test1, caribou_sp)
  colnames(caribou_domain)[(ncol(caribou_domain))]<-names(allrasters.NL[[i]])
  
}

for(i in 1:length(Q.allrasters.NL)){
  
  test1<-Q.allrasters.NL[[i]]
  
  caribou_domain <- add_column(caribou_domain, Emptycol = NA)
  caribou_domain[,(ncol(caribou_domain))]<-extract(test1, caribou_sp)
  colnames(caribou_domain)[(ncol(caribou_domain))]<-paste("q_",names(Q.allrasters.NL[[i]]), sep="")
  
}


for(i in 1:length(rsf.NL)){
  
  test1<-rsf.NL[[i]]
  
  caribou_domain <- add_column(caribou_domain, Emptycol = NA)
  caribou_domain[,(ncol(caribou_domain))]<-extract(test1, caribou_sp)
  colnames(caribou_domain)[(ncol(caribou_domain))]<-names(rsf.NL[[i]])
  
}



caribouExtract<-caribou_domain
elkExtract<-elk_domain

dir.create("OUTPUT_NEW/EXTRACT_DOMAINS")

save.image("OUTPUT_NEW/EXTRACT_DOMAINS/ready_for_social.Rdata")

rm(list=setdiff(ls(), c("caribouExtract", "elkExtract")))

save.image("OUTPUT_NEW/EXTRACT_DOMAINS/for_step4_ewc.Rdata")


