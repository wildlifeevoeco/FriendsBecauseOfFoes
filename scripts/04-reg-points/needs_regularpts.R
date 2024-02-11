### generate available from 100 MCP around hmmdata
library(dplyr)
library(adehabitatHR)
library(sp)
library(raster)
library(data.table)

load("readyfor_ssf.Rdata")

crs_RMNP<-CRS("+init=epsg:32614") 
crs_NL<-CRS("+init=epsg:32620") 
crs_NL2<-CRS("+init=epsg:32621") 

# UTM zone 21N
utmNL <- '+proj=utm +zone=21 ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# UTM zone 14N
utmMB <- '+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

compareCRS(crs_RMNP, utmMB)
compareCRS(crs_NL2, utmNL)

### proceeding with random points drawn from seasonal hmmdata

bear2.1a$TIME_ID<-paste(bear2.1a$ID, bear2.1a$ROUND_TIME, sep="_")
bear2a$TIME_ID<-paste(bear2a$ID, bear2a$ROUND_TIME, sep="_")
caribou2a$TIME_ID<-paste(caribou2a$ID, caribou2a$ROUND_TIME, sep="_")
coyote2.1a$TIME_ID<-paste(coyote2.1a$ID, coyote2.1a$ROUND_TIME, sep="_")
coyote2a$TIME_ID<-paste(coyote2a$ID, coyote2a$ROUND_TIME, sep="_")
elk2a$TIME_ID<-paste(elk2a$ID, elk2a$ROUND_TIME, sep="_")
wolf2a$TIME_ID<-paste(wolf2a$ID, wolf2a$ROUND_TIME, sep="_")

bearhmm_2hr$ANIM_ID<-sub("\\..*", "", bearhmm_2hr$ID)
bearhmm_4hr$ANIM_ID<-sub("\\..*", "", bearhmm_4hr$ID)
caribouhmm_2hr$ANIM_ID<-sub("\\..*", "", caribouhmm_2hr$ID)
coyotehmm_4hr$ANIM_ID<-sub("\\..*", "", coyotehmm_4hr$ID)
coyotehmm_8hr$ANIM_ID<-sub("\\..*", "", coyotehmm_8hr$ID)
elkhmm_2hr$ANIM_ID<-sub("\\..*", "", elkhmm_2hr$ID)
wolfhmm_1hr$ANIM_ID<-sub("\\..*", "", wolfhmm_1hr$ID)

bear2h<-full_join(bear2a[,c(2:11,13:20,23:34,37:38)], bearhmm_2hr[,c(6,17:21)], by="UID")
bear4h<-full_join(bear2.1a[,c(2:11,13:20,23:34,37:38)], bearhmm_4hr[,c(6,17:21)], by="UID")
caribou2h<-full_join(caribou2a[,c(2:11,13:20,23:34,37:38)], caribouhmm_2hr[,c(6,17:21)], by="UID")
coyote4h<-full_join(coyote2.1a[,c(2:11,13:20,23:34,37:38)], coyotehmm_4hr[,c(6,17:21)], by="UID")
coyote8h<-full_join(coyote2a[,c(2:11,13:20,23:34,37:38)], coyotehmm_8hr[,c(6,17:21)], by="UID")
elk2h<-full_join(elk2a[,c(2:11,13:20,23:32,35:36)], elkhmm_2hr[,c(6,17:21)], by="UID")
wolf1h<-full_join(wolf2a[,c(2:11,13:20,23:33,36:37)], wolfhmm_1hr[,c(6,17:21)], by="UID")

#### these should have been crs_NL2, but it might not matter, mcp generated from caribou_w from both crs_NL and crs_NL2 perfectly match
#### caribou 2h w points when no crs is assumed (via plot and points)

bear2h_mcp_s<-mcp(SpatialPoints(subset(bear2h, bear2h$season=="spring")[,1:2], proj4string=crs_NL), percent=100)
bear4h_mcp_s<-mcp(SpatialPoints(subset(bear4h, bear4h$season=="spring")[,1:2], proj4string=crs_NL), percent=100)
caribou2h_mcp_s<-mcp(SpatialPoints(subset(caribou2h, caribou2h$season=="spring")[,1:2], proj4string=crs_NL), percent=100)
caribou2h_mcp_w<-mcp(SpatialPoints(subset(caribou2h, caribou2h$season=="winter")[,1:2], proj4string=crs_NL), percent=100)
coyote4h_mcp_s<-mcp(SpatialPoints(subset(coyote4h, coyote4h$season=="spring")[,1:2], proj4string=crs_NL), percent=100)
coyote8h_mcp_s<-mcp(SpatialPoints(subset(coyote8h, coyote8h$season=="spring")[,1:2], proj4string=crs_NL), percent=100)
coyote8h_mcp_w<-mcp(SpatialPoints(subset(coyote8h, coyote8h$season=="winter")[,1:2], proj4string=crs_NL), percent=100)
elk2h_mcp_s<-mcp(SpatialPoints(subset(elk2h, elk2h$season=="spring")[,1:2], proj4string=crs_RMNP), percent=100)
elk2h_mcp_w<-mcp(SpatialPoints(subset(elk2h, elk2h$season=="winter")[,1:2], proj4string=crs_RMNP), percent=100)
wolf1h_mcp_s<-mcp(SpatialPoints(subset(wolf1h, wolf1h$season=="spring")[,1:2], proj4string=crs_RMNP), percent=100)
wolf1h_mcp_w<-mcp(SpatialPoints(subset(wolf1h, wolf1h$season=="winter")[,1:2], proj4string=crs_RMNP), percent=100)


#save.image("WIP_JUNE28_2021.Rdata")

rm(list= ls()[!(ls() %in% c('bear2h_mcp_s','bear4h_mcp_s','caribou2h_mcp_s','caribou2h_mcp_w',
                            'coyote4h_mcp_s','coyote8h_mcp_s','coyote8h_mcp_w',
                            'elk2h_mcp_s','elk2h_mcp_w','wolf1h_mcp_s','wolf1h_mcp_w',
                            'bear2h','bear4h','caribou2h',
                            'coyote4h','coyote8h','elk2h','wolf1h'))])



save.image("need_regpts.Rdata")


rm(list= ls()[!(ls() %in% c('bear2h','bear4h','caribou2h',
                            'coyote4h','coyote8h','elk2h','wolf1h'))])

save.image("usedpts.Rdata")

### CLEAR WORKSPACE

load("usedpts.Rdata")


### regular points.R to create _200m.Rds files

coyote8h_w_regpts <- readRDS("input/regpts/coyote8h_mcp_w_regpts_200m.Rds")
coyote8h_s_regpts <- readRDS("input/regpts/coyote8h_mcp_s_regpts_200m.Rds")
coyote4h_s_regpts <- readRDS("input/regpts/coyote4h_mcp_s_regpts_200m.Rds")

caribou2h_s_regpts <- readRDS("input/regpts/caribou2h_mcp_s_regpts_200m.Rds")
caribou2h_w_regpts <- readRDS("input/regpts/caribou2h_mcp_w_regpts_200m.Rds")

bear4h_s_regpts <- readRDS("input/regpts/bear4h_mcp_s_regpts_200m.Rds")
bear2h_s_regpts <- readRDS("input/regpts/bear2h_mcp_s_regpts_200m.Rds")

elk2h_s_regpts <- readRDS("input/regpts/elk2h_mcp_s_regpts_200m.Rds")
elk2h_w_regpts <- readRDS("input/regpts/elk2h_mcp_w_regpts_200m.Rds")

wolf1h_s_regpts <- readRDS("input/regpts/wolf1h_mcp_s_regpts_200m.Rds")
wolf1h_w_regpts <- readRDS("input/regpts/wolf1h_mcp_w_regpts_200m.Rds")

# 
# library(sf)
# 
# NL <- st_read("input/extent/NL/NL-Bounds.shp")
# 
# bear2h_s_regpts<-st_as_sf(bear2h_s_regpts, coords = c("EASTING", "NORTHING"), crs = 32620)
# 
# NL<-st_transform(NL, crs=st_crs(bear2h_s_regpts))
# 
# test2<-st_intersection(bear2h_s_regpts, NL, sparse=FALSE)







coyote8h_w_regpts$case<-0
coyote8h_s_regpts$case<-0
coyote4h_s_regpts$case<-0

bear2h_s_regpts$case<-0
bear4h_s_regpts$case<-0

caribou2h_w_regpts$case<-0
caribou2h_s_regpts$case<-0

elk2h_w_regpts$case<-0
elk2h_s_regpts$case<-0

wolf1h_w_regpts$case<-0
wolf1h_s_regpts$case<-0

coyote8h_w_regpts$season<-"winter"
coyote8h_s_regpts$season<-"spring"
coyote4h_s_regpts$season<-"spring"

bear2h_s_regpts$season<-"spring"
bear4h_s_regpts$season<-"spring"

caribou2h_w_regpts$season<-"winter"
caribou2h_s_regpts$season<-"spring"

elk2h_w_regpts$season<-"winter"
elk2h_s_regpts$season<-"spring"

wolf1h_w_regpts$season<-"winter"
wolf1h_s_regpts$season<-"spring"

bear2h$case<-1
bear4h$case<-1
coyote4h$case<-1
coyote8h$case<-1
caribou2h$case<-1
elk2h$case<-1
wolf1h$case<-1

library(dplyr)


bear2h_full<-bind_rows(bear2h,bear2h_s_regpts)
bear4h_full<-bind_rows(bear4h,bear4h_s_regpts)

coyote8h_full<-bind_rows(coyote8h,coyote8h_s_regpts,coyote8h_w_regpts)
coyote4h_full<-bind_rows(coyote4h,coyote4h_s_regpts)

caribou2h_full<-bind_rows(caribou2h,caribou2h_s_regpts,caribou2h_w_regpts)
elk2h_full<-bind_rows(elk2h,elk2h_s_regpts,elk2h_w_regpts)
wolf1h_full<-bind_rows(wolf1h,wolf1h_s_regpts,wolf1h_w_regpts)

rm(list= ls()[!(ls() %in% c('bear2h_full','bear4h_full','caribou2h_full',
                            'coyote4h_full','coyote8h_full','elk2h_full','wolf1h_full'))])


save.image("ready_for_extract2.Rdata")