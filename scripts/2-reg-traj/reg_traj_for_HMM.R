library(dplyr)
library(lubridate)
library(adehabitatLT)


bear <- readRDS("C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/output/1-data-prep/bear.Rds")
caribou <- readRDS("C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/output/1-data-prep/caribou.Rds")
coyote <- readRDS("C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/output/1-data-prep/coyote.Rds")
elk <- readRDS("C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/output/1-data-prep/elk.Rds")
wolf <- readRDS("C:/Users/ehanc/OneDrive/Desktop/EWC/ewc/output/1-data-prep/wolf.Rds")

coyote$id<-as.character(coyote$id)

colnames(bear)[1]<-"ID"
colnames(caribou)[1]<-"ID"
colnames(coyote)[1]<-"ID"
colnames(elk)[1]<-"ID"
colnames(wolf)[1]<-"ID"

### First generate data with regular trajectory
###  

round(summary(as.factor(round(coyote$fixRate, 2)))*100/nrow(coyote),0)
round(summary(as.factor(round(elk$fixRate, 2)))*100/nrow(elk),0)
round(summary(as.factor(round(wolf$fixRate, 2)))*100/nrow(wolf),0)
round(summary(as.factor(round(caribou$fixRate, 2)))*100/nrow(caribou),0)
round(summary(as.factor(round(bear$fixRate, 2)))*100/nrow(bear),0)


### 2hr = bear, caribou, elk
### 1hr = wolf
### 8hr = coyote
### 4hr = coyote, bear


### regular trajectories using adehabitatLT


### List of bear split by ID

id.list <- split(bear , f = bear$ID)

foo5hr<-function(dt) {return(dt> (5*60*60))} ###greater than 5 hr without location made into seperate burst
foo4.5hr<-function(dt) {return(dt> (4.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 3 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:17)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  

  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:29)])
  
  c.traj_5hr<-tryCatch(cutltraj(test5, "foo5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_5hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 2 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:32)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=120, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:34)])
  
  c.traj_4.5hr<-tryCatch(cutltraj(test5, "foo4.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_4.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())

df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=5)  ### at least 5 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*12)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_bear2hrdata.csv")

bear2<-df5


sum.bear2hrdata<-sum.df5

################












### List of bear split by ID - 4hr

id.list <- split(bear , f = bear$ID)

foo10hr<-function(dt) {return(dt> (10*60*60))} ###greater than 10 hr without location made into seperate burst
foo8.5hr<-function(dt) {return(dt> (8.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 10 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:17)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:29)])
  
  c.traj_10hr<-tryCatch(cutltraj(test5, "foo10hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_10hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 4 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:32)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,4]), error=function(e) NA)
  sub.list[[9]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,4]), error=function(e) NA)
  sub.list[[10]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,4]), error=function(e) NA)
  sub.list[[11]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,4]), error=function(e) NA)
  sub.list[[12]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,4]), error=function(e) NA)
  sub.list[[13]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,4]), error=function(e) NA)
  sub.list[[14]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,4]), error=function(e) NA)
  sub.list[[15]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,4]), error=function(e) NA)
  sub.list[[16]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=240, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:34)])
  
  c.traj_8.5hr<-tryCatch(cutltraj(test5, "foo8.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_8.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())
df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=10)  ### at least 10 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*6)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_bear4hrdata.csv")

bear2.1<-df5


sum.bear4hrdata<-sum.df5



### List of caribou split by ID

id.list <- split(caribou , f = caribou$ID)

foo5hr<-function(dt) {return(dt> (5*60*60))} ###greater than 5 hr without location made into seperate burst
foo4.5hr<-function(dt) {return(dt> (4.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 3 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:17)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:29)])
  
  c.traj_5hr<-tryCatch(cutltraj(test5, "foo5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_5hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 2 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:32)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=120, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:34)])
  
  c.traj_4.5hr<-tryCatch(cutltraj(test5, "foo4.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_4.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())
df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=5)  ### at least 5 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*12)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_caribou2hrdata.csv")

caribou2<-df5


sum.caribou2hrdata<-sum.df5





### List of elk split by ID

id.list <- split(elk , f = elk$ID)

foo5hr<-function(dt) {return(dt> (5*60*60))} ###greater than 5 hr without location made into seperate burst
foo4.5hr<-function(dt) {return(dt> (4.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 3 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:15)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "GMT")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:27)])
  
  c.traj_5hr<-tryCatch(cutltraj(test5, "foo5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_5hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 2 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:30)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=120, units="min", nlo=8))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=120, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "GMT")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:32)])
  
  c.traj_4.5hr<-tryCatch(cutltraj(test5, "foo4.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_4.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())
df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=5)  ### at least 5 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*12)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_elk2hrdata.csv")

elk2<-df5


sum.elk2hrdata<-sum.df5








### List of wolf split by ID

id.list <- split(wolf , f = wolf$ID)

foo2.5hr<-function(dt) {return(dt> (2.5*60*60))} ###greater than 2.5 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 2.5 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:16)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "GMT")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:28)])
  
  c.traj_2.5hr<-tryCatch(cutltraj(test5, "foo2.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_2.5hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 1 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:31)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=60, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=60, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=60, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=60, units="min", nlo=8))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=60, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "GMT")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:33)])
  
  c.traj_2.5hr<-tryCatch(cutltraj(test5, "foo2.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_2.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())
df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=2.5)  ### at least 2.5 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*24)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_wolf1hrdata.csv")

wolf2<-df5


sum.wolf1hrdata<-sum.df5






















### List of coyote split by ID - 8hr

id.list <- split(coyote , f = coyote$ID)

foo20hr<-function(dt) {return(dt> (20*60*60))} ###greater than 20 hr without location made into seperate burst
foo16.5hr<-function(dt) {return(dt> (16.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 20 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:17)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:29)])
  
  c.traj_20hr<-tryCatch(cutltraj(test5, "foo20hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_20hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 8 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:32)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=480, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=8))[,4]), error=function(e) NA)
  sub.list[[9]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=9))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=9))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=9))[,4]), error=function(e) NA)
  sub.list[[10]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=10))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=10))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=10))[,4]), error=function(e) NA)
  sub.list[[11]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=11))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=11))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=11))[,4]), error=function(e) NA)
  sub.list[[12]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=12))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=12))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=12))[,4]), error=function(e) NA)
  sub.list[[13]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=13))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=13))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=13))[,4]), error=function(e) NA)
  sub.list[[14]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=14))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=14))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=14))[,4]), error=function(e) NA)
  sub.list[[15]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=15))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=15))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=15))[,4]), error=function(e) NA)
  sub.list[[16]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=16))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=16))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=16))[,4]), error=function(e) NA)
  sub.list[[17]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=17))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=17))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=17))[,4]), error=function(e) NA)
  sub.list[[18]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=18))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=18))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=18))[,4]), error=function(e) NA)
  sub.list[[19]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=19))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=19))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=19))[,4]), error=function(e) NA)
  sub.list[[20]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=20))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=20))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=20))[,4]), error=function(e) NA)
  sub.list[[21]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=21))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=21))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=21))[,4]), error=function(e) NA)
  sub.list[[22]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=22))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=22))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=22))[,4]), error=function(e) NA)
  sub.list[[23]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=23))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=23))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=23))[,4]), error=function(e) NA)
  sub.list[[24]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=24))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=24))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=24))[,4]), error=function(e) NA)
  sub.list[[25]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=25))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=25))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=25))[,4]), error=function(e) NA)
  sub.list[[26]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=26))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=26))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=26))[,4]), error=function(e) NA)
  sub.list[[27]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=27))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=27))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=27))[,4]), error=function(e) NA)
  sub.list[[28]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=28))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=28))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=28))[,4]), error=function(e) NA)
  sub.list[[29]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=29))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=29))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=29))[,4]), error=function(e) NA)
  sub.list[[30]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=30))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=30))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=30))[,4]), error=function(e) NA)
  sub.list[[31]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=31))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=31))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=31))[,4]), error=function(e) NA)
  sub.list[[32]]<-tryCatch(summary(subsample(traj_15min2, dt=480, units="min", nlo=32))[,4]/(summary(subsample(traj_15min2, dt=480, units="min", nlo=32))[,3]+summary(subsample(traj_15min2, dt=480, units="min", nlo=32))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=480, units="min", nlo=which.min(sub.list))  ### subsampling to 8 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:34)])
  
  c.traj_16.5hr<-tryCatch(cutltraj(test5, "foo16.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_16.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than twenty days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())

df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=20)  ### at least 5 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*3)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_coyote8hrdata.csv")

coyote2<-df5


sum.coyote8hrdata<-sum.df5




### List of coyote split by ID - 4hr

id.list <- split(coyote , f = coyote$ID)

foo10hr<-function(dt) {return(dt> (10*60*60))} ###greater than 10 hr without location made into seperate burst
foo8.5hr<-function(dt) {return(dt> (8.5*60*60))} ###greater than 20 hr without location made into seperate burst

### Rounding data to nearest 15 minute interval with variable start times
### then splitting into seperate bursts if greater than 10 hours with no locations

id.list3<-list()

for(i in 1:length(id.list)){
  
  test<-id.list[[i]]
  
  ref<-floor_date(min(test$datetime), unit ="hour") - 15*60
  
  test2<-as.ltraj(xy=test[,c(8,9)], date=test$datetime, id=test$ID, typeII = TRUE, infolocs = test[,c(1:17)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15min trajectory
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15min trajectory
  
  test3<-ld(traj_15min2)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  
  test4<-subset(test3, !is.na(test3$x))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$ID, typeII = TRUE, infolocs = test4[,c(13:29)])
  
  c.traj_10hr<-tryCatch(cutltraj(test5, "foo10hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_10hr),  error=function(e) NULL)
  
  test6$dt.min<-tryCatch(test6$dt/60,  error=function(e) NULL)
  test6$dt.hr<-tryCatch(test6$dt.min/60,  error=function(e) NULL)
  
  
  id.list3[[i]]<-tryCatch(test6,  error=function(e) NA)
  
}

### bring all list together then split by burst then clean to 4 hr intervals

df <- bind_rows(id.list3, .id = "column_label")
id.list4 <- split(df , f = df$burst)

id.list5<-list()

for(i in 1:length(id.list4)){
  
  test<-id.list4[[i]]
  
  ref<-floor_date(min(test$datetime), "15 minutes")
  
  test2<-as.ltraj(xy=test[,c(2,3)], date=test$datetime, id=test$burst, typeII = TRUE, infolocs = test[,c(14:32)])
  
  ### 15min rarified
  traj_15min<- setNA(test2, ref, dt = 15, units = "min") ### fill in missing locations in regular 15 min trajectory
  
  traj_15min2<- sett0(traj_15min, ref, dt = 15, units = "min") ### round locations to the regular 15 min trajectory
  
  sub.list<-list()
  
  sub.list[[1]]<-summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=1))[,4])
  sub.list[[2]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=2))[,4]), error=function(e) NA)
  sub.list[[3]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=3))[,4]), error=function(e) NA)
  sub.list[[4]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=4))[,4]), error=function(e) NA)
  sub.list[[5]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=5))[,4]), error=function(e) NA)
  sub.list[[6]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=6))[,4]), error=function(e) NA)
  sub.list[[7]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=7))[,4]), error=function(e) NA)
  sub.list[[8]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=8))[,4]), error=function(e) NA)
  sub.list[[9]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=9))[,4]), error=function(e) NA)
  sub.list[[10]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=10))[,4]), error=function(e) NA)
  sub.list[[11]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=11))[,4]), error=function(e) NA)
  sub.list[[12]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=12))[,4]), error=function(e) NA)
  sub.list[[13]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=13))[,4]), error=function(e) NA)
  sub.list[[14]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=14))[,4]), error=function(e) NA)
  sub.list[[15]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=15))[,4]), error=function(e) NA)
  sub.list[[16]]<-tryCatch(summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,4]/(summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,3]+summary(subsample(traj_15min2, dt=240, units="min", nlo=16))[,4]), error=function(e) NA)
  
  sub.s<-subsample(traj_15min2, dt=240, units="min", nlo=which.min(sub.list))  ### subsampling to 2 hr interval with best start from 15min interval
  
  test3<-ld(sub.s)
  
  test3$datetime<-as.POSIXct(test3$datetime, origin = "1970-01-01", tz = "America/St_Johns")
  test3$ROUND_TIME<-test3$date
  test3$TIM_DIF<-as.numeric((test3$ROUND_TIME-test3$datetime))/60
  
  test4<-subset(test3, !is.na(test3$x))
  test4$HIGH_15R<-ifelse(test4$TIM_DIF>15,1, ifelse(test4$TIM_DIF< -15,1,0))
  
  test5<-as.ltraj(xy=test4[,c(1,2)], date=test4$datetime, id=test4$burst, typeII = TRUE, infolocs = test4[,c(13:34)])
  
  c.traj_8.5hr<-tryCatch(cutltraj(test5, "foo8.5hr(dt)", nextr = TRUE),  error=function(e) NULL)
  
  test6<-tryCatch(ld(c.traj_8.5hr),  error=function(e) NULL)
  
  test6$dt.min<-test6$dt/60
  test6$dt.hr<-test6$dt.min/60
  
  id.list5[[i]]<-test6
  
}


### Bringing it all back together

df2 <- bind_rows(id.list5, .id = "column_label")
#df2$id<-sub("\\..*", "", df2$id.1)

### removing bursts with less than 10 days of data
df3<-df2 %>% group_by(burst) %>% summarize(ID=ID[1],
                                           start=min(date), end=max(date), n=n())
df3$t.length<-(as.numeric(df3$end)-as.numeric(df3$start))/(60*60*24)

df4<-subset(df3, df3$t.length>=10)  ### at least 10 days covered by burst

df5<-df2 %>% filter(burst %in% df4$burst)

### Estimating rounding differences
df5$TIM_DIF<-(as.numeric(df5$ROUND_TIME)-as.numeric(df5$datetime))/(60*60)
df5$HIGH_TIMDIF<-ifelse(df5$TIM_DIF>=0.5 | df5$TIM_DIF <= -0.5,1,0)

### Summary information about each burst
sum.df5<-df5 %>% group_by(burst) %>% 
  summarize(start=min(date), end=max(date), n=n(), 
            avg.tim.dif=mean(TIM_DIF), min.tim.dif=min(TIM_DIF), max.tim.dif=max(TIM_DIF),num.high.td=sum(HIGH_TIMDIF),
            avg.dt.hr=mean(dt.hr, na.rm=T))

sum.df5$per.high.td<-sum.df5$num.high.td/sum.df5$n

sum.df5$t.length<-as.numeric(sum.df5$end-sum.df5$start)

sum.df5$max.locs<-floor(sum.df5$t.length*6)+2

sum.df5$per.miss<-1-(sum.df5$n/sum.df5$max.locs)

write.csv(sum.df5, "summary_coyote4hrdata.csv")

coyote2.1<-df5


sum.coyote4hrdata<-sum.df5


rm(list= ls()[!(ls() %in% c('bear2','bear2.1','caribou2','coyote2','coyote2.1','elk2','sum.bear2hrdata','sum.bear4hrdata',
                            'sum.caribou2hrdata','sum.coyote4hrdata','sum.coyote8hrdata','sum.elk2hrdata',
                            'sum.wolf1hrdata','wolf2'))])


sum.bear2hrdata2<- sum.bear2hrdata %>% filter(per.miss <= 0.2)
sum.bear4hrdata2<- sum.bear4hrdata %>% filter(per.miss <= 0.2)
sum.caribou2hrdata2<- sum.caribou2hrdata %>% filter(per.miss <= 0.2)
sum.elk2hrdata2<- sum.elk2hrdata %>% filter(per.miss <= 0.2)
sum.wolf1hrdata2<- sum.wolf1hrdata %>% filter(per.miss <= 0.2)
sum.coyote4hrdata2<- sum.coyote4hrdata %>% filter(per.miss <= 0.2)
sum.coyote8hrdata2<- sum.coyote8hrdata %>% filter(per.miss <= 0.2)


bear2a<-bear2 %>% filter(burst %in% sum.bear2hrdata2$burst)
bear2.1a<-bear2.1 %>% filter(burst %in% sum.bear4hrdata2$burst)
caribou2a<-caribou2 %>% filter(burst %in% sum.caribou2hrdata2$burst)
elk2a<-elk2 %>% filter(burst %in% sum.elk2hrdata2$burst)
wolf2a<-wolf2 %>% filter(burst %in% sum.wolf1hrdata2$burst)
coyote2a<-coyote2 %>% filter(burst %in% sum.coyote8hrdata2$burst)
coyote2.1a<-coyote2.1 %>% filter(burst %in% sum.coyote4hrdata2$burst)

coyote2a<-cbind(coyote2a[,1],coyote2a[,4:32],coyote2a[,2:3],coyote2a[,33:36])
coyote2.1a<-cbind(coyote2.1a[,1],coyote2.1a[,4:32],coyote2.1a[,2:3],coyote2.1a[,33:36])

length(unique(bear2a$ID))
length(unique(bear2.1a$ID))
length(unique(caribou2a$ID))
length(unique(elk2a$ID))
length(unique(wolf2a$ID))
length(unique(coyote2a$ID))
length(unique(coyote2.1a$ID))

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','sum.bear2hrdata2','sum.bear4hrdata2',
                            'sum.caribou2hrdata2','sum.coyote4hrdata2','sum.coyote8hrdata2','sum.elk2hrdata2',
                            'sum.wolf1hrdata2','wolf2a'))])

save.image("Ready_for_HMM.Rdata")

