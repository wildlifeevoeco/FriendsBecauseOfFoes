### Generating summary tables for RSFs ====
# Authors: Alec Robitaille, Michel Laforge

### Packages ----
pkgs <- c('piecewiseSEM', 'lme4')
lapply(pkgs, requires, character.only = TRUE)

### Input ----
paths <- dir('output/2-rsf', 'RSF', recursive = TRUE, full.names = TRUE)
names <- lapply(dir('output/2-rsf', 'RSF', recursive = TRUE), function(x) strsplit(x, '/'))
rsfs <- lapply(paths, readRDS)
names()
### Processing ----
digits <- 3
lapply(rsfs, function(r) {
  coefs <- coef(r)
  
  se <- sqrt(diag(vcov(r)))
  
  lci <- coefs - (se * 1.96)
  uci <- coefs + (se * 1.96)
  
  list()
  paste0(round(coefs, digits = digits), " [", 
           round(lci, digits = digits), ", ", 
           round(uci, digits = digits), "]")
})



## Winter

wolfwin<-readRDS("output/PredRSFRMNP/winterWolfRSF.RDS")

summary(wolfwin)

wolfwincoefs<-coef(wolfwin)

wolfwinSEs<-c(sqrt(diag(vcov(wolfwin))))

wolfwinLCI<-wolfwincoefs-(wolfwinSEs*1.96)
wolfwinUCI<-wolfwincoefs+(wolfwinSEs*1.96)

wolfwincoefsR<-round(wolfwincoefs,3)
wolfwinCI<-paste(wolfwincoefsR, " [",round(wolfwinLCI,3), ", ",round(wolfwinUCI,3),"]",sep="")

##Combine seasons

wolfData<-data.frame(names(wolfsprcoefs),wolfsprCI,wolfwinCI)
colnames(wolfData)<-c("Coefficient","Estimate + 95% CI, Spring","Estimate + 95% CI, Winter")

wolfData

write.csv(wolfData,"output/SummaryTablesAppendix/wolfData.csv")


#### Elk ####

## Spring

elkspr<-readRDS("output/PredRSFRMNP/springelkRSF.RDS")

elksprcoefs<-coef(elkspr)

elksprSEs<-c(sqrt(diag(vcov(elkspr))))

elksprLCI<-elksprcoefs-(elksprSEs*1.96)
elksprUCI<-elksprcoefs+(elksprSEs*1.96)

elksprcoefsR<-round(elksprcoefs,3)
elksprCI<-paste(elksprcoefsR, " [",round(elksprLCI,3), ", ",round(elksprUCI,3),"]",sep="")


## Winter

elkwin<-readRDS("output/PredRSFRMNP/winterelkRSF.RDS")

summary(elkwin)

elkwincoefs<-coef(elkwin)

elkwinSEs<-c(sqrt(diag(vcov(elkwin))))

elkwinLCI<-elkwincoefs-(elkwinSEs*1.96)
elkwinUCI<-elkwincoefs+(elkwinSEs*1.96)

elkwincoefsR<-round(elkwincoefs,3)
elkwinCI<-paste(elkwincoefsR, " [",round(elkwinLCI,3), ", ",round(elkwinUCI,3),"]",sep="")

##Combine seasons

elkData<-data.frame(names(elksprcoefs),elksprCI,elkwinCI)
colnames(elkData)<-c("Coefficient","Estimate + 95% CI, Spring","Estimate + 95% CI, Winter")

elkData

write.csv(elkData,"output/SummaryTablesAppendix/elkData.csv")



#### Coyote ####

## Spring

coyspr<-readRDS("output/PredRSFNL/coyoteSummerRSF.RDS")

coysprcoefs<-coef(coyspr)

coysprSEs<-c(sqrt(diag(vcov(coyspr))))

coysprLCI<-coysprcoefs-(coysprSEs*1.96)
coysprUCI<-coysprcoefs+(coysprSEs*1.96)

coysprcoefsR<-round(coysprcoefs,3)
coysprCI<-paste(coysprcoefsR, " [",round(coysprLCI,3), ", ",round(coysprUCI,3),"]",sep="")


## Winter

coywin<-readRDS("output/PredRSFNL/coyoteWinterRSF.RDS")

summary(coywin)

coywincoefs<-coef(coywin)

coywinSEs<-c(sqrt(diag(vcov(coywin))))

coywinLCI<-coywincoefs-(coywinSEs*1.96)
coywinUCI<-coywincoefs+(coywinSEs*1.96)

coywincoefsR<-round(coywincoefs,3)
coywinCI<-paste(coywincoefsR, " [",round(coywinLCI,3), ", ",round(coywinUCI,3),"]",sep="")

##Combine seasons

coyData<-data.frame(names(coysprcoefs),coysprCI,coywinCI)
colnames(coyData)<-c("Coefficient","Estimate + 95% CI, Spring","Estimate + 95% CI, Winter")

coyData

write.csv(coyData,"output/SummaryTablesAppendix/coyData.csv")




#### Bear ####

## Spring

bearspr<-readRDS("output/PredRSFNL/BearsSummerRSF.RDS")

bearsprcoefs<-coef(bearspr)

bearsprSEs<-c(sqrt(diag(vcov(bearspr))))

bearsprLCI<-bearsprcoefs-(bearsprSEs*1.96)
bearsprUCI<-bearsprcoefs+(bearsprSEs*1.96)

bearsprcoefsR<-round(bearsprcoefs,3)
bearsprCI<-paste(bearsprcoefsR, " [",round(bearsprLCI,3), ", ",round(bearsprUCI,3),"]",sep="")


bearData<-data.frame(names(bearsprcoefs),bearsprCI)
colnames(bearData)<-c("Coefficient","Estimate + 95% CI, Spring")

bearData

write.csv(bearData,"output/SummaryTablesAppendix/bearData.csv")



#### Caribou ####

## Spring

carspr<-readRDS("output/PredRSFNL/CaribouSummerRSF.RDS")

carsprcoefs<-coef(carspr)

carsprSEs<-c(sqrt(diag(vcov(carspr))))

carsprLCI<-carsprcoefs-(carsprSEs*1.96)
carsprUCI<-carsprcoefs+(carsprSEs*1.96)

carsprcoefsR<-round(carsprcoefs,3)
carsprCI<-paste(carsprcoefsR, " [",round(carsprLCI,3), ", ",round(carsprUCI,3),"]",sep="")


## Winter

carwin<-readRDS("output/PredRSFNL/CaribouWinterRSF.RDS")

summary(carwin)

carwincoefs<-coef(carwin)

carwinSEs<-c(sqrt(diag(vcov(carwin))))

carwinLCI<-carwincoefs-(carwinSEs*1.96)
carwinUCI<-carwincoefs+(carwinSEs*1.96)

carwincoefsR<-round(carwincoefs,3)
carwinCI<-paste(carwincoefsR, " [",round(carwinLCI,3), ", ",round(carwinUCI,3),"]",sep="")

##Combine seasons

carSpr<-cbind(names(carsprcoefs),carsprCI)
colnames(carSpr)<-c("Coefficient","Spring")
carWin<-cbind(names(carwincoefs),carwinCI)
colnames(carWin)<-c("Coefficient","Winter")


carData<-merge(carSpr,carWin,by="Coefficient",all=T)
carData

carData<-data.frame(names(carsprcoefs),carsprCI,carwinCI)
colnames(carData)<-c("Coefficient","Estimate + 95% CI, Spring","Estimate + 95% CI, Winter")

carData

write.csv(carData,"output/SummaryTablesAppendix/carData.csv")











wolfsprRs<-rsquared(wolfspr)[5]
wolfsprNPts<-rsquared(wolfspr)[4]
wolfsprNind<-17

wolfwinRs<-rsquared(wolfwin)[5]
wolfwinNPts<-rsquared(wolfwin)[4]
wolfwinNind<-15


elksprRs<-rsquared(elkspr)[5]
elksprNPts<-rsquared(elkspr)[4]
elksprNind<-24

elkwinRs<-rsquared(elkwin)[5]
elkwinNPts<-rsquared(elkwin)[4]
elkwinNind<-35

CoyD<-readRDS("output/ResidentCoyotes.RDS")
head(CoyD)
coysprRs<-rsquared(coyspr)[5]
coysprNPts<-rsquared(coyspr)[4]
coysprNind<-length(unique(subset(CoyD,season=="spring")$id))

coywinRs<-rsquared(coywin)[5]
coywinNPts<-rsquared(coywin)[4]
coywinNind<-length(unique(subset(CoyD,season=="winter")$id))

bears<-readRDS('output/1-data-prep/bear.Rds')
bearsprRs<-rsquared(bearspr)[5]
bearsprNPts<-rsquared(bearspr)[4]
bearsprNind<-length(unique(subset(bears,season=="spring")$id))

bearwinRs<-NA
bearwinNPts<-NA
bearwinNind<-NA


MRcar<-readRDS('output/1-data-prep/caribou.Rds')
carsprRs<-rsquared(carspr)[5]
carsprNPts<-rsquared(carspr)[4]
carsprNind<-length(unique(subset(MRcar,season=="spring")$id))

carwinRs<-rsquared(carwin)[5]
carwinNPts<-rsquared(carwin)[4]
carwinNind<-length(unique(subset(MRcar,season=="winter")$id))


Rsquared<-rbind(wolfsprRs,wolfwinRs,elksprRs,elkwinRs,coysprRs,coywinRs,bearsprRs,carsprRs,carwinRs)
RsquaredR<-round(Rsquared,3)
Individuals<-rbind(wolfsprNind,wolfwinNind,elksprNind,elkwinNind,coysprNind,
            coywinNind,bearsprNind,carsprNind,carwinNind)
Points<-rbind(wolfsprNPts,wolfwinNPts,elksprNPts,elkwinNPts,coysprNPts,
            coywinNPts,bearsprNPts,carsprNPts,carwinNPts)

Data<-cbind(RsquaredR,Individuals,Points)

str(Data)
rownames(Data)<-c("Wolf Spring","Wolf Winter","Elk Spring","Elk Winter","Coyote Spring",
                  "Coyote Winter","Black Bear Spring","Caribou Spring","Caribou Winter")

write.csv(Data,"output/SummaryTablesAppendix/summaryOfModels.CSV")

