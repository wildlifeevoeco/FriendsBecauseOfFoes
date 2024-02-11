### SOCIALITY ~ DOMAIN MODEL ----
# Authors: Alec Robitaille, Christina M Prokopenko, Hance Ellington

### Packages ----
pkgs <- c('data.table', 'lme4', 'ggplot2', 'Hmisc')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)

### Input data ----
# Which species?
species <- 'elk'
DT <- readRDS(paste0('output/4-sociality/', species, 'NNA.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

if (truelength(DT) == 0) alloc.col(DT)


### Maybe move this to NNA script?
# Dyads within 500m 
DT[dyadDist >= 500, bin500m := TRUE]
DT[dyadDist < 500, bin500m := FALSE]

# Duplicated dyads
DT[, nDyadTime := .N, by = dyadTime]
# TODO: add is duplicated column

summary(as.factor(DT$nDyadTime))


### removing duplicate dyad,times

DT2<-DT[!duplicated(DT$dyadTime),]  

summary(as.factor(DT2$nDyadTime))

### season subsets
DT_S<-subset(DT2, DT2$season=="spring")
DT_W<-subset(DT2, DT2$season=="winter")

### we arent currently using end...RSF columns but if we were we would need to fix them because they do not have .nn column

#### Because we are focused on sociality, we only want to look at dyads that:
### 1) had potential to be social (dyads within 500m)
### 2) dyads that choose to be social (dSl less 500m)

### we might be interested in the conditions that result in a dyad that had the potential to be social but choose to not be social

DTsoc<-DT2[dyadDist<=500 & dSI<=500]
DTsoc2<-DT2[dyadDist<=500]
DTsoc3<-DT2[dyadDist<=500 & dSI<=10000]
DTnsoc<-DT2[dyadDist<=500 & dSI>=500] 
DTsocNA<-DT2[dyadDist>500] 

### standardized avg RSF independents

DTsoc_S<-subset(DTsoc, DTsoc$season=="spring")
DTsoc_W<-subset(DTsoc, DTsoc$season=="winter")
DTsoc3_S<-subset(DTsoc3, DTsoc3$season=="spring")
DTsoc3_W<-subset(DTsoc3, DTsoc3$season=="winter")
DTnsoc_S<-subset(DTnsoc, DTnsoc$season=="spring")
DTnsoc_W<-subset(DTnsoc, DTnsoc$season=="winter")
DTsocNA_S<-subset(DTsocNA, DTsocNA$season=="spring")
DTsocNA_W<-subset(DTsocNA, DTsocNA$season=="winter")

round((nrow(DTsoc)+nrow(DTnsoc))/nrow(DT2), 2)
#proportion of dyadtimes where animals had potential to be social
round(nrow(DTsocNA)/nrow(DT2), 2)
#proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc)/(nrow(DTsoc)+nrow(DTnsoc)),2)
#proportion of dyadtimes where animals were social when they had potential to be social
round(nrow(DTnsoc)/(nrow(DTsoc)+nrow(DTnsoc)),2)
#proportion of dyadtimes where animals were not social when they had potential to be social

round(nrow(DTsoc3)/(nrow(DTsoc)+nrow(DTnsoc)),4)
#proportion of dyadtimes where animals were social when they had potential to be social

round((nrow(DTsoc_W)+nrow(DTnsoc_W))/nrow(DT_W), 2)
#proportion of dyadtimes where animals had potential to be social
round(nrow(DTsocNA_W)/nrow(DT_W), 2)
#proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc_W)/(nrow(DTsoc_W)+nrow(DTnsoc_W)),2)
#proportion of dyadtimes where animals were social when they had potential to be social
round(nrow(DTnsoc_W)/(nrow(DTsoc_W)+nrow(DTnsoc_W)),2)
#proportion of dyadtimes where animals were not social when they had potential to be social

round(nrow(DTsoc3_W)/(nrow(DTsoc_W)+nrow(DTnsoc_W)),4)
#proportion of dyadtimes where animals were social when they had potential to be social

round((nrow(DTsoc_S)+nrow(DTnsoc_S))/nrow(DT_S), 2)
#proportion of dyadtimes where animals had potential to be social
round(nrow(DTsocNA_S)/nrow(DT_S), 2)
#proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc_S)/(nrow(DTsoc_S)+nrow(DTnsoc_S)),2)
#proportion of dyadtimes where animals were social when they had potential to be social
round(nrow(DTnsoc_S)/(nrow(DTsoc_S)+nrow(DTnsoc_S)),2)
#proportion of dyadtimes where animals were not social when they had potential to be social

round(nrow(DTsoc3_S)/(nrow(DTsoc_S)+nrow(DTnsoc_S)),4)
#proportion of dyadtimes where animals were social when they had potential to be social

### for RMNP correlations

c<-c(2,5:ncol(DTsoc3_S)) ### for isolating correlation columns

CORSp<-round(cor(as.data.frame(DTsoc3_S[,..c])[, sapply(as.data.frame(DTsoc3_S[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORSp[abs(CORSp) < 0.5] <- ""  ## just helps me find any correlations worth noting

CORW<-round(cor(as.data.frame(DTsoc3_W[,..c])[, sapply(as.data.frame(DTsoc3_W[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORW[abs(CORW) < 0.5] <- ""  ## just helps me find any correlations worth noting


### plots of data

plot(as.factor(DTsoc3$season))
plot(as.factor(DTsoc3$dyadID))
plot(as.factor(DTsoc3$nWithin))
summary(as.factor(DTsoc3_W$nWithin))
summary(as.factor(DTsoc3_S$nWithin))

## WINTER (for NL predator=coyote in winter)

boxplot(DTsoc3_W$di)
hist(DTsoc3_W$di, breaks=20, freq=FALSE)


ggplot(DTsoc3_W, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,1)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)


ggplot(DTsoc3_S, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,1)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)

ggplot(DTsoc3_S, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,0.10)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)

ggplot(DTsoc3_W, aes(x=as.factor(DTsoc3_W$nWithin))) + geom_bar(aes(x=as.factor(DTsoc3_W$nWithin), y = ..count../sum(..count..)),fill = "grey", color = "black")+
  ylim(0,0.9)


ggplot(DTsoc3_S, aes(x=as.factor(DTsoc3_S$nWithin))) + geom_bar(aes(x=as.factor(DTsoc3_S$nWithin), y = ..count../sum(..count..)),fill = "grey", color = "black")+
  ylim(0,0.9)






boxplot(DTsoc3_W$avgpredatorRSF)
hist(DTsoc3_W$avgpredatorRSF)

boxplot(DTsoc3_W$avgpreyRSF)
hist(DTsoc3_W$avgpreyRSF)

## Spring (not needed for RMNP, low sample size)

boxplot(DTsoc3_S$di)
hist(DTsoc3_S$di)

boxplot(DTsoc3_S$avgpredatorRSF)
hist(DTsoc3_S$avgpredatorRSF)

boxplot(DTsoc3_S$avgpreyRSF)
hist(DTsoc3_S$avgpreyRSF)


h <- hist(DTsoc3_W$di) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100

g <- hist(DTsoc3_S$di) # or hist(x,plot=FALSE) to avoid the plot of the histogram
g$density = g$counts/sum(g$counts)*100


limits <- range(0,25)
par(mfrow=c(1,2))
plot(h,freq=FALSE, ylim=limits)
plot(g,freq=FALSE, ylim=limits)

library(visreg)
library(rgl)

###GLM
## RM
## winter
## winter

rmNN_W.DIpy<-
  glm(di ~ avgpreyRSF,
      data = DTsoc3_W)

summary(rmNN_W.DIpy1)

rmNN_W.DIpd<-
  glm(di ~ avgpredatorRSF,
      data = DTsoc3_W)

summary(rmNN_W.DIpd1)

rmNN_W.DI<-
  glm(di ~ avgpreyRSF*avgpredatorRSF,
      data = DTsoc3_W)

summary(rmNN_W.DI1)


rmNN_W.DI_A<-
  glm(di ~ avgpreyRSF+avgpredatorRSF,
      data = DTsoc3_W)

summary(rmNN_W.DI_A1)

########### For panelled figure 1

MOD.rmNN_W.DI<-rmNN_W.DI
MOD.rmNN_W.DI$coefficients[1]<-rmNN_W.DIpy$coefficients[1]
MOD.rmNN_W.DI$coefficients[2]<-rmNN_W.DIpy$coefficients[2]
MOD.rmNN_W.DI$coefficients[3]<-0
MOD.rmNN_W.DI$coefficients[4]<-0

## figure 1b
visreg2d(MOD.rmNN_W.DI, "avgpreyRSF", "avgpredatorRSF", plot.type="image", zlim=c(-1,1), main="", 
         xlab="Scaled estimate of selection value of elk landscape", 
         ylab="Scaled estimate of selection value of wolf landscape", zlab = "Predicted dynamic interaction of elk dyad")

### create raster of predicted DI from this modified model

MOD.rmNN_W.DI  # Elk winter

