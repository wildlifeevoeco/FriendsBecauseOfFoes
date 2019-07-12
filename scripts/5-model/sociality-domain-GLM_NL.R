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
species <- 'caribou'
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

### Standardized but did not center the dependents
### but not DI since it is already on -1 to 1 and I think it is better to leave the values of DI comparable between datasets
DTsoc$z.dSI<-scale(DTsoc$dSI, center=F, scale=T)
DTsoc$z.dyadDist<-scale(DTsoc$dyadDist, center=F, scale=T)
DTsoc$z.dAbsAng<-scale(DTsoc$dAbsAng, center=F, scale=T)

DTsoc3$z.dSI<-scale(DTsoc3$dSI, center=F, scale=T)
DTsoc3$z.dyadDist<-scale(DTsoc3$dyadDist, center=F, scale=T)
DTsoc3$z.dAbsAng<-scale(DTsoc3$dAbsAng, center=F, scale=T)

### standardized avg RSF independents

DTsoc$z.avgpreyRSF<-scale(DTsoc$avgpreyRSF, center=T, scale=T)
DTsoc$z.avgpredatorRSF<-scale(DTsoc$avgpredatorRSF, center=T, scale=T)
DTsoc$z.avgcoyoteRSF<-scale(DTsoc$avgcoyoteRSF, center=T, scale=T)
DTsoc$z.avgbearRSF<-scale(DTsoc$avgbearRSF, center=T, scale=T)

DTsoc3$z.avgpreyRSF<-scale(DTsoc3$avgpreyRSF, center=T, scale=T)
DTsoc3$z.avgpredatorRSF<-scale(DTsoc3$avgpredatorRSF, center=T, scale=T)
DTsoc3$z.avgcoyoteRSF<-scale(DTsoc3$avgcoyoteRSF, center=T, scale=T)
DTsoc3$z.avgbearRSF<-scale(DTsoc3$avgbearRSF, center=T, scale=T)

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


### for NL correlations

c<-c(2,5:ncol(DTsoc3_S)) ### for isolating correlation columns

CORSp<-round(cor(as.data.frame(DTsoc3_S[,..c])[, sapply(as.data.frame(DTsoc3_S[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORSp[abs(CORSp) < 0.5] <- ""  ## just helps me find any correlations worth noting

c<-c(2,5:10,12:14,16:25,27:38,42:54) ### for removing bear columns

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
hist(DTsoc3_W$di)

boxplot(DTsoc3_W$z.avgpredatorRSF)
hist(DTsoc3_W$z.avgpredatorRSF)

boxplot(DTsoc3_W$z.avgpreyRSF)
hist(DTso3c_W$z.avgpreyRSF)

## Spring

boxplot(DTsoc3_S$di)
hist(DTsoc3_S$di)

boxplot(DTsoc3_S$z.avgpredatorRSF)
hist(DTsoc3_S$z.avgpredatorRSF)

boxplot(DTsoc3_S$z.avgpreyRSF)
hist(DTsoc3_S$z.avgpreyRSF)

boxplot(DTsoc3_S$z.avgcoyoteRSF)
hist(DTsoc3_S$z.avgcoyoteRSF)

boxplot(DTsoc3_S$z.avgbearRSF)
hist(DTsoc3_S$z.avgbearRSF)

library(visreg)
library(rgl)

###GLM
## NL
## winter

nlNN_W.DIpy<-
  glm(di ~ z.avgpreyRSF,
      data = DTsoc3_W)

summary(nlNN_W.DIpy)

nlNN_W.DIpd<-
  glm(di ~ z.avgpredatorRSF,
      data = DTsoc3_W)

summary(nlNN_W.DIpd)

nlNN_W.DI<-
  glm(di ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc3_W)

summary(nlNN_W.DI)

## spring

nlNN_S.DIpy<-
  glm(di ~ z.avgpreyRSF,
      data = DTsoc3_S)

summary(nlNN_S.DIpy)

nlNN_S.DIpd<-
  glm(di ~ z.avgpredatorRSF,
      data = DTsoc3_S)

summary(nlNN_S.DIpd)

nlNN_S.DI<-
  glm(di ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc3_S)

summary(nlNN_S.DI)

### Bear v Coyote in Spring

nlNN_S.DI_Cpd<-
  glm(di ~ z.avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_Cpd)

nlNN_S.DI_C<-
  glm(di ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_C)

nlNN_S.DI_Bpd<-
  glm(di ~ z.avgbearRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_Bpd)

nlNN_S.DI_B<-
  glm(di ~ z.avgpreyRSF*z.avgbearRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_B)

### year-round coyote

nlNN.DIpy<-
  glm(di ~ z.avgpreyRSF,
      data = DTsoc3)

summary(nlNN.DIpy)

nlNN.DI_Cpd<-
  glm(di ~ z.avgcoyoteRSF,
      data = DTsoc3)

summary(nlNN.DI_Cpd)

nlNN.DI_C<-
  glm(di ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc3)

summary(nlNN.DI_C)


###  Make these plots on same axis scale
## winter caribou-coyote domain with di
visreg2d(nlNN_W.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") 

## spring caribou-coyote domain with di
visreg2d(nlNN_S.DI_C, "z.avgpreyRSF", "z.avgcoyoteRSF", plot.type="image") 

## year-round caribou-coyote domain with di
visreg2d(nlNN.DI_C, "z.avgpreyRSF", "z.avgcoyoteRSF", plot.type="image") 


###  Make these plots on same axis scale
## spring caribou-bear domain with di
visreg2d(nlNN_S.DI_B, "z.avgpreyRSF", "z.avgbearRSF", plot.type="image") 

## spring caribou-coyote domain with di
visreg2d(nlNN_S.DI_C, "z.avgpreyRSF", "z.avgcoyoteRSF", plot.type="image") 

## spring caribou-predator domain with di
visreg2d(nlNN_S.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") 
