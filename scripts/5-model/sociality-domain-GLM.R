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

### why dont all dyadTimes have duplicates

### removing duplicate dyad,times

DT2<-DT[!duplicated(DT$dyadTime),]  

DT2$avgpredatorRSF<-(DT2$predatorRSF+DT2$predatorRSF.nn)/2


#### temporary fix to avg predator column



### removing duplicates is okay but as data currently stands we lose information from the endpreyRSF and endpredatorRSF columns
## All other unique data columns have a counterpart "X".nn, we should create these for endpreyRSF and endpredatorRSF columns.


#### Because we are focused on sociality, we only want to look at dyads that:
### 1) had potential to be social (dyads within 500m)
### 2) dyads that choose to be social (dSl less 500m)

### we might be interested in the conditions that result in a dyad that had the potential to be social but choose to not be social

DTsoc<-DT2[dyadDist<=500 & dSI<=500] 
DTnsoc<-DT2[dyadDist<=500 & dSI>=500] 
DTsocNA<-DT2[dyadDist>500] 

### Standardized but did not center the dependents
### but not DI since it is already on -1 to 1 and I think it is better to leave the values of DI comparable between datasets
DTsoc$z.dSI<-scale(DTsoc$dSI, center=F, scale=T)
DTsoc$z.dyadDist<-scale(DTsoc$dyadDist, center=F, scale=T)
DTsoc$z.dAbsAng<-scale(DTsoc$dAbsAng, center=F, scale=T)

### standardized avg RSF independents

DTsoc$z.avgpreyRSF<-scale(DTsoc$avgpreyRSF, center=T, scale=T)
DTsoc$z.avgpredatorRSF<-scale(DTsoc$avgpredatorRSF, center=T, scale=T)
DTsoc$z.avgcoyoteRSF<-scale(DTsoc$avgcoyoteRSF, center=T, scale=T)
DTsoc$z.avgbearRSF<-scale(DTsoc$avgbearRSF, center=T, scale=T)

### temporary fix to di

DTsoc$diang2<-(-1*(DTsoc$dAbsAng-90)/90)
DTsoc$DI<-DTsoc$diang2*DTsoc$diDist

### season subsets

DT_S<-subset(DT2, DT2$season=="spring")
DT_W<-subset(DT2, DT2$season=="winter")

DTsoc_S<-subset(DTsoc, DTsoc$season=="spring")
DTsoc_W<-subset(DTsoc, DTsoc$season=="winter")

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

round((nrow(DTsoc_W)+nrow(DTnsoc_W))/nrow(DT_W), 2)
#proportion of dyadtimes where animals had potential to be social
round(nrow(DTsocNA_W)/nrow(DT_W), 2)
#proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc_W)/(nrow(DTsoc_W)+nrow(DTnsoc_W)),2)
#proportion of dyadtimes where animals were social when they had potential to be social
round(nrow(DTnsoc_W)/(nrow(DTsoc_W)+nrow(DTnsoc_W)),2)
#proportion of dyadtimes where animals were not social when they had potential to be social

round((nrow(DTsoc_S)+nrow(DTnsoc_S))/nrow(DT_S), 2)
#proportion of dyadtimes where animals had potential to be social
round(nrow(DTsocNA_S)/nrow(DT_S), 2)
#proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc_S)/(nrow(DTsoc_S)+nrow(DTnsoc_S)),2)
#proportion of dyadtimes where animals were social when they had potential to be social
round(nrow(DTnsoc_S)/(nrow(DTsoc_S)+nrow(DTnsoc_S)),2)
#proportion of dyadtimes where animals were not social when they had potential to be social




### for RMNP correlations

c<-c(2,5:ncol(DTsoc)) ### for isolating correlation columns

COR<-round(cor(as.data.frame(DTsoc[,..c])[, sapply(as.data.frame(DTsoc[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

COR[abs(COR) < 0.5] <- ""  ## just helps me find any correlations worth noting

CORSp<-round(cor(as.data.frame(DTsoc_S[,..c])[, sapply(as.data.frame(DTsoc_S[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORSp[abs(CORSp) < 0.5] <- ""  ## just helps me find any correlations worth noting

CORW<-round(cor(as.data.frame(DTsoc_W[,..c])[, sapply(as.data.frame(DTsoc_W[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORW[abs(CORW) < 0.5] <- ""  ## just helps me find any correlations worth noting




### for NL correlations

c<-c(2,5:14,16:25,27:38,42:54,56:ncol(DTsoc)) ### for removing bear columns

COR<-round(cor(as.data.frame(DTsoc[,..c])[, sapply(as.data.frame(DTsoc[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

COR[abs(COR) < 0.5] <- ""  ## just helps me find any correlations worth noting

c<-c(2,5:ncol(DTsoc)) ### for isolating correlation columns

CORSp<-round(cor(as.data.frame(DTsoc_S[,..c])[, sapply(as.data.frame(DTsoc_S[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORSp[abs(CORSp) < 0.5] <- ""  ## just helps me find any correlations worth noting

c<-c(2,5:11,13,14,16:22,24,25,27:29,33:38,42:52,54,56:ncol(DTsoc)) ### for removing bear columns

CORW<-round(cor(as.data.frame(DTsoc_W[,..c])[, sapply(as.data.frame(DTsoc_W[,..c]), is.numeric)], use = "complete.obs", method = "pearson"),2)

CORW[abs(CORW) < 0.5] <- ""  ## just helps me find any correlations worth noting






### plots of data

plot(as.factor(DTsoc$season))

### pratically no spring observations  of social animals, n = 50 for elk
### pretty even distribution across seasons for caribou

plot(as.factor(DTsoc$dyadID))


plot(as.factor(DTsoc$nWithin))
# most individuals only had 1 or 2 other known individuals within 500m for elk
# most individuals only had 1 other known individuals within 500m for caribou


### social dyads have more social encounters than others

## WINTER

boxplot(DTsoc_W$dyadDist)
hist(DTsoc_W$dyadDist)

boxplot(DTsoc_W$dSI)
hist(DTsoc_W$dSI)

boxplot(DTsoc_W$dAbsAng)
hist(DTsoc_W$dAbsAng)

boxplot(DTsoc_W$di)
hist(DTsoc_W$di)

boxplot(DTsoc_W$avgpredatorRSF)
hist(DTsoc_W$avgpredatorRSF)

boxplot(DTsoc_W$avgpreyRSF)
hist(DTsoc_W$avgpreyRSF)


## Spring (not needed for RMNP, low sample size)

boxplot(DTsoc_S$dyadDist)
hist(DTsoc_S$dyadDist)

boxplot(DTsoc_S$dSI)
hist(DTsoc_S$dSI)

boxplot(DTsoc_S$dAbsAng)
hist(DTsoc_S$dAbsAng)

boxplot(DTsoc_S$di)
hist(DTsoc_S$di)

boxplot(DTsoc_S$avgpredatorRSF)
hist(DTsoc_S$avgpredatorRSF)

boxplot(DTsoc_S$avgpreyRSF)
hist(DTsoc_S$avgpreyRSF)

boxplot(DTsoc_S$avgcoyoteRSF)
hist(DTsoc_S$avgcoyoteRSF)

boxplot(DTsoc_S$avgbearRSF)
hist(DTsoc_S$avgbearRSF)


library(visreg)
library(rgl)

###GLM
# general form SA <- glm(SocialMeasure ~ PreyHD*PredHD, data = rmnp)
## social measures are: dSI, dyadDist, dAbsAng, DI

### nWithin5000 is a special social measure that will use a different dataset

## RMNP
## winter
#dyadDist

rmNN_W.dd<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#dsl

rmNN_W.dsl<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#dAbsAng

rmNN_W.dAA<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#DI

rmNN_W.DI<-
  glm(DI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)


## spring
#dyadDist

rmNN_S.dd<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

#dsl

rmNN_S.dsl<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

#dAbsAng

rmNN_S.dAA<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

#DI

rmNN_S.DI<-
  glm(DI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

summary(DTsoc_W$DI)
summary(DTsoc_S$DI)  ### elk more social in winter not social in spring

summary(rmNN_W.dd)   ###
summary(rmNN_W.dsl)  ### elk less social in good elk habitat
summary(rmNN_W.dAA)  ### elk more social in good elk habitat
summary(rmNN_W.DI)   ### elk more social in good elk habitat and more social in elk-wolf domain

visreg2d(rmNN_W.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") 

summary(rmNN_S.dd)   ###
summary(rmNN_S.dsl)  ### 
summary(rmNN_S.dAA)  ### 
summary(rmNN_S.DI)   ### 

visreg2d(rmNN_S.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") ### note nothing was significant





## NL
## winter
#dyadDist

nlNN_W.dd<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#dsl

nlNN_W.dsl<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#dAbsAng

nlNN_W.dAA<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)

#DI

nlNN_W.DI<-
  glm(DI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_W)


## spring
#dyadDist

nlNN_S.dd<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

nlNN_S.dd_C<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc_S)

nlNN_S.dd_B<-
  glm(z.dyadDist ~ z.avgpreyRSF*z.avgbearRSF,
      data = DTsoc_S)

#dsl

nlNN_S.dsl<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

nlNN_S.dsl_C<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc_S)

nlNN_S.dsl_B<-
  glm(z.dSI ~ z.avgpreyRSF*z.avgbearRSF,
      data = DTsoc_S)

#dAbsAng

nlNN_S.dAA<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

nlNN_S.dAA_C<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc_S)

nlNN_S.dAA_B<-
  glm(z.dAbsAng ~ z.avgpreyRSF*z.avgbearRSF,
      data = DTsoc_S)

#DI

nlNN_S.DI<-
  glm(DI ~ z.avgpreyRSF*z.avgpredatorRSF,
      data = DTsoc_S)

nlNN_S.DI_C<-
  glm(DI ~ z.avgpreyRSF*z.avgcoyoteRSF,
      data = DTsoc_S)

nlNN_S.DI_B<-
  glm(DI ~ z.avgpreyRSF*z.avgbearRSF,
      data = DTsoc_S)

summary(DTsoc_W$DI)
summary(DTsoc_S$DI)  ### a little less social in spring but not a big difference

<<<<<<< refs/remotes/origin/master
=======
summary(nlNN_W.dd)   ### 
summary(nlNN_W.dsl)  ### 
summary(nlNN_W.dAA)  ### 
summary(nlNN_W.DI)   ### 

visreg2d(nlNN_W.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") 

summary(nlNN_S.dd)   ###
summary(nlNN_S.dsl)  ### 
summary(nlNN_S.dAA)  ### 
summary(nlNN_S.DI)   ### 

summary(nlNN_S.dd_C)   ###
summary(nlNN_S.dsl_C)  ### 
summary(nlNN_S.dAA_C)  ### 
summary(nlNN_S.DI_C)   ### 

summary(nlNN_S.dd_B)   ###
summary(nlNN_S.dsl_B)  ### 
summary(nlNN_S.dAA_B)  ### 
summary(nlNN_S.DI_B)   ### 

visreg2d(nlNN_S.DI, "z.avgpreyRSF", "z.avgpredatorRSF", plot.type="image") ### 
visreg2d(nlNN_S.DI_C, "z.avgpreyRSF", "z.avgcoyoteRSF", plot.type="image") ### 
visreg2d(nlNN_S.DI_B, "z.avgpreyRSF", "z.avgbearRSF", plot.type="image") ### 
>>>>>>> WIP - model code
