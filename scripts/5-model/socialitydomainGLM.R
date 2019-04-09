### SOCIALITY ~ DOMAIN MODEL ----
# Authors: Alec Robitaille, Christina M Prokopenko
# Purpose: 
# Inputs: sociality measures, predator and prey RSFs for RMNP and NL
# Outputs:
# TODO:  how to ignore dif in line endings/force git to use one


libs <- c('data.table',  
          'lme4',
          'ggplot2',
          'Hmisc')
lapply(libs, require, character.only = TRUE)

### Input data ----

#RMNP file

rmnp <- readRDS('output/nna/elkNNA.Rds')

nlold <- readRDS('output/nna/caribouNNA.Rds')
nl <- readRDS('output/nna/CaribouNNANEW.Rds')

### one case where dyad is just i-i, instead of i-j
### Alec has satisfied Hance that this error is okay and driven by the way QuadTree extracts nearest dyads

rmnp$bad.neighbor<-ifelse(rmnp$id==rmnp$neighbour1,1,0)
summary(rmnp$bad.neighbor)
        
rmnp$bin500m<-ifelse(rmnp$dyadDist>500,0,1) ## dyad within 500m or not

nl$bad.neighbor<-ifelse(nl$id==nl$neighbour1,1,0)
summary(nl$bad.neighbor)

nl$bin500m<-ifelse(nl$dyadDist>500,0,1) ## dyad within 500m or not

### removing this case plus just extracting dyads

rmnp.dyad1<-subset(rmnp, rmnp$bad.neighbor==0)
nl.dyad1<-subset(nl, nl$bad.neighbor==0)

### removing duplicate dyad,times

rmnp.dyad1$dyad.time<-paste(rmnp.dyad1$dyadID,rmnp.dyad1$timegroup,sep="_")

rmnp.dyad2<-rmnp.dyad1[!duplicated(rmnp.dyad1$dyad.time),]

nl.dyad1$dyad.time<-paste(nl.dyad1$dyadID,nl.dyad1$timegroup,sep="_")

nl.dyad2<-nl.dyad1[!duplicated(nl.dyad1$dyad.time),]

nl.dyad2$predatorRSF<-ifelse(nl.dyad2$season=="spring", nl.dyad2$CoyRSF+nl.dyad2$BearRSF, nl.dyad2$CoyRSF)
nl.dyad2$rpredatorRSF<-ifelse(nl.dyad2$season=="spring", nl.dyad2$rCoyRSF+nl.dyad2$rBearRSF, nl.dyad2$rCoyRSF)
nl.dyad2$dPredRSF<-ifelse(nl.dyad2$season=="spring", nl.dyad2$dCoyRSF+nl.dyad2$dBearRSF, nl.dyad2$dCoyRSF)
nl.dyad2$avgPredRSF<-ifelse(nl.dyad2$season=="spring", (nl.dyad2$predatorRSF+nl.dyad2$rpredatorRSF)/2, nl.dyad2$avgCoyRSF)



#### Because we are focused on sociality, we only want to look at animals that:
### 1) had potential to be social (dyads within 500m)
### 2) dyads that choose to be social (dSl less 500m)

### we might be interested in the conditions that result in a dyad that had the potential to be social but choose to not be social

nl.dyad3<-nl.dyad2[dyadDist<=500 & dSI<=500] #choose to be social (7637/9049; 84.4% of potential to be social were)
nl.dyad4<-nl.dyad2[dyadDist<=500 & dSI>=500] #choose not to be social (1412/9049; 15.6% of potentail to be social choose not to be)
nl.dyad5<-nl.dyad2[dyadDist>500] # did not have potential to be social (112159/121208; 92.5%)

rmnp.dyad3<-rmnp.dyad2[dyadDist<=500 & dSI<=500] #choose to be social (2147/2291; 93.7% of potential to be social were)
rmnp.dyad4<-rmnp.dyad2[dyadDist<=500 & dSI>=500] #choose not to be social (144/2291; 6.3% of potentail to be social choose not to be)
rmnp.dyad5<-rmnp.dyad2[dyadDist>500] # did not have potential to be social (14477/16768; 86.3%)

### data to summarize, test for correlation, etc.

### need to add nWithin500
nl.dyad3.1<-nl.dyad3[, c("CarRSF","CoyRSF","BearRSF","absAngle","stepLength","predatorRSF","season","dyadID",
       "julday","yr","dyadDist","dSI","dAbsAng","bin500m",
       "rCarRSF","rCoyRSF","rBearRSF","rabsAngle","rstepLength","rpredatorRSF",
       "dCarRSF","dCoyRSF","dBearRSF","dPredRSF",
       "avgCarRSF","avgCoyRSF","avgBearRSF","avgPredRSF"), with=FALSE]

rmnp.dyad3.1<-rmnp.dyad3[, c("absAngle","stepLength","predatorRSF","preyRSF","season","dyadID",
                           "julday","yr","dyadDist","dSI","dAbsAng","bin500m","nWithin500",
                           "rabsAngle","rstepLength","rpredatorRSF","rpreyRSF",
                           "dPredRSF","dPreyRSF","avgPredRSF","avgPreyRSF"), with=FALSE]

rmnp.dyad3.1$season <- as.factor(rmnp.dyad3.1$season)
rmnp.dyad3.1$dyadID <- as.factor(rmnp.dyad3.1$dyadID)
rmnp.dyad3.1$julday <- as.factor(rmnp.dyad3.1$julday)
rmnp.dyad3.1$yr <- as.factor(rmnp.dyad3.1$yr)
rmnp.dyad3.1$bin500m <- as.factor(rmnp.dyad3.1$bin500m)
rmnp.dyad3.1$nWithin500 <- as.factor(rmnp.dyad3.1$nWithin500)

nl.dyad3.1$season <- as.factor(nl.dyad3.1$season)
nl.dyad3.1$dyadID <- as.factor(nl.dyad3.1$dyadID)
nl.dyad3.1$julday <- as.factor(nl.dyad3.1$julday)
nl.dyad3.1$yr <- as.factor(nl.dyad3.1$yr)
nl.dyad3.1$bin500m <- as.factor(nl.dyad3.1$bin500m)
#nl.dyad3.1$nWithin500 <- as.factor(nl.dyad3.1$nWithin500) ### update after adding niwthin500


### need to fix dAbsAng it is giving us incorrect estimate of socialness AbsAng1 = 179 and AbsAng2 = -179, currently dAbsAng = 358 (highly unsocial)
### but these two angle actually suggest a highly social dyad, should be 2

### solution, I think is

### dAbsAng.mod = 180 - abs(abs(AbsAng1-AbsAng2)-180)

rmnp.dyad3.1$dAbsAng.trnsfrm<-180 - abs(abs(rmnp.dyad3.1$absAngle-rmnp.dyad3.1$rabsAngle)-180)
nl.dyad3.1$dAbsAng.trnsfrm<-180 - abs(abs(nl.dyad3.1$absAngle-nl.dyad3.1$rabsAngle)-180)


### Further, I have also approximated the DI value for each dyad based on the formula from Long and Nelson 2013
### I say approximated because we have turning angle as absolute (-180 to 180) and DI uses azimuth (which I think is a little different)

### Long and Nelson also suggest what to do when azimuth is NA, i.e. animal does not move
### however, I think are NAs originate because the animal did not have three locations available to calculate AbsAng
### because in all cases of NAs in either AbsAng or rAbsAng, the steplength is >0

### if these were NAs because of not moving Long and Nelson recommend if both members of dyad dont move DI = 1 if only one moves DI = 0

rmnp.dyad3.1$DI.ang<-(-1*(rmnp.dyad3.1$dAbsAng.trnsfrm-90)/90)
rmnp.dyad3.1$DI.dist<-(1-((rmnp.dyad3.1$dSI/(rmnp.dyad3.1$stepLength+rmnp.dyad3.1$rstepLength))^1))
rmnp.dyad3.1$DI<-rmnp.dyad3.1$DI.ang*rmnp.dyad3.1$DI.dist

nl.dyad3.1$DI.ang<-(-1*(nl.dyad3.1$dAbsAng.trnsfrm-90)/90)
nl.dyad3.1$DI.dist<-(1-((nl.dyad3.1$dSI/(nl.dyad3.1$stepLength+nl.dyad3.1$rstepLength))^1))
nl.dyad3.1$DI<-nl.dyad3.1$DI.ang*nl.dyad3.1$DI.dist

### DI is on scale of -1 (asocial) to 1 (social)
### should probably put this fix in earlier code segments

RMNP_cor<-c("predatorRSF","preyRSF","dPredRSF","dPreyRSF","dSI","dAbsAng.trnsfrm","avgPredRSF","avgPreyRSF","dyadDist","DI")
NL_cor<-c("predatorRSF","CarRSF","CoyRSF","BearRSF","dPredRSF","dCarRSF","dCoyRSF","dBearRSF","dSI","dAbsAng.trnsfrm","avgPredRSF","avgCarRSF","avgCoyRSF","avgBearRSF","dyadDist","DI")

summary(rmnp.dyad3.1[,RMNP_cor, with=FALSE])
summary(nl.dyad3.1[,NL_cor, with=FALSE])

### PredRSF and PreyRSF values are not quite on the same scale, going to standardize them at the 'avg' stages
### also standardized but did not center the dependents
### but not DI since it is already on -1 to 1 and I think it is better to leave the values of DI comparable between datasets

rmnp.dyad3.1$z.avgPreyRSF<-scale(rmnp.dyad3.1$avgPreyRSF, center=T, scale=T)
rmnp.dyad3.1$z.avgPredRSF<-scale(rmnp.dyad3.1$avgPredRSF, center=T, scale=T)

rmnp.dyad3.1$z.dSI<-scale(rmnp.dyad3.1$dSI, center=F, scale=T)
rmnp.dyad3.1$z.dyadDist<-scale(rmnp.dyad3.1$dyadDist, center=F, scale=T)
rmnp.dyad3.1$z.dAbsAng.trnsfrm<-scale(rmnp.dyad3.1$dAbsAng.trnsfrm, center=F, scale=T)

nl.dyad3.1$z.avgCarRSF<-scale(nl.dyad3.1$avgCarRSF, center=T, scale=T)
nl.dyad3.1$z.avgPredRSF<-scale(nl.dyad3.1$avgPredRSF, center=T, scale=T)

nl.dyad3.1$z.avgCoyRSF<-scale(nl.dyad3.1$avgCoyRSF, center=T, scale=T)
nl.dyad3.1$z.avgBearRSF<-scale(nl.dyad3.1$avgBearRSF, center=T, scale=T)

nl.dyad3.1$z.dSI<-scale(nl.dyad3.1$dSI, center=F, scale=T)
nl.dyad3.1$z.dyadDist<-scale(nl.dyad3.1$dyadDist, center=F, scale=T)
nl.dyad3.1$z.dAbsAng.trnsfrm<-scale(nl.dyad3.1$dAbsAng.trnsfrm, center=F, scale=T)


#### Testing for correlations in the data

rcorr(as.matrix(rmnp.dyad3.1[,RMNP_cor, with=FALSE])) 
rcorr(as.matrix(nl.dyad3.1[,NL_cor, with=FALSE]))

### No unexpected correlations
### note the strong correlation between dAbsAng.trnsfrm and DI


### plots of data

## FULL
## RMNP

boxplot(rmnp.dyad3.1$dyadDist)
hist(rmnp.dyad3.1$dyadDist)

boxplot(rmnp.dyad3.1$dSI)
hist(rmnp.dyad3.1$dSI)

boxplot(rmnp.dyad3.1$dAbsAng.trnsfrm)
hist(rmnp.dyad3.1$dAbsAng.trnsfrm)

boxplot(rmnp.dyad3.1$DI)
hist(rmnp.dyad3.1$DI)

boxplot(rmnp.dyad3.1$avgPredRSF)
hist(rmnp.dyad3.1$avgPredRSF)

boxplot(rmnp.dyad3.1$avgPreyRSF)
hist(rmnp.dyad3.1$avgPreyRSF)

boxplot(rmnp.dyad3.1$z.avgPredRSF)
hist(rmnp.dyad3.1$z.avgPredRSF)

boxplot(rmnp.dyad3.1$z.avgPreyRSF)
hist(rmnp.dyad3.1$z.avgPreyRSF)

## NL

boxplot(nl.dyad3.1$dyadDist)
hist(nl.dyad3.1$dyadDist)

boxplot(nl.dyad3.1$dSI)
hist(nl.dyad3.1$dSI)

boxplot(nl.dyad3.1$dAbsAng.trnsfrm)
hist(nl.dyad3.1$dAbsAng.trnsfrm)

boxplot(nl.dyad3.1$DI)
hist(nl.dyad3.1$DI)

boxplot(nl.dyad3.1$avgCoyRSF)
hist(nl.dyad3.1$avgCoyRSF)

boxplot(nl.dyad3.1$avgBearRSF)
hist(nl.dyad3.1$avgBearRSF)

boxplot(nl.dyad3.1$avgPredRSF)
hist(nl.dyad3.1$avgPredRSF)

boxplot(nl.dyad3.1$avgCarRSF)
hist(nl.dyad3.1$avgCarRSF)


boxplot(nl.dyad3.1$z.avgCoyRSF)
hist(nl.dyad3.1$z.avgCoyRSF)

boxplot(nl.dyad3.1$z.avgBearRSF)
hist(nl.dyad3.1$z.avgBearRSF)

boxplot(nl.dyad3.1$z.avgPredRSF)
hist(nl.dyad3.1$z.avgPredRSF)

boxplot(nl.dyad3.1$z.avgCarRSF)
hist(nl.dyad3.1$z.avgCarRSF)


plot(nl.dyad3.1$season)
plot(rmnp.dyad3.1$season)

### pratically no spring RMNP data

plot(nl.dyad3.1$julday)
plot(rmnp.dyad3.1$julday)

plot(nl.dyad3.1$dyadID)
plot(rmnp.dyad3.1$dyadID)

plot(nl.dyad3.1$yr)
plot(rmnp.dyad3.1$yr)

### only 2016 for RMNP

#### nwithin is special needs a different subset of data
# update NL when nwithin500 is added
plot(nl.dyad3.1$nWithin500)
plot(rmnp.dyad3.1$nWithin500)


library(visreg)
library(rgl)

###GLM
# general form SA <- glm(SocialMeasure ~ PreyHD*PredHD, data = rmnp)
## social measures are: dSI, dyadDist, dAbsAng, DI

### nWithin5000 is a special social measure that will use a different dataset

## RMNP
#dsl

rmNN.dsl<-
  glm(z.dSI ~ z.avgPreyRSF*z.avgPredRSF,
      data = rmnp.dyad3.1)

#dyadDist

rmNN.dd<-
  glm(z.dyadDist ~ z.avgPreyRSF*z.avgPredRSF,
      data = rmnp.dyad3.1)


#dAbsAng.trnsfrm

rmNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgPreyRSF*z.avgPredRSF,
      data = rmnp.dyad3.1)

#DI

rmNN.DI<-
  glm(DI ~ z.avgPreyRSF*z.avgPredRSF,
      data = rmnp.dyad3.1)

### RMNP is mostly winter data

RMNP_win<-rmnp.dyad3.1[season=="winter"]
RMNP_spr<-rmnp.dyad3.1[season=="spring"]

RMNP_win$z.avgPreyRSF<-scale(RMNP_win$avgPreyRSF, center=T, scale=T)
RMNP_win$z.avgPredRSF<-scale(RMNP_win$avgPredRSF, center=T, scale=T)

RMNP_spr$z.avgPreyRSF<-scale(RMNP_spr$avgPreyRSF, center=T, scale=T)
RMNP_spr$z.avgPredRSF<-scale(RMNP_spr$avgPredRSF, center=T, scale=T)

RMNP_win$z.dSI<-scale(RMNP_win$dSI, center=F, scale=T)
RMNP_win$z.dyadDist<-scale(RMNP_win$dyadDist, center=F, scale=T)
RMNP_win$z.dAbsAng.trnsfrm<-scale(RMNP_win$dAbsAng.trnsfrm, center=F, scale=T)

RMNP_spr$z.dSI<-scale(RMNP_spr$dSI, center=F, scale=T)
RMNP_spr$z.dyadDist<-scale(RMNP_spr$dyadDist, center=F, scale=T)
RMNP_spr$z.dAbsAng.trnsfrm<-scale(RMNP_spr$dAbsAng.trnsfrm, center=F, scale=T)

## WINTER
#dsl

rm_winNN.dsl<-
  glm(z.dSI ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)

#dyadDist

rm_winNN.dd<-
  glm(z.dyadDist ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)


#dAbsAng.trnsfrm

rm_winNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)

#DI

rm_winNN.DI<-
  glm(DI ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)

## SPRING
#dsl

rm_sprNN.dsl<-
  glm(z.dSI ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_spr)

#dyadDist

rm_sprNN.dd<-
  glm(z.dyadDist ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_spr)


#dAbsAng.trnsfrm

rm_sprNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_spr)

#DI

rm_sprNN.DI<-
  glm(DI ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_spr)






### Investigating results
### RMNP
summary(RMNP_win[,c(9,10,13,20:30)])
summary(RMNP_spr[,c(9,10,13,20:30)])
summary(RMNP_win$dyadID)
summary(RMNP_spr$dyadID)

summary(RMNP_win$DI)
summary(RMNP_spr$DI)  ### elk more social in winter not social in spring

### no overlap in dyad id's between seasons

### my inclination is to trust the RMNP_win results over both seasons because spring does appear to be different and uses different dyads


#### winter
# smaller values of dSI, dd, dAA are more social
# larger values of DI are more social

summary(rm_winNN.dsl)  ### elk less social in good elk habitat
summary(rm_winNN.dd)   ### weak trend elk more social in good wolf habitat
summary(rm_winNN.dAA)  ### elk more social in good elk habitat
summary(rm_winNN.DI)   ### elk more social in good elk habitat, weak trends for less social in good wolf habitat and more social in elk-wolf domain

visreg2d(rm_winNN.DI, "z.avgPreyRSF", "z.avgPredRSF", plot.type="image") 

### I think DI is most robust
### suggesting elk more social in good elk habitat

### otherwise some conflcting results - step length suggests (less social) but turning angle suggests more social in good elk habitat
###                                   - perhaps dyad distance is closer in good wolf habitat but support is weak


## NL
#dsl

nlNN.dsl<-
  glm(z.dSI ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)

#dyadDist

nlNN.dd<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)


#dAbsAng.trnsfrm

nlNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)

#DI

nlNN.DI<-
  glm(DI ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)

### NL is mostly evenly split between winter and spring

NL_win<-nl.dyad3.1[season=="winter"]
NL_spr<-nl.dyad3.1[season=="spring"]

NL_win$z.avgCarRSF<-scale(NL_win$avgCarRSF, center=T, scale=T)
NL_win$z.avgPredRSF<-scale(NL_win$avgPredRSF, center=T, scale=T)

NL_win$z.avgCoyRSF<-scale(NL_win$avgCoyRSF, center=T, scale=T)
NL_win$z.avgBearRSF<-scale(NL_win$avgBearRSF, center=T, scale=T)

NL_spr$z.avgCarRSF<-scale(NL_spr$avgCarRSF, center=T, scale=T)
NL_spr$z.avgPredRSF<-scale(NL_spr$avgPredRSF, center=T, scale=T)

NL_spr$z.avgCoyRSF<-scale(NL_spr$avgCoyRSF, center=T, scale=T)
NL_spr$z.avgBearRSF<-scale(NL_spr$avgBearRSF, center=T, scale=T)

NL_win$z.dSI<-scale(NL_win$dSI, center=F, scale=T)
NL_win$z.dyadDist<-scale(NL_win$dyadDist, center=F, scale=T)
NL_win$z.dAbsAng.trnsfrm<-scale(NL_win$dAbsAng.trnsfrm, center=F, scale=T)

NL_spr$z.dSI<-scale(NL_spr$dSI, center=F, scale=T)
NL_spr$z.dyadDist<-scale(NL_spr$dyadDist, center=F, scale=T)
NL_spr$z.dAbsAng.trnsfrm<-scale(NL_spr$dAbsAng.trnsfrm, center=F, scale=T)


## WINTER
#dsl

nl_winNN.dsl<-
  glm(z.dSI ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)

#dyadDist

nl_winNN.dd<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)


#dAbsAng.trnsfrm

nl_winNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)

#DI

nl_winNN.DI<-
  glm(DI ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)

## SPRING
#dsl

nl_sprNN.dsl<-
  glm(z.dSI ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_spr)

#dyadDist

nl_sprNN.dd<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_spr)


#dAbsAng.trnsfrm

nl_sprNN.dAA<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_spr)


#DI

nl_sprNN.DI<-
  glm(DI ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_spr)


## NL Coyote (year round)
#dsl

nlNN.dsl_coy<-
  glm(z.dSI ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)

#dyadDist

nlNN.dd_coy<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)


#dAbsAng.trnsfrm

nlNN.dAA_coy<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)

#DI

nlNN.DI_coy<-
  glm(DI ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)

#NL
## predator: bear/coyote only in spring (bears not active in winter so winter is the same as coyote winter)

## NL Coyote
### NL_spr
#dsl

nlNN.dsl_coy_spr<-
  glm(z.dSI ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_spr)

#dyadDist

nlNN.dd_coy_spr<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_spr)


#dAbsAng.trnsfrm

nlNN.dAA_coy_spr<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_spr)


#DI

nlNN.DI_coy_spr<-
  glm(DI ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_spr)

## NL Bear
### NL_spr
#dsl

nlNN.dsl_Bear_spr<-
  glm(z.dSI ~ z.avgCarRSF*z.avgBearRSF,
      data = NL_spr)

#dyadDist

nlNN.dd_Bear_spr<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgBearRSF,
      data = NL_spr)


#dAbsAng.trnsfrm

nlNN.dAA_Bear_spr<-
  glm(z.dAbsAng.trnsfrm ~ z.avgCarRSF*z.avgBearRSF,
      data = NL_spr)

#DI

nlNN.DI_Bear_spr<-
  glm(DI ~ z.avgCarRSF*z.avgBearRSF,
      data = NL_spr)





### NL

summary(nlNN.dsl)  ### caribou more social where good caribou and predator habitat & in good predator habitat; weak trend for more social in good caribou habitat
                    #### new results caribou more in good caribou habitat and where caribou and pred habitat is good

summary(nlNN.dd)   ### caribou less social in good caribou habitat and in good predator habitat but caribou more social where good caribou and predator habitat
                    #### new results caribou less social in good predator habitat but more social where good caribou and predator habitat

summary(nlNN.dAA)  ### caribou less social in good predator habitat
                    #### new results caribou less social in good caribou and good predator habitat and weak trend for less social in good caribou and predator habitat

summary(nlNN.DI)   ### caribou less social in good predator habitat and weak trend for less social in caribou-predator domain
                       #### new results caribou less social in good caribou and good predator habitat and weak trend for less social in good caribou and predator habitat

visreg2d(nlNN.DI, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")

#### winter
summary(nl_winNN.dsl)  ### caribou more social in good predator habitat
summary(nl_winNN.dd)   ### caribou less social in good caribou habitat but more social in caribou-predator domain
summary(nl_winNN.dAA)  ### caribou less social in good caribou habitat and in good predator habitat but more social in caribou-predator domain
summary(nl_winNN.DI)   ### caribou less social in good caribou habitat and in good predator habitat but more social in caribou-predator domain

visreg2d(nl_winNN.DI, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")

#### spring
summary(nl_sprNN.dsl)  ### weak trend for more social in good caribou habitat
#### new results nothing significant
summary(nl_sprNN.dd)   ### caribou more social in good caribou habitat but less social in good predator habitat and where good caribou and predator habitat
summary(nl_sprNN.dAA)  ### caribou less social in good predator habitat and weak trend for less social where good caribou and predator habitat 
#### new results nothing significant
summary(nl_sprNN.DI)   ### no trends in spring

### more social in winter than spring
visreg2d(nl_sprNN.DI, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")
visreg2d(nl_sprNN.dd, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")


### bear (only in spring)
summary(nlNN.dsl_Bear_spr)  ### caribou more social where good caribou and bear habitat
summary(nlNN.dd_Bear_spr)   ### caribou more social in good caribou habitat but less social in good bear habitat and where good caribou and bear habitat
summary(nlNN.dAA_Bear_spr)  ### caribou less social in good bear habitat
summary(nlNN.DI_Bear_spr)   ### weak trends only for less social in good caribou and good bear habitat

visreg2d(nlNN.DI_Bear_spr, "z.avgCarRSF", "z.avgBearRSF", plot.type="image")


### coyote (spring)
summary(nlNN.dsl_coy_spr)  ### weak trend for more social in good caribou habitat
summary(nlNN.dd_coy_spr)   ### caribou more social in good caribou habitat but less social in good coyote habitat and where good caribou and coyote habitat
summary(nlNN.dAA_coy_spr)  ### caribou less social where good caribou and good coyote habitat
#### new results nothing significant
summary(nlNN.DI_coy_spr)   ### weak trend for less social in good caribou habitat
#### new results nothing significant

visreg2d(nlNN.DI_coy_spr, "z.avgCarRSF", "z.avgCoyRSF", plot.type="image")

### coyote (winter) is the same as just winter

### coyote (year-round)
summary(nlNN.dsl_coy)  ### caribou more social where good coyote habitat and weak trend for more social where caribou and coyote habitat good
### new results caribou more social where good coyote habitat and more social where caribou and coyote habitat good

summary(nlNN.dd_coy)   ### caribou less social in good coyote habitat but more social where good caribou and coyote habitat and weak trend for less social in good caribou habitat
summary(nlNN.dAA_coy)  ### caribou less social in good coyote habitat but weak trend for more social in good caribou habitat
### new results caribou less social in good caribou habitat, good coyote habitat and less social where caribou and coyote habitat good

summary(nlNN.DI_coy)   ### caribou less social in good coyote habitat and weak trend for less social in caribou-coyote domain
### new results caribou less social in good caribou habitat, good coyote habitat and less social where caribou and coyote habitat good

visreg2d(nlNN.DI_coy, "z.avgCarRSF", "z.avgCoyRSF", plot.type="image")


### Key findings of NL - big difference between seasons - caribou not as social (DI) in spring and no relationships between DI and habitat in spring
### in winter caribou less social in good caribou habitat and in good coyote habitat but more social in caribou-coyote domain (supported by DI, dAA, dd)
### however in spring caribou dyad distance was lower when in good caribou habitat and higher when in good predator habitat or caribou-predator domain
### when seperating coyote and bear in spring not much difference in estimates of sociality

### results of NL (year-round) and coyote (year-round) I think are not robust (clearly seasonal differences exist)
### dSL likely not a good measurement on its own - but disagrees with results of other three

### I think we could focus our results and any further analysis just on DI and dyad distance
### and are more easily interpretable and have more support in literature

### Key plots

### winter DI RMNP
visreg2d(rm_winNN.DI, "z.avgPreyRSF", "z.avgPredRSF", plot.type="image") 

### winter DI NL
visreg2d(nl_winNN.DI, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")



### spring DD
visreg2d(nl_sprNN.dd, "z.avgCarRSF", "z.avgPredRSF", plot.type="image")











### what is this???
#NL file
# nl <- 
dd <- data.table(sapply(seq(0, 1000, by = 50), function(x){
  nrow(unique(rmnp[dyadDist < x, .(dyadID, timegroup, dSI)]))
}), 
seq(0,1000,by = 50))

ggplot(dd) + 
  geom_line(aes(V2, V1)) + 
  labs(x = 'dyadDist', y = 'n rows')

unique(rmnp[dyadDist < 150, .(dyadID, timegroup)])


#rmnp[dyadDist < 500, qplot()
