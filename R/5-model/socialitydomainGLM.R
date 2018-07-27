### SOCIALITY ~ DOMAIN MODEL ----
# Authors: Alec Robitaille, Christina M Prokopenko
# Purpose: 
# Inputs: sociality measures, predator and prey RSFs for RMNP and NL
# Outputs:
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

libs <- c('data.table',  
          'lme4',
          'ggplot2',
          'Hmisc')
lapply(libs, require, character.only = TRUE)

### Input data ----

#RMNP file

rmnp <- readRDS('output/nna/elkNNA.Rds')

nl <- readRDS('output/nna/caribouNNA.Rds')

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
nl.dyad3.1<-nl.dyad3[, c("CarRSF","CoyRSF","BearRSF","absAngle","relAngle","stepLength","predatorRSF","season","dyadID",
       "julday","yr","dyadDist","dSI","dAbsAng","bin500m",
       "rCarRSF","rCoyRSF","rBearRSF","rabsAngle","rrelAngle","rstepLength","rpredatorRSF",
       "dCarRSF","dCoyRSF","dBearRSF","dPredRSF",
       "avgCarRSF","avgCoyRSF","avgBearRSF","avgPredRSF"), with=FALSE]

rmnp.dyad3.1<-rmnp.dyad3[, c("absAngle","relAngle","stepLength","predatorRSF","preyRSF","season","dyadID",
                           "julday","yr","dyadDist","dSI","dAbsAng","bin500m","nWithin500",
                           "rabsAngle","rrelAngle","rstepLength","rpredatorRSF","rpreyRSF",
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



RMNP_cor<-c("predatorRSF","preyRSF","dPredRSF","dPreyRSF","dSI","dAbsAng","avgPredRSF","avgPreyRSF","dyadDist")
NL_cor<-c("predatorRSF","CarRSF","CoyRSF","BearRSF","dPredRSF","dCarRSF","dCoyRSF","dBearRSF","dSI","dAbsAng","avgPredRSF","avgCarRSF","avgCoyRSF","avgBearRSF","dyadDist")

summary(rmnp.dyad3.1[,RMNP_cor, with=FALSE])
summary(nl.dyad3.1[,NL_cor, with=FALSE])

### PredRSF and PreyRSF values are not quite on the same scale, going to standardize them at the 'avg' stages

rmnp.dyad3.1$z.avgPreyRSF<-scale(rmnp.dyad3.1$avgPreyRSF, center=T, scale=T)
rmnp.dyad3.1$z.avgPredRSF<-scale(rmnp.dyad3.1$avgPredRSF, center=T, scale=T)

rmnp.dyad3.1$z.dSI<-scale(rmnp.dyad3.1$dSI, center=T, scale=T)
rmnp.dyad3.1$z.dyadDist<-scale(rmnp.dyad3.1$dyadDist, center=T, scale=T)
rmnp.dyad3.1$z.dAbsAng<-scale(rmnp.dyad3.1$dAbsAng, center=T, scale=T)

nl.dyad3.1$z.avgCarRSF<-scale(nl.dyad3.1$avgCarRSF, center=T, scale=T)
nl.dyad3.1$z.avgPredRSF<-scale(nl.dyad3.1$avgPredRSF, center=T, scale=T)

nl.dyad3.1$z.avgCoyRSF<-scale(nl.dyad3.1$avgCoyRSF, center=T, scale=T)
nl.dyad3.1$z.avgBearRSF<-scale(nl.dyad3.1$avgBearRSF, center=T, scale=T)

nl.dyad3.1$z.dSI<-scale(nl.dyad3.1$dSI, center=T, scale=T)
nl.dyad3.1$z.dyadDist<-scale(nl.dyad3.1$dyadDist, center=T, scale=T)
nl.dyad3.1$z.dAbsAng<-scale(nl.dyad3.1$dAbsAng, center=T, scale=T)



#### Testing for correlations in the data

rcorr(as.matrix(rmnp.dyad3.1[,RMNP_cor, with=FALSE])) 
rcorr(as.matrix(nl.dyad3.1[,NL_cor, with=FALSE]))

### No unexpected correlations


### plots of data

## FULL
## RMNP

boxplot(rmnp.dyad3.1$dyadDist)
hist(rmnp.dyad3.1$dyadDist)

boxplot(rmnp.dyad3.1$dSI)
hist(rmnp.dyad3.1$dSI)

boxplot(rmnp.dyad3.1$dAbsAng)
hist(rmnp.dyad3.1$dAbsAng)

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

boxplot(nl.dyad3.1$dAbsAng)
hist(nl.dyad3.1$dAbsAng)

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



###GLM
# general form SA <- glm(SocialMeasure ~ PreyHD*PredHD, data = rmnp)
## social measures are: dSI, dyadDist, dAbsAng

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


#dAbsAng

rmNN.dAA<-
  glm(z.dAbsAng ~ z.avgPreyRSF*z.avgPredRSF,
      data = rmnp.dyad3.1)

### RMNP is mostly winter data

RMNP_win<-rmnp.dyad3.1[season=="winter"]
RMNP_spr<-rmnp.dyad3.1[season=="spring"]

RMNP_win$z.avgPreyRSF<-scale(RMNP_win$avgPreyRSF, center=T, scale=T)
RMNP_win$z.avgPredRSF<-scale(RMNP_win$avgPredRSF, center=T, scale=T)

RMNP_spr$z.avgPreyRSF<-scale(RMNP_spr$avgPreyRSF, center=T, scale=T)
RMNP_spr$z.avgPredRSF<-scale(RMNP_spr$avgPredRSF, center=T, scale=T)

RMNP_win$z.dSI<-scale(RMNP_win$dSI, center=T, scale=T)
RMNP_win$z.dyadDist<-scale(RMNP_win$dyadDist, center=T, scale=T)
RMNP_win$z.dAbsAng<-scale(RMNP_win$dAbsAng, center=T, scale=T)

RMNP_spr$z.dSI<-scale(RMNP_spr$dSI, center=T, scale=T)
RMNP_spr$z.dyadDist<-scale(RMNP_spr$dyadDist, center=T, scale=T)
RMNP_spr$z.dAbsAng<-scale(RMNP_spr$dAbsAng, center=T, scale=T)

## WINTER
#dsl

rm_winNN.dsl<-
  glm(z.dSI ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)

#dyadDist

rm_winNN.dd<-
  glm(z.dyadDist ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_win)


#dAbsAng

rm_winNN.dAA<-
  glm(z.dAbsAng ~ z.avgPreyRSF*z.avgPredRSF,
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


#dAbsAng

rm_sprNN.dAA<-
  glm(z.dAbsAng ~ z.avgPreyRSF*z.avgPredRSF,
      data = RMNP_spr)


## NL
#dsl

nlNN.dsl<-
  glm(z.dSI ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)

#dyadDist

nlNN.dd<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgPredRSF,
      data = nl.dyad3.1)


#dAbsAng

nlNN.dAA<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgPredRSF,
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

NL_win$z.dSI<-scale(NL_win$dSI, center=T, scale=T)
NL_win$z.dyadDist<-scale(NL_win$dyadDist, center=T, scale=T)
NL_win$z.dAbsAng<-scale(NL_win$dAbsAng, center=T, scale=T)

NL_spr$z.dSI<-scale(NL_spr$dSI, center=T, scale=T)
NL_spr$z.dyadDist<-scale(NL_spr$dyadDist, center=T, scale=T)
NL_spr$z.dAbsAng<-scale(NL_spr$dAbsAng, center=T, scale=T)


## WINTER
#dsl

nl_winNN.dsl<-
  glm(z.dSI ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)

#dyadDist

nl_winNN.dd<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_win)


#dAbsAng

nl_winNN.dAA<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgPredRSF,
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


#dAbsAng

nl_sprNN.dAA<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgPredRSF,
      data = NL_spr)





#NL
## predator: bear/coyote

## NL Coyote
#dsl

nlNN.dsl_coy<-
  glm(z.dSI ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)

#dyadDist

nlNN.dd_coy<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)


#dAbsAng

nlNN.dAA_coy<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgCoyRSF,
      data = nl.dyad3.1)


#NL
## predator: bear/coyote

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


#dAbsAng

nlNN.dAA_coy_spr<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_spr)



#NL
## predator: bear/coyote

## NL Coyote
### NL_win
#dsl

nlNN.dsl_coy_win<-
  glm(z.dSI ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_win)

#dyadDist

nlNN.dd_coy_win<-
  glm(z.dyadDist ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_win)


#dAbsAng

nlNN.dAA_coy_win<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgCoyRSF,
      data = NL_win)


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


#dAbsAng

nlNN.dAA_Bear_spr<-
  glm(z.dAbsAng ~ z.avgCarRSF*z.avgBearRSF,
      data = NL_spr)



### Investigating results
### RMNP

summary(rmNN.dsl)  ### elk more social in good elk habitat
summary(rmNN.dd)   ### elk more social where good elk and wolf habitat
summary(rmNN.dAA)  ### elk more social where good elk and wolf habitat

#### winter
summary(rm_winNN.dsl)  ### elk more social in good elk habitat
summary(rm_winNN.dd)   
summary(rm_winNN.dAA)  

#### spring, n = 48
summary(rm_sprNN.dsl)  
summary(rm_sprNN.dd)   ### elk less social where good elk but more social in good wolf habitat and where good elk and wolf habitat
summary(rm_sprNN.dAA)  


### NL

summary(nlNN.dsl)  ### caribou less social where good caribou and predator habitat & in good predator habitat
summary(nlNN.dd)   ### caribou more social in good caribou habitat and in good predator habitat but caribou less social where good caribou and predator habitat
summary(nlNN.dAA)  ### caribou less social in good caribou habitat but more social in good predator habitat

#### winter
summary(nl_winNN.dsl)  ### caribou less social in good predator habitat
summary(nl_winNN.dd)   ### caribou more social in good caribou habitat but less social where good caribou and good predator habitat
summary(nl_winNN.dAA)  ### caribou more social in good predator habitat

#### spring
summary(nl_sprNN.dsl)  
summary(nl_sprNN.dd)   ### caribou less social in good caribou habitat but more social in good predator habitat and where good caribou and predator habitat
summary(nl_sprNN.dAA)  ### caribou more social in good caribou habitat and in good predator habitat

### bear (only in spring)
summary(nlNN.dsl_Bear_spr)  ### caribou less social where good caribou and bear habitat
summary(nlNN.dd_Bear_spr)   ### caribou less social in good caribou habitat but more social in good bear habitat and where good caribou and bear habitat
summary(nlNN.dAA_Bear_spr)  ### caribou more social in good bear habitat

### coyote (year-round)
summary(nlNN.dsl_coy)  ### caribou less social where good coyote habitat
summary(nlNN.dd_coy)   ### caribou more social in good coyote habitat but less social where good caribou and coyote habitat
summary(nlNN.dAA_coy)  ### caribou less social in good caribou habitat but more social in good coyote habitat

### coyote (spring)
summary(nlNN.dsl_coy_spr)  ### 
summary(nlNN.dd_coy_spr)   ### caribou less social in good caribou habitat but more social in good coyote habitat and where good caribou and coyote habitat
summary(nlNN.dAA_coy_spr)  ### caribou more social in good caribou habitat and where good caribou and good coyote habitat

### coyote (winter)
summary(nlNN.dsl_coy_win)  ### caribou less social where good coyote habitat
summary(nlNN.dd_coy_win)   ### caribou more social in good caribou habitat but less social where good caribou and coyote habitat
summary(nlNN.dAA_coy_win)  ### caribou more social in good coyote habitat


###Plot
#heatmap of social response across domains 
#prey on Y, predator on X, hot colours is positive social response, cold is negative

library(visreg)
library(rgl)

visreg2d(nlNN.dd_coy_spr, "z.avgCarRSF", "z.avgCoyRSF", plot.type="image")
visreg2d(nlNN.dd_coy_win, "z.avgCarRSF", "z.avgCoyRSF", plot.type="image")
visreg2d(nlNN.dd_Bear_spr, "z.avgCarRSF", "z.avgBearRSF", plot.type="image")






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
