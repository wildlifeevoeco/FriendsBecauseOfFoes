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

### PredRSF and PreyRSF values are not quite on the same scale, going to standardize them at the 'd' and 'avg' stages, also including transformation to proportion

rmnp.dyad3.1$sc_dPreyRSF<-scale(rmnp.dyad3.1$dPreyRSF, center=T, scale=T)
rmnp.dyad3.1$sc_dPredRSF<-scale(rmnp.dyad3.1$dPredRSF, center=T, scale=T)

rmnp.dyad3.1$sc_avgPreyRSF<-scale(rmnp.dyad3.1$avgPreyRSF, center=T, scale=T)
rmnp.dyad3.1$sc_avgPredRSF<-scale(rmnp.dyad3.1$avgPredRSF, center=T, scale=T)

nl.dyad3.1$sc_dCarRSF<-scale(nl.dyad3.1$dCarRSF, center=T, scale=T)
nl.dyad3.1$sc_dPredRSF<-scale(nl.dyad3.1$dPredRSF, center=T, scale=T)

nl.dyad3.1$sc_avgCarRSF<-scale(nl.dyad3.1$avgCarRSF, center=T, scale=T)
nl.dyad3.1$sc_avgPredRSF<-scale(nl.dyad3.1$avgPredRSF, center=T, scale=T)

nl.dyad3.1$sc_dCoyRSF<-scale(nl.dyad3.1$dCoyRSF, center=T, scale=T)
nl.dyad3.1$sc_dBearRSF<-scale(nl.dyad3.1$dBearRSF, center=T, scale=T)

nl.dyad3.1$sc_avgCoyRSF<-scale(nl.dyad3.1$avgCoyRSF, center=T, scale=T)
nl.dyad3.1$sc_avgBearRSF<-scale(nl.dyad3.1$avgBearRSF, center=T, scale=T)


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

boxplot(rmnp.dyad3.1$sc_avgPredRSF)
hist(rmnp.dyad3.1$sc_avgPredRSF)

boxplot(rmnp.dyad3.1$sc_avgPreyRSF)
hist(rmnp.dyad3.1$sc_avgPreyRSF)

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


boxplot(nl.dyad3.1$sc_avgCoyRSF)
hist(nl.dyad3.1$sc_avgCoyRSF)

boxplot(nl.dyad3.1$sc_avgBearRSF)
hist(nl.dyad3.1$sc_avgBearRSF)

boxplot(nl.dyad3.1$sc_avgPredRSF)
hist(nl.dyad3.1$sc_avgPredRSF)

boxplot(nl.dyad3.1$sc_avgCarRSF)
hist(nl.dyad3.1$sc_avgCarRSF)


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
rmNN.dsl_avg<-
  glm(dSI ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad3.1)


rmNN.dsl_scavg<-
  glm(dSI ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = rmnp.dyad3.1)

#dyadDist
rmNN.dd_avg<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad3.1)


rmNN.dd_scavg<-
  glm(dyadDist ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = rmnp.dyad3.1)


#dAbsAng
rmNN.dAA_avg<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad3.1)


rmNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = rmnp.dyad3.1)

### RMNP is mostly winter data

RMNP_win<-rmnp.dyad3.1[season=="winter"]
RMNP_spr<-rmnp.dyad3.1[season=="spring"]


## WINTER
#dsl
rm_winNN.dsl_avg<-
  glm(dSI ~ avgPreyRSF*avgPredRSF,
      data = RMNP_win)


rm_winNN.dsl_scavg<-
  glm(dSI ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_win)

#dyadDist
rm_winNN.dd_avg<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = RMNP_win)


rm_winNN.dd_scavg<-
  glm(dyadDist ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_win)


#dAbsAng
rm_winNN.dAA_avg<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = RMNP_win)


rm_winNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_win)


## SPRING
#dsl
rm_sprNN.dsl_avg<-
  glm(dSI ~ avgPreyRSF*avgPredRSF,
      data = RMNP_spr)


rm_sprNN.dsl_scavg<-
  glm(dSI ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_spr)

#dyadDist
rm_sprNN.dd_avg<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = RMNP_spr)


rm_sprNN.dd_scavg<-
  glm(dyadDist ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_spr)


#dAbsAng
rm_sprNN.dAA_avg<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = RMNP_spr)


rm_sprNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgPreyRSF*sc_avgPredRSF,
      data = RMNP_spr)





## NL
#dsl
nlNN.dsl_avg<-
  glm(dSI ~ avgCarRSF*avgPredRSF,
      data = nl.dyad3.1)


nlNN.dsl_scavg<-
  glm(dSI ~ sc_avgCarRSF*sc_avgPredRSF,
      data = nl.dyad3.1)

#dyadDist
nlNN.dd_avg<-
  glm(dyadDist ~ avgCarRSF*avgPredRSF,
      data = nl.dyad3.1)


nlNN.dd_scavg<-
  glm(dyadDist ~ sc_avgCarRSF*sc_avgPredRSF,
      data = nl.dyad3.1)


#dAbsAng
nlNN.dAA_avg<-
  glm(dAbsAng ~ avgCarRSF*avgPredRSF,
      data = nl.dyad3.1)


nlNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgCarRSF*sc_avgPredRSF,
      data = nl.dyad3.1)


### NL is mostly evenly split between winter and spring

NL_win<-nl.dyad3.1[season=="winter"]
NL_spr<-nl.dyad3.1[season=="spring"]


## WINTER
#dsl
nl_winNN.dsl_avg<-
  glm(dSI ~ avgCarRSF*avgPredRSF,
      data = NL_win)


nl_winNN.dsl_scavg<-
  glm(dSI ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_win)

#dyadDist
nl_winNN.dd_avg<-
  glm(dyadDist ~ avgCarRSF*avgPredRSF,
      data = NL_win)


nl_winNN.dd_scavg<-
  glm(dyadDist ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_win)


#dAbsAng
nl_winNN.dAA_avg<-
  glm(dAbsAng ~ avgCarRSF*avgPredRSF,
      data = NL_win)


nl_winNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_win)


## SPRING
#dsl
nl_sprNN.dsl_avg<-
  glm(dSI ~ avgCarRSF*avgPredRSF,
      data = NL_spr)


nl_sprNN.dsl_scavg<-
  glm(dSI ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_spr)

#dyadDist
nl_sprNN.dd_avg<-
  glm(dyadDist ~ avgCarRSF*avgPredRSF,
      data = NL_spr)


nl_sprNN.dd_scavg<-
  glm(dyadDist ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_spr)


#dAbsAng
nl_sprNN.dAA_avg<-
  glm(dAbsAng ~ avgCarRSF*avgPredRSF,
      data = NL_spr)


nl_sprNN.dAA_scavg<-
  glm(dAbsAng ~ sc_avgCarRSF*sc_avgPredRSF,
      data = NL_spr)





# potential subsets of SA data:
## time: winter/spring
## cover: open/closed
## predator: bear/coyote



rmNNSpring <-
  glm(dSI ~ elkspring + wolfspring + elkspring:wolfspring,
      data = rmnp[season == 'spring'])


###Plot
#heatmap of social response across domains 
#prey on Y, predator on X, hot colours is positive social response, cold is negative

library(visreg)
library(rgl)

visreg2d(rmNN.dsl.avg.d500, "avgPreyRSF", "avgPredRSF", plot.type="image")
visreg2d(rmNN.dsl.avg.d500, "avgPreyRSF", "avgPredRSF", plot.type="persp")

visreg2d(rmNN.dsl.avg.d500, "avgPreyRSF", "avgPredRSF", plot.type="rgl")


visreg2d(rmNN.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="image")
visreg2d(rmNN.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="persp")

visreg2d(rmNN.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="rgl")




visreg2d(rmNN.ln.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="image")
visreg2d(rmNN.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="persp")

visreg2d(rmNN.dsl.avg.d50, "avgPreyRSF", "avgPredRSF", plot.type="rgl")














### old code playing around with thresholds and correlations


thresh<-seq(50,500,50) ### sequence of threshold values

### testing correlation
### we should test for correlation at any other levels we make thresholds
### we need to test for correlation with angles but there are NAs
RMNP.COR_LIST<-list()
RMNP.CORn_LIST<-list()
RMNP.CORp_LIST<-list()
RMNP.SUM_LIST<-list()

for(i in 1:length(thresh)){
  
  RMNP.SUM_LIST[[1]]<-summary(rmnp.dyad3)
  RMNP.COR_LIST[[1]]<-rcorr(as.matrix(rmnp.dyad3[,c(1:5,10:12,15:23)]))[[1]]
  RMNP.CORn_LIST[[1]]<-rcorr(as.matrix(rmnp.dyad3[,c(1:5,10:12,15:23)]))[[2]]
  RMNP.CORp_LIST[[1]]<-rcorr(as.matrix(rmnp.dyad3[,c(1:5,10:12,15:23)]))[[3]]
  
  RMNP.SUM_LIST[[i+1]]<-summary(rmnp.dyad3[dyadDist < thresh[i]])
  RMNP.COR_LIST[[i+1]]<-rcorr(as.matrix(rmnp.dyad3[dyadDist < thresh[i]][,c(1:5,10:12,15:23)]))[[1]] 
  RMNP.CORn_LIST[[i+1]]<-rcorr(as.matrix(rmnp.dyad3[dyadDist < thresh[i]][,c(1:5,10:12,15:23)]))[[2]]
  RMNP.CORp_LIST[[i+1]]<-rcorr(as.matrix(rmnp.dyad3[dyadDist < thresh[i]][,c(1:5,10:12,15:23)]))[[3]]
  
}

NL.COR_LIST<-list()
NL.CORn_LIST<-list()
NL.CORp_LIST<-list()
NL.SUM_LIST<-list()

for(i in 1:length(thresh)){
  
  NL.SUM_LIST[[1]]<-summary(nl.dyad3)
  NL.COR_LIST[[1]]<-rcorr(as.matrix(nl.dyad3[,c(1:7,12:14,16:30)]))[[1]]
  NL.CORn_LIST[[1]]<-rcorr(as.matrix(nl.dyad3[,c(1:7,12:14,16:30)]))[[2]]
  NL.CORp_LIST[[1]]<-rcorr(as.matrix(nl.dyad3[,c(1:7,12:14,16:30)]))[[3]]
  
  NL.SUM_LIST[[i+1]]<-summary(nl.dyad3[dyadDist < thresh[i]])
  NL.COR_LIST[[i+1]]<-rcorr(as.matrix(nl.dyad3[dyadDist < thresh[i]][,c(1:7,12:14,16:30)]))[[1]] 
  NL.CORn_LIST[[i+1]]<-rcorr(as.matrix(nl.dyad3[dyadDist < thresh[i]][,c(1:7,12:14,16:30)]))[[2]]
  NL.CORp_LIST[[i+1]]<-rcorr(as.matrix(nl.dyad3[dyadDist < thresh[i]][,c(1:7,12:14,16:30)]))[[3]]
  
}






### sample sizes by threshold

thresh1nl<-seq(50,max(nl.dyad3$dyadDist),50) ### sequence of threshold values
thresh1rmnp<-seq(50,max(rmnp.dyad3$dyadDist),50) ### sequence of threshold values


sum_rmnp<-as.data.frame(matrix(ncol=3, nrow=(1+length(thresh1rmnp))))
colnames(sum_rmnp)<-c("dataset","n","NA_dAbsAng")

sum_rmnp$dataset<-c(max(rmnp.dyad3$dyadDist),thresh1rmnp)
sum_rmnp$n[1]<-nrow(rmnp.dyad3)
sum_rmnp$NA_dAbsAng[1]<-length(subset(rmnp.dyad3$dAbsAng, is.na(rmnp.dyad3$dAbsAng)))

for(i in 1:length(thresh1rmnp)){
  
  sum_rmnp$n[i+1]<-nrow(rmnp.dyad3[dyadDist < thresh1rmnp[i]])
  sum_rmnp$NA_dAbsAng[i+1]<-length(subset(rmnp.dyad3[dyadDist < thresh1rmnp[i]]$dAbsAng, is.na(rmnp.dyad3[dyadDist < thresh1rmnp[i]]$dAbsAng)))
}


sum_nl<-as.data.frame(matrix(ncol=3, nrow=(1+length(thresh1nl))))
colnames(sum_nl)<-c("dataset","n","NA_dAbsAng")

sum_nl$dataset<-c(max(nl.dyad3$dyadDist),thresh1nl)
sum_nl$n[1]<-nrow(nl.dyad3)
sum_nl$NA_dAbsAng[1]<-length(subset(nl.dyad3$dAbsAng, is.na(nl.dyad3$dAbsAng)))

for(i in 1:length(thresh1nl)){
  
  sum_nl$n[i+1]<-nrow(nl.dyad3[dyadDist < thresh1nl[i]])
  sum_nl$NA_dAbsAng[i+1]<-length(subset(nl.dyad3[dyadDist < thresh1nl[i]]$dAbsAng, is.na(nl.dyad3[dyadDist < thresh1nl[i]]$dAbsAng)))
}

### visual of sample size by threshold

plot(sum_nl$dataset,sum_nl$n)  
plot(sum_rmnp$dataset,sum_rmnp$n)

























rmNN.dsl_scd<-
  glm(dSI ~ sc_dPreyRSF*sc_dPredRSF,
      data = rmnp.dyad3.1)

rmNN.dsl_prd<-
  glm(dSI ~ pr_dPreyRSF*pr_dPredRSF,
      data = rmnp.dyad3.1)






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
