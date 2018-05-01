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
          'Hmsic')
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

### data to summarize, test for correlation, etc.

### need to add nWithin500
nl.dyad3<-nl.dyad2[, c("CarRSF","CoyRSF","BearRSF","absAngle","relAngle","stepLength","predatorRSF","season","dyadID",
       "julday","yr","dyadDist","dSI","dAbsAng","bin500m",
       "rCarRSF","rCoyRSF","rBearRSF","rabsAngle","rrelAngle","rstepLength","rpredatorRSF",
       "dCarRSF","dCoyRSF","dBearRSF","dPredRSF",
       "avgCarRSF","avgCoyRSF","avgBearRSF","avgPredRSF"), with=FALSE]

rmnp.dyad3<-rmnp.dyad2[, c("absAngle","relAngle","stepLength","predatorRSF","preyRSF","season","dyadID",
                           "julday","yr","dyadDist","dSI","dAbsAng","bin500m","nWithin500",
                           "rabsAngle","rrelAngle","rstepLength","rpredatorRSF","rpreyRSF",
                           "dPredRSF","dPreyRSF","avgPredRSF","avgPreyRSF"), with=FALSE]

rmnp.dyad3$season <- as.factor(rmnp.dyad3$season)
rmnp.dyad3$dyadID <- as.factor(rmnp.dyad3$dyadID)
rmnp.dyad3$julday <- as.factor(rmnp.dyad3$julday)
rmnp.dyad3$yr <- as.factor(rmnp.dyad3$yr)
rmnp.dyad3$bin500m <- as.factor(rmnp.dyad3$bin500m)
rmnp.dyad3$nWithin500 <- as.factor(rmnp.dyad3$nWithin500)

nl.dyad3$season <- as.factor(nl.dyad3$season)
nl.dyad3$dyadID <- as.factor(nl.dyad3$dyadID)
nl.dyad3$julday <- as.factor(nl.dyad3$julday)
nl.dyad3$yr <- as.factor(nl.dyad3$yr)
nl.dyad3$bin500m <- as.factor(nl.dyad3$bin500m)
#nl.dyad3$nWithin500 <- as.factor(nl.dyad3$nWithin500) ### update after adding niwthin500

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

### sample sizes

sum_rmnp<-as.data.frame(matrix(ncol=3, nrow=11))
colnames(sum_rmnp)<-c("dataset","n","NA_dAbsAng")

sum_rmnp$dataset<-c("full",thresh)
sum_rmnp$n[1]<-nrow(rmnp.dyad3)
sum_rmnp$NA_dAbsAng[1]<-length(subset(rmnp.dyad3$dAbsAng, is.na(rmnp.dyad3$dAbsAng)))

for(i in 1:length(thresh)){
  
  sum_rmnp$n[i+1]<-nrow(rmnp.dyad3[dyadDist < thresh[i]])
  sum_rmnp$NA_dAbsAng[i+1]<-length(subset(rmnp.dyad3[dyadDist < thresh[i]]$dAbsAng, is.na(rmnp.dyad3[dyadDist < thresh[i]]$dAbsAng)))
}


sum_nl<-as.data.frame(matrix(ncol=3, nrow=11))
colnames(sum_nl)<-c("dataset","n","NA_dAbsAng")

sum_nl$dataset<-c("full",thresh)
sum_nl$n[1]<-nrow(nl.dyad3)
sum_nl$NA_dAbsAng[1]<-length(subset(nl.dyad3$dAbsAng, is.na(nl.dyad3$dAbsAng)))

for(i in 1:length(thresh)){
  
  sum_nl$n[i+1]<-nrow(nl.dyad3[dyadDist < thresh[i]])
  sum_nl$NA_dAbsAng[i+1]<-length(subset(nl.dyad3[dyadDist < thresh[i]]$dAbsAng, is.na(nl.dyad3[dyadDist < thresh[i]]$dAbsAng)))
}


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



     
###GLM
# SA <- glm(SocialMeasure ~ PreyHD*PredHD, data = rmnp)
## social measures are: dSI, dyadDist, dAbsAng, bin500m, nWithin5000

# round(cor(rmnp.dyad1[,c(17,18,26,27,14,25,32,31,34,35,37,38)]),2)

#dsl
rmNN.ln.dsl.avg.d500<-
  glm(log(dSI+0.0001) ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dsl.avg.d500<-
  glm(dSI ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dsl.id1.d500<-
  glm(dSI ~ preyRSF*predatorRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dsl.id2.d500<-
  glm(dSI ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad2[dyadDist < 500])

summary(rmNN.ln.dsl.avg.d500)
summary(rmNN.dsl.avg.d500)
summary(rmNN.dsl.id1.d500)
summary(rmNN.dsl.id2.d500)

rmNN.ln.dsl.avg.d50<-
  glm(log(dSI+0.0001) ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dsl.avg.d50<-
  glm(dSI ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 50])


rmNN.dsl.id1.d50<-
  glm(dSI ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dsl.id2.d50<-
  glm(dSI ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

summary(rmNN.ln.dsl.avg.d50)
summary(rmNN.dsl.avg.d50)
summary(rmNN.dsl.id1.d50)
summary(rmNN.dsl.id2.d50)

### DyadDist
rmNN.ln.dyadDist.avg.d500<-
  glm(log(dyadDist+0.0001) ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dyadDist.avg.d500<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dyadDist.id1.d500<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad2[dyadDist < 500])

rmNN.dyadDist.id2.d500<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad2[dyadDist < 500])

summary(rmNN.ln.dyadDist.avg.d500)
summary(rmNN.dyadDist.avg.d500)
summary(rmNN.dyadDist.id1.d500)
summary(rmNN.dyadDist.id2.d500)

rmNN.ln.dyadDist.avg.d500to1000<-
  glm(log(dyadDist+0.0001) ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

rmNN.dyadDist.avg.d500to1000<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

rmNN.dyadDist.id1.d500to1000<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

rmNN.dyadDist.id2.d500to1000<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

summary(rmNN.ln.dyadDist.avg.d500to1000)
summary(rmNN.dyadDist.avg.d500to1000)
summary(rmNN.dyadDist.id1.d500to1000)
summary(rmNN.dyadDist.id2.d500to1000)

rmNN.ln.dyadDist.avg.d50<-
  glm(log(dyadDist+0.0001) ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dyadDist.avg.d50<-
  glm(dyadDist ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dyadDist.id1.d50<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dyadDist.id2.d50<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

summary(rmNN.ln.dyadDist.avg.d50)
summary(rmNN.dyadDist.avg.d50)
summary(rmNN.dyadDist.id1.d50)
summary(rmNN.dyadDist.id2.d50)

# bin500m

rmNN.bin500m.avg<-
  glm(bin500m ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad2)

rmNN.bin500m.id1<-
  glm(bin500m ~ preyRSF*predatorRSF,
      data = rmnp.dyad2)

rmNN.bin500m.id2<-
  glm(bin500m ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad2)

summary(rmNN.bin500m.avg)
summary(rmNN.bin500m.id1)
summary(rmNN.bin500m.id2)



#turning angle

rmNN.dAbsAng.avg.d500<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dAbsAng.id1.d500<-
  glm(dAbsAng ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dAbsAng.id2.d500<-
  glm(dAbsAng ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

summary(rmNN.dAbsAng.avg.d500)
summary(rmNN.dAbsAng.id1.d500)
summary(rmNN.dAbsAng.id2.d500)

rmNN.dAbsAng.avg.d50<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dAbsAng.id1.d50<-
  glm(dAbsAng ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dAbsAng.id2.d50<-
  glm(dAbsAng ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

summary(rmNN.dAbsAng.avg.d50)
summary(rmNN.dAbsAng.id1.d50)
summary(rmNN.dAbsAng.id2.d50)

rmNN.dAbsAng.avg.d250<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 250])

rmNN.dAbsAng.id1.d250<-
  glm(dAbsAng ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 250])

rmNN.dAbsAng.id2.d250<-
  glm(dAbsAng ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 250])

summary(rmNN.dAbsAng.avg.d250)
summary(rmNN.dAbsAng.id1.d250)
summary(rmNN.dAbsAng.id2.d250)

rmNN.dAbsAng.avg.d100<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 100])

rmNN.dAbsAng.id1.d100<-
  glm(dAbsAng ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 100])

rmNN.dAbsAng.id2.d100<-
  glm(dAbsAng ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 100])

summary(rmNN.dAbsAng.avg.d100)
summary(rmNN.dAbsAng.id1.d100)
summary(rmNN.dAbsAng.id2.d100)

rmNN.dAbsAng.avg.d150<-
  glm(dAbsAng ~ avgPreyRSF*avgPredRSF,
      data = rmnp.dyad1[dyadDist < 150])

rmNN.dAbsAng.id1.d150<-
  glm(dAbsAng ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 150])

rmNN.dAbsAng.id2.d150<-
  glm(dAbsAng ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 150])

summary(rmNN.dAbsAng.avg.d150)
summary(rmNN.dAbsAng.id1.d150)
summary(rmNN.dAbsAng.id2.d150)











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

