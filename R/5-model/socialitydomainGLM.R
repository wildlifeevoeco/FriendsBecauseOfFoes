### SOCIALITY ~ DOMAIN MODEL ----
# Authors: Alec Robitaille, Christina M Prokopenko
# Purpose: 
# Inputs: sociality measures, predator and prey RSFs for RMNP and NL
# Outputs:
# Project: Easter Week Challenge 2018
# Copyright: ./LICENSE.md 

libs <- c('data.table',  
          'lme4',
          'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input data ----

#RMNP file

setwd("C:/Users/ehanc/Desktop/GitLab/ewc/output/nna")

rmnp <- readRDS('elkNNA.Rds')

### creating average RSF values for each dyad
### Alec you might want to put this in your data prep code

rmnp$avg.PredRSF<-rowMeans(matrix(c(rmnp$preyRSF,rmnp$rpreyRSF),ncol=2))
rmnp$avg.PreyRSF<-rowMeans(matrix(c(rmnp$preyRSF,rmnp$rpreyRSF),ncol=2))

### one case where dyad is just i-i, instead of i-j
### this needs to be addressed, why did this occur

rmnp$bad.neighbor<-ifelse(rmnp$id==rmnp$neighbour1,1,0)

### removing this case for now, plus just extracting dyads

rmnp.dyad1<-subset(rmnp, rmnp$bad.neighbor==0)

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
## social measures are: dSI, dyadDist, dAbsAng, nWithin5000

# round(cor(rmnp.dyad1[,c(17,18,26,27,14,25,32,31,34,35,37,38)]),2)

rmNN.dsl.avg.d500<-
  glm(dSI ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dsl.id1.d500<-
  glm(dSI ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dsl.id2.d500<-
  glm(dSI ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

summary(rmNN.dsl.avg.d500)
summary(rmNN.dsl.id1.d500)
summary(rmNN.dsl.id2.d500)

rmNN.dsl.avg.d50<-
  glm(dSI ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dsl.id1.d50<-
  glm(dSI ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dsl.id2.d50<-
  glm(dSI ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

summary(rmNN.dsl.avg.d50)
summary(rmNN.dsl.id1.d50)
summary(rmNN.dsl.id2.d50)

rmNN.dsl.avg.d100<-
  glm(dSI ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 100])

rmNN.dsl.id1.d100<-
  glm(dSI ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 100])

rmNN.dsl.id2.d100<-
  glm(dSI ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 100])

summary(rmNN.dsl.avg.d100)
summary(rmNN.dsl.id1.d100)
summary(rmNN.dsl.id2.d100)

### interesting results disappear at 150m

rmNN.dyadDist.avg.d500<-
  glm(dyadDist ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dyadDist.id1.d500<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

rmNN.dyadDist.id2.d500<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 500])

summary(rmNN.dyadDist.avg.d500)
summary(rmNN.dyadDist.id1.d500)
summary(rmNN.dyadDist.id2.d500)

rmNN.dyadDist.avg.d500to1000<-
  glm(dyadDist ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

rmNN.dyadDist.id1.d500to1000<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

rmNN.dyadDist.id2.d500to1000<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist > 500 & dyadDist < 1000])

summary(rmNN.dyadDist.avg.d500to1000)
summary(rmNN.dyadDist.id1.d500to1000)
summary(rmNN.dyadDist.id2.d500to1000)

rmNN.dyadDist.avg.d50<-
  glm(dyadDist ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dyadDist.id1.d50<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

rmNN.dyadDist.id2.d50<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 50])

summary(rmNN.dyadDist.avg.d50)
summary(rmNN.dyadDist.id1.d50)
summary(rmNN.dyadDist.id2.d50)

rmNN.dyadDist.avg.d250<-
  glm(dyadDist ~ avg.PreyRSF*avg.PredRSF,
      data = rmnp.dyad1[dyadDist < 250])

rmNN.dyadDist.id1.d250<-
  glm(dyadDist ~ preyRSF*predatorRSF,
      data = rmnp.dyad1[dyadDist < 250])

rmNN.dyadDist.id2.d250<-
  glm(dyadDist ~ rpreyRSF*rpredatorRSF,
      data = rmnp.dyad1[dyadDist < 250])

summary(rmNN.dyadDist.avg.d250)
summary(rmNN.dyadDist.id1.d250)
summary(rmNN.dyadDist.id2.d250)

rmNN.dAbsAng.avg.d500<-
  glm(dAbsAng ~ avg.PreyRSF*avg.PredRSF,
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
  glm(dAbsAng ~ avg.PreyRSF*avg.PredRSF,
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

rmNNSpring <-
  glm(dSI ~ elkspring + wolfspring + elkspring:wolfspring,
      data = rmnp[season == 'spring'])

#turning angle

#movement rate


# potential subsets of SA data:
## time: winter/spring
## cover: open/closed
## predator: bear/coyote



###Plot
#heatmap of social response across domains 
#prey on Y, predator on X, hot colours is positive social response, cold is negative






