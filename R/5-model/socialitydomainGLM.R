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


### testing correlation
### we should test for correlation at any other levels we make thresholds
### we need to test for correlation with angles but there are NAs

summary(rmnp.dyad2[,c(14,17,18,20,25:28,31:37)])

round(cor(rmnp.dyad2[,c(14,17,18,25:27,31,32,34:37)]),2)

summary(rmnp.dyad2[dyadDist < 500][,c(14,17,18,20,25:28,31:37)])

round(cor(rmnp.dyad2[dyadDist < 500][,c(14,17,18,25:27,31,32,34:37)]),2)

summary(rmnp.dyad2[dyadDist < 50][,c(14,17,18,20,25:28,31:37)])

round(cor(rmnp.dyad2[dyadDist < 50][,c(14,17,18,25:27,31,32,34:37)]),2)


### sample sizes

nrow(rmnp.dyad2)
nrow(rmnp.dyad2[dyadDist < 500])
nrow(rmnp.dyad2[dyadDist < 50])




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



rmnp.dyad3<-rmnp.dyad2[dSI<1000]



     
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

