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

### playing around with some histograms of di


h1 <- hist(DTsoc3_W$di) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

g1 <- hist(DTsoc3_S$di) # or hist(x,plot=FALSE) to avoid the plot of the histogram
g1$density = g1$counts/sum(g1$counts)*100


limits <- range(0,25)
par(mfrow=c(1,2))
plot(h1,freq=FALSE, ylim=limits)
plot(g1,freq=FALSE, ylim=limits)

#####

## WINTER (for NL predator=coyote in winter)

boxplot(DTsoc3_W$di)
hist(DTsoc3_W$di)

boxplot(DTsoc3_W$avgpredatorRSF)
hist(DTsoc3_W$avgpredatorRSF)

boxplot(DTsoc3_W$avgpreyRSF)
hist(DTsoc3_W$avgpreyRSF)

## Spring

boxplot(DTsoc3_S$di)
hist(DTsoc3_S$di)

boxplot(DTsoc3_S$avgpredatorRSF)
hist(DTsoc3_S$avgpredatorRSF)

boxplot(DTsoc3_S$avgpreyRSF)
hist(DTsoc3_S$avgpreyRSF)

boxplot(DTsoc3_S$avgcoyoteRSF)
hist(DTsoc3_S$avgcoyoteRSF)

boxplot(DTsoc3_S$avgbearRSF)
hist(DTsoc3_S$avgbearRSF)

library(visreg)
library(rgl)

###GLM
## NL
## winter

nlNN_W.DIpy<-
  glm(di ~ avgpreyRSF,
      data = DTsoc3_W)

summary(nlNN_W.DIpy)

nlNN_W.DIpd<-
  glm(di ~ avgpredatorRSF,
      data = DTsoc3_W)

summary(nlNN_W.DIpd)

nlNN_W.DI<-
  glm(di ~ avgpreyRSF*avgpredatorRSF,
      data = DTsoc3_W)

summary(nlNN_W.DI)


nlNN_W.DI_A<-
  glm(di ~ avgpreyRSF+avgpredatorRSF,
      data = DTsoc3_W)

summary(nlNN_W.DI_A)

## spring

nlNN_S.DIpy<-
  glm(di ~ avgpreyRSF,
      data = DTsoc3_S)

summary(nlNN_S.DIpy)

nlNN_S.DIpd<-
  glm(di ~ avgpredatorRSF,
      data = DTsoc3_S)

summary(nlNN_S.DIpd)

nlNN_S.DI<-
  glm(di ~ avgpreyRSF*avgpredatorRSF,
      data = DTsoc3_S)

summary(nlNN_S.DI)

nlNN_S.DI_A<-
  glm(di ~ avgpreyRSF+avgpredatorRSF,
      data = DTsoc3_S)

summary(nlNN_S.DI_A)

### Bear v Coyote in Spring

nlNN_S.DI_Cpd<-
  glm(di ~ avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_Cpd)

nlNN_S.DI_C<-
  glm(di ~ avgpreyRSF*avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_C)

nlNN_S.DI_C_A<-
  glm(di ~ avgpreyRSF+avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_C_A)

nlNN_S.DI_Bpd<-
  glm(di ~ avgbearRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_Bpd)

nlNN_S.DI_B<-
  glm(di ~ avgpreyRSF*avgbearRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_B)

nlNN_S.DI_B_A<-
  glm(di ~ avgpreyRSF+avgbearRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_B_A)

## bear and coyote  

nlNN_S.DI_CB_A<-
  glm(di ~ avgpreyRSF+avgbearRSF+avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_CB_A)

nlNN_S.DI_CB<-
  glm(di ~ avgpreyRSF*avgbearRSF+avgpreyRSF*avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_CB)


nlNN_S.DI_B_C<-
  glm(di ~ avgpreyRSF*avgbearRSF+avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_B_C)

nlNN_S.DI_C_B<-
  glm(di ~ avgbearRSF+avgpreyRSF*avgcoyoteRSF,
      data = DTsoc3_S)
summary(nlNN_S.DI_C_B)

### year-round coyote

nlNN.DIpy<-
  glm(di ~ avgpreyRSF,
      data = DTsoc3)

summary(nlNN.DIpy)

nlNN.DI_Cpd<-
  glm(di ~ avgcoyoteRSF,
      data = DTsoc3)

summary(nlNN.DI_Cpd)

nlNN.DI_C<-
  glm(di ~ avgpreyRSF*avgcoyoteRSF,
      data = DTsoc3)

summary(nlNN.DI_C)


nlNN.DI_C_A<-
  glm(di ~ avgpreyRSF+avgcoyoteRSF,
      data = DTsoc3)

summary(nlNN.DI_C_A)





p<-ggplot(DTsoc3_W,aes(avgpreyRSF,avgpredatorRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgpredatorRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,0.80)+xlim(0,0.35)
p + geom_density_2d()

p<-ggplot(DTsocNA_W,aes(avgpreyRSF,avgpredatorRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgpredatorRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,0.80)+xlim(0,0.35)
p + geom_density_2d()

p<-ggplot(DTsoc3_S,aes(avgpreyRSF,avgpredatorRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgpredatorRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0.25,1.35)+xlim(0,0.36)
p + geom_density_2d()

p<-ggplot(DTsocNA_S,aes(avgpreyRSF,avgpredatorRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgpredatorRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0.25,1.35)+xlim(0,0.36)
p + geom_density_2d()

p<-ggplot(DTsoc3_S,aes(avgpreyRSF,avgcoyoteRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgcoyoteRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,1)+xlim(0,0.36)
p + geom_density_2d()

p<-ggplot(DTsocNA_S,aes(avgpreyRSF,avgcoyoteRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgcoyoteRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,1)+xlim(0,0.36)
p + geom_density_2d()

p<-ggplot(DTsoc3_S,aes(avgpreyRSF,avgbearRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgbearRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,0.36)+xlim(0,0.36)
p + geom_density_2d()

p<-ggplot(DTsocNA_S,aes(avgpreyRSF,avgbearRSF))
p<-p + geom_point(aes(colour = avgpreyRSF*avgbearRSF))+scale_fill_gradient(low = "lightpink1", high = "red4",aesthetics = "colour")+ylim(0,0.36)+xlim(0,0.36)
p + geom_density_2d()







### Figure 1a.1
MOD.nlNN_W.DI<-nlNN_W.DI_A
MOD.nlNN_W.DI$coefficients
visreg2d(MOD.nlNN_W.DI, "avgpreyRSF", "avgpredatorRSF", plot.type="image", ylim=c(0,0.80), xlim=c(0,0.35), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of coyote landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100) 


### Figure 2a
MOD.nlNN_S.DI<-nlNN_S.DI
MOD.nlNN_S.DI$coefficients[1]<-0
visreg2d(MOD.nlNN_S.DI, "avgpreyRSF", "avgpredatorRSF", plot.type="image", ylim=c(0.25,1.35), xlim=c(0,0.35), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of coyote and bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100) 


### Figure 2b (note all coefficients are meaningful so no modification)
MOD.nlNN_S.DI_C<-nlNN_S.DI_C
MOD.nlNN_S.DI_C$coefficients[1]<-0
visreg2d(MOD.nlNN_S.DI_C, "avgpreyRSF", "avgcoyoteRSF", plot.type="image", ylim=c(0,1), xlim=c(0,0.35), zlim=c(-1,1), main="", 
         xlab="Scaled estimate of selection value of caribou landscape", 
         ylab="Scaled estimate of selection value of coyote landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  

### Figure 2c (note all coefficients are meaningful so no modification)
MOD.nlNN_S.DI_B<-nlNN_S.DI_B
MOD.nlNN_S.DI_B$coefficients[3]<-0
visreg2d(MOD.nlNN_S.DI_B, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0,0.35), xlim=c(0,0.35), zlim=c(-1,1), main="", 
         xlab="Scaled estimate of selection value of caribou landscape", 
         ylab="Scaled estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  


### Figure 3a-e
### first modifying coefficients to zero if their se > beta
### 

MOD.nlNN_S.DI_CB<-nlNN_S.DI_CB
MOD.nlNN_S.DI_CB$coefficients[1]<-0
MOD.nlNN_S.DI_CB$coefficients[4]<-0  ### because we are going to set specific coyote values into intercept
MOD.nlNN_S.DI_CB$coefficients[6]<-0

MOD.nlNN_S.DI_CB_A<-MOD.nlNN_S.DI_CB
MOD.nlNN_S.DI_CB_B<-MOD.nlNN_S.DI_CB
MOD.nlNN_S.DI_CB_C<-MOD.nlNN_S.DI_CB
MOD.nlNN_S.DI_CB_D<-MOD.nlNN_S.DI_CB
MOD.nlNN_S.DI_CB_E<-MOD.nlNN_S.DI_CB

MOD.nlNN_S.DI_CB_A$coefficients[1]<-0.4397792*0.1718
MOD.nlNN_S.DI_CB_B$coefficients[1]<-0.4397792*0.2872
MOD.nlNN_S.DI_CB_C$coefficients[1]<-0.4397792*0.3263
MOD.nlNN_S.DI_CB_D$coefficients[1]<-0.4397792*0.3859
MOD.nlNN_S.DI_CB_E$coefficients[1]<-0.4397792*0.9753

visreg2d(MOD.nlNN_S.DI_CB_A, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0.02,0.31), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  

visreg2d(MOD.nlNN_S.DI_CB_B, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0.02,0.31), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  

visreg2d(MOD.nlNN_S.DI_CB_C, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0.02,0.31), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  

visreg2d(MOD.nlNN_S.DI_CB_D, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0.02,0.31), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  

visreg2d(MOD.nlNN_S.DI_CB_E, "avgpreyRSF", "avgbearRSF", plot.type="image", ylim=c(0.02,0.31), zlim=c(-1,1), main="", 
         xlab="Estimate of selection value of caribou landscape", 
         ylab="Estimate of selection value of bear landscape",
         zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)  





