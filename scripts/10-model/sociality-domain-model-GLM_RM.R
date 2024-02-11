#
## SOCIALITY ~ DOMAIN MODEL ----
# Authors: Alec Robitaille, Christina M Prokopenko, Hance Ellington

# ### Packages ----
pkgs <- c('data.table', 'lme4', 'ggplot2', 'Hmisc')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)

setwd("C:/Users/ehanc/OneDrive/Desktop/ewc/ewc")

### Input data ----
# Which species?
species <- 'elk'
DT <- readRDS(paste0('OUTPUT_NEW/SOCIAL/', species, 'NNA_log2.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

#if (truelength(DT) == 0) alloc.col(DT)

### Maybe move this to NNA script?
# Dyads within 50m 
DT[distance >= 50, bin50m := TRUE]
DT[distance < 50, bin50m := FALSE]

# Duplicated dyads
DT[, nDyadTime := .N, by = dyadTime]


summary(as.factor(DT$nDyadTime))


### removing duplicate dyad,times

DT2<-DT[!duplicated(DT$dyadTime),]  

summary(as.factor(DT2$nDyadTime))


DT2<-subset(DT2, !is.na(DT2$distance))


### season subsets
DT_S<-subset(DT2, DT2$season=="spring")
DT_W<-subset(DT2, DT2$season=="winter")

### we arent currently using end...RSF columns but if we were we would need to fix them because they do not have .nn column

#### Because we are focused on sociality, we only want to look at dyads that:
### 1) had potential to be social (dyads within 50m)
### 2) dyads that choose to be social (dSl less 50m)

### we might be interested in the conditions that result in a dyad that had the potential to be social but choose to not be social


#### change as needed for dyad distance threshold
DTsoc3_50<-DT2[distance<=50 & dSI<=10000]


### standardized avg RSF independents

DTsoc3_S<-subset(DTsoc3_50, DTsoc3_50$season=="spring")
DTsoc3_W<-subset(DTsoc3_50, DTsoc3_50$season=="winter")

round(nrow(DTsoc3_50)/nrow(DT2), 2)
#proportion of dyadtimes where animals had potential to be social
# round(nrow(DTsocNA_50)/nrow(DT2), 2)
# #proportion of dyadtimes where animals did not have potential to be social

round(nrow(DTsoc3_W)/nrow(DT_W), 2)
#proportion of dyadtimes where animals had potential to be social
# round(nrow(DTsocNA_W)/nrow(DT_W), 2)
# #proportion of dyadtimes where animals did not have potential to be social


round(nrow(DTsoc3_S)/nrow(DT_S), 2)
#proportion of dyadtimes where animals had potential to be social
# round(nrow(DTsocNA_S)/nrow(DT_S), 2)
# #proportion of dyadtimes where animals did not have potential to be social


### for RMNP correlations

c1<-c(63:82) ### for isolating correlation columns
CORW<-round(cor(as.data.frame(DTsoc3_W[,..c1])[, sapply(as.data.frame(DTsoc3_W[,..c1]), is.numeric)], use = "complete.obs", method = "pearson"),2)

# c2<-c(4,7:9,14:16,21,35:38) ### for isolating correlation columns
# CORW2<-round(cor(as.data.frame(DTsoc3_W[,..c2])[, sapply(as.data.frame(DTsoc3_W[,..c2]), is.numeric)], use = "complete.obs", method = "pearson"),2)


### plots of data

plot(as.factor(DTsoc3_50$season))
# plot(as.factor(DTsoc3$dyadID))
# plot(as.factor(DTsoc3$nWithin))
# summary(as.factor(DTsoc3_W$nWithin))
# summary(as.factor(DTsoc3_S$nWithin))

## WINTER (for NL predator=coyote in winter)

boxplot(DTsoc3_W$di)
hist(DTsoc3_W$di, breaks=20, freq=FALSE)

library(ggplot2)

ggplot(DTsoc3_W, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,0.1)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)
  

### domains to model based on correlations
# prey
#avgwinter_elk_st1_2
#avgwinter_elk_st2_2
# predator
#avgwinter_wolf_st1_1
#avgwinter_wolf_st1_2
#avgwinter_wolf_st2_2




# boxplot(DTsoc3_W$avgelk_class)
# hist(DTsoc3_W$avgelk_class)
# 
# boxplot(DTsoc3_W$avgelk_raw)
# hist(DTsoc3_W$avgelk_raw)
# 
# boxplot(DTsoc3_W$avgelk_rsf)
hist(DTsoc3_W$avgelk_rsf)

# boxplot(DTsoc3_W$avgwolf_class)
# hist(DTsoc3_W$avgwolf_class)
# 
# boxplot(DTsoc3_W$avgwolf_raw)
# hist(DTsoc3_W$avgwolf_raw)
# 
# boxplot(DTsoc3_W$avgwolf_rsf)
hist(DTsoc3_W$avgwolf_rsf)

## Spring (not needed for RMNP, low sample size)

library(visreg)
library(rgl)



###GLM
## RM
## winter


###GLM
## RM
## winter
## prey state 1, predator state 2

rmNN_null<-
  glm(di ~ 1,
      data = DTsoc3_W)

summary(rmNN_null)



rmNN_W.DIpy_rsf<-
  glm(di ~ avgelk_rsf,
      data = DTsoc3_W)

summary(rmNN_W.DIpy_rsf)

rmNN_W.DIpd_rsf<-
  glm(di ~ avgwolf_rsf,
      data = DTsoc3_W)

summary(rmNN_W.DIpd_rsf)

rmNN_W.DI_rsf<-
  glm(di ~ avgelk_rsf*avgwolf_rsf,
      data = DTsoc3_W)

summary(rmNN_W.DI_rsf)


rmNN_W.DI_A_rsf<-
  glm(di ~ avgelk_rsf+avgwolf_rsf,
      data = DTsoc3_W)

summary(rmNN_W.DI_A_rsf)






library(jtools)
library(interactions)

# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, data = DTsoc3_W, linearity.check = TRUE, plot.points = TRUE)
# 
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, plot.points = TRUE)
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, plot.points = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, partial.residuals = TRUE)
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, partial.residuals = TRUE, interval = TRUE, int.width = 0.8, robust=TRUE)
# interact_plot(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0), interval = TRUE, int.width = 0.8, robust=TRUE)

probe_interaction(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, robust=T, control.fdr = TRUE)

library(ggstance)
library(broom.mixed)

plot(sim_slopes(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, robust=T))

johnson_neyman(rmNN_W.DI_rsf, pred = avgwolf_rsf, modx = avgelk_rsf, control.fdr = TRUE)

effect_plot(rmNN_W.DI_A_rsf, pred = avgwolf_rsf, interval = TRUE, rug = TRUE)
effect_plot(rmNN_W.DI_A_rsf, pred = avgelk_rsf, interval = TRUE, rug = TRUE)


save.image("OUTPUT_NEW/FINAL/RMNP_results_log_50m.Rdata")
