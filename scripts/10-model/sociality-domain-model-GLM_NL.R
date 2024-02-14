### SOCIALITY ~ DOMAIN MODEL ----

### Packages ----
library(data.table)
library(lme4)
library(ggplot2)
library(Hmisc)
library(sandwich)
library(interactions)
library(ggstance)
library(broom.mixed)
library(visreg)
library(rgl)


### Input data ----
# Which species?
species <- 'caribou'
DT <- readRDS(paste0(species, 'NNA_log2.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

if (truelength(DT) == 0) alloc.col(DT)



#DT<-DT[,c(1:13,42:48,77:111)]

### Maybe move this to NNA script?
# Dyads within 50m 
DT[distance >= 50, bin50m := TRUE]
DT[distance < 50, bin50m := FALSE]



# Duplicated dyads
DT[, nDyadTime := .N, by = dyadTime]
# TODO: add is duplicated column

summary(as.factor(DT$nDyadTime))




### removing duplicate dyad,times

DT2<-DT[!duplicated(DT$dyadTime),]  

DT2<-subset(DT2, !is.na(DT2$distance))

summary(as.factor(DT2$nDyadTime))

### season subsets
DT_S<-subset(DT2, DT2$season=="spring")
DT_W<-subset(DT2, DT2$season=="winter")

### we arent currently using end...RSF columns but if we were we would need to fix them because they do not have .nn column

#### Because we are focused on sociality, we only want to look at dyads that:
### 1) had potential to be social (dyads within 50m)
### 2) dyads that choose to be social (dSl less 50m)

### we might be interested in the conditions that result in a dyad that had the potential to be social but choose to not be social

#### change as needed for dyad distance threshold


DTsoc3_50<-DT2[distance<=50]




### standardized avg RSF independents

# DTsoc_S<-subset(DTsoc_50, DTsoc_50$season=="spring")
# DTsoc_W<-subset(DTsoc_50, DTsoc_50$season=="winter")
DTsoc3_S<-subset(DTsoc3_50, DTsoc3_50$season=="spring")
DTsoc3_W<-subset(DTsoc3_50, DTsoc3_50$season=="winter")
# DTnsoc_S<-subset(DTnsoc_50, DTnsoc_50$season=="spring")
# DTnsoc_W<-subset(DTnsoc_50, DTnsoc_50$season=="winter")
# DTsocNA_S<-subset(DTsocNA_50, DTsocNA_50$season=="spring")
# DTsocNA_W<-subset(DTsocNA_50, DTsocNA_50$season=="winter")

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
#proportion of dyadtimes where animals did not have potential to be social


### for NL correlations

c1<-c(59:76) ### for isolating correlation columns
c2<-c(59:76) ### for isolating correlation columns

CORSp<-round(cor(as.data.frame(DTsoc3_S[,..c1])[, sapply(as.data.frame(DTsoc3_S[,..c1]), is.numeric)], use = "complete.obs", method = "pearson"),2)
# CORSp2<-round(cor(as.data.frame(DTsoc3_S[,..c2])[, sapply(as.data.frame(DTsoc3_S[,..c2]), is.numeric)], use = "complete.obs", method = "pearson"),2)

#CORSp[abs(CORSp) < 0.5] <- ""  ## just helps me find any correlations worth noting
# CORSp2[abs(CORSp2) < 0.5] <- ""  ## just helps me find any correlations worth noting

# c1<-c(33:36,47:50) ### for isolating correlation columns
# c2<-c(2,4,7:9,14:16,47:50) ### for isolating correlation columns

CORW<-round(cor(as.data.frame(DTsoc3_W[,..c2])[, sapply(as.data.frame(DTsoc3_W[,..c2]), is.numeric)], use = "complete.obs", method = "pearson"),2)
# CORW2<-round(cor(as.data.frame(DTsoc3_W[,..c2])[, sapply(as.data.frame(DTsoc3_W[,..c2]), is.numeric)], use = "complete.obs", method = "pearson"),2)
# 
# #CORW[abs(CORW) < 0.5] <- ""  ## just helps me find any correlations worth noting
# CORW2[abs(CORW2) < 0.5] <- ""  ## just helps me find any correlations worth noting

### plots of data

# plot(as.factor(DTsoc3$season))
# plot(as.factor(DTsoc3$dyadID))
# plot(as.factor(DTsoc3$nWithin))
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


par(mfrow=c(1,1))

#####

## WINTER (for NL predator=coyote in winter)

boxplot(DTsoc3_W$di)
hist(DTsoc3_W$di)



ggplot(DTsoc3_W, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,0.1)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)


# 
# boxplot(DTsoc3_W$avgcaribouw_class)
# hist(DTsoc3_W$avgcaribouw_class)
# 
# boxplot(DTsoc3_W$avgcaribouw_raw)
# hist(DTsoc3_W$avgcaribouw_raw)

boxplot(DTsoc3_W$avgcaribouw_rsf_LOG_n)
hist(DTsoc3_W$avgcaribouw_rsf_LOG_n)
# 
# boxplot(DTsoc3_W$avgcoyotew_class)
# hist(DTsoc3_W$avgcoyotew_class)
# 
# boxplot(DTsoc3_W$avgcoyotew_raw)
# hist(DTsoc3_W$avgcoyotew_raw)

boxplot(DTsoc3_W$avgcoyotew_rsf_LOG_n)
hist(DTsoc3_W$avgcoyotew_rsf_LOG_n)

## Spring

boxplot(DTsoc3_S$di)
hist(DTsoc3_S$di)



ggplot(DTsoc3_S, aes(x=di)) + geom_histogram(aes(x=di, y = ..count../sum(..count..)),binwidth = 0.05, fill = "grey", color = "black")+
  ylim(0,0.1)+
  xlim(-1.05,1.05)+
  geom_vline(aes(xintercept=mean(di)), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.80), color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=1)



#boxplot(DTsoc3_S$avgcaribous_class)
# hist(DTsoc3_S$avgcaribous_class)
# 
# #boxplot(DTsoc3_S$avgcaribous_raw)
# hist(DTsoc3_S$avgcaribous_raw)

#boxplot(DTsoc3_S$avgcaribous_rsf)
hist(DTsoc3_S$avgcaribous_rsf_LOG_n)
hist(DTsoc3_S$avgcaribous_rsf_LOG)
hist(DTsoc3_S$avgcaribous_rsf)

# #boxplot(DTsoc3_S$avgcoyotes_class)
# hist(DTsoc3_S$avgcoyotes_class)
# 
# #boxplot(DTsoc3_S$avgcoyotes_raw)
# hist(DTsoc3_S$avgcoyotes_raw)

#boxplot(DTsoc3_S$avgcoyotes_rsf)
hist(DTsoc3_S$avgcoyotes_rsf_LOG_n)
hist(DTsoc3_S$avgcoyotes_rsf_LOG)
hist(DTsoc3_S$avgcoyotes_rsf)

# #boxplot(DTsoc3_S$avgbear_class)
# hist(DTsoc3_S$avgbear_class)
# 
# #boxplot(DTsoc3_S$avgbear_raw)
# hist(DTsoc3_S$avgbear_raw)

#boxplot(DTsoc3_S$avgbear_rsf)
hist(DTsoc3_S$avgbear_rsf_LOG_n)



###GLM
## NL
## winter

nlNN_W_null<-
  glm(di ~ 1,
      data = DTsoc3_W)

summary(nlNN_W_null)


nlNN_W.DIpy_rsf<-
  glm(di ~ avgcaribouw_rsf_LOG_n,
      data = DTsoc3_W)

summary(nlNN_W.DIpy_rsf)

nlNN_W.DIpd_rsf<-
  glm(di ~ avgcoyotew_rsf_LOG_n,
      data = DTsoc3_W)

summary(nlNN_W.DIpd_rsf)

nlNN_W.DI_rsf<-
  glm(di ~ avgcaribouw_rsf_LOG_n*avgcoyotew_rsf_LOG_n,
      data = DTsoc3_W)

summary(nlNN_W.DI_rsf)

nlNN_W.DI_A_rsf<-
  glm(di ~ avgcaribouw_rsf_LOG_n+avgcoyotew_rsf_LOG_n,
      data = DTsoc3_W)

summary(nlNN_W.DI_A_rsf)



# plot(sim_slopes(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, robust=T))

johnson_neyman(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, control.fdr = TRUE,
               sig.color = "#00BFC4",
               insig.color = "gray50")

probe_interaction(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, control.fdr = TRUE)


effect_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, interval = TRUE, rug = TRUE)
effect_plot(nlNN_W.DI_rsf, pred = avgcaribouw_rsf_LOG_n, interval = TRUE, rug = TRUE)


visreg2d(nlNN_W.DI_rsf, "avgcaribouw_rsf_LOG_n", "avgcoyotew_rsf_LOG_n", plot.type="image", zlim=c(0.25,0.6), main="",
         xlab="Caribou foraging domain",
         ylab="Coyote hunting domain", zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)


## spring
rsf_nlNN_S_null<-
  glm(di ~ 1,
      data = DTsoc3_S)

summary(rsf_nlNN_S_null)

rsf_nlNN_S.DIpy<-
  glm(di ~ avgcaribous_rsf_LOG_n,
      data = DTsoc3_S)

summary(rsf_nlNN_S.DIpy)




### Bear v Coyote in Spring

rsf_nlNN_S.DI_Cpd<-
  glm(di ~ avgcoyotes_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_Cpd)

rsf_nlNN_S.DI_C<-
  glm(di ~ avgcaribous_rsf_LOG_n*avgcoyotes_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_C)

rsf_nlNN_S.DI_C_A<-
  glm(di ~ avgcaribous_rsf_LOG_n+avgcoyotes_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_C_A)

rsf_nlNN_S.DI_Bpd<-
  glm(di ~ avgbear_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_Bpd)

rsf_nlNN_S.DI_B<-
  glm(di ~ avgcaribous_rsf_LOG_n*avgbear_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_B)

rsf_nlNN_S.DI_B_A<-
  glm(di ~ avgcaribous_rsf_LOG_n+avgbear_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_B_A)

## bear and coyote  

rsf_nlNN_S.DI_CB_A<-
  glm(di ~ avgcaribous_rsf_LOG_n+avgbear_rsf_LOG_n+avgcoyotes_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_CB_A)

rsf_nlNN_S.DI_CB<-
  glm(di ~ avgcaribous_rsf_LOG_n*avgbear_rsf_LOG_n+avgcaribous_rsf_LOG_n*avgcoyotes_rsf_LOG_n,
      data = DTsoc3_S)
summary(rsf_nlNN_S.DI_CB)

# 
# rsf_nlNN_S.DI_B_C<-
#   glm(di ~ avgcaribous_rsf*avgbear_rsf+avgcoyotes_rsf,
#       data = DTsoc3_S)
# summary(rsf_nlNN_S.DI_B_C)
# 
# rsf_nlNN_S.DI_C_B<-
#   glm(di ~ avgbear_rsf+avgcaribous_rsf*avgcoyotes_rsf,
#       data = DTsoc3_S)
# summary(rsf_nlNN_S.DI_C_B)
# 
# 
# rsf_nlNN_S.DI_3W<-
#   glm(di ~ avgbear_rsf*avgcaribous_rsf*avgcoyotes_rsf,
#       data = DTsoc3_S)
# summary(rsf_nlNN_S.DI_3W)



johnson_neyman(rsf_nlNN_S.DI_C, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE,
               sig.color = "#00BFC4",
               insig.color = "gray50")

johnson_neyman(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE,
               sig.color = "#00BFC4",
               insig.color = "gray50")

effect_plot(rsf_nlNN_S.DI_CB_A, pred = avgcoyotes_rsf_LOG_n, interval = TRUE, rug = TRUE)
effect_plot(rsf_nlNN_S.DI_CB_A, pred = avgcaribous_rsf_LOG_n, interval = TRUE, rug = TRUE)
effect_plot(rsf_nlNN_S.DI_CB_A, pred = avgbear_rsf_LOG_n, interval = TRUE, rug = TRUE)


visreg2d(rsf_nlNN_S.DI_CB, "avgcaribous_rsf_LOG_n", "avgcoyotes_rsf_LOG_n", plot.type="image", zlim=c(0,1), main="",
         xlab="Caribou foraging domain",
         ylab="Coyote hunting domain", zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)














# rsf_nlNN_S.DI_B_C
# rsf_nlNN_S.DI_B
# 
# rsf_nlNN_S.DI_CB
# 
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, data = DTsoc3_W, linearity.check = TRUE, plot.points = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n_LOG_n, modx = avgcaribous_rsf_LOG_n, data = DTsoc3_S, linearity.check = TRUE, plot.points = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n_LOG_n, modx = avgcaribous_rsf_LOG_n, data = DTsoc3_S, linearity.check = TRUE, plot.points = TRUE)
# 
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, plot.points = TRUE)
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, plot.points = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, partial.residuals = TRUE)
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, partial.residuals = TRUE, interval = TRUE, int.width = 0.8, robust=TRUE)
# interact_plot(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0), interval = TRUE, int.width = 0.8, robust=TRUE)
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, plot.points = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, plot.points = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE, interval = TRUE, int.width = 0.8, robust=TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0), interval = TRUE, int.width = 0.8, robust=TRUE)
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, plot.points = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, plot.points = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0))
# 
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE, interval = TRUE, int.width = 0.8, robust=TRUE)
# interact_plot(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, partial.residuals = TRUE,  modx.values = c(0, 0.25, 0.50, 0.75, 1.0), interval = TRUE, int.width = 0.8, robust=TRUE)
# 


probe_interaction(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, control.fdr = TRUE)

# probe_interaction(rsf_nlNN_S.DI_B, pred = avgbear_rsf, modx = avgcaribous_rsf, robust=T)
# probe_interaction(rsf_nlNN_S.DI_C, pred = avgcoyotes_rsf, modx = avgcaribous_rsf, robust=T)

probe_interaction(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE)
probe_interaction(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE)

# probe_interaction(rsf_nlNN_S.DI_3W, pred = avgbear_rsf, modx = avgcaribous_rsf, mod2 = avgcoyotes_rsf, robust=T)


# plot(sim_slopes(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, robust=T))
# plot(sim_slopes(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, robust=T))
# plot(sim_slopes(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, robust=T))

johnson_neyman(nlNN_W.DI_rsf, pred = avgcoyotew_rsf_LOG_n, modx = avgcaribouw_rsf_LOG_n, control.fdr = TRUE)
johnson_neyman(rsf_nlNN_S.DI_CB, pred = avgbear_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE)
johnson_neyman(rsf_nlNN_S.DI_CB, pred = avgcoyotes_rsf_LOG_n, modx = avgcaribous_rsf_LOG_n, control.fdr = TRUE)



visreg2d(rsf_nlNN_S.DI_CB, "avgcaribous_rsf_LOG_n", "avgbear_rsf_LOG_n", plot.type="image", zlim=c(0,0.5), main="",
         xlab="Caribou foraging domain",
         ylab="Black bear hunting domain", zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)


visreg2d(nlNN_W.DI_rsf, "avgcaribouw_rsf_LOG_n", "avgcoyotew_rsf_LOG_n", plot.type="image", zlim=c(-0.5,0.80), main="",
         xlab="Caribou foraging domain",
         ylab="Coyote hunting domain", zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)



visreg2d(rsf_nlNN_S.DI_C, "avgcaribous_rsf_LOG_n", "avgcoyotes_rsf_LOG_n", plot.type="image", zlim=c(0,0.5), main="",
         xlab="Caribou foraging domain",
         ylab="Coyote hunting domain", zlab = "Predicted dynamic interaction of caribou dyad",nlevels=100)



###control.fdr = TRUE
### (Esarey & Sumner, 2017)

dir.create("OUTPUT_NEW/FINAL")
save.image("NL_results_LOG_N.Rdata")
