####
library(lubridate)
library(momentuHMM)

options(scipen = 999999)

load("Ready_for_HMM.Rdata")



### these scripts needs a little monitoring to find good priors for hmm


### 2 state movement model for bear
# black bear hibernation period as already been removed

bear2a$UID<-paste(bear2a$ID, bear2a$datetime, sep="_")

bear2b<-bear2a[,c(2,3,13,37,33)]

colnames(bear2b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(bear2b, timeStep = "2 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(50,700,50,350),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

bearhmm_2hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(bearhmm_2hr$STATE2))

sum.s2<- as.data.frame(bearhmm_2hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(bearhmm_2hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_bear_st2_2hr.csv")
write.csv(sum.s2.ID, "sum_bear_st2_2hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr'))])


### bear 4hr
### 2 state movement model for bear
# black bear hibernation period as already been removed

bear2.1a$UID<-paste(bear2.1a$ID, bear2.1a$datetime, sep="_")

bear2.1b<-bear2.1a[,c(2,3,13,37,33)]

colnames(bear2.1b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(bear2.1b, timeStep = "4 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

#plot(HMMData)

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(50,700,50,350,0.0006,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

bearhmm_4hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(bearhmm_4hr$STATE2))

sum.s2<- as.data.frame(bearhmm_4hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(bearhmm_4hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_bear_st2_4hr.csv")
write.csv(sum.s2.ID, "sum_bear_st2_4hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr'))])








### caribou 2hr
### 2 state movement model for caribou

caribou2a$UID<-paste(caribou2a$ID, caribou2a$datetime, sep="_")

caribou2b<-caribou2a[,c(2,3,13,37,33)]

colnames(caribou2b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(caribou2b, timeStep = "2 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)


# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(50,700,50,350,0.0001,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

caribouhmm_2hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(caribouhmm_2hr$STATE2))

sum.s2<- as.data.frame(caribouhmm_2hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(caribouhmm_2hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_caribou_st2_2hr.csv")
write.csv(sum.s2.ID, "sum_caribou_st2_2hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr','caribouhmm_2hr'))])













### elk 2hr
### 2 state movement model for elk

elk2a$UID<-paste(elk2a$ID, elk2a$datetime, sep="_")

elk2b<-elk2a[,c(2,3,13,35,31)]

colnames(elk2b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(elk2b, timeStep = "2 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)


# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(50,700,50,350,0.0009,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

elkhmm_2hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(elkhmm_2hr$STATE2))

sum.s2<- as.data.frame(elkhmm_2hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(elkhmm_2hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_elk_st2_2hr.csv")
write.csv(sum.s2.ID, "sum_elk_st2_2hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr','caribouhmm_2hr','elkhmm_2hr'))])








### wolf 1hr
### 2 state movement model for wolf

wolf2a$UID<-paste(wolf2a$ID, wolf2a$datetime, sep="_")

wolf2b<-wolf2a[,c(2,3,13,36,32)]

colnames(wolf2b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(wolf2b, timeStep = "1 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)


# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(50,700,50,350,0.003,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

wolfhmm_1hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(wolfhmm_1hr$STATE2))

sum.s2<- as.data.frame(wolfhmm_1hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(wolfhmm_1hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_wolf_st2_1hr.csv")
write.csv(sum.s2.ID, "sum_wolf_st2_1hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr','caribouhmm_2hr','elkhmm_2hr','wolfhmm_1hr'))])







### coyote 8hr
### 2 state movement model for coyote

coyote2a$UID<-paste(coyote2a$ID, coyote2a$datetime, sep="_")

coyote2b<-coyote2a[,c(2,3,13,37,33)]

colnames(coyote2b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(coyote2b, timeStep = "8 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)


# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(100,1000,100,500,0.001,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

coyotehmm_8hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(coyotehmm_8hr$STATE2))

sum.s2<- as.data.frame(coyotehmm_8hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(coyotehmm_8hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_coyote_st2_8hr.csv")
write.csv(sum.s2.ID, "sum_coyote_st2_8hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr','caribouhmm_2hr','elkhmm_2hr','wolfhmm_1hr',
                            'coyotehmm_8hr'))])








### coyote 4hr
### 2 state movement model for coyote

coyote2.1a$UID<-paste(coyote2.1a$ID, coyote2.1a$datetime, sep="_")

coyote2.1b<-coyote2.1a[,c(2,3,13,37,33)]

colnames(coyote2.1b)<-c('x','y','ID','UID','time')

set.seed(20)

ln.prior <- function(theta) dnorm(theta[2],-4,2,log=TRUE)  ### per B. McClintock

crwOut <- crawlWrap(coyote2.1b, timeStep = "4 hours", theta=c(6.855, -0.007), fixPar=c(NA,NA), prior=ln.prior)

HMMData <- prepData(data=crwOut)

#plot(HMMData)


# Indices of steps of length zero
whichzero1 <- which(HMMData$step == 0)
# Proportion of steps of length zero in the data set
length(whichzero1)/nrow(HMMData)
### use this value as zero mass for state 1

dist <- list(step = "gamma", angle = "wrpcauchy")
Par0_m2 <- list(step=c(100,1000,100,500,0.002,0),angle=c(3,0.3,0.3,0.5))

m2 <- fitHMM(data = HMMData, nbStates = 2 , dist = dist, Par0 = Par0_m2, estAngleMean = list(angle=TRUE))
m2

states2 <- viterbi(m2)
plot(m2)

HMMData$STATE2<-states2

sp2 <- stateProbs(m2)

HMMData$PROB2_1<-sp2[,1]
HMMData$PROB2_2<-sp2[,2]

### Probability of local state given global assigned state
HMMData$PROB_G2_L2<-ifelse(HMMData$STATE2==1,HMMData$PROB2_1,HMMData$PROB2_2)

coyotehmm_4hr<-subset(HMMData, !is.na(HMMData$UID))

### Total steps assigned to each state
summary(as.factor(coyotehmm_4hr$STATE2))

sum.s2<- as.data.frame(coyotehmm_4hr %>% group_by(STATE2) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))
sum.s2.ID<- as.data.frame(coyotehmm_4hr %>% group_by(STATE2, ID) %>% summarise(n=n(),mean=mean(step, na.rm=T), sd=sd(step, na.rm=T), min=min(step, na.rm=T),max=max(step, na.rm=T)))

write.csv(sum.s2, "sum_coyote_st2_4hr.csv")
write.csv(sum.s2.ID, "sum_coyote_st2_4hr_ID.csv")

rm(list= ls()[!(ls() %in% c('bear2a','bear2.1a','caribou2a','coyote2a','coyote2.1a','elk2a','wolf2a',
                            'sum.bear2hrdata2','sum.bear4hrdata2','sum.caribou2hrdata2','sum.coyote4hrdata2',
                            'sum.coyote8hrdata2','sum.elk2hrdata2','sum.wolf1hrdata2',
                            'bearhmm_2hr','bearhmm_4hr','caribouhmm_2hr','elkhmm_2hr','wolfhmm_1hr',
                            'coyotehmm_8hr','coyotehmm_4hr'))])



save.image("readyfor_ssf.Rdata")

