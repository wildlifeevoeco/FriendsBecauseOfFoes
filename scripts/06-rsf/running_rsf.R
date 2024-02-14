#### 

load("output/READY_FOR_MODELS2_new.Rdata")

rm(list=setdiff(ls(), c("bear2h_rsf","caribou2h_rsf","coyote4h_rsf","coyote8h_rsf",
                        "elk2h_rsf","wolf1h_rsf")))

library(dplyr)


### RMNP

elk2h_rsf_spr_st1<-subset(elk2h_rsf, elk2h_rsf$season=="spring" & (elk2h_rsf$STATE2==1 | is.na(elk2h_rsf$STATE2)))
elk2h_rsf_spr_st2<-subset(elk2h_rsf, elk2h_rsf$season=="spring" & (elk2h_rsf$STATE2==2| is.na(elk2h_rsf$STATE2)))
elk2h_rsf_win_st1<-subset(elk2h_rsf, elk2h_rsf$season=="winter" & (elk2h_rsf$STATE2==1| is.na(elk2h_rsf$STATE2)))
elk2h_rsf_win_st2<-subset(elk2h_rsf, elk2h_rsf$season=="winter" & (elk2h_rsf$STATE2==2| is.na(elk2h_rsf$STATE2)))

wolf1h_rsf_spr_st1<-subset(wolf1h_rsf, wolf1h_rsf$season=="spring" & (wolf1h_rsf$STATE2==1| is.na(wolf1h_rsf$STATE2)))
wolf1h_rsf_spr_st2<-subset(wolf1h_rsf, wolf1h_rsf$season=="spring" & (wolf1h_rsf$STATE2==2| is.na(wolf1h_rsf$STATE2)))
wolf1h_rsf_win_st1<-subset(wolf1h_rsf, wolf1h_rsf$season=="winter" & (wolf1h_rsf$STATE2==1| is.na(wolf1h_rsf$STATE2)))
wolf1h_rsf_win_st2<-subset(wolf1h_rsf, wolf1h_rsf$season=="winter" & (wolf1h_rsf$STATE2==2| is.na(wolf1h_rsf$STATE2)))

nrow(elk2h_rsf_spr_st1)
sum(elk2h_rsf_spr_st1$case==1)
(nrow(elk2h_rsf_spr_st1)-sum(elk2h_rsf_spr_st1$case==1))/sum(elk2h_rsf_spr_st1$case==1)

nrow(elk2h_rsf_spr_st2)
sum(elk2h_rsf_spr_st2$case==1)
(nrow(elk2h_rsf_spr_st2)-sum(elk2h_rsf_spr_st2$case==1))/sum(elk2h_rsf_spr_st2$case==1)

nrow(elk2h_rsf_win_st1)
sum(elk2h_rsf_win_st1$case==1)
(nrow(elk2h_rsf_win_st1)-sum(elk2h_rsf_win_st1$case==1))/sum(elk2h_rsf_win_st1$case==1)

nrow(elk2h_rsf_win_st2)
sum(elk2h_rsf_win_st2$case==1)
(nrow(elk2h_rsf_win_st2)-sum(elk2h_rsf_win_st2$case==1))/sum(elk2h_rsf_win_st2$case==1)

nrow(wolf1h_rsf_spr_st1)
sum(wolf1h_rsf_spr_st1$case==1)
(nrow(wolf1h_rsf_spr_st1)-sum(wolf1h_rsf_spr_st1$case==1))/sum(wolf1h_rsf_spr_st1$case==1)

nrow(wolf1h_rsf_spr_st2)
sum(wolf1h_rsf_spr_st2$case==1)
(nrow(wolf1h_rsf_spr_st2)-sum(wolf1h_rsf_spr_st2$case==1))/sum(wolf1h_rsf_spr_st2$case==1)

nrow(wolf1h_rsf_win_st1)
sum(wolf1h_rsf_win_st1$case==1)
(nrow(wolf1h_rsf_win_st1)-sum(wolf1h_rsf_win_st1$case==1))/sum(wolf1h_rsf_win_st1$case==1)

nrow(wolf1h_rsf_win_st2)
sum(wolf1h_rsf_win_st2$case==1)
(nrow(wolf1h_rsf_win_st2)-sum(wolf1h_rsf_win_st2$case==1))/sum(wolf1h_rsf_win_st2$case==1)


# sum.elk2h<-elk2h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                 avg.ag=round(mean(ag),4), sd.ag=round(sd(ag),4), min.ag=round(min(ag),4), max.ag=round(max(ag),4),
#                                 avg.bg=round(mean(bg),4), sd.bg=round(sd(bg),4), min.bg=round(min(bg),4), max.bg=round(max(bg),4),
#                                 avg.cf=round(mean(cf),4), sd.cf=round(sd(cf),4), min.cf=round(min(cf),4), max.cf=round(max(cf),4),
#                                 avg.df=round(mean(df),4), sd.df=round(sd(df),4), min.df=round(min(df),4), max.df=round(max(df),4),
#                                 avg.gl=round(mean(gl),4), sd.gl=round(sd(gl),4), min.gl=round(min(gl),4), max.gl=round(max(gl),4),
#                                 avg.mh=round(mean(mh),4), sd.mh=round(sd(mh),4), min.mh=round(min(mh),4), max.mh=round(max(mh),4),
#                                 avg.mf=round(mean(mf),4), sd.mf=round(sd(mf),4), min.mf=round(min(mf),4), max.mf=round(max(mf),4),
#                                 avg.od=round(mean(od),4), sd.od=round(sd(od),4), min.od=round(min(od),4), max.od=round(max(od),4),
#                                 avg.lf=round(mean(lf),4), sd.lf=round(sd(lf),4), min.lf=round(min(lf),4), max.lf=round(max(lf),4),
#                                 avg.wt=round(mean(wt),4), sd.wt=round(sd(wt),4), min.wt=round(min(wt),4), max.wt=round(max(wt),4),
#                                 avg.ru=round(mean(ru),4), sd.ru=round(sd(ru),4), min.ru=round(min(ru),4), max.ru=round(max(ru),4))
# 
# sum.wolf1h<-wolf1h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                                 avg.ag=round(mean(ag),4), sd.ag=round(sd(ag),4), min.ag=round(min(ag),4), max.ag=round(max(ag),4),
#                                                 avg.bg=round(mean(bg),4), sd.bg=round(sd(bg),4), min.bg=round(min(bg),4), max.bg=round(max(bg),4),
#                                                 avg.cf=round(mean(cf),4), sd.cf=round(sd(cf),4), min.cf=round(min(cf),4), max.cf=round(max(cf),4),
#                                                 avg.df=round(mean(df),4), sd.df=round(sd(df),4), min.df=round(min(df),4), max.df=round(max(df),4),
#                                                 avg.gl=round(mean(gl),4), sd.gl=round(sd(gl),4), min.gl=round(min(gl),4), max.gl=round(max(gl),4),
#                                                 avg.mh=round(mean(mh),4), sd.mh=round(sd(mh),4), min.mh=round(min(mh),4), max.mh=round(max(mh),4),
#                                                 avg.mf=round(mean(mf),4), sd.mf=round(sd(mf),4), min.mf=round(min(mf),4), max.mf=round(max(mf),4),
#                                                 avg.od=round(mean(od),4), sd.od=round(sd(od),4), min.od=round(min(od),4), max.od=round(max(od),4),
#                                                 avg.lf=round(mean(lf),4), sd.lf=round(sd(lf),4), min.lf=round(min(lf),4), max.lf=round(max(lf),4),
#                                                 avg.wt=round(mean(wt),4), sd.wt=round(sd(wt),4), min.wt=round(min(wt),4), max.wt=round(max(wt),4),
#                                                 avg.ru=round(mean(ru),4), sd.ru=round(sd(ru),4), min.ru=round(min(ru),4), max.ru=round(max(ru),4))
# 

#### ag very rare
#### dropping

### testing for correlation

cor.elk2h.spr.st1<-round(cor(elk2h_rsf_spr_st1[,c(37:47)]),2)
cor.elk2h.spr.st2<-round(cor(elk2h_rsf_spr_st2[,c(37:47)]),2)
cor.elk2h.win.st1<-round(cor(elk2h_rsf_win_st1[,c(37:47)]),2)
cor.elk2h.win.st2<-round(cor(elk2h_rsf_win_st2[,c(37:47)]),2)

cor.wolf1h.spr.st1<-round(cor(wolf1h_rsf_spr_st1[,c(38:48)]),2)
cor.wolf1h.spr.st2<-round(cor(wolf1h_rsf_spr_st2[,c(38:48)]),2)
cor.wolf1h.win.st1<-round(cor(wolf1h_rsf_win_st1[,c(38:48)]),2)
cor.wolf1h.win.st2<-round(cor(wolf1h_rsf_win_st2[,c(38:48)]),2)


### no major correlation issues (|R|>0.7)

# 
# all.lc<-rbind(elk2h_rsf_win_st1[,37:44], elk2h_rsf_win_st2[,37:44], wolf1h_rsf_win_st1[,38:45], wolf1h_rsf_win_st2[,38:45])
# all.ru<-c(elk2h_rsf_win_st1$ru, elk2h_rsf_win_st2$ru, wolf1h_rsf_win_st1$ru, wolf1h_rsf_win_st2$ru)
# 
# min.log_RMNP<-0.1*min(all.lc[all.lc>0])
# min.rlog_RMNP<-0.1*min(all.ru[all.ru>0])

elk.win.st1 <- glm(case ~ bg + cf + mh + mf + od  + log(lf*1000 + 1) + log(wt*1000 + 1) + ru,  family = "binomial", data=elk2h_rsf_win_st1)
elk.win.st2 <- glm(case ~ bg + cf + mh + mf + od  + log(lf*1000 + 1) + log(wt*1000 + 1) + ru,  family = "binomial", data=elk2h_rsf_win_st2)

wolf.win.st1 <- glm(case ~ bg + cf + mh + mf + od  + log(lf*1000 + 1) + log(wt*1000 + 1) + ru,  family = "binomial", data=wolf1h_rsf_win_st1)
wolf.win.st2 <- glm(case ~ bg + cf + mh + mf + od  + log(lf*1000 + 1) + log(wt*1000 + 1) + ru,  family = "binomial", data=wolf1h_rsf_win_st2)

### NL

caribou2h_rsf_spr_st1<-subset(caribou2h_rsf, caribou2h_rsf$season=="spring" & (caribou2h_rsf$STATE2==1| is.na(caribou2h_rsf$STATE2)))
caribou2h_rsf_spr_st2<-subset(caribou2h_rsf, caribou2h_rsf$season=="spring" & (caribou2h_rsf$STATE2==2| is.na(caribou2h_rsf$STATE2)))
caribou2h_rsf_win_st1<-subset(caribou2h_rsf, caribou2h_rsf$season=="winter" & (caribou2h_rsf$STATE2==1| is.na(caribou2h_rsf$STATE2)))
caribou2h_rsf_win_st2<-subset(caribou2h_rsf, caribou2h_rsf$season=="winter" & (caribou2h_rsf$STATE2==2| is.na(caribou2h_rsf$STATE2)))

bear2h_rsf_spr_st1<-subset(bear2h_rsf, bear2h_rsf$season=="spring" & (bear2h_rsf$STATE2==1| is.na(bear2h_rsf$STATE2)))
bear2h_rsf_spr_st2<-subset(bear2h_rsf, bear2h_rsf$season=="spring" & (bear2h_rsf$STATE2==2| is.na(bear2h_rsf$STATE2)))

colnames(coyote4h_rsf)

coyote4h_rsf<-coyote4h_rsf[,c(1:40,43:45,41,46,42)]  ### not sure where they got unordered but this fixes them to the same order

coyote4h_rsf_spr_st1<-subset(coyote4h_rsf, coyote4h_rsf$season=="spring" & (coyote4h_rsf$STATE2==1| is.na(coyote4h_rsf$STATE2)))
coyote4h_rsf_spr_st2<-subset(coyote4h_rsf, coyote4h_rsf$season=="spring" & (coyote4h_rsf$STATE2==2| is.na(coyote4h_rsf$STATE2)))

colnames(coyote8h_rsf)

coyote8h_rsf<-coyote8h_rsf[,c(1:40,43:45,41,46,42)] ### not sure where they got unordered but this fixes them to the same order

coyote8h_rsf_win_st1<-subset(coyote8h_rsf, coyote8h_rsf$season=="spring" & (coyote8h_rsf$STATE2==1| is.na(coyote8h_rsf$STATE2)))
coyote8h_rsf_win_st2<-subset(coyote8h_rsf, coyote8h_rsf$season=="spring" & (coyote8h_rsf$STATE2==2| is.na(coyote8h_rsf$STATE2)))

### sample size and used not used ratio
nrow(caribou2h_rsf_spr_st1)
sum(caribou2h_rsf_spr_st1$case==1)
(nrow(caribou2h_rsf_spr_st1)-sum(caribou2h_rsf_spr_st1$case==1))/sum(caribou2h_rsf_spr_st1$case==1)

nrow(caribou2h_rsf_spr_st2)
sum(caribou2h_rsf_spr_st2$case==1)
(nrow(caribou2h_rsf_spr_st2)-sum(caribou2h_rsf_spr_st2$case==1))/sum(caribou2h_rsf_spr_st2$case==1)

nrow(caribou2h_rsf_win_st1)
sum(caribou2h_rsf_win_st1$case==1)
(nrow(caribou2h_rsf_win_st1)-sum(caribou2h_rsf_win_st1$case==1))/sum(caribou2h_rsf_win_st1$case==1)

nrow(caribou2h_rsf_win_st2)
sum(caribou2h_rsf_win_st2$case==1)
(nrow(caribou2h_rsf_win_st2)-sum(caribou2h_rsf_win_st2$case==1))/sum(caribou2h_rsf_win_st2$case==1)

nrow(bear2h_rsf_spr_st1)
sum(bear2h_rsf_spr_st1$case==1)
(nrow(bear2h_rsf_spr_st1)-sum(bear2h_rsf_spr_st1$case==1))/sum(bear2h_rsf_spr_st1$case==1)

nrow(bear2h_rsf_spr_st2)
sum(bear2h_rsf_spr_st2$case==1)
(nrow(bear2h_rsf_spr_st2)-sum(bear2h_rsf_spr_st2$case==1))/sum(bear2h_rsf_spr_st2$case==1)

nrow(coyote4h_rsf_spr_st1)
sum(coyote4h_rsf_spr_st1$case==1)
(nrow(coyote4h_rsf_spr_st1)-sum(coyote4h_rsf_spr_st1$case==1))/sum(coyote4h_rsf_spr_st1$case==1)

nrow(coyote4h_rsf_spr_st2)
sum(coyote4h_rsf_spr_st2$case==1)
(nrow(coyote4h_rsf_spr_st2)-sum(coyote4h_rsf_spr_st2$case==1))/sum(coyote4h_rsf_spr_st2$case==1)

nrow(coyote8h_rsf_win_st1)
sum(coyote8h_rsf_win_st1$case==1)
(nrow(coyote8h_rsf_win_st1)-sum(coyote8h_rsf_win_st1$case==1))/sum(coyote8h_rsf_win_st1$case==1)

nrow(coyote8h_rsf_win_st2)
sum(coyote8h_rsf_win_st2$case==1)
(nrow(coyote8h_rsf_win_st2)-sum(coyote8h_rsf_win_st2$case==1))/sum(coyote8h_rsf_win_st2$case==1)






# 
# sum.caribou<-caribou2h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                 avg.f=round(mean(f),2), sd.f=round(sd(f),2), min.f=round(min(f),2), max.f=round(max(f),2),
#                                 avg.l=round(mean(l),2), sd.l=round(sd(l),2), min.l=round(min(l),2), max.l=round(max(l),2),
#                                 avg.r=round(mean(r),2), sd.r=round(sd(r),2), min.r=round(min(r),2), max.r=round(max(r),2),
#                                 avg.s=round(mean(s),2), sd.s=round(sd(s),2), min.s=round(min(s),2), max.s=round(max(s),2),
#                                 avg.lf=round(mean(lf),2), sd.lf=round(sd(lf),2), min.lf=round(min(lf),2), max.lf=round(max(lf),2),
#                                 avg.w=round(mean(w),2), sd.w=round(sd(w),2), min.w=round(min(w),2), max.w=round(max(w),2),
#                                 avg.ru=round(mean(ru),2), sd.ru=round(sd(ru),2), min.ru=round(min(ru),2), max.ru=round(max(ru),2))
# sum.bear2h<-bear2h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                                 avg.f=round(mean(f),2), sd.f=round(sd(f),2), min.f=round(min(f),2), max.f=round(max(f),2),
#                                                 avg.l=round(mean(l),2), sd.l=round(sd(l),2), min.l=round(min(l),2), max.l=round(max(l),2),
#                                                 avg.r=round(mean(r),2), sd.r=round(sd(r),2), min.r=round(min(r),2), max.r=round(max(r),2),
#                                                 avg.s=round(mean(s),2), sd.s=round(sd(s),2), min.s=round(min(s),2), max.s=round(max(s),2),
#                                                 avg.lf=round(mean(lf),2), sd.lf=round(sd(lf),2), min.lf=round(min(lf),2), max.lf=round(max(lf),2),
#                                                 avg.w=round(mean(w),2), sd.w=round(sd(w),2), min.w=round(min(w),2), max.w=round(max(w),2),
#                                                 avg.ru=round(mean(ru),2), sd.ru=round(sd(ru),2), min.ru=round(min(ru),2), max.ru=round(max(ru),2))
# 
# 
# sum.coyote4h<-coyote4h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                                 avg.f=round(mean(f),2), sd.f=round(sd(f),2), min.f=round(min(f),2), max.f=round(max(f),2),
#                                                 avg.l=round(mean(l),2), sd.l=round(sd(l),2), min.l=round(min(l),2), max.l=round(max(l),2),
#                                                 avg.r=round(mean(r),2), sd.r=round(sd(r),2), min.r=round(min(r),2), max.r=round(max(r),2),
#                                                 avg.s=round(mean(s),2), sd.s=round(sd(s),2), min.s=round(min(s),2), max.s=round(max(s),2),
#                                                 avg.lf=round(mean(lf),2), sd.lf=round(sd(lf),2), min.lf=round(min(lf),2), max.lf=round(max(lf),2),
#                                                 avg.w=round(mean(w),2), sd.w=round(sd(w),2), min.w=round(min(w),2), max.w=round(max(w),2),
#                                                 avg.ru=round(mean(ru),2), sd.ru=round(sd(ru),2), min.ru=round(min(ru),2), max.ru=round(max(ru),2))
# 
# sum.coyote8h<-coyote8h_rsf %>% 
#   group_by(season, STATE2, case) %>% summarize(count=n(),
#                                                 avg.f=round(mean(f),2), sd.f=round(sd(f),2), min.f=round(min(f),2), max.f=round(max(f),2),
#                                                 avg.l=round(mean(l),2), sd.l=round(sd(l),2), min.l=round(min(l),2), max.l=round(max(l),2),
#                                                 avg.r=round(mean(r),2), sd.r=round(sd(r),2), min.r=round(min(r),2), max.r=round(max(r),2),
#                                                 avg.s=round(mean(s),2), sd.s=round(sd(s),2), min.s=round(min(s),2), max.s=round(max(s),2),
#                                                 avg.lf=round(mean(lf),2), sd.lf=round(sd(lf),2), min.lf=round(min(lf),2), max.lf=round(max(lf),2),
#                                                 avg.w=round(mean(w),2), sd.w=round(sd(w),2), min.w=round(min(w),2), max.w=round(max(w),2),
#                                                 avg.ru=round(mean(ru),2), sd.ru=round(sd(ru),2), min.ru=round(min(ru),2), max.ru=round(max(ru),2))
# 


### testing for correlation

cor.caribou2h.spr.st1<-round(cor(caribou2h_rsf_spr_st1[,c(39:45)]),2)
cor.caribou2h.spr.st2<-round(cor(caribou2h_rsf_spr_st2[,c(39:45)]),2)
cor.caribou2h.win.st1<-round(cor(caribou2h_rsf_win_st1[,c(39:45)]),2)
cor.caribou2h.win.st2<-round(cor(caribou2h_rsf_win_st2[,c(39:45)]),2)

cor.bear2h.spr.st1<-round(cor(bear2h_rsf_spr_st1[,c(39:45)]),2)
cor.bear2h.spr.st2<-round(cor(bear2h_rsf_spr_st2[,c(39:45)]),2)

cor.coyote4h.spr.st1<-round(cor(coyote4h_rsf_spr_st1[,c(40:46)]),2)
cor.coyote4h.spr.st2<-round(cor(coyote4h_rsf_spr_st2[,c(40:46)]),2)

cor.coyote8h.win.st1<-round(cor(coyote8h_rsf_win_st1[,c(40:46)]),2)
cor.coyote8h.win.st2<-round(cor(coyote8h_rsf_win_st2[,c(40:46)]),2)


### no major correlation issues (|R|>0.7)

# all.lc<-rbind(caribou2h_rsf_win_st1[,39:42], caribou2h_rsf_win_st2[,39:42], caribou2h_rsf_spr_st1[,39:42], caribou2h_rsf_spr_st2[,39:42],
#               bear2h_rsf_spr_st1[,39:42], bear2h_rsf_spr_st2[,39:42],
#               coyote8h_rsf_win_st1[,39:42], coyote8h_rsf_win_st2[,39:42], coyote4h_rsf_spr_st1[,39:42], coyote4h_rsf_spr_st2[,39:42])
# 
# all.ru<-c(caribou2h_rsf_win_st1$ru, caribou2h_rsf_win_st2$ru, caribou2h_rsf_spr_st1$ru, caribou2h_rsf_spr_st2$ru,
#               bear2h_rsf_spr_st1$ru, bear2h_rsf_spr_st2$ru,
#               coyote8h_rsf_win_st1$ru, coyote8h_rsf_win_st2$ru, coyote4h_rsf_spr_st1$ru, coyote4h_rsf_spr_st2$ru)
# 
# min.log_NL<-0.1*min(all.lc[all.lc>0])
# min.rlog_NL<-0.1*min(all.ru[all.ru>0])

caribou.spr.st1 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=caribou2h_rsf_spr_st1)
caribou.spr.st2 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=caribou2h_rsf_spr_st2)
caribou.win.st1 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=caribou2h_rsf_win_st1)
caribou.win.st2 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=caribou2h_rsf_win_st2)

bear.spr.st1 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=bear2h_rsf_spr_st1)
bear.spr.st2 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=bear2h_rsf_spr_st2)

coyote4.spr.st1 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=coyote4h_rsf_spr_st1)
coyote4.spr.st2 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=coyote4h_rsf_spr_st2)

coyote.win.st1 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=coyote8h_rsf_win_st1)
coyote.win.st2 <- glm(case ~ f + l + r + s  + log(lf*1000 + 1) + log(w*1000 + 1) + ru,  family = "binomial", data=coyote8h_rsf_win_st2)


res.elk.win.st1<-as.data.frame(t(summary(elk.win.st1)$coefficients[,c(1,2)]))
res.elk.win.st1<-tibble::rownames_to_column(res.elk.win.st1, "VALUE")
res.elk.win.st1$species<-"elk"
res.elk.win.st1$season<-"winter"
res.elk.win.st1$state<-1

res.elk.win.st2<-as.data.frame(t(summary(elk.win.st2)$coefficients[,c(1,2)]))
res.elk.win.st2<-tibble::rownames_to_column(res.elk.win.st2, "VALUE")
res.elk.win.st2$species<-"elk"
res.elk.win.st2$season<-"winter"
res.elk.win.st2$state<-2

colnames(res.elk.win.st1)[2:10]<-c("intercept","bg","cf","mh","mf","od","lf","wt","ru")
colnames(res.elk.win.st2)[2:10]<-c("intercept","bg","cf","mh","mf","od","lf","wt","ru")

res.wolf.win.st1<-as.data.frame(t(summary(wolf.win.st1)$coefficients[,c(1,2)]))
res.wolf.win.st1<-tibble::rownames_to_column(res.wolf.win.st1, "VALUE")
res.wolf.win.st1$species<-"wolf"
res.wolf.win.st1$season<-"winter"
res.wolf.win.st1$state<-1

res.wolf.win.st2<-as.data.frame(t(summary(wolf.win.st2)$coefficients[,c(1,2)]))
res.wolf.win.st2<-tibble::rownames_to_column(res.wolf.win.st2, "VALUE")
res.wolf.win.st2$species<-"wolf"
res.wolf.win.st2$season<-"winter"
res.wolf.win.st2$state<-2

colnames(res.wolf.win.st1)[2:10]<-c("intercept","bg","cf","mh","mf","od","lf","wt","ru")
colnames(res.wolf.win.st2)[2:10]<-c("intercept","bg","cf","mh","mf","od","lf","wt","ru")

res.elk<-rbind(res.elk.win.st1,res.elk.win.st2)

res.wolf<-rbind(res.wolf.win.st1,res.wolf.win.st2)

res.RMNP<-rbind(res.elk.win.st1,res.elk.win.st2,res.wolf.win.st1,res.wolf.win.st2)

### NL



res.caribou.spr.st1<-as.data.frame(t(summary(caribou.spr.st1)$coefficients[,c(1,2)]))
res.caribou.spr.st1<-tibble::rownames_to_column(res.caribou.spr.st1, "VALUE")
res.caribou.spr.st1$species<-"caribou"
res.caribou.spr.st1$season<-"spring"
res.caribou.spr.st1$state<-1

res.caribou.spr.st2<-as.data.frame(t(summary(caribou.spr.st2)$coefficients[,c(1,2)]))
res.caribou.spr.st2<-tibble::rownames_to_column(res.caribou.spr.st2, "VALUE")
res.caribou.spr.st2$species<-"caribou"
res.caribou.spr.st2$season<-"spring"
res.caribou.spr.st2$state<-2

res.caribou.win.st1<-as.data.frame(t(summary(caribou.win.st1)$coefficients[,c(1,2)]))
res.caribou.win.st1<-tibble::rownames_to_column(res.caribou.win.st1, "VALUE")
res.caribou.win.st1$species<-"caribou"
res.caribou.win.st1$season<-"winter"
res.caribou.win.st1$state<-1

res.caribou.win.st2<-as.data.frame(t(summary(caribou.win.st2)$coefficients[,c(1,2)]))
res.caribou.win.st2<-tibble::rownames_to_column(res.caribou.win.st2, "VALUE")
res.caribou.win.st2$species<-"caribou"
res.caribou.win.st2$season<-"winter"
res.caribou.win.st2$state<-2

res.bear.spr.st1<-as.data.frame(t(summary(bear.spr.st1)$coefficients[,c(1,2)]))
res.bear.spr.st1<-tibble::rownames_to_column(res.bear.spr.st1, "VALUE")
res.bear.spr.st1$species<-"bear2"
res.bear.spr.st1$season<-"spring"
res.bear.spr.st1$state<-1

res.bear.spr.st2<-as.data.frame(t(summary(bear.spr.st2)$coefficients[,c(1,2)]))
res.bear.spr.st2<-tibble::rownames_to_column(res.bear.spr.st2, "VALUE")
res.bear.spr.st2$species<-"bear2"
res.bear.spr.st2$season<-"spring"
res.bear.spr.st2$state<-2






res.coyote4.spr.st1<-as.data.frame(t(summary(coyote4.spr.st1)$coefficients[,c(1,2)]))
res.coyote4.spr.st1<-tibble::rownames_to_column(res.coyote4.spr.st1, "VALUE")
res.coyote4.spr.st1$species<-"coyote4"
res.coyote4.spr.st1$season<-"spring"
res.coyote4.spr.st1$state<-1

res.coyote4.spr.st2<-as.data.frame(t(summary(coyote4.spr.st2)$coefficients[,c(1,2)]))
res.coyote4.spr.st2<-tibble::rownames_to_column(res.coyote4.spr.st2, "VALUE")
res.coyote4.spr.st2$species<-"coyote4"
res.coyote4.spr.st2$season<-"spring"
res.coyote4.spr.st2$state<-2

res.coyote.win.st1<-as.data.frame(t(summary(coyote.win.st1)$coefficients[,c(1,2)]))
res.coyote.win.st1<-tibble::rownames_to_column(res.coyote.win.st1, "VALUE")
res.coyote.win.st1$species<-"coyote8"
res.coyote.win.st1$season<-"winter"
res.coyote.win.st1$state<-1

res.coyote.win.st2<-as.data.frame(t(summary(coyote.win.st2)$coefficients[,c(1,2)]))
res.coyote.win.st2<-tibble::rownames_to_column(res.coyote.win.st2, "VALUE")
res.coyote.win.st2$species<-"coyote8"
res.coyote.win.st2$season<-"winter"
res.coyote.win.st2$state<-2

res.caribou<-rbind(res.caribou.spr.st1,res.caribou.spr.st2,res.caribou.win.st1,res.caribou.win.st2)
res.bear2<-rbind(res.bear.spr.st1,res.bear.spr.st2)
res.coyote4<-rbind(res.coyote4.spr.st1,res.coyote4.spr.st2)
res.coyote8<-rbind(res.coyote.win.st1,res.coyote.win.st2)

res.NL<-rbind(res.caribou.spr.st1,res.caribou.spr.st2,res.caribou.win.st1,res.caribou.win.st2,
              res.bear.spr.st1,res.bear.spr.st2,
              res.coyote4.spr.st1,res.coyote4.spr.st2,
              res.coyote.win.st1,res.coyote.win.st2)

colnames(res.NL)[2:9]<-c("intercept","f","l","r","s","lf","w","ru")
#   
# dir.create("OUTPUT_NEW")  
# dir.create("OUTPUT_NEW/rsf_results")
# 
write.csv(res.RMNP, "log_rsf_res_RMNP_logLF_W.csv")
write.csv(res.NL, "log_rsf_res_NL_logLF_W.csv")


####  RERUN and RESAVE TO REMOVE CHAFF  ######

save.image("log_RSF_MODELS_NEW_LOG.Rdata")



require(performance)
# random intercept model
r2(elk.win.st1)
check_collinearity(elk.win.st1)

r2(wolf.win.st2)
check_collinearity(wolf.win.st2)

r2(caribou.win.st1)
check_collinearity(caribou.win.st1)

r2(caribou.spr.st1)
check_collinearity(caribou.spr.st1)

r2(bear.spr.st2)
check_collinearity(bear.spr.st2)

r2(coyote4.spr.st2)
check_collinearity(coyote4.spr.st2)

r2(coyote.win.st2)
check_collinearity(coyote.win.st2)

