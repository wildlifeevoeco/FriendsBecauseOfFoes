
library(data.table)
library(ggplot2)
library(gridExtra)


coyote = fread("input/Coyote.csv")


##### COYOTE ######
length(unique(coyote$ANIMAL_ID))

coyote[,c("FIX_ID","VENDOR_CL","AGE","COLLAR_FILE_ID","EXCLUDE","DOP","LOCQUAL",
          "VALIDATED","COLLAR_TYPE_CL","COLLAR_ID","Fix_Time_Delta","Map_Quality",
          "EPSG_CODE","NAV") := NULL]

coyote[, c("X_COORD", "Y_COORD") := lapply(.SD, as.numeric), .SDcols = c("X_COORD", "Y_COORD")]

coyote[, c("Date","Time") := .(as.IDate(FIX_DATE, format = "%d.%m.%Y"), as.ITime(FIX_TIME, format = "%H:%M:%S"))]
coyote[,datetime := as.POSIXct(paste(FIX_DATE,FIX_TIME), format = "%Y-%m-%d %H:%M:%S" )]
coyote[ , julday := yday(FIX_DATE)]
coyote[ , Year := year(FIX_DATE)]

coyote[, uniqueN(ANIMAL_ID), by = "Year"]


length(unique(coyote$ANIMAL_ID))
unique(coyote$HERD)
unique(coyote$Year)

coyote2 <- subset(coyote, Y_COORD > 40 & Y_COORD < 60 & X_COORD < -30 & X_COORD > -60)

ggplot(coyote2) +
  geom_point(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID)),alpha = 0.5) +
  theme(legend.position = 'none') +
  facet_wrap(~Year)




coy2008 = subset(coyote2, Year == "2008")
coy2009 = subset(coyote2, Year == "2009")
coy2010 = subset(coyote2, Year == "2010")
coy2011 = subset(coyote2, Year == "2011")
coy2012 = subset(coyote2, Year == "2012")
coy2013 = subset(coyote2, Year == "2013")
coy2014 = subset(coyote2, Year == "2014")



aa = ggplot(coy2008[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
bb = ggplot(coy2009[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
cc = ggplot(coy2010[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
dd = ggplot(coy2011[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ee = ggplot(coy2012[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ff = ggplot(coy2013[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ee = ggplot(coy2014[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
grid.arrange(aa,bb,cc,dd,ee,ff,ee,ncol = 4, nrow = 2)