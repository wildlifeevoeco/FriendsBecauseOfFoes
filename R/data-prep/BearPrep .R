

library(data.table)
library(ggplot2)
library(gridExtra)


bear = fread("input/Bears.csv")

## BEAR ###
length(unique(bear$ANIMAL_ID))

bear[,c("FIX_ID","VENDOR_CL","AGE","COLLAR_FILE_ID","EXCLUDE","DOP","LOCQUAL",
        "VALIDATED","COLLAR_TYPE_CL","COLLAR_ID","Fix_Time_Delta","Map_Quality",
        "EPSG_CODE","NAV") := NULL]

is.data.table(bear)
bear[, c("X_COORD", "Y_COORD") := lapply(.SD, as.numeric), .SDcols = c("X_COORD", "Y_COORD")]

bear[, c("Date","Time") := .(as.IDate(FIX_DATE, format = "%d.%m.%Y"), as.ITime(FIX_TIME, format = "%H:%M:%S"))]
bear[,datetime := as.POSIXct(paste(FIX_DATE,FIX_TIME), format = "%Y-%m-%d %H:%M:%S" )]
bear[ , julday := yday(FIX_DATE)]
bear[ , Year := year(FIX_DATE)]

bear[, uniqueN(ANIMAL_ID), by = "Year"]


ggplot(bear) +
  geom_point(aes(Y_COORD, X_COORD, color = factor(ANIMAL_ID)),alpha = 0.5)


bear2 <- subset(bear, Y_COORD > 40 & Y_COORD < 60 & X_COORD < -30 & X_COORD > -70)

bear2008 = subset(bear2, Year == "2008")
bear2009 = subset(bear2, Year == "2009")
bear2010 = subset(bear2, Year == "2010")
bear2011 = subset(bear2, Year == "2011")
bear2012 = subset(bear2, Year == "2012")
bear2013 = subset(bear2, Year == "2013")



## quick look at where the bears are located:
aa = ggplot(bear2008[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
bb = ggplot(bear2009[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
cc = ggplot(bear2010[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
dd = ggplot(bear2011[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ee = ggplot(bear2012[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
ff = ggplot(bear2013[order(datetime)]) +
  geom_path(aes(X_COORD, Y_COORD, color = factor(ANIMAL_ID),
                group = factor(ANIMAL_ID)), alpha =0.5) +
  theme(legend.position = 'none')
grid.arrange(aa,bb,cc,dd,ee,ff,ncol = 3, nrow = 2)




