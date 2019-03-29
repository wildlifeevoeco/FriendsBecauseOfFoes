### Set variables ----
# Season thresholds
winter <- c(1, 72)
spring <- c(141, 212)



### Set output columns (for prep) ----
outputVariables <- c('id', 'species', 'season', 'timegroup',
                     'idate', 'itime', 'datetime', 
                     'EASTING', 'NORTHING',
                     'julday', 'yr', 'mnth', 'stepLength', 'moveRate', 'fixRate')
