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


rmnp[dyadDist < 500, qplot()

     
     ###GLM
#nearest neighbour 
# rmNN <- glm(TurnAngle ~ PreyHD + PredHD + PreyHD:PredHD, data = rmnp)
###GLM
 
# SA <- glm(SocialMeasure ~ PreyHD + PredHD + PreyHD:PredHD, data = rmnp)
## social measures are: dSI, nWithin5000,

rmNNWinter <-
  glm(absAngle ~ elkwinter + wolfwinter + elkwinter:wolfwinter,
      data = rmnp[season == 'winter'])

rmNNSpring <-
  glm(absAngle ~ elkspring + wolfspring + elkspring:wolfspring,
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






