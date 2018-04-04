
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

### Input data ----

#RMNP file
rmnp <-
#NL file
nl <- 


###GLM
#nearest neighbour 

rmNN <- glm(TurnAngle ~ PreyHD + PredHD + PreyHD:PredHD, data = rmnp)


#turning angle

#movement rate


###Plot
#heatmap of social response across domains 
#prey on Y, predator on X, hot colours is positive social response, cold is negative






