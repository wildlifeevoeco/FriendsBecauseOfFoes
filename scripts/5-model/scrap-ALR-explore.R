### ALR scrap explore ====

### Packages ----
# devtools::install_github('tidyverse/ggplot2')

pkgs <- c('data.table', 'ggplot2', 'patchwork')
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



# Notes to self:
# DT is edge_nn + number of individuals within threshold distance
# If nn > 500, the DI should be set to 0


### New variables ----
# Dyads within 500m 
DT[dyadDist >= 500, bin500m := TRUE]
DT[dyadDist < 500, bin500m := FALSE]

# Duplicated dyads
DT[, nDyadTime := .N, by = dyadTime]



# Global DI
DTsoc3[, globalDI := mean(di), .(season, dyadID)]
DTsoc3[, globalDIAngle := mean(diAngle), .(season, dyadID)]
DTsoc3[, globalDIDist := mean(diDist), .(season, dyadID)]


### Set DI to 0 if > 500m between dyads? ----
DT




### Plots ----
g1 <- ggplot(DTsoc3, aes(avgpreyRSF, globalDI, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc3, aes(avgpredatorRSF, globalDI, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g1 / g2



g1 <- ggplot(DTsoc3, aes(avgpreyRSF, globalDIAngle, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc3, aes(avgpredatorRSF, globalDIAngle, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g1 / g2


g1 <- ggplot(DTsoc3, aes(avgpreyRSF, globalDIDist, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc3, aes(avgpredatorRSF, globalDIDist, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  #facet_wrap(~cut_interval(dyadDist, 6)) + 
  labs(title = species) +
  geom_smooth(method = glm)

g1 / g2



### More plots ----
# nlNN_W.DI<-
#   glm(di ~ z.avgpreyRSF*z.avgpredatorRSF,
#       data = DTsoc3_W)

DTsoc3[, diffrsf := (avgpredatorRSF - avgpreyRSF) / (avgpredatorRSF + avgpreyRSF)]

ggplot(DTsoc3, aes(diffrsf, di, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  # facet_wrap(~season) + 
  labs(title = species) +
  geom_smooth(method = glm)


# ggplot(DTsoc3, aes(z.avgcoyoteRSF, z.avgpreyRSF)) + 
geom_point(size = 2, alpha = 0.1) +
  facet_wrap(~cut_interval(di, 2)) +
  theme_bw() +
  labs(title = species)

ggplot(DTsoc3, aes(avgpredatorRSF, avgpreyRSF)) + 
  geom_point(size = 2, alpha = 0.1) +
  facet_wrap(~cut_interval(di, 2)) +
  theme_bw() +
  labs(title = species)

ggplot(DTsoc3, aes(coyoteRSF, preyRSF)) + 
  geom_point(size = 2, alpha = 0.1) +
  facet_grid(season~cut_interval(di, 2)) +
  theme_bw() +
  labs(title = species)


g1 <- ggplot(DTsoc3, aes(z.avgbearRSF, di, color = z.avgpreyRSF)) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_steps2() +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = species)

g2 <- ggplot(DTsoc3, aes(z.avgcoyoteRSF, di, color = z.avgpreyRSF)) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_steps2() +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = species)

g1 / g2 +
  plot_layout(guides = 'collect')

### Triple plot
g1 <- ggplot(DTsoc3, aes(z.avgpredatorRSF, z.avgpreyRSF, color = di)) + 
  geom_point(size = 2, alpha = 0.1) +
  # scale_color_steps2() +
  facet_wrap(~season) +
  geom_smooth(method = glm, formula = di ~ z.avgpreyRSF*z.avgpredatorRSF) + 
  theme_bw() +
  labs(title = species)

g2 <- ggplot(DTsoc3, aes(z.avgpredatorRSF, di)) + 
  geom_point(size = 2, alpha = 0.1) +
  # scale_color_steps2() +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = species)

g3 <- ggplot(DTsoc3, aes(z.avgpreyRSF, di)) + 
  geom_point(size = 2, alpha = 0.1) +
  # scale_color_steps2() +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = species)

(g1 / g2  / g3 &
    geom_smooth(method = glm)) +
  plot_layout(guides = 'collect')


ggplot(DTsoc3, aes(z.avgpreyRSF, z.avgpredatorRSF, color = di), size = 5, alpha = 0.5) + 
  geom_point() +
  scale_color_steps2() +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = species)
# geom_smooth(method = glm)