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
caribou <- readRDS(paste0('output/4-sociality/', 'caribou', 'NNA-ALR.Rds'))
elk <- readRDS(paste0('output/4-sociality/', 'elk', 'NNA-ALR.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

caribou[, species := 'caribou']
elk[, species := 'elk']
DT <- rbindlist(list(caribou, elk), fill = TRUE)

if (truelength(DT) == 0) alloc.col(DT)

# Notes to self:
# DT is edge_nn + number of individuals within threshold distance
# If nn > 500, the DI should be set to 0

### Variables ----
# Z
# TODO: should this be seasonal? by species?
#
DT[, z.avgpreyRSF := scale(avgpreyRSF, center = T, scale = T)]
DT[, z.avgpredatorRSF := scale(avgpredatorRSF, center = T, scale = T)]

# By species*seasonal
DT[, z.avgpreyRSFBy := scale(avgpreyRSF, center = T, scale = T), .(species, season)]
DT[, z.avgpredatorRSFBy := scale(avgpredatorRSF, center = T, scale = T), .(species, season)]

# Dyads within 500m 
setnames(DT, 'distance', 'dyadDist')
DT[dyadDist >= 500, bin500m := TRUE]
DT[dyadDist < 500, bin500m := FALSE]

# Drop duplicated dyads
DT <- unique(DT[!is.na(NN)], by = c('dyadID', 'timegroup'))


# Set DI to 0 if > 500m between dyads
DT[dyadDist >= 100, di0 := 0]
DT[dyadDist < 100, di0 := di]


# Global DI
DT[, globalDI := mean(di0), .(season, dyadID)]
DT[, globalDIAngle := mean(diAngle), .(season, dyadID)]
DT[, globalDIDist := mean(diDist), .(season, dyadID)]

DTsoc <- DT[dyadDist < 500]

# Attempts at pred:prey domain
DTsoc[, absDifRSF := (predatorRSF - preyRSF) / (predatorRSF + preyRSF)]

ggplot(DTsoc, aes(dyadDist, di)) +
  geom_point() +
  geom_smooth(method = glm)

DTsoc[between(di, -0.2, 0.2)]

DTsoc[, qplot(dyadDist, di)]

g1 <- ggplot(DTsoc[!between(di, -0.2, 0.2)], aes(absDifRSF, di, color = season)) +
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc, aes(predatorRSF, di, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g3 <- ggplot(DTsoc, aes(preyRSF, di, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g1 / g2 / g3


# Dist vs diff in RSF
ggplot(DTsoc, aes(dyadDist, dpreyRSF)) +
  geom_point(color = 'black', alpha = 0.5) +
  geom_smooth(method = glm)

ggplot(DTsoc, aes(dyadDist, dpredatorRSF)) +
  geom_point(color = 'black', alpha = 0.5) +
  geom_smooth(method = glm)



# Global DI vs global average for dyads
g1 <- ggplot(DTsoc, aes(globavgpreyRSF, globalDI, color = season)) +
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc, aes(globavgpreyRSF, globalDIAngle, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g3 <- ggplot(DTsoc, aes(globavgpreyRSF, globalDIDist, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(species ~ season) +
  # labs(title = species) +
  geom_smooth(method = glm)

g1 / g2 / g3
ggsave('globals-prey-dropcut.png', width = 10, height = 10)

mod <- glm(di ~ avgpreyRSF*avgpredatorRSF + ,
    data = DTsoc)


melted <- melt(
  DTsoc,
  measure.vars = c(
    'z.avgpredatorRSFBy',
    'z.avgpreyRSFBy'),
  id.vars = c('species',
              'season',
              'dyadID',
              'timegroup')
)
melted[DTsoc, di := di, on = c('dyadID', 'timegroup')]

ggplot(melted, aes(value, di)) + 
  geom_point(color = 'grey', alpha = 0.5) +
  geom_smooth(method = glm) + 
  facet_grid(species + season ~ variable)
ggsave('scrap.png', width = 10, height = 4, dpi = 300)


### Sample land cover ----
rasters <- dir('output/1-data-prep/covariates/NL', full.names = TRUE)
names <- gsub('prep', '', gsub('.tif', '', dir('output/1-data-prep/covariates/NL')))

# Sample rasters
library(raster)
DTsoc[, (names) := lapply(rasters, FUN = function(r){
  extract(raster(r), matrix(c(EASTING, NORTHING), ncol = 2))})]

meltSoc <- melt(DTsoc, 
                measure.vars = names,
                id.vars =  c('species',
                             'season',
                             'dyadID',
                             'timegroup',
                             'di'))
pdf('scrap2.pdf')
lapply(names, function(nm) {
  ggplot(DTsoc, aes(get(nm), di)) + 
    geom_point() + 
    geom_smooth(method = glm) +
    xlab(nm)
})
dev.off()


### Plots ----
ggplot(DTsoc, aes(avgpreyRSF, avgpredatorRSF, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(season ~ species) +
  # labs(title = species) +
  geom_smooth(method = glm)


g1 <- ggplot(DTsoc, aes(avgpreyRSF, avgpredatorRSF, color = season)) +
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(season + species ~ cut_interval(di, 4)) +
  # labs(title = species) +
  geom_smooth(method = glm)

g2 <- ggplot(DTsoc, aes(avgpreyRSF, avgpredatorRSF, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(season ~ cut_interval(di, 4)) +
  # labs(title = species) +
  geom_smooth(method = glm)

g3 <- ggplot(DTsoc, aes(avgpreyRSF, avgpredatorRSF, color = season)) + 
  geom_point(color = 'grey', aes(shape = season)) + 
  facet_grid(season ~ cut_interval(di, 4)) +
  # labs(title = species) +
  geom_smooth(method = glm)

g1 / g2 / g3


### More plots ----
# nlNN_W.DI<-
#   glm(di ~ z.avgpreyRSF*z.avgpredatorRSF,
#       data = DTsoc3_W)

DTsoc3[, diffrsf := (avgpredatorRSF - avgpreyRSF) / (avgpredatorRSF + avgpreyRSF)]


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