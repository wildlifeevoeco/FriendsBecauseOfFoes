### ALR scrap explore - group size ====

### Packages ----
pkgs <- c('data.table', 'ggplot2', 'patchwork')
p <- suppressPackageStartupMessages(lapply(
  pkgs, 
  library, 
  character.only = TRUE)
)


### Input data ----
# Which species?
caribou <- readRDS(paste0('output/4-sociality/', 'caribou', 'NNA-group-ALR.Rds'))
elk <- readRDS(paste0('output/4-sociality/', 'elk', 'NNA-group-ALR.Rds'))

coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'


coordCols <- c('EASTING', 'NORTHING')
idCol <- 'id'

caribou[, species := 'caribou']
elk[, species := 'elk']
DT <- rbindlist(list(caribou, elk), fill = TRUE)

if (truelength(DT) == 0) alloc.col(DT)



### Averages ----
# TODO: Need .nn
# DT[, mean(predatorRSF), by = .(dyadID, timegroup)]



### Plots ----
DT


DTsoc <- DT[distance < 50 & nByGroup > 2]
g1 <- ggplot(DTsoc, aes(preyRSF, nByGroup))
g2 <- ggplot(DTsoc, aes(predatorRSF, nByGroup))
  

g <- g1 / g2 &
  facet_grid(species ~ season) &
  # facet_grid(cut_interval(distance, 4) ~  species + season, scales = 'free') &
  geom_point(color = 'grey') & 
  geom_smooth(method = glm)

ggsave('graphics/group-size.png', g, dpi = 100,
       height = 10)
