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



