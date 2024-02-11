### Packages ----
library(data.table)
library(raster)
library(rgeos)
library(lme4)
library(car)
library(piecewiseSEM)
library(adehabitatHR)

### Variables ----
source('scripts/0-variables/variables.R')


load("need_regpts.Rdata")

gridsize <-200

l <- grep('mcp', ls(), value = TRUE)
lsregpts <- lapply(l, function(m) {
  regPts <- generate_grid(
    pol = get(m),
    spacing = gridsize,
    crs = proj4string(get(m))
  )
  setnames(regPts, c('x', 'y'))
  saveRDS(regPts, paste0('input/regpts/', m, '_regpts_200m.Rds'))
  return(1)
})