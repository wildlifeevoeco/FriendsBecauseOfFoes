library(data.table)
library(sp)
library(adehabitatHR)
library(raster)
library(rgeos)
library(lme4)
library(car)
library(piecewiseSEM)

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
  saveRDS(regPts, paste0(m, '_regpts_200m.Rds'))
  return(1)
})