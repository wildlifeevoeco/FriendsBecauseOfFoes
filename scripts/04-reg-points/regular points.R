pkgs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc', 'rgeos', 'lme4', 'car','piecewiseSEM')
p <- suppressPackageStartupMessages(lapply(
  pkgs,
  library,
  character.only = TRUE)
)

source("scripts/0-variables/variables.R")

generate_grid <- function(pol, spacing, crs) {
  rn <- n <- dif <- NULL
  
  if (missing(spacing))
    stop('spacing is missing')
  if (missing(crs))
    stop('crs is missing')
  if (missing(pol))
    stop('pol is missing')
  
  r <- raster::extent(pol)
  ra <- raster::raster(r)
  raster::res(ra) <- c(spacing, spacing)
  
  raster::projection(ra) <- sp::CRS(crs)
  
  rSP <- raster::rasterToPoints(ra, spatial = TRUE)
  
  data.table::data.table(
    rSP@coords[sp::over(pol, rSP, returnList = TRUE)[[1]], ]
  )
}


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